#' Download NOAA Coral Reef Watch Virtual Station Data.
#' @author Bart.Huntley@dpaw.wa.gov.au 01/04/2015
#' @author Florian.Mayer@dpaw.wa.gov.au

require(lubridate) || install.packages("lubridate")
require(dplyr) || install.packages("dplyr")
require(plyr) || install.packages("plyr")
require(ckanr)
setwd("~/projects/data-pipelines/scripts/indicators/water_temp")

#------------------------------------------------------------------------------#
# Config
#' A config for NOAA datasets
#' 
#' Structure: Three levels of named lists
#' Level 1: A list of lists, one per NOAA station
#' Level 2: 
#'  stations = NOAA virtual station names, each has a distinct download URL
#'  sites = named list (level 3)
#' Level 3: 
#'  site = MPA site name
#'  (rest) = model formula parameters
stations <- list(
#   list(
#     station = "MontgomeryReef", 
#     sites = NULL),
  list(
    resid = "2f123c1b-6c6f-432d-a0a9-d1b30287604b",
    station="RowleyShoals", 
    sites = list(
      list(site = "Clerke", 
           INT = 2.077, SST0 = 0.338, SST1 = 0.588, SST2 = 0, SIN = 0.157, COS = 0.328),
      list(site = "Imperieuse", 
           INT = 1.029, SST0 = 0.362, SST1 = 0.592, SST2 = 0, SIN = 0.142, COS = 0.168),
      list(site = "Mermaid",
           INT = 3.159, SST0 = 0.282, SST1 = 0.603, SST2 = 0, SIN = 0.07, COS = 0.165)  
    )
  ),
  list(
    resid = "d2dcbf39-0c42-4864-93d0-90abd50b6760",
    station = "EightyMileBeach", 
    sites = NULL),
  list(
    resid = "ccd17e18-ef9f-4509-bd51-8de3988ec6b0",
    station = "DampierArchipelago", 
    sites = NULL),
  list(
    resid = "f4b3ca65-4945-4ef1-9c11-24a9a299ecd2",
    station = "MontebelloIslands",
    sites = list(
      list(site = "Ah Chong Island", 
           INT = -3.902, SST0 = 0.585, SST1 = 0.567, SST2 = 0, SIN = -0.072, COS = 0.46),
      list(site = "Lowendal Shelf",
           INT = -1.84, SST0 = 0.279, SST1 = 0.756, SST2 = 0, SIN = 0.167, COS = 1.368),
      list(site = "Dugong Reef",
           INT = -2.395, SST0 = 0.319, SST1 = 0.736, SST2 = 0, SIN = 0.288, COS = 1.651),
      list(site = "Southern Barrow Shoals", 
           INT = -15.43, SST0 = 0.804, SST1 = 0.737, SST2 = 0, SIN = -0.508, COS = 0.989)
    )
  ),
  list(
    resid = "53cb93c3-c2a0-410a-af38-a94e0a74b3f9",
    station = "Ningaloo",
    sites = list(
      list(site = "Point Murat",
           INT = -5.86, SST0 = 0.381, SST1 = 0.801, SST2 = 0, SIN = -0.02, COS = 1.334)
    )
  ),
  list(
    resid = "4856e417-f6c4-4b61-887e-536107a91f89",
    station = "CoralBay", 
    sites = NULL),
  list(
    resid = "cea2c210-353a-40b4-9093-937002bbca60",
    station = "SharkBay", 
    sites = NULL),
  list(
    resid = "3da1eaa8-e8c1-4fe2-8eb4-70472c3dd33e",
    station = "JurienBay", 
    sites = NULL),
  list(
    resid = "2ad65f43-fe7a-4c8b-b700-3eea2eb95699",
    station = "Marmion",
    sites = list( 
      list(site = "Hillarys",
           INT = 7.859, SST0 = 0.005, SST1 = 0.29, SST2 = 0.276, SIN = 1.192, COS = 0.06)
    )
  ),
  list(
    resid = "2dec5601-46d5-4169-bfa9-2f771e1d125e",
    station = "ShoalwaterIslands", 
    sites = NULL),
  list(
    resid = "2a1b9cbd-d3a6-4fbf-b25b-5428e400d29e",
    station = "NgariCapes", 
    sites = NULL)
)

#------------------------------------------------------------------------------#
# Data loading

#' Generate NOAA url for a given station
#' 
#' Example URL: http://coralreefwatch.noaa.gov/satellite/vs_added/
#' data_timeseries/vs_wa_ts_MontgomeryReef_Australia.txt
#' 
#' @param station The station name as exactly used in file path
#' @return A complete URL to a NOAA virtual station
make_noaa_url <- function(station){
  paste0("http://coralreefwatch.noaa.gov/satellite/vs_added/",
         "data_timeseries/vs_wa_ts_",station,"_Australia.txt")
}


#' Load CSV data from a URL and guess variable classes
#'
#' Reads numbers as numeric
#' Reads strings as factor
#' Converts column with names indicating date format ("Date", "date") to POSIXct
#' Will result in either numeric, POSIXct, or string/factor classes
#'
#' Date is parsed with lubridate::parse_date_time
#' 
#' The URL is generated from the station name and the NOAA URL pattern.
#' Data are read without header, as the header mismatches the column number,
#' including the unquoted (gaah) strings at the end (cols 15:17).
#' The station name (cols 15:17) is discarded.
#' Helper variables for SST of thre previous one to three observations are added,
#' where there are no previous observations at the beginning of the data frame, 
#' the first observation is used.
#' Column names are restored.
#' New columns are added:
#' date - The start date (good enough to plot) with standard name "date"
#' date.start - The start date again with exlanatory name
#' date.end - The end date 
#' site - The station_name
#' Columns with individual date parts are discarded.
#'
#' @param station_name A valid station name
#' @param ldo Lubridate date orders, default: "YmdHMSz", "YmdHMS","Ymd","dmY"
#' @param ltz Lubridate time zone, default: "Australia/Perth"
#' @param dcn Date column names, default: "date", "Date"
get_data_noaa_sst <- function(station_name, verbose=T){
  url <- make_noaa_url(station_name)
  if(verbose) print(paste("Downloading", url))
  df <- read.table(url, skip=1)
  
  # Discard station name, add previous observations for modelling convenience
  df <- cbind(
    df[,1:14],
    c(df[1, 9], df[1:nrow(df)-1, 9]),
    c(df[1:2, 9], df[2:nrow(df)-2, 9]),
    c(df[1:3, 9], df[3:nrow(df)-3, 9])
  )  
  
  # Restore column names
  names(df) <- c("BYYY", "BM", "BD", "BH", "EYYY", "EM", "ED", "EH",  
                 "SST", "SSTANOM", "HOTSPOT", "DHW", "Lat", "Lon",
                 "SSTm1","SSTm2","SSTm3")
  
  # Parse dates, add station_name, apply custom models, select columns
  df <- df %>%
    dplyr::mutate( 
      date.start = lubridate::parse_date_time(paste(BYYY, BM, BD), "Ymd"),
      date.end = lubridate::parse_date_time(paste(EYYY, EM, ED), "Ymd"),
      DOYp7 = yday(lubridate::parse_date_time(paste(BYYY, BM, BD), "Ymd")) + 7,
      noaa_station = station_name
    ) %>%
    dplyr::select(
      date.start, date.end, 
      #mISST, 
      SST, SSTANOM, HOTSPOT, DHW, Lat, Lon, noaa_station, 
      SSTm1, SSTm2, SSTm3, DOYp7)
  df
}


#------------------------------------------------------------------------------#
# Data modelling
# Functions to apply mISST model to NOAA data for one site, one station, all stations
#
#' Calculate mISST from NOAA SST data for one site
#' 
#' For each named list inside sites_config, noaa_data will be replicated with 
#' the list's name as factor level name, and the list's parameters applied to
#' a model formula, which then is run on noaa_data's column "SST" and added as 
#' modelled in situ seawater temperature "mISST".
#' 
#' @param data Data from get_data_noaa_sst(station_name)
#' @param site_pars Model parameters as named list, e.g. stations[[2]]$sites[[1]]
model_misst_for_one_site <- function(site_pars, data){
  data <- data %>%
    dplyr::mutate( 
      site = as.character(site_pars$site),
      mISST = (site_pars$INT 
               + SST*site_pars$SST0 
               + SSTm1*site_pars$SST1 
               + SSTm2*site_pars$SST2 
               + site_pars$SIN*sin(2*pi*(DOYp7/365)) 
               + site_pars$COS*cos(2*pi*(DOYp7/365)))
    ) %>%
    dplyr::select(
      date.start, date.end, 
      mISST, SST, SSTANOM, HOTSPOT, DHW, 
      Lat, Lon, 
      site, noaa_station)
  data
}


#' Model mISST from NOAA SST data for all sites within one station
#' 
#' Run model_misst_for_one_site on data for each site, rbind all resulting df.
#' Creates and writes to CSV one data.frame with data from 
#' get_data_noaa_sst(station_name) plus two new variables "site" and "mISST", 
#' with nrow = length(pars$sites) * nrow(data).
#' 
#' @param data Data from get_data_noaa_sst(station_name)
#' @param station_pars Station and Site names plus corresponding model parameters 
#'  as nested named lists, e.g. stations[[2]]
model_misst_for_one_station <- function(station_pars){
  data <- get_data_noaa_sst(station_pars$station)
  if(!is.null(station_pars$sites)){
    data_list <- lapply(station_pars$sites, model_misst_for_one_site, data=data)
    data <- plyr::ldply(data_list)
  }
  write.csv(data, file=paste0("data_", station_pars$station, ".csv"), row.names=F)
}


#' Model mISST for all stations as per configuration
#' 
#' Data will be downloaded from NOAA, manipulated, and written as CSV to the 
#' working directory.
#' 
#' @param pars The configuration, a nested list of lists like `stations`
model_misst_for_all_stations <- function(pars){
  output <- lapply(pars, model_misst_for_one_station)
}

#' Upload one CSV file to CKAN
#' 
#' @param station_pars Station and Site names plus corresponding model parameters 
#'  as nested named lists, e.g. stations[[2]]
upload_one_file <- function(station_pars){
  r <- ckanr::resource_update(station_pars$resid,
                         paste0("data_", station_pars$station, ".csv"))
  print(paste("Updated Resource", r$name))
  # TODO: update dataset: luo,lub
}

#' Upload all files to CKAN
#' 
#' @param stations A config of all stations
upload_all_files <- function(stations){
  d <- lapply(stations, upload_one_file)
}

# Run code:
model_misst_for_all_stations(stations)
upload_all_files(stations)
setwd("~/projects/data-pipelines")

