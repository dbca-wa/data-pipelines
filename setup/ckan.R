#' R functions to retrieve content from CKAN
#' @author Florian.Mayer@dpaw.wa.gov.au
#'
if (!require("pacman")) install.packages("pacman")
pacman::p_load(bitops,
               caTools,
               ckanr,
               colorspace,
               dplyr,
               ggplot2,
               Hmisc,
               knitr,
               lubridate,
               markdown,
               readr,
               rjson,
               RColorBrewer,
               RCurl,
               scales,
               shiny,
               stringr,
               update=F)

if (file.exists("~/projects/data-pipelines/setup/ckan_setup.R")) source("~/projects/data-pipelines/setup/ckan_setup.R")

# Default to DPaW internal data catalogue
ckanr::ckanr_setup(url="http://internal-data.dpaw.wa.gov.au/")
default_resource_id <- "ccc68eb7-8105-4cc8-8112-57bf1558e82f"

#' Get CPR data from intermediary CSV by park and asset slug
#'
#' Requires a CSV with raw data at the speficied or supplied URL
#' The CSV used here is created in ../reports/publish.py
#'
#' @param prk The park name as String
#' @param ast The asset name as String
#' @param url The URL of the CSV
#' @return a data.frame with one row of factors park, asset, and the six CPR
#' indicator flags.
#' @export
getCPR <- function(prk, ast, url = "../reports/cpr.csv"
                   #url = ckan_res("ece209a7-5974-4e04-9711-ee6457bc4a59")$url
){
  cpr = read.table(url, header=TRUE, skip=0, sep=",")
  subset(cpr, park == prk & asset == ast)
}

#' Texify a string of text
#'
#' Substitutes characters with special meaning in Latex
#' with their Latex counterpart.
#'
#' @param text A string of text to be substituted.
#' @return The input text after substitution.
#' @import Hmisc
#' @export
#' @usage
#' texify("Testing % ~ $ & _ m2 sqm mL-1 L-1 < > ≥ ≤ ±")
texify <- function(text){
  text = gsub("%", "\\\\%", text)
  text = gsub("~", "\\\\~", text)
  #text = gsub("$", "\\\\$", text)
  text = gsub("&", "\\\\&", text)
  text = gsub("_", " ", text)
  text = gsub("R2", "$R^2$", text)
  text = gsub("m2", "$m^2$", text)
  text = gsub("m-2", "$m^-2$", text)
  text = gsub("sqm ", "$m^2$ ", text)
  text = gsub(" mL-1 ", " $mL^{-1}$ ", text)
  text = gsub(" L-1 ", " $L^{-1}$ ", text)
  text = gsub("<", "$<$", text)
  text = gsub(">", "$>$", text)
  text = gsub("≥", "$\\\\geq$", text)
  text = gsub("≤", "$\\\\leq$", text)
  text = gsub("±", "$\\\\pm$", text)
  text = gsub("+/-", "$\\\\pm$", text)
  text = gsub("=", "$=$", text)
  text = gsub("°", "$\\\\textdegree$", text)


  # add more substitutions as required
  return(Hmisc::escapeBS(text))
}

#' Return the value of a key in a dictionary or display a "missing" message
#'
#' @param dict A dictionary, as returned by ckanr::package_show
#' @param key A key as quoted string
#' @param texify Whether to convert the value from markdown to latex (default: F)
get_key <- function(dict, key, texify=F){
  val = dict[[key]]
  if (is.null(val)) {
    val = "not available"
    print(paste("Missing key:", key, "at", dict$url))
  }
  if (texify==T){ val <- texify(val)}
  val
}

#' Return a named list of metadata of a CKAN resource and its dataset
#'
#' Onto a named list containing CKAN API's resource_show() response result,
#' various useful bits are added, such as the resource's package_show result,
#' as well as vigorously abbreviated important bits of metadata used in the
#' Latex macro `ckr`.
#'
#' @param resource_id An existing CKAN resource id
#' @param url The base url of the resource's CKAN catalogue,
#'  optional, default: configured ckanr default
#' @return A named list containing resource and dataset metadata:
#'  top level:
#'    23 default `ckanr::resource_show` keys
#'    d (containing 34 default `ckanr::package_show` keys)
#'    ind - the dataset's title (heading, texified)
#'    syn - the dataset's description (synopsis, texified)
#'    pth - the local file path if the url is downloaded with wget
#'    cap - the resource's description (caption, texified)
#'    ori - the dataset's url
#'    src - the dataset's extra field "citation", texified
#'    luo <- the dataset's "last updated on"
#'    lub <- the dataset's maintainer (last updated by)
#'
ckan_res <- function(resource_id,
                     url=get_default_url()){
  if (resource_id == "") resource_id <- default_resource_id
  r <- ckanr::resource_show(resource_id, url = url)
  d <- ckanr::package_show(r$package_id, url = url)
  r$d <- d
  r$ind <- texify(d$title)
  r$syn <- ""#texify(d$notes) # a hack to exclude the unused synopsis
  r$pth <- strsplit(r$url, "//")[[1]][2]
  r$cap <- texify(r$description)
  r$ori <- paste0(url, "dataset/", d$id)
  r$src <- texify(d$citation) #get_key(d, "citation", texify=T)
  r$luo <- d$last_updated_on #get_key(d, "last_updated_on")
  r$lub <- d$maintainer
  r
}

#' Return an R data.frame with content from CKAN as required for MPA Figures
#'
#' @param urlstring The resource id of the figure itself.
#' @param type The Indicator type (Condition, Pressure, Response) - kept for
#'  backwards compatibility
#' @param debug Debug noise
#' @param url The CKAN base url, optional, default: `ckanr::get_default_url()`
mpa <- function(url,
                type = 'Condition', debug = FALSE, ckanurl=get_default_url()){
  r <- ckan_res(strsplit(url, '/')[[1]][7])
  r
}

#' Load a CSV from a URL
#'
#' @param url The URL of a CSV file
#' @param date_colnames The column names of date columns, default:
#'    'date', 'Date', date.start', 'date.end', 'year', 'Year'
#' @param date_formats The date formats to expect, default:
#'    'YmdHMSz', 'YmdHMS','Ymd','dmY', 'Y'
#' @param timezone The timezone, default: 'Australia/Perth'
#' @return A data.frame of the CSV, with parsed dates and strings as factors
load_csv <- function(url,
                     date_colnames = c('date', 'Date',
                                       'date.start', 'date.end',
                                       'start.date','end.date',
                                       'year', 'Year'),
                     date_formats = c('YmdHMSz', 'YmdHMS','Ymd','dmY', 'Y'),
                     timezone = 'Australia/Perth'){
  df <-read.table(url, sep = ',', header = T, stringsAsFactors = T)
  cn <- names(df)
  df[cn %in% date_colnames] <- lapply(
    df[cn %in% date_colnames],
    function(x){x<- lubridate::parse_date_time(x,
                                               orders = date_formats,
                                               tz = timezone)}
  )
  names(df) <- Hmisc::capitalize(names(df))
  df
}

#' Load CSV from CKAN given a resource id
#'
#' @param res_id The resource id of a CKAN CSV resource
#' @param ckanurl The base url of the CKAN catalogue, default: ckanr::get_default_url()
#' @param parse_dates Whether to parse date_colnames as dates of format date_formats
#'    into PosixCt
#' @param date_colnames The column names of date columns, default: 'date', 'Date',
#'    'date.start', 'date.end', 'year', 'Year'
#' @param date_formats The date formats to expect, default: 'YmdHMSz', 'YmdHMS','Ymd','dmY', 'Y'
#' @param timezone The timezone, default: 'Australia/Perth'
#' @return A data.frame of the CSV, with parsed dates and strings as factors
load_ckan_csv <- function(res_id,
                          ckanurl = ckanr::get_default_url(),
                          parse_dates = T,
                          date_colnames = c('date', 'Date',
                                            'date.start', 'date.end',
                                            'start.date','end.date',
                                            'year', 'Year'),
                          date_formats = c('YmdHMSz', 'YmdHMS','Ymd','dmY', 'Y'),
                          timezone = 'Australia/Perth'){
  r <- ckan_res(res_id, url = ckanurl)
  df <- load_csv(r$url,
                 date_colnames = date_colnames,
                 date_formats = date_formats,
                 timezone = timezone)
  df
}
