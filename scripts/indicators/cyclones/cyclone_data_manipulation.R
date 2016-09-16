setwd("~/projects/data-pipelines/scripts/indicators/cyclones")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(dplyr)
library(rworldmap)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "8abacf5a-adbe-4754-9b8d-dca22b48f332"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date')) #NOT READING ALL CELLS!


##############################################################################
#Create a subset of data for a specific, named cyclone

cyclone = subset(d, NAME %in% c("Olwyn "))#Generates another data frame which is only the data from a specific cyclone
cyclone$order <- 1:nrow(cyclone) 
cyclone$name <- do.call(paste, c(cyclone[c("NAME", "order")], sep = "_"))
cyclone$year <- rep(2013,nrow(cyclone)) ##Change year value to when the cyclone was

#Generate lat/longs for mapping and prepare for export to GPS Visualiser

cyclonelat = cyclone[, c(81, 3, 8, 9)]
names(cyclonelat)[names(cyclonelat) == 'LAT'] <- 'latitude'
names(cyclonelat)[names(cyclonelat) == 'LON'] <- 'longitude'
names(cyclonelat)[names(cyclonelat) == 'TM'] <- 'desc'
write.table(cyclonelat, "cyclonelat.csv", row.names = FALSE, sep = ",")

#OR plot on r-map

newmap <- getMap(resolution = "high")
plot(newmap, xlim = c(110, 150), ylim = c(-43, -12), asp = 1)
points(cyclonelat$longitude, cyclonelat$latitude, col = "black", cex = .6)

#Extract specific data from a perimeter of interest

cycl_perimeter=cyclone[34:35, c(82, 1, 3, 17, 8, 9)] #type in the specific range of row numbers ([a:e, c(...)]) of interest for each perimeter
write.table(cycl_perimeter, "cyclone_perimeter.csv", row.names = FALSE, sep = ",")

# STILL NEED A WAY TO APPEND THESE RESULTS TO CYCLONE SUMMARY FILE
## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "cyclones_MBIMP.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")

