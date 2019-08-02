#
#Clear environment
rm(list=ls()) 

# *** IMPORTANT ***
# Change upper limit of memory to be able to import the file
# This step needs to be done every time you open R Studio and before loading the package

options(java.parameters = "- Xmx1024m")

## PACKAGES ----

library(xlsx) # open xlsx files
library(plyr) # tools to split & combine data
library(progress) # create progress bar
library(ggplot2) # make plots
#library(dplyr) #more tools to split and combine data 


## OPTIONS ----

# Set the working directory (must use forward slash / )
setwd("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Seagrass/Data/Precision Analysis 2018")

#
# *** IMPORTANT ***
#
# Create a folder in the working directory to save your output files
# Name of folder here:
outputFolder <- "Precision Analysis Output Files"

# Name of the data file
thedate <- strftime(Sys.Date(),"%y%m%d") #retrieve the date to go into file name
fileName <- paste0("DampierSeagrass2018_PrecisionAnalysis_",thedate,".xlsx")
  
# Number of iterations to run
iterat <- 5

# Name of produced output files
lvl1 <- "StatsOutput_Level1.xlsx"



## FUNCTIONS ----

# Calculate standard error of the mean
sem <- function(x) sqrt(var(x)/length(x))



## READ DATA FILE ----

#import whole datasheet
sg <-read.csv("DMP_SEAGRASS_2018_PRECISIONANALYSIS_Dot Point Measurements.csv", header=TRUE) #note: this is for reading a CSV rather than excel spreadsheet

# Check if file was imported correctly
head(sg)



## MAIN CODE ----

# Get Site names
nSites <- unique( as.character(sg$Site) )

# Get major category names (Level 1)
level1Catg <- unique( as.character(sg$Seagrass) )

# Number of points per photo
maxPoints <- max(sg$PointNo)

# Create progress bar to show loop progression
pb <- progress_bar$new(format = " Running loop [:bar] :percent completion in :eta",
                       total = length(nSites), clear = FALSE, width= 80)


## PREPARE OUTPUT ----

# Create empty data frame where we will save mean and SE for all sites (long format)
# Columns will be SiteCode, Points used, Level1Class or Level3Class, Mean, and SEM, so adding 5 columns
statsLv1DF <- matrix(NA, nrow = 0, ncol = 5)



## RUN CALCULATIONS ----


# *****
# Loop for each site
for (iSite in nSites) {

  # Display progress bar on screen
  pb$tick()  
  
  
  # Subset main data frame for the site
  siteDF <- sg[sg$Site == iSite, ]
  
  # Get image list from the Site
  nImages <- as.character( unique(siteDF$Image) )

  # Start a countdown of points to remove
  for (iPoints in maxPoints : 0 ) {
  
  
      # Define iterations to run
      # when we are using all 20 points there is only one proportion that we can obtain, so we run one iteration
      # after we start subtracting points we run the iterations defined above in Options
      if (iPoints == maxPoints) {
        nIterat = 1
      } else {
        nIterat = iterat  }

    
      # Empty matrix to append and save all iterations
      # SiteCode, Level Class, Freq, Prop, and Iteration so 5 columns
      propsL1DF <- matrix(NA, nrow = 0, ncol = 5)
      
      
      # *****
      # Run multiple iterations
      for (iIterat in 1:nIterat) {
        
        
        # Sample n points from the points available in each image (iPoints+1 since the first is zero)
        # Repeat random selection for each image
        randomPts <- rep( sort(sample(x = 0:maxPoints, size =  iPoints+1, replace = FALSE)), times = length(nImages) )
        # Subset site data frame by selected points
        subSiteDF <- siteDF[siteDF$PointNo %in% randomPts, ]
        
        # Create an empty matrix to merge the proportions of all iterations per site
        # Data here will be replaced every iteration
        iteratL1DF <- data.frame("Site" = iSite, "Seagrass" = level1Catg)
        
        # Calculate frequency of Level 1 or 3 categories for the site
        level1Stat <- count(subSiteDF, vars = c("Site", "Seagrass"))

        # Calculate proportion of Levels 1 and 3 over all images
        level1Stat$Prop <- ( level1Stat$freq / nrow(subSiteDF) ) * 100


        # Append data of this iteration into the data frame to average iterations
        iteratL1DF <- merge(iteratL1DF, level1Stat, all = TRUE)

        # Set zero counts
        iteratL1DF[is.na(iteratL1DF)] <- 0
        
        # Add iteration count in a new column
        iteratL1DF$Iterat <- iIterat
        
        
        # Save them in a main data frame to export
        propsL1DF <- rbind(propsL1DF, iteratL1DF)

        
        # Clean environment
        rm(subSiteDF); rm(level1Stat); rm(iteratL1DF); rm(randomPts)

        
      } # close iterations loop

      # clean environment
      rm(iIterat); rm(nIterat)
      
      # Export raw iteration data for each site
      write.csv(propsL1DF, paste0(outputFolder, "/", iSite, "_", iPoints+1, "Pts_iter_Level1.csv"), row.names = FALSE, quote = FALSE) 


      # *****
      # CALCULATE MEAN AND SEM
      
      # Level 1 Categories  
      statsLv1 <- aggregate(propsL1DF$Prop, by = list(propsL1DF$Seagrass), mean)
      statsLv1$SE <- aggregate(propsL1DF$Prop, by = list(propsL1DF$Seagrass), sem)[[2]]
      # Add a column with this site name
      statsLv1 <- cbind(rep(iSite, nrow(statsLv1)), rep(iPoints+1, nrow(statsLv1)), statsLv1)
      # Change default column names
      colnames(statsLv1) = c("Site", "nPoints", "Seagrass", "Mean", "SE")

      
      # Save Site stats into main data frame
      statsLv1DF <- rbind(statsLv1DF, statsLv1)
      
      
      # Clean environment
      rm(propsL1DF); rm(statsLv1)
  } # close points loop
    
    # Clean environment
    rm(iPoints); rm(siteDF)

} # close sites loop

# Clean environment
rm(iSite); rm(pb); rm(iterat); rm(maxPoints)



## EXPORT OUTPUT ----

# Saving to working directory
write.xlsx(statsLv1DF, lvl1, col.names = TRUE, row.names = FALSE)


## DISPLAY OUTPUT ----


# *****
# LEVEL 1 CATEGORIES
p1 <- ggplot(subset(statsLv1DF, Site == "MALUS"), aes(Seagrass, Mean))

p1 + 
  geom_point(aes(colour = factor(nPoints))) +
  facet_wrap(~ Site)

