setwd("~/projects/data-pipelines/scripts/indicators/seagrass/MMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "619d13a7-5df5-46e3-8391-50a2390e8df2"
txt_rid <- "a25672bd-15d8-4644-933f-3eaa9fe6b320"

#percent cover plots
png_MMP_overallpercentcover_rid <- "067ef164-58df-4845-94f1-9b626b6b0763"
png_MMP_overallpercentcover_fn <-"MMP_overallpercentcover.png"
png_MMP_percentcover_rid <-"4791979d-087e-4ef7-a819-7d5687d4dbe5"
png_MMP_percentcover_fn <-"MMP_percentcover.png"

################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid)
d<-Camera_data
names(d)[names(d) == 'Region'] <- 'Park'###Changes column name

####################################################################################################
#Define graphic properties
#####################################################################################################

pd <- position_dodge(0.1)
graphics = theme(axis.text.x=element_text(size = 12, angle=45, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
                 axis.title.x=element_text(size=15,face="bold"),
                 axis.title.y=element_text(size=15,face="bold"), #removes y axis title
                 axis.text.y=element_text(size = 12),
                 axis.line=element_line(colour="black"), #sets axis lines
                 plot.title =element_text(size = 15, hjust = 0.05),
                 panel.grid.minor = element_blank(), #removes minor grid lines
                 panel.grid.major = element_blank(), #removes major grid lines
                 panel.border=element_blank(), #removes border
                 panel.background=element_blank(), #needed to ensure integrity of axis lines
                 legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank())


##################################################################################
#Percent cover calculations for all data
##################################################################################
# All seagrass pooled

MMP = subset (d, Park=="Marmion Marine Park")

detach("package:dplyr", unload=TRUE)

cover=count(MMP, c("Location", "Site", "Year", "Level5Class")) #counts number of observations per site, per year
cover_obs=count(cover, c("Location", "Site", "Year"), "freq") #counts number of observations made at each site per year
cover_add <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
pos_cover = subset(cover_add, Level5Class %in% c("Posidonia sinuosa","Posidonia australis")) #Extracts cover information only
MMP_SG = count(pos_cover, c("Location", "Site", "Year", "freq.1"), "freq")
names(MMP_SG)[4] <- "total_count" #Rename column to make more sense
names(MMP_SG)[5] <- "pos_count" #Rename column to make more sense
MMP_SG$percent = MMP_SG$pos_count/MMP_SG$total_count *100 #Calculate percent cover

library(dplyr)

##################################################################################
#Create subsets for each 'sector (south, centre, north) for MMP
##################################################################################

MMP_south = subset(MMP_SG, Site %in% c("North Beach", "Sorrento"))
MMP_centre = subset(MMP_SG, Site %in% c("Hillarys Channel" , "Wreck Rock", "Mullaloo"))
MMP_north = subset(MMP_SG, Site %in% c("Ocean Reef Outer", "Ocean Reef Inner", "Burns Rocks"))

####################################################################################
#PERCENT COVER
####################################################################################

make_cover <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(percent)),
      mean = mean(percent, na.rm = TRUE),
      sd   = sd(percent, na.rm = TRUE),
      se   = sd(percent, na.rm = TRUE) / sqrt(N)
    )
}

#######################################################################
#overall cover

MMP_cover <- make_cover(MMP_SG)

MMP_percentcover_plot <- ggplot(MMP_cover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2011,2017,2), limits=c(min(2011),
                                                          max(2017))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

MMP_percentcover_plot

attach(MMP_cover)
MannKendall(mean)
detach(MMP_cover)

#################################################################
#MMP_North percent cover

MMP_north_cover <- make_cover(MMP_north)

MMP_north_percentcover_plot <- ggplot(MMP_north_cover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2011,2017,2), limits=c(min(2011),
                                                          max(2017))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("a) North")+
    # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

MMP_north_percentcover_plot

attach(MMP_north_cover)
MannKendall(mean)
detach(MMP_north_cover)

###########################################################################
#MMP_centre percent cover

MMP_centre_cover <- make_cover(MMP_centre)

MMP_centre_percentcover_plot <- ggplot(MMP_centre_cover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2011,2017,2), limits=c(min(2011),
                                                          max(2017))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("b) Centre")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

MMP_centre_percentcover_plot

attach(MMP_centre_cover)
MannKendall(mean)
detach(MMP_centre_cover)

###########################################################################
#MMP_south percent cover

MMP_south_cover <- make_cover(MMP_south)

MMP_south_percentcover_plot <- ggplot(MMP_south_cover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2011,2017,2), limits=c(min(2011),
                                                          max(2017))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("c) South")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

MMP_south_percentcover_plot

attach(MMP_south_cover)
MannKendall(mean)
detach(MMP_south_cover)

#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

png(png_MMP_overallpercentcover_fn, width=500, height=300)
grid.arrange(MMP_percentcover_plot)
dev.off()

png(png_MMP_percentcover_fn, width=500, height=900)
grid.arrange(MMP_north_percentcover_plot, MMP_centre_percentcover_plot, MMP_south_percentcover_plot, ncol=1)
dev.off()

#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_MMP_overallpercentcover_rid, png_MMP_overallpercentcover_fn)
ckanr::resource_update(png_MMP_percentcover_rid, png_MMP_percentcover_fn)
ckanr::resource_update(txt_rid, "percent_cover_code.R")
