setwd("~/projects/data-pipelines/scripts/indicators/seagrass/MMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(gridExtra)
library(ggplot2)
library (plyr)
library(Kendall)

######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"#CKAN resource ID for data
txt_rid <- "b6b3ded6-40e1-4449-be54-6745800ad4d7"#CKAN resource ID for r-script

#Shoot density plots
png_shoot_density_rid <- "4d6b35db-b49a-4804-a293-314bfc123dd0"#CKAN resource ID for final figure (png)
png_shoot_density_fn = "MMP shoot density.png"#Name of final figure
png_MMP_shoot_density_rid <- "7b595274-5ca6-412a-a691-c910b60e87a2"#CKAN resource ID for final figure (png)
png_MMP_shoot_density_fn = "MMP overall shoot density.png"#Name of final figure

#Maximum canopy height plots
png_height_rid <- "7582fa4d-5982-4f1f-9e2c-6701f4da8da7"#CKAN resource ID for final figure (png)
png_height_fn = "MMP height.png"#Name of final figure
png_MMP_height_rid <- "e316d75c-48e9-4ae5-8cb8-2201ff9cb74e"#CKAN resource ID for final figure (png)
png_MMP_height_fn = "MMP overall height.png"#Name of final figure

###################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid)

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
#Create subsets for each 'sector (south, centre, north) for MMP
##################################################################################

MMP = subset(d, Park %in% c("MMP"))
MMP_south = subset(d, Site %in% c("North Beach", "Sorrento"))
MMP_centre = subset(d, Site %in% c("Hillarys Channel" , "Wreck Rock", "Mullaloo"))
MMP_north = subset(d, Site %in% c("Ocean Reef Outer", "Ocean Reef Inner", "Burns Rocks"))

####################################################################################
#SHOOT DENSITY
####################################################################################

make_density <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(Posidonia_sinuosa)),
      mean = mean(Posidonia_sinuosa, na.rm = TRUE),
      sd   = sd(Posidonia_sinuosa, na.rm = TRUE),
      se   = sd(Posidonia_sinuosa, na.rm = TRUE) / sqrt(N)
    )
}

#OverallShoot density

MMP_shootdensity <- make_density(MMP)

MMP_shootdensity_plot <- ggplot(MMP_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_shootdensity$Year-0.125), max(MMP_shootdensity$Year+0.125)), breaks=min(MMP_shootdensity$Year):max(MMP_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(25)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  #ggtitle("All MMP")+
  theme_bw() + graphics

MMP_shootdensity_plot

##############################################################
#MMP_north Shoot density

MMP_north_shootdensity <- make_density(MMP_north)

MMP_north_shootdensity_plot <- ggplot(MMP_north_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_north_shootdensity$Year-0.125), max(MMP_north_shootdensity$Year+0.125)), breaks=min(MMP_north_shootdensity$Year):max(MMP_north_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(25)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("a) North")+
  theme_bw() + graphics

MMP_north_shootdensity_plot

#############################################################
#MMP_centre shoot density

MMP_centre_shootdensity <- make_density(MMP_centre)

MMP_centre_shootdensity_plot<-ggplot(MMP_centre_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_centre_shootdensity$Year-0.125), max(MMP_centre_shootdensity$Year+0.125)), breaks=min(MMP_centre_shootdensity$Year):max(MMP_centre_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(25)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("b) Centre")+
  theme_bw() + graphics

MMP_centre_shootdensity_plot

#################################################################
#MMP_south shoot density

MMP_south_shootdensity <- make_density(MMP_south)

MMP_south_shootdensity_plot<-ggplot(MMP_south_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_south_shootdensity$Year-0.125), max(MMP_south_shootdensity$Year+0.125)), breaks=min(MMP_south_shootdensity$Year):max(MMP_south_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(25)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("c) South")+
  theme_bw() + graphics

MMP_south_shootdensity_plot

####################################################################################
#MAXIMUM CANOPY HEIGHT
####################################################################################

make_maxheight <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(Maximum_height_mm)),
      mean = mean(Maximum_height_mm, na.rm = TRUE),
      sd   = sd(Maximum_height_mm, na.rm = TRUE),
      se   = sd(Maximum_height_mm, na.rm = TRUE) / sqrt(N)
    )
}

###########################################################
#Overall maximum canopy height density

MMP_maxheight <- make_maxheight(MMP)

MMP_maxheight_plot <- ggplot(MMP_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_maxheight$Year-0.125), max(MMP_maxheight$Year+0.125)), breaks=min(MMP_maxheight$Year):max(MMP_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) max. height (mm)", sep = ""))) +
  #ggtitle("All MMP")+
  theme_bw() + graphics

MMP_maxheight_plot

#########################################################
#MMP_south max canopy height

MMP_south_maxheight <- make_density(MMP_south)

MMP_south_maxheight_plot <- ggplot(MMP_south_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_south_maxheight$Year-0.125), max(MMP_south_maxheight$Year+0.125)), breaks=min(MMP_south_maxheight$Year):max(MMP_south_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) max. height (mm)", sep = ""))) +
  ggtitle("c) South")+
  theme_bw() + graphics

MMP_south_maxheight_plot

#############################################################
#MMP_centre maximum canopy height

MMP_centre_maxheight <- make_maxheight(MMP_centre)

MMP_centre_maxheight_plot <- ggplot(MMP_centre_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_centre_maxheight$Year-0.125), max(MMP_centre_maxheight$Year+0.125)), breaks=min(MMP_centre_maxheight$Year):max(MMP_centre_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) max. height (mm)", sep = ""))) +
  ggtitle("b) Centre")+
  theme_bw() + graphics

MMP_centre_maxheight_plot

#################################################################
#MMP_north maximum canopy height

MMP_north_maxheight <- make_maxheight(MMP_north)

MMP_north_maxheight_plot <- ggplot(MMP_north_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_north_maxheight$Year-0.125), max(MMP_north_maxheight$Year+0.125)), breaks=min(MMP_north_maxheight$Year):max(MMP_north_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) max. height (mm)", sep = ""))) +
  ggtitle("c) North")+
  theme_bw() + graphics

MMP_north_maxheight_plot


####################################################################################
#MEAN CANOPY HEIGHT
####################################################################################

make_meanheight <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(Mean_height_mm)),
      mean = mean(Mean_height_mm, na.rm = TRUE),
      sd   = sd(Mean_height_mm, na.rm = TRUE),
      se   = sd(Mean_height_mm, na.rm = TRUE) / sqrt(N)
    )
}

#Overall mean canopy height density

MMP_meanheight <- make_meanheight(MMP)

MMP_meanheight_plot <- ggplot(MMP_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_meanheight$Year-0.125), max(MMP_meanheight$Year+0.125)), breaks=min(MMP_meanheight$Year):max(MMP_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) 80th percentile height (mm)", sep = ""))) +
  #ggtitle("All MMP")+
  theme_bw() + graphics

MMP_meanheight_plot

###################################################################
#MMP_south mean canopy height

MMP_south_meanheight <- make_meanheight(MMP_south)

MMP_south_meanheight_plot <- ggplot(MMP_south_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_south_meanheight$Year-0.125), max(MMP_south_meanheight$Year+0.125)), breaks=min(MMP_south_meanheight$Year):max(MMP_south_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) 80th percentile height (mm)", sep = ""))) +
  # ggtitle("c) South")+
  theme_bw() + graphics

MMP_south_meanheight_plot

#############################################################
#MMP_centre mean canopy height

MMP_centre_meanheight <- make_meanheight(MMP_centre)

MMP_centre_meanheight_plot <- ggplot(MMP_centre_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_centre_meanheight$Year-0.125), max(MMP_centre_meanheight$Year+0.125)), breaks=min(MMP_centre_meanheight$Year):max(MMP_centre_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) 80th percentile height (mm)", sep = ""))) +
  # ggtitle("b) Centre")+
  theme_bw() + graphics

MMP_centre_meanheight_plot

#################################################################
#MMP_north mean canopy height

MMP_north_meanheight <- make_meanheight(MMP_north)

MMP_north_meanheight_plot <- ggplot(MMP_north_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(MMP_north_meanheight$Year-0.125), max(MMP_north_meanheight$Year+0.125)), breaks=min(MMP_north_meanheight$Year):max(MMP_north_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) 80th percentile height (mm)", sep = ""))) +
  # ggtitle("a) North")+
  theme_bw() + graphics

MMP_north_meanheight_plot

#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

#Shoot density

png(png_MMP_shoot_density_fn, width=500, height=300)
grid.arrange(MMP_shootdensity_plot)
dev.off()

png(png_shoot_density_fn, width=500, height=900)
grid.arrange(MMP_north_shootdensity_plot, MMP_centre_shootdensity_plot, MMP_south_shootdensity_plot, ncol=1)
dev.off()

#Maximum canopy height
png(png_MMP_height_fn, width=500, height=600)
grid.arrange(MMP_maxheight_plot, MMP_meanheight_plot, ncol=1)
dev.off()

png(png_height_fn, width=1000, height=900)
grid.arrange(MMP_north_maxheight_plot, MMP_north_meanheight_plot, MMP_centre_maxheight_plot, MMP_centre_meanheight_plot, MMP_south_maxheight_plot, MMP_south_meanheight_plot, ncol=2)
dev.off()

#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_MMP_shoot_density_rid, png_MMP_shoot_density_fn)
ckanr::resource_update(png_shoot_density_rid, png_shoot_density_fn)
ckanr::resource_update(png_MMP_height_rid, png_MMP_max_height_fn)
ckanr::resource_update(png_height_rid, png_max_height_fn)

ckanr::resource_update(txt_rid, "MMP_shoot_density_and_canopy_height_code.R")

#####################################################################################
#set workdir to main report location
setwd("~/projects")
######################################################################################
