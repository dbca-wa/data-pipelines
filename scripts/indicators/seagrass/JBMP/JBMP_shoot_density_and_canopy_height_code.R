setwd("~/projects/data-pipelines/scripts/indicators/seagrass/JBMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)
library(dplyr)
# install.packages("Kendall")
library (Kendall)


######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"#CKAN resource ID for data
txt_rid <- "ef76d134-cc13-4c54-8864-0c441a36a127"#CKAN resource ID for r-script

#Shoot density plots
pdf_shoot_density_rid <- "485457c3-a66f-4a6b-aa60-909736986ff8"#CKAN resource ID for final figure (pdf)
pdf_shoot_density_fn = "JBMP shoot density.pdf"#Name of final figure
png_shoot_density_rid <- "af4bc41b-6477-4571-9038-1f5784502bdf"#CKAN resource ID for final figure (png)
png_shoot_density_fn = "JBMP shoot density.png"#Name of final figure
png_JBMP_shoot_density_rid <- "186519a9-72aa-4d78-afd6-bce672f476f0"#CKAN resource ID for final figure (png)
png_JBMP_shoot_density_fn = "JBMP overall shoot density.png"#Name of final figure

#Maximum canopy height plots
pdf_max_height_rid <- "4d9fde3a-2b45-4f09-bfff-25fc1a2cac73"#CKAN resource ID for final figure (pdf)
pdf_max_height_fn = "JBMP max height.pdf"#Name of final figure
png_max_height_rid <- "8f406852-0b15-46cb-b972-c960c4f3bfc8"#CKAN resource ID for final figure (png)
png_max_height_fn = "JBMP max height.png"#Name of final figure
png_JBMP_max_height_rid <- "a440fa20-596a-474a-a91f-b1108c99f633"#CKAN resource ID for final figure (png)
png_JBMP_max_height_fn = "JBMP overall max height.png"#Name of final figure

#Mean canopy height plots
pdf_mean_height_rid <- "ce18973e-3336-466e-834b-e08abfeec5be"#CKAN resource ID for final figure (pdf)
pdf_mean_height_fn = "JBMP mean height.pdf"#Name of final figure
png_mean_height_rid <- "964b26d5-e008-42a9-9d5c-7269fdf58b6c"#CKAN resource ID for final figure (png)
png_mean_height_fn = "JBMP mean height.png"#Name of final figure
png_JBMP_mean_height_rid <- "6f94e423-87ee-4224-964e-fa97b7e6a016"#CKAN resource ID for final figure (png)
png_JBMP_mean_height_fn = "JBMP overall mean height.png"#Name of final figure


###################################################################################################
#Load data
###################################################################################################
d<-seagrass_data
# d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))
names(d)[names(d) == 'Park_name'] <- 'Park'###Changes column name
names(d)[names(d) == 'Site_name'] <- 'Site'###Changes column name


####################################################################################################
#Define graphic properties
#####################################################################################################

pd <- position_dodge(0.1)
graphics = theme(axis.text.x=element_text(angle=45, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
                 axis.title.x=element_text(), #removes x axis title
                 axis.title.y=element_text(), #removes y axis title
                 axis.line=element_line(colour="black"), #sets axis lines
                 plot.title =element_text(hjust = 0.05),
                 panel.grid.minor = element_blank(), #removes minor grid lines
                 panel.grid.major = element_blank(), #removes major grid lines
                 panel.border=element_blank(), #removes border
                 panel.background=element_blank(), #needed to ensure integrity of axis lines
                 legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank())


##################################################################################
#Create subsets for each 'sector (south, centre, north) for JBMP
##################################################################################

JBMP = subset(d, Park %in% c("JBMP"))
JBMP_south = subset(d, Site %in% c("Kangaroo Point", "Cervantes Island", "Green Island"))
JBMP_centre = subset(d, Site %in% c("Jurien Impact Site 2.5" , "Boullanger Island 2.5", "Boullanger Island 3.5" , "Boullanger Island 5.5"))
JBMP_north = subset(d, Site %in% c("Fishermans Island 2.5","Fishermans Island 3.5", "Fishermans Island 5.5"))


####################################################################################
#SHOOT DENSITY
####################################################################################

#OverallShoot density
JBMP_shootdensity <- plyr::ddply(JBMP, .(Year), summarise,
                                 N    = length(!is.na(Posidonia_sinuosa)),
                                 mean = mean(Posidonia_sinuosa, na.rm=TRUE),
                                 sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
                                 se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

JBMP_shootdensity_plot <- ggplot(JBMP_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_shootdensity$Year-0.125), max(JBMP_shootdensity$Year+0.125)), breaks=min(JBMP_shootdensity$Year):max(JBMP_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  # xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics

JBMP_shootdensity_plot

attach(JBMP_shootdensity)
MannKendall(mean)
detach(JBMP_shootdensity)

##############################################################################################
#JBMP_south Shoot density

JBMP_south_shootdensity <- plyr::ddply(JBMP_south, .(Year), summarise,
                     N    = length(!is.na(Posidonia_sinuosa)),
                     mean = mean(Posidonia_sinuosa, na.rm=TRUE),
                     sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
                     se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

JBMP_south_shootdensity_plot <- ggplot(JBMP_south_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") +
  scale_x_continuous(limits=c(min(JBMP_south_shootdensity$Year-0.125), max(JBMP_south_shootdensity$Year+0.125)), breaks=min(JBMP_south_shootdensity$Year):max(JBMP_south_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  # xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("a) South") +
  # geom_smooth(method=lm, colour = 1, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

JBMP_south_shootdensity_plot

attach(JBMP_south_shootdensity)
MannKendall(mean)
detach(JBMP_south_shootdensity)

#############################################################
#JBMP_centre shoot density

JBMP_centre_shootdensity <- ddply(JBMP_centre, .(Year), summarise,
                     N    = length(!is.na(Posidonia_sinuosa)),
                     mean = mean(Posidonia_sinuosa, na.rm=TRUE),
                     sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
                     se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

JBMP_centre_shootdensity_plot<-ggplot(JBMP_centre_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_centre_shootdensity$Year-0.125), max(JBMP_centre_shootdensity$Year+0.125)), breaks=min(JBMP_centre_shootdensity$Year):max(JBMP_centre_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  # xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("b) Centre")+
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics

JBMP_centre_shootdensity_plot

attach(JBMP_centre_shootdensity)
MannKendall(mean)
detach(JBMP_centre_shootdensity)

###########################################################################
#JBMP_north shoot density

JBMP_north_shootdensity <- ddply(JBMP_north, .(Year), summarise,
                     N    = length(!is.na(Posidonia_sinuosa)),
                     mean = mean(Posidonia_sinuosa, na.rm=TRUE),
                     sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
                     se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

JBMP_north_shootdensity_plot<-ggplot(JBMP_north_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_north_shootdensity$Year-0.125), max(JBMP_north_shootdensity$Year+0.125)), breaks=min(JBMP_north_shootdensity$Year):max(JBMP_north_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("d) North")+
  # geom_smooth(method=loess, colour = 1, se=TRUE, fullrange=TRUE) +
  theme_bw() + graphics

JBMP_north_shootdensity_plot


attach(JBMP_north_shootdensity)
MannKendall(mean)
detach(JBMP_north_shootdensity)

####################################################################################
#MAXIMUM CANOPY HEIGHT
####################################################################################
#Overall maximum canopy height density

JBMP_maxheight <- plyr::ddply(JBMP, .(Year), summarise,
                             N    = length(!is.na(Maximum_height_mm)),
                             mean = mean(Maximum_height_mm, na.rm=TRUE),
                             sd   = sd(Maximum_height_mm, na.rm=TRUE),
                             se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

JBMP_maxheight_plot <- ggplot(JBMP_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_maxheight$Year-0.125), max(JBMP_maxheight$Year+0.125)), breaks=min(JBMP_maxheight$Year):max(JBMP_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, se=FALSE, fullrange=FALSE)+
  theme_bw() + graphics

JBMP_maxheight_plot

attach(JBMP_maxheight)
MannKendall(mean)
detach(JBMP_maxheight)

##########################################################################################\
#JBMP_south max canopy height
JBMP_south_maxheight <- plyr::ddply(JBMP_south, .(Year), summarise,
                                       N    = length(!is.na(Maximum_height_mm)),
                                       mean = mean(Maximum_height_mm, na.rm=TRUE),
                                       sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                       se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

JBMP_south_maxheight_plot <- ggplot(JBMP_south_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_south_maxheight$Year-0.125), max(JBMP_south_maxheight$Year+0.125)), breaks=min(JBMP_south_maxheight$Year):max(JBMP_south_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("a) South") +
  # geom_smooth(method=lm, colour = 1, se=FALSE, fullrange=TRUE)+
    theme_bw() + graphics

JBMP_south_maxheight_plot

attach(JBMP_south_maxheight)
MannKendall(mean)
detach(JBMP_south_maxheight)

#############################################################
#JBMP_centre max canopy height

JBMP_centre_maxheight <- ddply(JBMP_centre, .(Year), summarise,
                                   N    = length(!is.na(Maximum_height_mm)),
                                   mean = mean(Maximum_height_mm, na.rm=TRUE),
                                   sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                   se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

JBMP_centre_maxheight_plot<-ggplot(JBMP_centre_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_centre_maxheight$Year-0.125), max(JBMP_centre_maxheight$Year+0.125)), breaks=min(JBMP_centre_maxheight$Year):max(JBMP_centre_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("b) Centre")+
  # geom_smooth(method=lm, colour = 1, se=FALSE, fullrange=FALSE)+
    theme_bw() + graphics

JBMP_centre_maxheight_plot

attach(JBMP_centre_maxheight)
MannKendall(mean)
detach(JBMP_centre_maxheight)

#################################################################
#JBMP_north max canopy height

JBMP_north_maxheight <- ddply(JBMP_north, .(Year), summarise,
                                      N    = length(!is.na(Maximum_height_mm)),
                                      mean = mean(Maximum_height_mm, na.rm=TRUE),
                                      sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                      se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

JBMP_north_maxheight_plot <- ggplot(JBMP_north_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_north_maxheight$Year-0.125), max(JBMP_north_maxheight$Year+0.125)), breaks=min(JBMP_north_maxheight$Year):max(JBMP_north_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("c) North")+
  # geom_smooth(method=lm, colour = 1, se=FALSE, fullrange=FALSE)+
  theme_bw() + graphics

JBMP_north_maxheight_plot

attach(JBMP_north_maxheight)
MannKendall(mean)
detach(JBMP_north_maxheight)


####################################################################################
#MEAN CANOPY HEIGHT
####################################################################################
#Overall mean canopy height density

JBMP_meanheight <- plyr::ddply(JBMP, .(Year), summarise,
                              N    = length(!is.na(Mean_height_mm)),
                              mean = mean(Mean_height_mm, na.rm=TRUE),
                              sd   = sd(Mean_height_mm, na.rm=TRUE),
                              se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

JBMP_meanheight_plot <- ggplot(JBMP_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_meanheight$Year-0.125), max(JBMP_meanheight$Year+0.125)), breaks=min(JBMP_meanheight$Year):max(JBMP_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean height (mm)", sep = ""))) +
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=FALSE)+
  theme_bw() + graphics

JBMP_meanheight_plot

attach(JBMP_meanheight)
MannKendall(mean)
detach(JBMP_meanheight)

######################################################################################################################
#JBMP_south mean canopy height

JBMP_south_meanheight <- plyr::ddply(JBMP_south, .(Year), summarise,
                                    N    = length(!is.na(Mean_height_mm)),
                                    mean = mean(Mean_height_mm, na.rm=TRUE),
                                    sd   = sd(Mean_height_mm, na.rm=TRUE),
                                    se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

JBMP_south_meanheight_plot <- ggplot(JBMP_south_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2013), max(JBMP_south_meanheight$Year+0.125)), breaks=min(JBMP_south_meanheight$Year):max(JBMP_south_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean height (mm)", sep = ""))) +
  ggtitle("a) South") +
  # geom_smooth(method=lm, colour = 1, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

JBMP_south_meanheight_plot

attach(JBMP_south_meanheight)
MannKendall(mean)
detach(JBMP_south_meanheight)

#############################################################
#JBMP_centre mean canopy height

JBMP_centre_meanheight <- ddply(JBMP_centre, .(Year), summarise,
                                N    = length(!is.na(Mean_height_mm)),
                                mean = mean(Mean_height_mm, na.rm=TRUE),
                                sd   = sd(Mean_height_mm, na.rm=TRUE),
                                se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

JBMP_centre_meanheight_plot<-ggplot(JBMP_centre_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_centre_meanheight$Year-0.125), max(JBMP_centre_meanheight$Year+0.125)), breaks=min(JBMP_centre_meanheight$Year):max(JBMP_centre_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("b) Centre")+
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics

JBMP_centre_meanheight_plot

attach(JBMP_centre_meanheight)
MannKendall(mean)
detach(JBMP_centre_meanheight)

#################################################################
#JBMP_north mean canopy height

JBMP_north_meanheight <- ddply(JBMP_north, .(Year), summarise,
                                   N    = length(!is.na(Mean_height_mm)),
                                   mean = mean(Mean_height_mm, na.rm=TRUE),
                                   sd   = sd(Mean_height_mm, na.rm=TRUE),
                                   se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

JBMP_north_meanheight_plot <- ggplot(JBMP_north_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_north_meanheight$Year-0.125), max(JBMP_north_meanheight$Year+0.125)), breaks=min(JBMP_north_meanheight$Year):max(JBMP_north_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("c) North")+
  # geom_smooth(method=lm, colour = 1, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

JBMP_north_meanheight_plot

attach(JBMP_north_meanheight)
MannKendall(mean)
detach(JBMP_north_meanheight)


#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

#Shoot density

png(png_JBMP_shoot_density_fn, width=500, height=300)
grid.arrange(JBMP_shootdensity_plot)
dev.off()

# pdf(pdf_shoot_density_fn, width=8, height=7)
# grid.arrange(JBMP_south_shootdensity_plot, JBMP_centre_shootdensity_plot, JBMP_north_shootdensity_plot,ncol=1)
# dev.off()

png(png_shoot_density_fn, width=600, height=800)
grid.arrange(JBMP_north_shootdensity_plot, JBMP_centre_shootdensity_plot, JBMP_south_shootdensity_plot, ncol=1)
dev.off()

#Maximum canopy height
png(png_JBMP_max_height_fn, width=500, height=300)
grid.arrange(JBMP_maxheight_plot)
dev.off()

# pdf(pdf_max_height_fn, width=8, height=7)
# grid.arrange(JBMP_south_maxheight_plot, JBMP_centre_maxheight_plot, JBMP_north_maxheight_plot, ncol=1)
# dev.off()

png(png_max_height_fn, width=600, height=800)
grid.arrange(JBMP_north_maxheight_plot, JBMP_centre_maxheight_plot, JBMP_south_maxheight_plot, ncol=1)
dev.off()

#Mean canopy height
png(png_JBMP_mean_height_fn, width=500, height=300)
grid.arrange(JBMP_meanheight_plot)
dev.off()

# pdf(pdf_mean_height_fn, width=8, height=7)
# grid.arrange(JBMP_south_meanheight_plot, JBMP_centre_meanheight_plot, JBMP_north_meanheight_plot, ncol=1)
# dev.off()

png(png_mean_height_fn, width=600, height=800)
grid.arrange(JBMP_north_meanheight_plot, JBMP_centre_meanheight_plot, JBMP_south_meanheight_plot, ncol=1)
dev.off()


#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_JBMP_shoot_density_rid, png_JBMP_shoot_density_fn)
ckanr::resource_update(png_shoot_density_rid, png_shoot_density_fn)
ckanr::resource_update(png_JBMP_max_height_rid, png_JBMP_max_height_fn)
ckanr::resource_update(png_max_height_rid, png_max_height_fn)
ckanr::resource_update(png_JBMP_mean_height_rid, png_JBMP_mean_height_fn)
ckanr::resource_update(png_mean_height_rid, png_mean_height_fn)
ckanr::resource_update(txt_rid, "JBMP_shoot_density_and_canopy_height_code.R")

