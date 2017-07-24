setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SIMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(Kendall)
library(plyr)
library(gridExtra)


######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"#CKAN resource ID for data
txt_rid <- "ef76d134-cc13-4c54-8864-0c441a36a127"#CKAN resource ID for r-script

#Shoot density plots
png_shoot_density_rid <- "af4bc41b-6477-4571-9038-1f5784502bdf"#CKAN resource ID for final figure (png)
png_shoot_density_fn = "SIMP shoot density.png"#Name of final figure
png_SIMP_shoot_density_rid <- "186519a9-72aa-4d78-afd6-bce672f476f0"#CKAN resource ID for final figure (png)
png_SIMP_shoot_density_fn = "SIMP overall shoot density.png"#Name of final figure

#Maximum canopy height plots
png_max_height_rid <- "8f406852-0b15-46cb-b972-c960c4f3bfc8"#CKAN resource ID for final figure (png)
png_max_height_fn = "SIMP max height.png"#Name of final figure
png_SIMP_max_height_rid <- "a440fa20-596a-474a-a91f-b1108c99f633"#CKAN resource ID for final figure (png)
png_SIMP_max_height_fn = "SIMP overall max height.png"#Name of final figure

################################################################################
#Load data
################################################################################

d <- load_ckan_csv(csv_rid) %>% mutate(Site = Site_name)
d<- All_SG_data
names(d)[names(d) == 'Site_name'] <- 'Site'###Changes column name

################################################################################
#Define graphic properties
################################################################################

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

################################################################################
#Create subsets for each 'sector (south, centre, north) for SIMP
################################################################################
SIMP <- d %>% filter(Park == "SIMP")
SIMP_south <- SIMP %>% filter(Site %in% c("Becher Point", "Becher Point SZ", "Port Kennedy"))
SIMP_warnbro <- SIMP %>% filter(Site %in% c("Warnbro Sound 2.5m" , "Warnbro Sound 3.2m", "Warnbro Sound 5.2m" ,
  "Warnbro Sound 7.0m", "Warnbro Sound 2.0m" , "Mersey Point"))
SIMP_shoalwater <- SIMP %>% filter(Site %in% c("Penguin Island" , "Seal Island", "Bird Island"))
SIMP_north <- SIMP %>% filter(Site %in% c("Causeway"))

################################################################################
#SHOOT DENSITY
################################################################################
#OverallShoot density
SIMP_shootdensity <- plyr::ddply(SIMP, .(Year), summarise,
  N    = length(!is.na(Posidonia_sinuosa)),
  mean = mean(Posidonia_sinuosa, na.rm=TRUE),
  sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
  se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

SIMP_shootdensity_plot <- ggplot(SIMP_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SIMP_shootdensity$Year-0.125),
                              max(SIMP_shootdensity$Year+0.125)),
                     breaks=min(SIMP_shootdensity$Year):max(SIMP_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_shootdensity_plot

attach(SIMP_shootdensity)
MannKendall(mean)
detach(SIMP_shootdensity)

#SIMP_south Shoot density
SIMP_south_shootdensity <- plyr::ddply(SIMP_south, .(Year), summarise,
                     N    = length(!is.na(Posidonia_sinuosa)),
                     mean = mean(Posidonia_sinuosa, na.rm=TRUE),
                     sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
                     se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

SIMP_south_shootdensity_plot <- ggplot(SIMP_south_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") +
  scale_x_continuous(limits=c(min(SIMP_south_shootdensity$Year-0.125), max(SIMP_south_shootdensity$Year+0.125)), breaks=min(SIMP_south_shootdensity$Year):max(SIMP_south_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("a) Becher Point") +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_south_shootdensity_plot

attach(SIMP_south_shootdensity)
MannKendall(mean)
detach(SIMP_south_shootdensity)

#############################################################
#Warnbro Sound shoot density

SIMP_warnbro_shootdensity <- ddply(SIMP_warnbro, .(Year), summarise,
                     N    = length(!is.na(Posidonia_sinuosa)),
                     mean = mean(Posidonia_sinuosa, na.rm=TRUE),
                     sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
                     se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

SIMP_warnbro_shootdensity_plot<-ggplot(SIMP_warnbro_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SIMP_warnbro_shootdensity$Year-0.125), max(SIMP_warnbro_shootdensity$Year+0.125)), breaks=min(SIMP_warnbro_shootdensity$Year):max(SIMP_warnbro_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("b) Warnbro Sound")+
  geom_smooth(method=lm, colour = 1, linetype =1, se=TRUE,fullrange=TRUE)+
  theme_bw() + graphics

SIMP_warnbro_shootdensity_plot

attach(SIMP_warnbro_shootdensity)
MannKendall(mean)
detach(SIMP_warnbro_shootdensity)

#################################################################
#SIMP_shoalwater Bay shoot density

SIMP_shoalwater_shootdensity <- ddply(SIMP_shoalwater, .(Year), summarise,
                     N    = length(!is.na(Posidonia_sinuosa)),
                     mean = mean(Posidonia_sinuosa, na.rm=TRUE),
                     sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
                     se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

SIMP_shoalwater_shootdensity_plot <- ggplot(SIMP_shoalwater_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SIMP_shoalwater_shootdensity$Year-0.125), max(SIMP_shoalwater_shootdensity$Year+0.125)), breaks=min(SIMP_shoalwater_shootdensity$Year):max(SIMP_shoalwater_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("c) Shoalwater Bay")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se = FALSE, fullrange=TRUE) +
  theme_bw() + graphics

SIMP_shoalwater_shootdensity_plot

attach(SIMP_shoalwater_shootdensity)
MannKendall(mean)
detach(SIMP_shoalwater_shootdensity)

###########################################################################
#SIMP_north shoot density

SIMP_north_shootdensity <- ddply(SIMP_north, .(Year), summarise,
                     N    = length(!is.na(Posidonia_sinuosa)),
                     mean = mean(Posidonia_sinuosa, na.rm=TRUE),
                     sd   = sd(Posidonia_sinuosa, na.rm=TRUE),
                     se   = sd(Posidonia_sinuosa, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_sinuosa)) ))

SIMP_north_shootdensity_plot<-ggplot(SIMP_north_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SIMP_north_shootdensity$Year-0.125), max(SIMP_north_shootdensity$Year+0.125)), breaks=min(SIMP_north_shootdensity$Year):max(SIMP_north_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("d) Point Peron")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se = FALSE, fullrange=TRUE) +
  theme_bw() + graphics

SIMP_north_shootdensity_plot

attach(SIMP_north_shootdensity)
MannKendall(mean)
detach(SIMP_north_shootdensity)

####################################################################################
#MAXIMUM CANOPY HEIGHT
####################################################################################

#Overall maximum canopy height density
SIMP_maxheight <- plyr::ddply(SIMP, .(Year), summarise,
                             N    = length(!is.na(Maximum_height_mm)),
                             mean = mean(Maximum_height_mm, na.rm=TRUE),
                             sd   = sd(Maximum_height_mm, na.rm=TRUE),
                             se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SIMP_maxheight_plot <- ggplot(SIMP_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2009), max(SIMP_maxheight$Year+0.125)), breaks=min(SIMP_maxheight$Year):max(SIMP_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=FALSE)+
  theme_bw() + graphics

SIMP_maxheight_plot

attach(SIMP_maxheight)
MannKendall(mean)
detach(SIMP_maxheight)

#SIMP_south max canopy height
SIMP_south_maxheight <- plyr::ddply(SIMP_south, .(Year), summarise,
                                       N    = length(!is.na(Maximum_height_mm)),
                                       mean = mean(Maximum_height_mm, na.rm=TRUE),
                                       sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                       se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SIMP_south_maxheight_plot <- ggplot(SIMP_south_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SIMP_south_maxheight$Year-0.125), max(SIMP_south_maxheight$Year+0.125)), breaks=min(SIMP_south_maxheight$Year):max(SIMP_south_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("a) Becher Point") +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
    theme_bw() + graphics

SIMP_south_maxheight_plot

attach(SIMP_south_maxheight)
MannKendall(mean)
detach(SIMP_south_maxheight)

#############################################################
#Warnbro Sound max canopy height

SIMP_warnbro_maxheight <- ddply(SIMP_warnbro, .(Year), summarise,
                                   N    = length(!is.na(Maximum_height_mm)),
                                   mean = mean(Maximum_height_mm, na.rm=TRUE),
                                   sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                   se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SIMP_warnbro_maxheight_plot<-ggplot(SIMP_warnbro_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2009), max(SIMP_warnbro_maxheight$Year+0.125)), breaks=min(SIMP_warnbro_maxheight$Year):max(SIMP_warnbro_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("b) Warnbro Sound")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=FALSE)+
    theme_bw() + graphics

SIMP_warnbro_maxheight_plot

attach(SIMP_warnbro_maxheight)
MannKendall(mean)
detach(SIMP_warnbro_maxheight)

#################################################################
#SIMP_shoalwater maxy canopy height

SIMP_shoalwater_maxheight <- ddply(SIMP_shoalwater, .(Year), summarise,
                                      N    = length(!is.na(Maximum_height_mm)),
                                      mean = mean(Maximum_height_mm, na.rm=TRUE),
                                      sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                      se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SIMP_shoalwater_maxheight_plot <- ggplot(SIMP_shoalwater_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2009), max(SIMP_shoalwater_maxheight$Year+0.125)), breaks=min(SIMP_shoalwater_maxheight$Year):max(SIMP_shoalwater_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("c) Shoalwater Bay")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=FALSE)+
  theme_bw() + graphics

SIMP_shoalwater_maxheight_plot

attach(SIMP_shoalwater_maxheight)
MannKendall(mean)
detach(SIMP_shoalwater_maxheight)

###########################################################################
#SIMP_north max canopy height

SIMP_north_maxheight <- ddply(SIMP_north, .(Year), summarise,
                                 N    = length(!is.na(Maximum_height_mm)),
                                 mean = mean(Maximum_height_mm, na.rm=TRUE),
                                 sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                 se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SIMP_north_maxheight_plot<-ggplot(SIMP_north_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SIMP_north_maxheight$Year-0.125), max(SIMP_north_maxheight$Year+0.125)), breaks=min(SIMP_north_maxheight$Year):max(SIMP_north_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(expression(paste("Mean max height (mm)", sep = ""))) +
  ggtitle("d) Point Peron")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_north_maxheight_plot

attach(SIMP_north_maxheight)
MannKendall(mean)
detach(SIMP_north_maxheight)


#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

#Shoot density

png(png_SIMP_shoot_density_fn, width=500, height=300)
grid.arrange(SIMP_shootdensity_plot)
dev.off()

png(png_shoot_density_fn, width=1000, height=800)
grid.arrange(SIMP_south_shootdensity_plot, SIMP_warnbro_shootdensity_plot, SIMP_shoalwater_shootdensity_plot, SIMP_north_shootdensity_plot, ncol=2)
dev.off()

#Maximum canopy height
png(png_SIMP_max_height_fn, width=500, height=300)
grid.arrange(SIMP_maxheight_plot)
dev.off()

png(png_max_height_fn, width=1000, height=800)
grid.arrange(SIMP_south_maxheight_plot, SIMP_warnbro_maxheight_plot, SIMP_shoalwater_maxheight_plot, SIMP_north_maxheight_plot, ncol=2)
dev.off()

#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_SIMP_shoot_density_rid, png_SIMP_shoot_density_fn)
ckanr::resource_update(png_shoot_density_rid, png_shoot_density_fn)
ckanr::resource_update(png_SIMP_max_height_rid, png_SIMP_max_height_fn)
ckanr::resource_update(png_max_height_rid, png_max_height_fn)
ckanr::resource_update(png_SIMP_mean_height_rid, png_SIMP_mean_height_fn)
ckanr::resource_update(png_mean_height_rid, png_mean_height_fn)
ckanr::resource_update(txt_rid, "SIMP_shoot_density_and_canopy_height_code.R")

#####################################################################################
#set workdir to main report location
setwd("~/projects")
######################################################################################
