setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SIMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(gridExtra)
library(ggplot2)
library (plyr)
library(Kendall)
######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "619d13a7-5df5-46e3-8391-50a2390e8df2"
txt_rid <- "a25672bd-15d8-4644-933f-3eaa9fe6b320"

#percent cover plots
png_SIMP_overallpercentcover_rid <- "f98ef2b7-2c97-4d7b-a0d7-10d3001a6dfb"
png_SIMP_overallpercentcover_fn <-"SIMP_overallpercentcover.png"
png_SIMP_percentcover_rid <-"70afbaf6-33d5-4e4c-9b9b-933eca251d36"
png_SIMP_percentcover_fn <-"SIMP_percentcover.png"

###################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid)
d<-Camera_data
names(d)[names(d) == 'Region'] <- 'Park'###Changes column name

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
                 legend.key = element_blank()
)

##################################################################################
#Percent cover calculations for all data
##################################################################################
# All seagrass pooled

SIMP = subset (d, Park=="Shoalwater Islands Marine Park")

SIMP$Location <- as.factor(SIMP$Location)
cover <- SIMP %>% add_count(Site, Year)
cover <- plyr::ddply(cover, .(Year, Location, Site, Level1Class, n), summarise,
                                 add_count    = length(!is.na(Level1Class)))
SIMP_SG <- subset (cover, Level1Class %in% c("SEAGRASS"))

names(SIMP_SG)[4] <- "category" #Rename column to make more sense
names(SIMP_SG) [5] <- "total_count"
names(SIMP_SG)[6] <- "category_count" #Rename column to make more sense

SIMP_SG$percent = SIMP_SG$category_count/SIMP_SG$total_count *100

##################################################################################
#Create subsets for each 'sector (south, centre, north) for SIMP
##################################################################################

SIMP_south = subset(SIMP_SG, Site %in% c("Becher Point", "Becher Point SZ", "Port Kennedy"))
SIMP_warnbro = subset(SIMP_SG, Site %in% c("Warnbro Sound 2.5m" , "Warnbro Sound 3.2m", "Warnbro Sound 5.2m" , "Mersey Point"))
SIMP_shoalwater = subset(SIMP_SG, Site %in% c("Penguin Island" , "Seal Island", "Bird Island"))
SIMP_north = subset(SIMP_SG, Site %in% c("Causeway"))

####################################################################################
#PERCENT COVER
####################################################################################

SIMP_percentcover <- plyr::ddply(SIMP_SG, .(Year), summarise,
                     N    = length(!is.na(percent)),
                     mean = mean(percent, na.rm=TRUE),
                     sd   = sd(percent, na.rm=TRUE),
                     se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SIMP_percentcover_plot <- ggplot(SIMP_percentcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SIMP_percentcover$Year-0.125), max(SIMP_percentcover$Year+0.125)), breaks=min(SIMP_percentcover$Year):max(SIMP_percentcover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  # ggtitle("a) Vecher Point")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_percentcover_plot

attach(SIMP_percentcover)
MannKendall(mean)
detach(SIMP_percentcover)

############################################################################################
#SIMP_south Shoot density

SIMP_south_percentcover <- plyr::ddply(SIMP_south, .(Year), summarise,
                     N    = length(!is.na(percent)),
                     mean = mean(percent, na.rm=TRUE),
                     sd   = sd(percent, na.rm=TRUE),
                     se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SIMP_south_percentcover_plot <- ggplot(SIMP_south_percentcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(SIMP_south_percentcover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("d) Becher Point")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_south_percentcover_plot

attach(SIMP_south_percentcover)
MannKendall(mean)
detach(SIMP_south_percentcover)

################################################################################
#Warnbro Sound percent cover

SIMP_warnbro_percentcover <- plyr::ddply(SIMP_warnbro, .(Year), summarise,
                   N    = length(!is.na(percent)),
                   mean = mean(percent, na.rm=TRUE),
                   sd   = sd(percent, na.rm=TRUE),
                   se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SIMP_warnbro_percentcover_plot<-ggplot(SIMP_warnbro_percentcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(2016))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("c) Warnbro Sound")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_warnbro_percentcover_plot

attach(SIMP_warnbro_percentcover)
MannKendall(mean)
detach(SIMP_warnbro_percentcover)

####################################################################################
#SIMP_shoalwater Bay percent cover

SIMP_shoalwater_percentcover <- plyr::ddply(SIMP_shoalwater, .(Year), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SIMP_shoalwater_percentcover_plot <- ggplot(SIMP_shoalwater_percentcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(SIMP_shoalwater_percentcover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  ggtitle("b) Shoalwater Bay")+
  theme_bw() + graphics

SIMP_shoalwater_percentcover_plot

attach(SIMP_shoalwater_percentcover)
MannKendall(mean)
detach(SIMP_shoalwater_percentcover)

########################################################################################
#SIMP_north percent cover

SIMP_north_percentcover <- plyr::ddply(SIMP_north, .(Year), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SIMP_north_percentcover_plot<-ggplot(SIMP_north_percentcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(SIMP_north_percentcover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("a) Point Peron")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_north_percentcover_plot

attach(SIMP_north_percentcover)
MannKendall(mean)
detach(SIMP_north_percentcover)

#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

png(png_SIMP_overallpercentcover_fn, width=500, height=300)
grid.arrange(SIMP_percentcover_plot)
dev.off()

png(png_SIMP_percentcover_fn, width=800, height=550)
grid.arrange(SIMP_north_percentcover_plot, SIMP_shoalwater_percentcover_plot, SIMP_warnbro_percentcover_plot, SIMP_south_percentcover_plot, ncol=2)
dev.off()

#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_SIMP_overallpercentcover_rid, png_SIMP_overallpercentcover_fn)
ckanr::resource_update(png_SIMP_percentcover_rid, png_SIMP_percentcover_fn)
ckanr::resource_update(txt_rid, "percent_cover_code.R")

#####################################################################################
#set workdir to main report location
setwd("~/projects")
######################################################################################
