setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SIMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(gridExtra)
library(ggplot2)
library (plyr)
library(Kendall)
library(dplyr)
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

SIMP = subset (d, Park=="Shoalwater Islands Marine Park")

detach("package:dplyr", unload=TRUE)

cover=count(SIMP, c("Location", "Site", "Year", "Level5Class")) #counts number of observations per site, per year
cover_obs=count(cover, c("Location", "Site", "Year"), "freq") #counts number of observations made at each site per year
cover_add <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
pos_cover = subset(cover_add, Level5Class %in% c("Posidonia sinuosa","Posidonia australis")) #Extracts cover information only
SIMP_SG = count(pos_cover, c("Location", "Site", "Year", "freq.1"), "freq")
names(SIMP_SG)[4] <- "total_count" #Rename column to make more sense
names(SIMP_SG)[5] <- "pos_count" #Rename column to make more sense
SIMP_SG$percent = SIMP_SG$pos_count/SIMP_SG$total_count *100 #Calculate percent cover

library(dplyr)

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

#overall
SIMP_cover <- make_cover(SIMP_SG)

SIMP_percentcover_plot <- ggplot(SIMP_cover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(SIMP_cover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_percentcover_plot

attach(SIMP_cover)
MannKendall(mean)
detach(SIMP_cover)

############################################################################################
#SIMP_south cover

SIMP_south_percentcover <- make_cover(SIMP_south)

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

SIMP_warnbro_percentcover <- make_cover(SIMP_warnbro)

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

SIMP_shoalwater_percentcover <- make_cover(SIMP_shoalwater)

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
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_shoalwater_percentcover_plot

attach(SIMP_shoalwater_percentcover)
MannKendall(mean)
detach(SIMP_shoalwater_percentcover)

########################################################################################
#SIMP_north percent cover

SIMP_north_percentcover <- make_cover(SIMP_north)

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
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

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

png(png_SIMP_percentcover_fn, width=1000, height=600)
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
