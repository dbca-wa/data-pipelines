setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SIMP")
source("~/projects/data-pipelines/scripts/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)


######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "2eadb2a8-2b7a-4d96-9822-4a2c5389d16a"
txt_rid <- "1c0627b7-72d0-40a4-b11b-3f6a7a41c0a7"

#percent cover plots
pdf_SIMP_overallpercentcover_rid <- "95c93dc8-2eba-4bd7-9f79-14d474085300"
pdf_SIMP_overallpercentcover_fn <- "SIMP_overallpercentcover.pdf"
png_SIMP_overallpercentcover_rid <- "f98ef2b7-2c97-4d7b-a0d7-10d3001a6dfb"
png_SIMP_overallpercentcover_fn <-"SIMP_overallpercentcover.png"
png_SIMP_percentcover_rid <-"70afbaf6-33d5-4e4c-9b9b-933eca251d36"
png_SIMP_percentcover_fn <-"SIMP_percentcover.png"

###################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))


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

cover=count(d, c("Site", "Year", "Level5Class")) #counts number of observations per site, per year
cover_obs=count(cover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
cover_add <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
pos_cover = subset(cover_add, Level5Class %in% c("Posidonia sinuosa","Posidonia australis")) #Extracts cover information only
pos_total = count(pos_cover, c("Site", "Year", "freq.1"), "freq")
names(pos_total)[3] <- "total_count" #Rename column to make more sense
names(pos_total)[4] <- "pos_count" #Rename column to make more sense
pos_total$percent = pos_total$pos_count/pos_total$total_count *100 #Calculate percent cover


##################################################################################
#Create subsets for each 'sector (south, centre, north) for SIMP
##################################################################################

SIMP_south = subset(pos_total, Site %in% c("Becher Point", "Becher Point SZ", "Port Kennedy"))
SIMP_warnbro = subset(pos_total, Site %in% c("Warnbro Sound 2.5m" , "Warnbro Sound 3.2m", "Warnbro Sound 5.2m" , "Mersey Point"))
SIMP_shoalwater = subset(pos_total, Site %in% c("Penguin Island" , "Seal Island", "Bird Island"))
SIMP_north = subset(pos_total, Site %in% c("Causeway"))

####################################################################################
#PERCENT COVER
####################################################################################

#Overall percent cover
SIMP_percentcover <- plyr::ddply(pos_total, .(Year), summarise,
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
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  # ggtitle("a) Vecher Point")+
  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
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
  scale_x_continuous(limits=c(min(SIMP_south_percentcover$Year-0.125), max(SIMP_south_percentcover$Year+0.125)), breaks=min(SIMP_south_percentcover$Year):max(SIMP_south_percentcover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("a) Becher Point (n = 3 sites)")+
  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
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
  scale_x_continuous(limits=c(min(SIMP_warnbro_percentcover$Year-0.125), max(SIMP_warnbro_percentcover$Year+0.125)), breaks=min(SIMP_warnbro_percentcover$Year):max(SIMP_warnbro_percentcover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("b) Warnbro Sound (n = 4 site)")+
  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
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
  scale_x_continuous(limits=c(min(SIMP_shoalwater_percentcover$Year-0.125), max(SIMP_shoalwater_percentcover$Year+0.125)), breaks=min(SIMP_shoalwater_percentcover$Year):max(SIMP_shoalwater_percentcover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  ggtitle("c) Shoalwater Bay (n = 3 sites)")+
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
  scale_x_continuous(limits=c(min(SIMP_north_percentcover$Year-0.125), max(SIMP_north_percentcover$Year+0.125)), breaks=min(SIMP_north_percentcover$Year):max(SIMP_north_percentcover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m", ")", sep = ""))) +
  ggtitle("d) Point Peron (n = 1 site)")+
  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_north_percentcover_plot

attach(SIMP_north_percentcover)
MannKendall(mean)
detach(SIMP_north_percentcover)

#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

pdf(pdf_SIMP_overallpercentcover_fn, width=8, height=7)
grid.arrange(SIMP_percentcover_plot)
dev.off()

png(png_SIMP_overallpercentcover_fn, width=500, height=300)
grid.arrange(SIMP_percentcover_plot)
dev.off()

png(png_SIMP_percentcover_fn, width=500, height=300)
grid.arrange(SIMP_south_percentcover_plot, SIMP_warnbro_percentcover_plot, SIMP_shoalwater_percentcover_plot, SIMP_north_percentcover_plot, ncol=2)
dev.off()

#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(pdf_SIMP_overallpercentcover_rid, pdf_SIMP_overallpercentcover_fn)
ckanr::resource_update(png_SIMP_overallpercentcover_rid, png_SIMP_overallpercentcover_fn)
ckanr::resource_update(png_SIMP_percentcover_rid, png_SIMP_percentcover_fn)
ckanr::resource_update(txt_rid, "percent_cover_code.R")

#####################################################################################
#set workdir to main report location
setwd("~/projects")
######################################################################################
