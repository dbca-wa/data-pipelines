setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SBMP")
source("~/projects/data-pipelines/setup/ckan.R")


library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)
library(car)
library (Kendall)

######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "20180ac3-9b5a-493e-9669-b7a2aff28f68"#CKAN resource ID for data
txt_rid <- "0bba4f55-e3af-490e-88bb-738660ef16e2"#CKAN resource ID for r-script

#Percent cover
pdf_SBMP_amphib_percentcover_rid <- "a3748180-746a-4897-b135-02e9068a1ce1"#CKAN resource ID for final figure (pdf)
pdf_SBMP_amphib_percentcover_fn = "SBMP percent cover Amphibolis.pdf"#Name of final figure
png_SBMP_amphib_percentcover_rid <- "22c9123f-80be-4f24-ab9b-caeb6df8fa13"#CKAN resource ID for final figure (pdf)
png_SBMP_amphib_percentcover_fn = "SBMP percent cover Amphibolis.png"#Name of final figure
pdf_SBMP_pos_percentcover_rid <- "ad4b61b9-f9e7-4f53-a8f3-90ee3628aa5d"#CKAN resource ID for final figure (pdf)
pdf_SBMP_pos_percentcover_fn = "SBMP percent cover Posidonia.pdf"#Name of final figure
png_SBMP_pos_percentcover_rid <- "22c9123f-80be-4f24-ab9b-caeb6df8fa13"#CKAN resource ID for final figure (pdf)
png_SBMP_pos_percentcover_fn = "SBMP percent cover Posidonia.png"#Name of final figure
png_SBMP_overall_percentcover_rid <- "f8508298-dc4c-4099-8c02-ce9ced6c5142"#CKAN resource ID for final figure (png)
png_SBMP_overall_percentcover_fn = "SBMP overall percent cover.png"#Name of final figure

###################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))
names(d)[names(d) == 'Park_name'] <- 'Park'###Changes column name
names(d)[names(d) == 'Sites'] <- 'Site'###Changes column name

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
                 legend.justification=c(1,1), legend.position=c(1,1), # Positions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank())

##################################################################################
#Percent cover calculations for all data
##################################################################################

# All seagrass pooled
SGcover=count(d, c("Site", "Year", "Location", "Level1Class")) #counts number of observations per site, per year
SGcover_obs=count(SGcover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
SBMP_SGpercentcover <- join(SGcover, SGcover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
names(SBMP_SGpercentcover)[4] <- "category" #Rename column to make more sense
names(SBMP_SGpercentcover)[5] <- "category_count" #Rename column to make more sense
names(SBMP_SGpercentcover) [6] <- "total_count"
SBMP_SGpercentcover$percent = SBMP_SGpercentcover$category_count/SBMP_SGpercentcover$total_count *100

SG_cover <- subset(SBMP_SGpercentcover, category == c("SEAGRASS"))

#Amphibolis and Posidonia seperated
cover=count(d, c("Site", "Year","Location", "Level3Class")) #counts number of observations per site, per year
cover_obs=count(cover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
SBMP_percentcover <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
names(SBMP_percentcover)[5] <- "category_count" #Rename column to make more sense
names(SBMP_percentcover)[6] <- "total_count" #Rename column to make more sense
names(SBMP_percentcover) [4] <- "genus"
SBMP_percentcover$percent = SBMP_percentcover$category_count/SBMP_percentcover$total_count *100

amphibolis_cover <- subset(SBMP_percentcover, genus == c("Amphibolis spp."))
amphib_cover <- subset (amphibolis_cover, Site %in% c("East Peron", "Monkey Mia Control", "Monkey Mia outer bank", "Pearl Farm Control", "Peron Site 3", "Herald Bight west Amphibolis", "Useless Loop North Amphibolis",  "0456 Shark Bay Amphibolis", "0481 Shark Bay Amphibolis",  "0433 Shark Bay Amphibolis",  "0459 Shark Bay Amphibolis",  "0466 Shark Bay Amphibolis", "0380 Settlement Amphibolis", "0595 Shark Bay Amphibolis",  "0037 Shark Bay Amphibolis",  "Wooramel North Amphibolis", "Disappointment Reach Amphibolis", "Gladstone Marker",  "Gladestone site 2",  "Herald Loop"))
posidonia_cover <- subset(SBMP_percentcover, genus == c("Posidonia spp."))
pos_cover <- subset (posidonia_cover, Site %in% c("Monkey Mia inner bank", "Peron Site 4", "Denham", "Sandy Point", "Big Lagoon", "South Passage", "Useless Loop South", "Useless Loop North", "Peron South", "Disappointment Reach", "Wooramel North", "Monkey Mia Inner Bank", "Monkey Mia Pearl Control", "East Peron", "0380 Settlement", "Monkey Mia South", "Monkey Mia south_outer"))

# Posidonia and Amphibolis region subsets
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Western Gulf"] <- "Western Gulf")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Dirk Hartog Island"] <- "Western Gulf")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "South-Western Gulf"] <- "Peron")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Peron West"] <- "Peron")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Monkey Mia"] <- "Monkey Mia")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Monkey Mia Control"] <- "Monkey Mia")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Monkey Mia outer bank"] <- "Monkey Mia")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Pearl Farm Control"] <- "Monkey Mia")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Peron Site 3"] <- "Monkey Mia")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Peron East"] <- "Monkey Mia")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Eastern Gulf"] <- "Eastern Gulf")
amphib_cover = within(amphib_cover, levels(Location)[levels(Location) == "Gladstone"] <- "Eastern Gulf")

pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Western Gulf"] <- "Western Gulf")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Dirk Hartog Island"] <- "Western Gulf")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "South-Western Gulf"] <- "Peron")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Peron West"] <- "Peron")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Monkey Mia"] <- "Monkey Mia")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Peron East"] <- "Monkey Mia")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Eastern Gulf"] <- "Eastern Gulf")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Gladstone"] <- "Eastern Gulf")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Monkey Mia inner bank"] <- "Eastern Gulf")
pos_cover = within(pos_cover, levels(Location)[levels(Location) == "Peron Site 4"] <- "Eastern Gulf")


amphib_cover$Location <- factor(amphib_cover$Location, levels= c("Western Gulf", "Peron", "Monkey Mia", "Eastern Gulf"))
amphib_cover<-amphib_cover[order(amphib_cover$Location), ]

pos_cover$Location <- factor(pos_cover$Location, levels= c("Western Gulf", "Peron", "Monkey Mia", "Eastern Gulf"))
pos_cover<-pos_cover[order(pos_cover$Location), ]

#################################################################
#PERCENT COVER
#################################################################

#Overall percent cover
All_SBMP_cover <- plyr::ddply(SG_cover, .(Year), summarise,
                                  N    = length(!is.na(percent)),
                                  mean = mean(percent, na.rm=TRUE),
                                  sd   = sd(percent, na.rm=TRUE),
                                  se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SBMP_percentcover_plot <- ggplot(All_SBMP_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(limits=c(min(All_SBMP_cover$Year-0.125), max(All_SBMP_cover$Year+0.125)), breaks=min(All_SBMP_cover$Year):max(All_SBMP_cover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(Mean~percent~cover~(all~seagrass))) +
#  facet_wrap(~ Category, nrow = 2)+
#  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_percentcover_plot

attach(All_SBMP_cover)
MannKendall(mean)
detach(All_SBMP_cover)

#############################################################
#Overall Amphibolis percent cover

SBMP_amphib_cover <- plyr::ddply(amphib_cover, .(Year), summarise,
                          N    = length(!is.na(percent)),
                          mean = mean(percent, na.rm=TRUE),
                          sd   = sd(percent, na.rm=TRUE),
                          se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SBMP_amphibolis_percentcover_plot <- ggplot(SBMP_amphib_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_amphib_cover$Year-0.125), max(SBMP_amphib_cover$Year+0.125)), breaks=min(SBMP_amphib_cover$Year):max(SBMP_amphib_cover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(Mean~percent~cover~of~italic(Amphibolis))) +
  #  facet_wrap(~ Category, nrow = 2)+
  #  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_amphibolis_percentcover_plot

attach(SBMP_amphib_cover)
MannKendall(mean)
detach(SBMP_amphib_cover)

#############################################################
#Overall Posidonia percent cover
SBMP_posidonia_cover <- plyr::ddply(posidonia_cover, .(Year), summarise,
                                 N    = length(!is.na(percent)),
                                 mean = mean(percent, na.rm=TRUE),
                                 sd   = sd(percent, na.rm=TRUE),
                                 se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SBMP_posidonia_percentcover_plot <- ggplot(SBMP_posidonia_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_posidonia_cover$Year-0.125), max(SBMP_posidonia_cover$Year+0.125)), breaks=min(SBMP_posidonia_cover$Year):max(SBMP_posidonia_cover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(Mean~percent~cover~of ~italic(Posidonia))) +
  #  facet_wrap(~ Category, nrow = 2)+
  #  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_posidonia_percentcover_plot

attach(SBMP_posidonia_cover)
MannKendall(mean)
detach(SBMP_posidonia_cover)

#############################################################
#Amphibolis percent cover

SBMP_amphib_cover <- plyr::ddply(amphib_cover, .(Year, Location), summarise,
                                 N    = length(!is.na(percent)),
                                 mean = mean(percent, na.rm=TRUE),
                                 sd   = sd(percent, na.rm=TRUE),
                                 se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SBMP_amphib_percentcover_plot <- ggplot(SBMP_amphib_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_amphib_cover$Year-0.125), max(SBMP_amphib_cover$Year+0.125)), breaks=min(SBMP_amphib_cover$Year):max(SBMP_amphib_cover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(Mean~percent~cover~of~italic(Amphibolis))) +
  facet_wrap(~ Location, nrow = 4)+
  #  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_amphib_percentcover_plot


attach(SBMP_amphib_cover)
MannKendall(mean)
detach(SBMP_amphib_cover)

#############################################################
#Posidonia percent cover

SBMP_pos_cover <- plyr::ddply(pos_cover, .(Year, Location), summarise,
                                      N    = length(!is.na(percent)),
                                      mean = mean(percent, na.rm=TRUE),
                                      sd   = sd(percent, na.rm=TRUE),
                                      se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SBMP_pos_percentcover_plot <- ggplot(SBMP_pos_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_pos_cover$Year-0.125), max(SBMP_pos_cover$Year+0.125)), breaks=min(SBMP_pos_cover$Year):max(SBMP_pos_cover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(Mean~percent~cover~of~italic(Posidonia))) +
  facet_wrap(~ Location, nrow = 4)+
  #  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_pos_percentcover_plot

attach(SBMP_amphib_cover)
MannKendall(mean)
detach(SBMP_amphib_cover)

#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

#Percent cover

png(png_SBMP_overall_percentcover_fn, width=500, height=800)
grid.arrange(SBMP_percentcover_plot, SBMP_amphibolis_percentcover_plot, SBMP_posidonia_percentcover_plot)
dev.off()

pdf(png_SBMP_overall_percentcover_fn, width=8, height=7)
grid.arrange(SBMP_percentcover_plot, SBMP_amphibolis_percentcover_plot, SBMP_posidonia_percentcover_plot)
dev.off()

png(png_SBMP_amphib_percentcover_fn, width=600, height=800)
grid.arrange(SBMP_amphib_percentcover_plot)
dev.off()

pdf(pdf_SBMP_amphib_percentcover_fn, width=8, height=7)
grid.arrange(SBMP_amphib_percentcover_plot)
dev.off()

png(png_SBMP_pos_percentcover_fn, width=600, height=800)
grid.arrange(SBMP_pos_percentcover_plot)
dev.off()

pdf(pdf_SBMP_pos_percentcover_fn, width=8, height=7)
grid.arrange(SBMP_pos_percentcover_plot)
dev.off()

#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_SBMP_overall_percentcover_rid, png_SBMP_overall_percentcover_fn)

ckanr::resource_update(pdf_SBMP_amphib_percentcover_rid, pdf_SBMP_amphib_percentcover_fn)
ckanr::resource_update(png_SBMP_amphib_percentcover_rid, png_SBMP_amphib_percentcover_fn)

ckanr::resource_update(pdf_SBMP_pos_percentcover_rid, pdf_SBMP_pos_percentcover_fn)
ckanr::resource_update(png_SBMP_pos_percentcover_rid, png_SBMP_pos_percentcover_fn)

ckanr::resource_update(txt_rid, "SBpercent_cover_code.R")

#####################################################################################
#set workdir to main report location
setwd("~/projects")
######################################################################################
