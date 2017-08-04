setwd("~/projects/data-pipelines/scripts/indicators/seagrass/JBMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "b0b546ed-ab74-4592-8429-7175cd637de4"#CKAN resource ID for data
txt_rid <- "0bba4f55-e3af-490e-88bb-738660ef16e2"#CKAN resource ID for r-script


#Percent cover
png_JBMP_overall_percentcover_rid <- "f8508298-dc4c-4099-8c02-ce9ced6c5142"#CKAN resource ID for final figure (png)
png_JBMP_overall_percentcover_fn = "JBMP overall percent cover.png"#Name of final figure
png_JBMP_percentcover_rid <- "96d9a4f5-4041-4070-9ba5-6bb518341321"#CKAN resource ID for final figure (pdf)
png_JBMP_percentcover_fn = "JBMP percent cover.png"#Name of final figure

###################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))
d<-Camera_data
names(d)[names(d) == 'Region'] <- 'Park'###Changes column name

####################################################################################################
#Define graphic properties
#####################################################################################################

pd <- position_dodge(0.1)
graphics = theme(axis.text.x=element_text(angle=45, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
                 axis.title.x=element_text(size=12,face="bold"),
                 axis.title.y=element_text(size=14,face="bold"), #removes y axis title
                 axis.text.y=element_text(),
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
#Percent cover calculations for all data
##################################################################################

# All seagrass pooled
JBMP = subset (d, Park=="Jurien Bay Marine Park")
JBMP$Location <- as.factor(JBMP$Location)


JBMP = within(JBMP, levels(Location)[levels(Location) == "Fishermans Island"] <- "North")
JBMP = within(JBMP, levels(Location)[levels(Location) == "Boullanger Island"] <- "Centre")
JBMP = within(JBMP, levels(Location)[levels(Location) == "Jurien Town"] <- "Centre")
JBMP = within(JBMP, levels(Location)[levels(Location) == "South Cervantes"] <- "South")
JBMP = within(JBMP, levels(Location)[levels(Location) == "Green Island"] <- "South")
JBMP = within(JBMP, levels(Location)[levels(Location) == "Kangaroo Point"] <- "South")

str(JBMP)

SGcover=count(JBMP, c("Site", "Zone", "Year", "Location", "Level1Class")) #counts number of observations per site, per year
SGcover_obs=count(SGcover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
SBMP_SGpercentcover <- join(SGcover, SGcover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
names(SBMP_SGpercentcover)[5] <- "category" #Rename column to make more sense
names(SBMP_SGpercentcover)[6] <- "category_count" #Rename column to make more sense
names(SBMP_SGpercentcover) [7] <- "total_count"
SBMP_SGpercentcover$percent = SBMP_SGpercentcover$category_count/SBMP_SGpercentcover$total_count *100

SG_cover <- subset(SBMP_SGpercentcover, category == c("SEAGRASS"))


# Region subsets
JBMP<-SG_cover
JBMP_south = subset(SG_cover, Location %in% c("South"))
JBMP_centre = subset(SG_cover, Location %in% c("Centre"))
JBMP_north = subset(SG_cover, Location %in% c( "North"))

#################################################################
#PERCENT COVER
#################################################################

#Overall percent cover

JBMP_cover <- plyr::ddply(JBMP, .(Year), summarise,
                              N    = length(!is.na(percent)),
                              mean = mean(percent, na.rm=TRUE),
                              sd   = sd(percent, na.rm=TRUE),
                              se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

JBMP_percentcover_plot <- ggplot(JBMP_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_cover$Year-0.125), max(JBMP_cover$Year+0.125)), breaks=min(JBMP_cover$Year):max(JBMP_cover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year")+
  ylab(expression(paste("Mean percent cover"))) +
  # ggtitle("a)")+
  #  facet_wrap(~ Category, nrow = 2)+
  #  geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw()+ graphics

JBMP_percentcover_plot

attach(JBMP_cover)
MannKendall(mean)
detach(JBMP_cover)

################################################################
#JBMP_south percent cover

JBMP_south_cover <- plyr::ddply(JBMP_south, .(Year), summarise,
                          N    = length(!is.na(percent)),
                          mean = mean(percent, na.rm=TRUE),
                          sd   = sd(percent, na.rm=TRUE),
                          se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

JBMP_south_plot <- ggplot(JBMP_south_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(breaks = seq(2011,2016,1), limits=c(min(2011),(max(2016))))+
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste(" "))) +
  ggtitle("c) South")+
  theme_bw() +graphics

JBMP_south_plot

attach(JBMP_south_cover)
MannKendall(mean)
detach(JBMP_south_cover)

################################################################
#JBMP_centre percent cover

JBMP_centre_cover <- plyr::ddply(JBMP_centre, .(Year), summarise,
                                N    = length(!is.na(percent)),
                                mean = mean(percent, na.rm=TRUE),
                                sd   = sd(percent, na.rm=TRUE),
                                se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

JBMP_centre_plot <- ggplot(JBMP_centre_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_centre_cover$Year-0.125), max(JBMP_centre_cover$Year+0.125)), breaks=min(JBMP_centre_cover$Year):max(JBMP_centre_cover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover")))+
  ggtitle("b) Centre")+
  theme_bw()+ graphics+
  theme(axis.title.x=element_blank())

JBMP_centre_plot

attach(JBMP_centre_cover)
MannKendall(mean)
detach(JBMP_centre_cover)

################################################################
#JBMP_north percent cover

JBMP_north_cover <- plyr::ddply(JBMP_north, .(Year), summarise,
                                N    = length(!is.na(percent)),
                                mean = mean(percent, na.rm=TRUE),
                                sd   = sd(percent, na.rm=TRUE),
                                se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

JBMP_north_plot <- ggplot(JBMP_north_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(limits=c(min(JBMP_north_cover$Year-0.125), max(JBMP_north_cover$Year+0.125)), breaks=min(JBMP_north_cover$Year):max(JBMP_north_cover$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste(" "))) +
  ggtitle("a) North")+
  theme_bw()+ graphics+
  theme(axis.title.x=element_blank())

JBMP_north_plot

attach(JBMP_north_cover)
MannKendall(mean)
detach(JBMP_north_cover)


JBMP_facet_cover <- plyr::ddply(JBMP, .(Year, Location), summarise,
                                N    = length(!is.na(percent)),
                                mean = mean(percent, na.rm=TRUE),
                                sd   = sd(percent, na.rm=TRUE),
                                se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

JBMP_facet_plot <- ggplot(JBMP_facet_cover, aes(x=Year, y=mean))+#, colour = Category, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", linetype = 1, position=pd) +
  geom_point(position=pd, size=3) + # 21 is filled circle
  scale_x_continuous(breaks = seq(2010,2016,1), limits=c(min(2010),(max(2016))))+
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste(" "))) +
  ggtitle("c) South")+
  facet_wrap(Location)+
  theme_bw() +graphics

JBMP_facet_plot

attach(JBMP_south_cover)
MannKendall(mean)
detach(JBMP_south_cover)


#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

#Percent cover

png(png_JBMP_overall_percentcover_fn, width=600, height=400)
grid.arrange(JBMP_percentcover_plot, ncol=1)
dev.off()

png(png_JBMP_percentcover_fn, width=600, height=800)
grid.arrange(JBMP_north_plot, JBMP_centre_plot, JBMP_south_plot, ncol = 1)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "percent_cover_code.R")

# Step 6: set workdir to main report location
setwd("~/projects")
