setwd("~/projects/data-pipelines/scripts/indicators/macroalgae/MMP")
source("~/projects/data-pipelines/setup/ckan.R")


library(gridExtra)
library(ggplot2)
library (plyr)
library(Kendall)
library(dplyr)

######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

# csv_rid <- "619d13a7-5df5-46e3-8391-50a2390e8df2"
# txt_rid <- "a25672bd-15d8-4644-933f-3eaa9fe6b320"

#percent cover plots
# png_SIMP_overallpercentcover_rid <- "f98ef2b7-2c97-4d7b-a0d7-10d3001a6dfb"
png_MMP_overallpercentcover_fn <-"MMP_overallpercentcover.png"
# png_SIMP_percentcover_rid <-"70afbaf6-33d5-4e4c-9b9b-933eca251d36"
png_MMP_percentcover_fn <-"MMP_percentcover.png"

###################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid)
d<-PER_All_Macroalgae
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
# All canopy algae pooled

MMP = subset (d, Park=="Marmion Marine Park")
unique(MMP$Baseclassmodifiers)
detach("package:dplyr", unload=TRUE)

cover=count(MMP, c("Site", "Year", "Baseclassmodifiers")) #counts number of observations per site, per year
cover_obs=count(cover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
cover_add <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
canopy_cover = subset(cover_add, Baseclassmodifiers %in% c("Canopy")) #Extracts cover information only
MMP_canopy = count(canopy_cover, c("Site", "Year", "freq.1"), "freq")
names(MMP_canopy)[3] <- "total_count" #Rename column to make more sense
names(MMP_canopy)[4] <- "canopy_count" #Rename column to make more sense
MMP_canopy$percent = MMP_canopy$canopy_count/MMP_canopy$total_count *100 #Calculate percent cover

library(dplyr)

##################################################################################
#Create subsets for each 'sector (south, centre, north) for SIMP
##################################################################################
unique(MMP_canopy$Year)
MMP_south = subset(MMP_canopy, Site %in% c("Watermans", "Watermans Outer"))
MMP_centre = subset(MMP_canopy, Site %in% c("Little Island", "The Lumps"))
MMP_north = subset(MMP_canopy, Site %in% c("Burns Rocks", "Burns Rocks Offshore", "Three Mile Reef Inner","Three Mile Reef Outer"))

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

#####################################################################
#overall
MMP_cover <- make_cover(MMP_canopy)

MMP_percentcover_plot <- ggplot(MMP_cover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(SIMP_cover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

MMP_percentcover_plot

attach(MMP_cover)
MannKendall(mean)
detach(MMP_cover)

#############################################################
#South cover

MMP_south_percentcover <- make_cover(MMP_south)

MMP_south_percentcover_plot <- ggplot(MMP_south_percentcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(2015))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("c) South")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

MMP_south_percentcover_plot

attach(MMP_south_percentcover)
MannKendall(mean)
detach(MMP_south_percentcover)

#############################################################
#Centre percent cover

MMP_centre_percentcover <- make_cover(MMP_centre)

MMP_centre_percentcover_plot <- ggplot(MMP_centre_percentcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(2015))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("b) Centre")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

MMP_centre_percentcover_plot

attach(MMP_south_percentcover)
MannKendall(mean)
detach(MMP_south_percentcover)

#######################################################################################
#SIMP_north percent cover

MMP_north_percentcover <- make_cover(MMP_north)

MMP_north_percentcover_plot <- ggplot(MMP_north_percentcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  # geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2012,2017,1), limits=c(min(2012), max(2015))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) canopy cover", sep = ""))) +
  ggtitle("a) North")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

MMP_north_percentcover_plot

attach(MMP_south_percentcover)
MannKendall(mean)
detach(MMP_south_percentcover)

#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

png(png_MMP_overallpercentcover_fn, width=500, height=300)
grid.arrange(MMP_percentcover_plot)
dev.off()

png(png_MMP_percentcover_fn, width=500, height=900)
grid.arrange(MMP_north_percentcover_plot, MMP_centre_percentcover_plot, MMP_south_percentcover_plot, ncol=1)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "percent_cover_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
