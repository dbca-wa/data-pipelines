setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SBMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"
pdf_rid <- "24a36936-1905-46c1-8bde-a438a7e376a7"
txt_rid <- "51ffebf0-1d50-4fe3-8638-78727da3f214"
pdf_fn = "final.pdf"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))

names(d)[names(d) == 'Park_name'] <- 'Park'###Changes column name
names(d)[names(d) == 'Sites'] <- 'Site'###Changes column name 
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
#SBMP_east Bundles per stem_Amphibolis

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
SBMP_e = subset(d, Site %in% c("Wooramel North_Amphibolis", "Disappointment Reach_Amphibolis", "Gladstone Site 2_Amphibolis",  "Herald Loop_Amphibolis", "Gladstone Marker_Amphibolis")) 

d_sum <- plyr::ddply(SBMP_e, .(Year, Zone), summarise,
                     N    = length(!is.na(Amphib_clusters.per.stem)),
                     mean = mean(Amphib_clusters.per.stem, na.rm=TRUE),
                     sd   = sd(Amphib_clusters.per.stem, na.rm=TRUE),
                     se   = sd(Amphib_clusters.per.stem, na.rm=TRUE) / sqrt(length(!is.na(Amphib_clusters.per.stem)) ))

SBMP_e_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.00, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean leaf bundles per stem", sep = ""))) +
  ggtitle("a) Wooramel_Amphibolis")+
  theme_bw() + graphics

SBMP_e_plot

#############################################################
#SBMP Monkey Mia Bundles per stem_Amphibolis

SBMP_m = subset(d, Site %in% c("Herald Bight_west_Amphibolis" , "Monkey Mia control_Amphibolis", "Monkey Mia Outer Bank_Amphibolis"))

d_sum_SBMPm <- ddply(SBMP_m, .(Year, Zone), summarise,
                     N    = length(!is.na(Amphib_clusters.per.stem)),
                     mean = mean(Amphib_clusters.per.stem, na.rm=TRUE),
                     sd   = sd(Amphib_clusters.per.stem, na.rm=TRUE),
                     se   = sd(Amphib_clusters.per.stem, na.rm=TRUE) / sqrt(length(!is.na(Amphib_clusters.per.stem)) ))

SBMP_m_plot<-ggplot(d_sum_SBMPm, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.0, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014-0.125), max(d_sum_SBMPm$Year+0.125)), breaks=min(d_sum_SBMPm$Year):max(d_sum_SBMPm$Year)) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean leaf bundles per stem", sep = ""))) +
  ggtitle("b) Monkey Mia_Amphibolis")+
  theme_bw() + graphics

SBMP_m_plot

#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=3)
grid.arrange(SBMP_e_plot, SBMP_m_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "Amphib_bundles_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
