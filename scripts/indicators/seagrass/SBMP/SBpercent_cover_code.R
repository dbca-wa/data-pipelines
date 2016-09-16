setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SBMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "20180ac3-9b5a-493e-9669-b7a2aff28f68"
pdf_rid <- "ad4b61b9-f9e7-4f53-a8f3-90ee3628aa5d"
txt_rid <- "0bba4f55-e3af-490e-88bb-738660ef16e2"
pdf_fn = "final.pdf"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))

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

cover=count(d, c("Region", "Zone", "Site", "Year", "Level1Class")) #counts number of observations per site, per year
cover_obs=count(cover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
cover_add <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
pos_cover = subset(cover_add, Level1Class %in% c("SEAGRASS")) #Extracts cover information only
pos_total = count(pos_cover, c("Region","Zone", "Site", "Year", "freq.1"), "freq")
names(pos_total)[5] <- "total_count" #Rename column to make more sense
names(pos_total)[6] <- "pos_count" #Rename column to make more sense
pos_total$percent = pos_total$pos_count/pos_total$total_count *100 #Calculate percent cover

##################################################################################
#SBMP_Wooramel percent cover

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
SBMP_s = subset(pos_total, Site %in% c("Wooramel North", "Wooramel South","Disappointment Reach", "Disappointment Reach-Amphibolis"," Wooramel North-Amphibolis" )) 

d_sum <- plyr::ddply(SBMP_s, .(Year, Zone), summarise,
                     N    = length(!is.na(percent)),
                     mean = mean(percent, na.rm=TRUE),
                     sd   = sd(percent, na.rm=TRUE),
                     se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SBMP_s_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("a) Wooramel")+
  theme_bw() + graphics

SBMP_s_plot

#############################################################
#SBMP Monkley Mia percent cover
SBMP_m = subset(pos_total, Site %in% c("Herald Bight-West-Amphibolis" , "Monkey Mia South-outer", "Monkey Mia inner bank" , "Pearl Farm Control", "Monkey Mia South" , "East Peron"))

d_sum_sbmpm <- plyr::ddply(SBMP_m, .(Year, Zone), summarise,
                   N    = length(!is.na(percent)),
                   mean = mean(percent, na.rm=TRUE),
                   sd   = sd(percent, na.rm=TRUE),
                   se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SBMP_m_plot<-ggplot(d_sum_sbmpm, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_sbmpm$Year-0.125), max(d_sum_sbmpm$Year+0.125)), breaks=min(d_sum_sbmpm$Year):max(d_sum_sbmpm$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("b) Peron Peninsula east")+
  theme_bw() + graphics

SBMP_m_plot

#################################################################
#SBMP_Peron west percent cover

SBMP_den = subset(pos_total, Site %in% c("Big Lagoon" , "Peron South", "Denham"))

d_sum_den <- plyr::ddply(SBMP_den, .(Year, Zone), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))


SBMP_den_plot <- ggplot(d_sum_den, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_den$Year-0.125), max(d_sum_den$Year+0.125)), breaks=min(d_sum_den$Year):max(d_sum_den$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("c) Peron Peninsula west")+
  theme_bw() + graphics

SBMP_den_plot

###########################################################################
#SBMP_western gulf percent cover

SBMP_w = subset(pos_total, Site %in% c("Useless Loop North", "South Passage", "Sandy Point","Useless Loop South"))

d_sum_sbmpw <- plyr::ddply(SBMP_w, .(Year, Zone), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

SBMP_w_plot<-ggplot(d_sum_sbmpw, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_sbmpw$Year-0.125), max(d_sum_sbmpw$Year+0.125)), breaks=min(d_sum_sbmpw$Year):max(d_sum_sbmpw$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m", ")", sep = ""))) +
  ggtitle("d) Western Gulf")+
  theme_bw() + graphics

SBMP_w_plot
#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(SBMP_s_plot, SBMP_m_plot, SBMP_den_plot, SBMP_w_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "SBpercent_cover_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
