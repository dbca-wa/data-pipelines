setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SBMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)
library (Kendall)


######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"#CKAN resource ID for data
txt_rid <- "6b003575-fa64-4a48-8ae6-7f6988daa263"#CKAN resource ID for r-script

#Amphibolis density
png_SBMP_amphib_density_rid <- "46ed6691-e6bf-4280-a718-e33d58170da4"#CKAN resource ID for final figure (pdf)
png_SBMP_amphib_density_fn = "SBMP leaf bundles.png"#Name of final figure
png_SBMP_overall_amphib_density_rid <- "fa41c293-46d8-4aec-9d8f-6637166d2e7a"#CKAN resource ID for final figure (png)
png_SBMP_overall_amphib_density_fn = "SBMP overall leaf bundles.png"#Name of final figure

#Leaf bundle plots
png_SBMP_leaf_bundles_rid <- "2ca887ab-e3f7-4fc0-8176-65d33902c9c6"#CKAN resource ID for final figure (pdf)
png_SBMP_leaf_bundles_fn = "SBMP leaf bundles.png"#Name of final figure
png_SBMP_overall_leaf_bundles_rid <- "f6b664ad-932b-4311-a785-a617309e7a24"#CKAN resource ID for final figure (png)
png_SBMP_overall_leaf_bundles_fn = "SBMP overall leaf bundles.png"#Name of final figure

#leaves per bundle plots
png_SBMP_leaf_per_bundle_rid <- "19cc4504-6af9-49b2-bb21-c8dc68d2daf8"#CKAN resource ID for final figure (pdf)
png_SBMP_leaf_per_bundle_fn = "SBMP leaves per bundle.png"#Name of final figure
png_SBMP_overall_leaf_per_bundle_rid <- "7939e8bb-4843-4c75-a840-b17ef281b592"#CKAN resource ID for final figure (png)
png_SBMP_overall_leaf_per_bundle_fn = "SBMP overall leaves per bundle.png"#Name of final figure

###################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))
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

#################################################################################
#Create subsets for each 'sector (Wooramel, Monkey Mia, Peron and Western Gulf) for SBMP
##################################################################################

SBMP_amphib = subset (d, Method=="Amphibolis density" & Park %in% c("SBMP"))
SBMP_amphib_westerngulf= subset(SBMP_amphib, Site %in% c("SBMR 0464 Amphibolis", "0037 Shark Bay Amphibolis", "0380 Settlement Amphibolis", "Useless Loop North_Amphibolis"))
SBMP_amphib_peron = subset(SBMP_amphib, Site %in% c("SBMR 0581 Amphibolis","SBMR 0433 Amphibolis","SBMR 0459 Amphibolis","SBMP 0595 Amphibolis","SBMR 0466 Amphibolis"))
SBMP_amphib_monkeymia = subset(SBMP_amphib, Site %in% c("Herald Bight west Amphibolis", "Monkey Mia Outer Bank_Amphibolis", "Monkey Mia control_Amphibolis"))
SBMP_amphib_wooramel = subset(SBMP_amphib, Site %in% c("Gladstone Site 2_Amphibolis", "Gladstone Marker_Amphibolis", "Herald Loop_Amphibolis", "Wooramel north_Amphibolis", "Disappointment Reach_Amphibolis"))

SBMP = subset (d, Method=="Amphibolis morphology" & Park %in% c("SBMP"))
SBMP_westerngulf = subset(SBMP, Site %in% c("SBMR 0464 Amphibolis", "0037 Shark Bay Amphibolis", "0380 Settlement Amphibolis", "Useless Loop North_Amphibolis"))
SBMP_peron = subset(SBMP, Site %in% c("SBMR 0581 Amphibolis","SBMR 0433 Amphibolis","SBMR 0459 Amphibolis","SBMP 0595 Amphibolis","SBMR 0466 Amphibolis"))
SBMP_monkeymia = subset(SBMP, Site %in% c("Herald Bight west Amphibolis", "Monkey Mia Outer Bank_Amphibolis", "Monkey Mia control_Amphibolis"))
SBMP_wooramel = subset(SBMP, Site %in% c("Gladstone Site 2_Amphibolis", "Gladstone Marker_Amphibolis", "Herald Loop_Amphibolis", "Wooramel north_Amphibolis", "Disappointment Reach_Amphibolis"))


#################################################################
#AMPHIBOLIS DENSITY
#################################################################

#Overall Amphibolis density
SBMP_amphibdensity <- plyr::ddply(SBMP_amphib, .(Year), summarise,
                                 N    = length(!is.na(Amphibolis_antarctica)),
                                 mean = mean(Amphibolis_antarctica, na.rm=TRUE),
                                 sd   = sd(Amphibolis_antarctica, na.rm=TRUE),
                                 se   = sd(Amphibolis_antarctica, na.rm=TRUE) / sqrt(length(!is.na(Amphibolis_antarctica)) ))

SBMP_amphibdensity_plot <- ggplot(SBMP_amphibdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_amphibdensity$Year-0.125), max(SBMP_amphibdensity$Year+0.125)), breaks=min(SBMP_amphibdensity$Year):max(SBMP_amphibdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(Mean~density~of~italic(Amphibolis)~("0.04m"^2))) +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_amphibdensity_plot

attach(SBMP_amphibdensity)
MannKendall(mean)
detach(SBMP_amphibdensity)

##################################################################################
#Western gulf Amphibolis density

SBMP_amphib_westerngulf_density <- plyr::ddply(SBMP_amphib_westerngulf, .(Year), summarise,
                                  N    = length(!is.na(Amphibolis_antarctica)),
                                  mean = mean(Amphibolis_antarctica, na.rm=TRUE),
                                  sd   = sd(Amphibolis_antarctica, na.rm=TRUE),
                                  se   = sd(Amphibolis_antarctica, na.rm=TRUE) / sqrt(length(!is.na(Amphibolis_antarctica)) ))

SBMP_amphib_westerngulf_density_plot <- ggplot(SBMP_amphib_westerngulf_density, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_amphib_westerngulf_density$Year-0.125), max(SBMP_amphib_westerngulf_density$Year+0.125)), breaks=min(SBMP_amphib_westerngulf_density$Year):max(SBMP_amphib_westerngulf_density$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(Mean~density~of~italic(Amphibolis)~("0.04m"^2))) +
  ggtitle("a) Western Gulf") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_amphib_westerngulf_density_plot

attach(SBMP_amphib_westerngulf_density)
MannKendall(mean)
detach(SBMP_amphib_westerngulf_density)

#############################################################
#Peron shoot Amphibolis density

SBMP_amphib_peron_density <- plyr::ddply(SBMP_amphib_peron, .(Year), summarise,
                                               N    = length(!is.na(Amphibolis_antarctica)),
                                               mean = mean(Amphibolis_antarctica, na.rm=TRUE),
                                               sd   = sd(Amphibolis_antarctica, na.rm=TRUE),
                                               se   = sd(Amphibolis_antarctica, na.rm=TRUE) / sqrt(length(!is.na(Amphibolis_antarctica)) ))

SBMP_amphib_peron_density_plot <- ggplot(SBMP_amphib_peron_density, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_amphib_peron_density$Year-0.125), max(SBMP_amphib_peron_density$Year+0.125)), breaks=min(SBMP_amphib_peron_density$Year):max(SBMP_amphib_peron_density$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(Mean~density~of~italic(Amphibolis)~("0.04m"^2))) +
  ggtitle("b) Peron") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_amphib_peron_density_plot

attach(SBMP_amphib_peron_density)
MannKendall(mean)
detach(SBMP_amphib_peron_density)

#############################################################################################
#monkey mia Amphibolis density

SBMP_amphib_monkeymia_density <- plyr::ddply(SBMP_amphib_monkeymia, .(Year), summarise,
                                         N    = length(!is.na(Amphibolis_antarctica)),
                                         mean = mean(Amphibolis_antarctica, na.rm=TRUE),
                                         sd   = sd(Amphibolis_antarctica, na.rm=TRUE),
                                         se   = sd(Amphibolis_antarctica, na.rm=TRUE) / sqrt(length(!is.na(Amphibolis_antarctica)) ))

SBMP_amphib_monkeymia_density_plot <- ggplot(SBMP_amphib_monkeymia_density, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_amphib_monkeymia_density$Year-0.125), max(SBMP_amphib_monkeymia_density$Year+0.125)), breaks=min(SBMP_amphib_monkeymia_density$Year):max(SBMP_amphib_monkeymia_density$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(Mean~density~of~italic(Amphibolis)~("0.04m"^2))) +
  ggtitle("c) Monkey Mia") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_amphib_monkeymia_density_plot

attach(SBMP_amphib_monkeymia_density)
MannKendall(mean)
detach(SBMP_amphib_monkeymia_density)

###########################################################################
#SBMP_wooramel Amphibolis density

SBMP_amphib_wooramel_density <- plyr::ddply(SBMP_amphib_wooramel, .(Year), summarise,
                                         N    = length(!is.na(Amphibolis_antarctica)),
                                         mean = mean(Amphibolis_antarctica, na.rm=TRUE),
                                         sd   = sd(Amphibolis_antarctica, na.rm=TRUE),
                                         se   = sd(Amphibolis_antarctica, na.rm=TRUE) / sqrt(length(!is.na(Amphibolis_antarctica)) ))

SBMP_amphib_wooramel_density_plot <- ggplot(SBMP_amphib_wooramel_density, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_amphib_wooramel_density$Year-0.125), max(SBMP_amphib_wooramel_density$Year+0.125)), breaks=min(SBMP_amphib_wooramel_density$Year):max(SBMP_amphib_wooramel_density$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(Mean~density~of~italic(Amphibolis)~("0.04m"^2))) +
  ggtitle("d) Wooramel") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics

SBMP_amphib_wooramel_density_plot

attach(SBMP_amphib_wooramel_density)
MannKendall(mean)
detach(SBMP_amphib_wooramel_density)


####################################################################################
#Leaf bundles per stem
####################################################################################

#Overall bundles per stem

SBMP_bundles <- plyr::ddply(SBMP, .(Year), summarise,
                                 N    = length(!is.na(Amphib_clusters_per_stem)),
                                 mean = mean(Amphib_clusters_per_stem, na.rm=TRUE),
                                 sd   = sd(Amphib_clusters_per_stem, na.rm=TRUE),
                                 se   = sd(Amphib_clusters_per_stem, na.rm=TRUE) / sqrt(length(!is.na(Amphib_clusters_per_stem)) ))

SBMP_bundles_plot <- ggplot(SBMP_bundles, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_bundles$Year+0.125)), breaks=min(SBMP_bundles$Year):max(SBMP_bundles$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaf bundles per stem", sep = ""))) +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_bundles_plot

attach(SBMP_bundles)
MannKendall(mean)
detach(SBMP_bundles)

####################################################################################
#western gulf bundles per stem

SBMP_westerngulf_bundles <- plyr::ddply(SBMP_westerngulf, .(Year), summarise,
                            N    = length(!is.na(Amphib_clusters_per_stem)),
                            mean = mean(Amphib_clusters_per_stem, na.rm=TRUE),
                            sd   = sd(Amphib_clusters_per_stem, na.rm=TRUE),
                            se   = sd(Amphib_clusters_per_stem, na.rm=TRUE) / sqrt(length(!is.na(Amphib_clusters_per_stem)) ))

SBMP_westerngulf_bundles_plot <- ggplot(SBMP_westerngulf_bundles, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_westerngulf_bundles$Year+0.125)), breaks=min(SBMP_westerngulf_bundles$Year):max(SBMP_westerngulf_bundles$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaf bundles per stem", sep = ""))) +
  ggtitle("a) Western Gulf") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_westerngulf_bundles_plot

attach(SBMP_westerngulf_bundles)
MannKendall(mean)
detach(SBMP_westerngulf_bundles)

####################################################################################
#peron bundles per stem

SBMP_peron_bundles <- plyr::ddply(SBMP_peron, .(Year), summarise,
                                        N    = length(!is.na(Amphib_clusters_per_stem)),
                                        mean = mean(Amphib_clusters_per_stem, na.rm=TRUE),
                                        sd   = sd(Amphib_clusters_per_stem, na.rm=TRUE),
                                        se   = sd(Amphib_clusters_per_stem, na.rm=TRUE) / sqrt(length(!is.na(Amphib_clusters_per_stem)) ))

SBMP_peron_bundles_plot <- ggplot(SBMP_peron_bundles, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_peron_bundles$Year+0.125)), breaks=min(SBMP_peron_bundles$Year):max(SBMP_peron_bundles$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaf bundles per stem", sep = ""))) +
  ggtitle("b) Peron") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_peron_bundles_plot

attach(SBMP_westerngulf_bundles)
MannKendall(mean)
detach(SBMP_westerngulf_bundles)

####################################################################################
#monkeymia bundles per stem

SBMP_monkeymia_bundles <- plyr::ddply(SBMP_monkeymia, .(Year), summarise,
                                  N    = length(!is.na(Amphib_clusters_per_stem)),
                                  mean = mean(Amphib_clusters_per_stem, na.rm=TRUE),
                                  sd   = sd(Amphib_clusters_per_stem, na.rm=TRUE),
                                  se   = sd(Amphib_clusters_per_stem, na.rm=TRUE) / sqrt(length(!is.na(Amphib_clusters_per_stem)) ))

SBMP_monkeymia_bundles_plot <- ggplot(SBMP_monkeymia_bundles, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2009), max(SBMP_monkeymia_bundles$Year+0.125)), breaks=min(SBMP_monkeymia_bundles$Year):max(SBMP_monkeymia_bundles$Year)) +
  scale_y_continuous(limits=c(min(0), max(30)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaf bundles per stem", sep = ""))) +
  ggtitle("c) Monkey Mia") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_monkeymia_bundles_plot

attach(SBMP_westerngulf_bundles)
MannKendall(mean)
detach(SBMP_westerngulf_bundles)

####################################################################################
#wooramel bundles per stem

SBMP_wooramel_bundles <- plyr::ddply(SBMP_wooramel, .(Year), summarise,
                                      N    = length(!is.na(Amphib_clusters_per_stem)),
                                      mean = mean(Amphib_clusters_per_stem, na.rm=TRUE),
                                      sd   = sd(Amphib_clusters_per_stem, na.rm=TRUE),
                                      se   = sd(Amphib_clusters_per_stem, na.rm=TRUE) / sqrt(length(!is.na(Amphib_clusters_per_stem)) ))

SBMP_wooramel_bundles_plot <- ggplot(SBMP_wooramel_bundles, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_wooramel_bundles$Year+0.125)), breaks=min(SBMP_wooramel_bundles$Year):max(SBMP_wooramel_bundles$Year)) +
  scale_y_continuous(limits=c(min(0), max(30)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaf bundles per stem", sep = ""))) +
  ggtitle("d) Wooramel") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_wooramel_bundles_plot

attach(SBMP_westerngulf_bundles)
MannKendall(mean)
detach(SBMP_westerngulf_bundles)


####################################################################################
#Leaves per bundle
####################################################################################

#Overall leaves per bundle

SBMP_leafperbundle <- plyr::ddply(SBMP, .(Year), summarise,
                            N    = length(!is.na(Mean_leaves_per_cluster)),
                            mean = mean(Mean_leaves_per_cluster, na.rm=TRUE),
                            sd   = sd(Mean_leaves_per_cluster, na.rm=TRUE),
                            se   = sd(Mean_leaves_per_cluster, na.rm=TRUE) / sqrt(length(!is.na(Mean_leaves_per_cluster)) ))

SBMP_leafperbundle_plot <- ggplot(SBMP_leafperbundle, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_leafperbundle$Year+0.125)), breaks=min(SBMP_leafperbundle$Year):max(SBMP_leafperbundle$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaves per bundle", sep = ""))) +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_leafperbundle_plot

attach(SBMP_leafperbundle)
MannKendall(mean)
detach(SBMP_leafperbundle)

####################################################################################
#western gulf leaf per bundle

SBMP_westerngulf_leafperbundle <- plyr::ddply(SBMP_westerngulf, .(Year), summarise,
                                        N    = length(!is.na(Mean_leaves_per_cluster)),
                                        mean = mean(Mean_leaves_per_cluster, na.rm=TRUE),
                                        sd   = sd(Mean_leaves_per_cluster, na.rm=TRUE),
                                        se   = sd(Mean_leaves_per_cluster, na.rm=TRUE) / sqrt(length(!is.na(Mean_leaves_per_cluster)) ))

SBMP_westerngulf_leafperbundle_plot <- ggplot(SBMP_westerngulf_leafperbundle, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_westerngulf_leafperbundle$Year+0.125)), breaks=min(SBMP_westerngulf_leafperbundle$Year):max(SBMP_westerngulf_leafperbundle$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaves per bundle", sep = ""))) +
  ggtitle("a) Western Gulf") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_westerngulf_leafperbundle_plot

attach(SBMP_westerngulf_leafperbundle)
MannKendall(mean)
detach(SBMP_westerngulf_leafperbundle)

####################################################################################
#peron leaf per bundle

SBMP_peron_leafperbundle <- plyr::ddply(SBMP_peron, .(Year), summarise,
                                  N    = length(!is.na(Mean_leaves_per_cluster)),
                                  mean = mean(Mean_leaves_per_cluster, na.rm=TRUE),
                                  sd   = sd(Mean_leaves_per_cluster, na.rm=TRUE),
                                  se   = sd(Mean_leaves_per_cluster, na.rm=TRUE) / sqrt(length(!is.na(Mean_leaves_per_cluster)) ))

SBMP_peron_leafperbundle_plot <- ggplot(SBMP_peron_leafperbundle, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_peron_leafperbundle$Year+0.125)), breaks=min(SBMP_peron_leafperbundle$Year):max(SBMP_peron_leafperbundle$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaves per bundle", sep = ""))) +
  ggtitle("b) Peron") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_peron_leafperbundle_plot

attach(SBMP_peron_leafperbundle)
MannKendall(mean)
detach(SBMP_peron_leafperbundle)

####################################################################################
#monkeymia leaf per bundle

SBMP_monkeymia_leafperbundle <- plyr::ddply(SBMP_monkeymia, .(Year), summarise,
                                      N    = length(!is.na(Mean_leaves_per_cluster)),
                                      mean = mean(Mean_leaves_per_cluster, na.rm=TRUE),
                                      sd   = sd(Mean_leaves_per_cluster, na.rm=TRUE),
                                      se   = sd(Mean_leaves_per_cluster, na.rm=TRUE) / sqrt(length(!is.na(Mean_leaves_per_cluster)) ))

SBMP_monkeymia_leafperbundle_plot <- ggplot(SBMP_monkeymia_leafperbundle, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_monkeymia_leafperbundle$Year+0.125)), breaks=min(SBMP_monkeymia_leafperbundle$Year):max(SBMP_monkeymia_leafperbundle$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaves per bundle", sep = ""))) +
  ggtitle("c) Monkey Mia") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_monkeymia_leafperbundle_plot

attach(SBMP_monkeymia_leafperbundle)
MannKendall(mean)
detach(SBMP_monkeymia_leafperbundle)

####################################################################################
#wooramel leaf per bundle

SBMP_wooramel_leafperbundle <- plyr::ddply(SBMP_wooramel, .(Year), summarise,
                                     N    = length(!is.na(Mean_leaves_per_cluster)),
                                     mean = mean(Mean_leaves_per_cluster, na.rm=TRUE),
                                     sd   = sd(Mean_leaves_per_cluster, na.rm=TRUE),
                                     se   = sd(Mean_leaves_per_cluster, na.rm=TRUE) / sqrt(length(!is.na(Mean_leaves_per_cluster)) ))

SBMP_wooramel_leafperbundle_plot <- ggplot(SBMP_wooramel_leafperbundle, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2014), max(SBMP_wooramel_leafperbundle$Year+0.125)), breaks=min(SBMP_wooramel_leafperbundle$Year):max(SBMP_wooramel_leafperbundle$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean no. of leaves per bundle", sep = ""))) +
  ggtitle("d) Wooramel") +
  geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SBMP_wooramel_leafperbundle_plot

attach(SBMP_wooramel_leafperbundle)
MannKendall(mean)
detach(SBMP_wooramel_leafperbundle)

####################################################################################
#MAXIMUM CANOPY HEIGHT
####################################################################################

#Overall max height
SBMP_maxheight <- plyr::ddply(SBMP, .(Year), summarise,
                              N    = length(!is.na(Maximum_height_mm)),
                              mean = mean(Maximum_height_mm, na.rm=TRUE),
                              sd   = sd(Maximum_height_mm, na.rm=TRUE),
                              se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))
SBMP_maxheight

SBMP_maxheight_plot <- ggplot(SBMP_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_maxheight$Year-0.125), max(SBMP_maxheight$Year+0.125)), breaks=min(SBMP_maxheight$Year):max(SBMP_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(600)))+
  xlab("Year") +
  ylab(expression(paste("Mean maximum canopy height (mm)", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, linetype=3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(axis.text.x=element_text(angle=45, size = 10, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
        axis.title.x=element_text( size = 15), #removes x axis title
        axis.title.y=element_text(size = 15), #removes y axis title
        axis.text.y=element_text(size = 10))

SBMP_maxheight_plot

attach(SBMP_maxheight)
MannKendall(mean)
detach(SBMP_maxheight)


#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

#Amphibolis density

png(png_SBMP_overall_amphib_density_fn, width=500, height=300)
grid.arrange(SBMP_amphibdensity_plot)
dev.off()

png(png_SBMP_amphib_density_fn, width=1000, height=800)
grid.arrange(SBMP_amphib_westerngulf_density_plot, SBMP_amphib_peron_density_plot, SBMP_amphib_monkeymia_density_plot,ncol=2)
dev.off()

#Leaf bundles per stem

png(png_SBMP_overall_leaf_bundles_fn, width=500, height=300)
grid.arrange(SBMP_leafperbundle_plot)
dev.off()

png(png_SBMP_leaf_bundles_fn, width=1000, height=800)
grid.arrange(SBMP_westerngulf_bundles_plot, SBMP_peron_bundles_plot, SBMP_monkeymia_bundles_plot, SBMP_wooramel_bundles_plot,ncol=2)
dev.off()

#Leaves per bundle per stem

png(png_SBMP_overall_leaf_per_bundle_fn, width=500, height=300)
grid.arrange(SBMP_leafperbundle_plot)
dev.off()

png(png_SBMP_leaf_per_bundle_fn, width=1000, height=800)
grid.arrange(SBMP_westerngulf_leafperbundle_plot, SBMP_peron_leafperbundle_plot, SBMP_monkeymia_leafperbundle_plot, SBMP_wooramel_leafperbundle_plot, ncol=2)
dev.off()

#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_SBMP_overall_amphib_density_rid,png_SBMP_overall_amphib_density_fn)
ckanr::resource_update(png_SBMP_amphib_density_rid, png_SBMP_amphib_density_fn)

ckanr::resource_update(png_SBMP_overall_leaf_bundles_rid, png_SBMP_overall_leaf_bundles_fn)
ckanr::resource_update(png_SBMP_leaf_bundles_rid, png_SBMP_leaf_bundles_fn)

ckanr::resource_update(png_SBMP_overall_leaf_per_bundle_rid, png_SBMP_overall_leaf_per_bundle_fn)
ckanr::resource_update(png_SBMP_leaf_per_bundle_rid, png_SBMP_leaf_per_bundle_fn)

ckanr::resource_update(txt_rid, "Amphib_bundles_code.R")

#####################################################################################
#set workdir to main report location
setwd("~/projects/mpa-reporting/reports")
######################################################################################
