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

#Shoot density plots
png_SBMP_shoot_density_rid <- "adcea303-211a-4f54-a993-32ba228e5aae"#CKAN resource ID for final figure (png)
png_SBMP_shoot_density_fn = "SBMP shoot density.png"#Name of final figure
png_SBMP_overall_shoot_density_rid <- "2d0e900e-4b3a-419d-b8df-e8ed655b667a"#CKAN resource ID for final figure (png)
png_SBMP_overall_shoot_density_fn = "SBMP overall shoot density.png"#Name of final figure

#Maximum canopy height plots
png_SBMP_max_height_rid <- "ab48be40-8830-402b-9618-30740c50e822"#CKAN resource ID for final figure (png)
png_SBMP_max_height_fn = "SBMP max height.png"#Name of final figure
png_SBMP_overall_max_height_rid <- "a5630713-39f5-4228-955c-7b3c34e8fa64"#CKAN resource ID for final figure (png)
png_SBMP_overall_max_height_fn = "SBMP overall max height.png"#Name of final figure


#Mean canopy height plots
png_SBMP_mean_height_rid <- "720e61e6-7d37-4456-a49c-48998e3ffe49"#CKAN resource ID for final figure (png)
png_SBMP_mean_height_fn = "SBMP mean height.png"#Name of final figure
png_SBMP_overall_mean_height_rid <- "ebe43d88-6cef-4589-bf50-a0cdc04508fb"#CKAN resource ID for final figure (png)
png_SBMP_overall_mean_height_fn = "SBMP overall mean height.png"#Name of final figure


###################################################################################################
#Load data
###################################################################################################

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))
names(d)[names(d) == 'Site_name'] <- 'Site'###Changes column name


####################################################################################################
#Define graphic properties
#####################################################################################################

pd <- position_dodge(0.1)
graphics = theme(axis.text.x=element_text(angle=45, size = 15, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
                 axis.title.x=element_text(size = 20), #removes x axis title
                 axis.title.y=element_text(size = 20), #removes y axis title
                 axis.text.y=element_text(size = 15),
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
#Create subsets for each 'sector (south, centre, north) for SBMP
##################################################################################

SBMP1 = subset(d, Park %in% c("SBMP"))
SBMP = subset(SBMP1, Method %in% c("Posidonia transect/quadrats"))
SBMP_westerngulf = subset(d, Site %in% c("0380 Settlement", "Sandy Point", "South Passage", "Useless Loop North", "Useless Loop South"))
SBMP_peron = subset(d, Site %in% c("Big Lagoon", "Denham", "Peron South"))
SBMP_monkeymia = subset(d, Site %in% c("Monkey Mia Inner Bank" , "Monkey Mia Pearl Control", "East Peron" , "Monkey Mia South", "Monkey Mia south_outer"))
SBMP_wooramel = subset(d, Site %in% c("Wooramel North", "Disappointment Reach"))

####################################################################################
#SHOOT DENSITY
####################################################################################

#OverallShoot density
SBMP_shootdensity <- plyr::ddply(SBMP, .(Year), summarise,
                                 N    = length(!is.na(Posidonia_australis)),
                                 mean = mean(Posidonia_australis, na.rm=TRUE),
                                 sd   = sd(Posidonia_australis, na.rm=TRUE),
                                 se   = sd(Posidonia_australis, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_australis)) ))

SBMP_shootdensity_plot <- ggplot(SBMP_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_shootdensity$Year-0.125), max(SBMP_shootdensity$Year+0.125)), breaks=min(SBMP_shootdensity$Year):max(SBMP_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(axis.text.x=element_text(angle=45, size = 10, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
        axis.title.x=element_text( size = 15), #removes x axis title
        axis.title.y=element_text(size = 15), #removes y axis title
        axis.text.y=element_text(size = 10))

SBMP_shootdensity_plot

attach(SBMP_shootdensity)
MannKendall(mean)
detach(SBMP_shootdensity)

##################################################################################
#SBMP_western gulf Shoot density

SBMP_westerngulf_shootdensity <- plyr::ddply(SBMP_westerngulf, .(Year), summarise,
                                             N    = length(!is.na(Posidonia_australis)),
                                             mean = mean(Posidonia_australis, na.rm=TRUE),
                                             sd   = sd(Posidonia_australis, na.rm=TRUE),
                                             se   = sd(Posidonia_australis, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_australis)) ))

SBMP_westerngulf_shootdensity_plot <- ggplot(SBMP_westerngulf_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") +
  scale_x_continuous(limits=c(min(SBMP_westerngulf_shootdensity$Year-0.125), max(SBMP_westerngulf_shootdensity$Year+0.125)), breaks=min(SBMP_westerngulf_shootdensity$Year):max(SBMP_westerngulf_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("a) Western Gulf")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_westerngulf_shootdensity_plot

attach(SBMP_westerngulf_shootdensity)
MannKendall(mean)
detach(SBMP_westerngulf_shootdensity)

#############################################################
#Peron shoot density

SBMP_peron_shootdensity <- ddply(SBMP_peron, .(Year), summarise,
                                 N    = length(!is.na(Posidonia_australis)),
                                 mean = mean(Posidonia_australis, na.rm=TRUE),
                                 sd   = sd(Posidonia_australis, na.rm=TRUE),
                                 se   = sd(Posidonia_australis, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_australis)) ))

SBMP_peron_shootdensity_plot<-ggplot(SBMP_peron_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_peron_shootdensity$Year-0.125), max(SBMP_peron_shootdensity$Year+0.125)), breaks=min(SBMP_peron_shootdensity$Year):max(SBMP_peron_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("b) Peron")+
  # geom_smooth(method=lm, colour = 1, linetype =3, se=FALSE,fullrange=TRUE)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_peron_shootdensity_plot

attach(SBMP_peron_shootdensity)
MannKendall(mean)
detach(SBMP_peron_shootdensity)

#################################################################
#SBMP_Monkey Mia shoot density

SBMP_monkeymia_shootdensity <- ddply(SBMP_monkeymia, .(Year), summarise,
                                     N    = length(!is.na(Posidonia_australis)),
                                     mean = mean(Posidonia_australis, na.rm=TRUE),
                                     sd   = sd(Posidonia_australis, na.rm=TRUE),
                                     se   = sd(Posidonia_australis, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_australis)) ))

SBMP_monkeymia_shootdensity_plot <- ggplot(SBMP_monkeymia_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_monkeymia_shootdensity$Year-0.125), max(SBMP_monkeymia_shootdensity$Year+0.125)), breaks=min(SBMP_monkeymia_shootdensity$Year):max(SBMP_monkeymia_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(25)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("c) Monkey Mia")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se = FALSE, fullrange=TRUE) +
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_monkeymia_shootdensity_plot

attach(SBMP_monkeymia_shootdensity)
MannKendall(mean)
detach(SBMP_monkeymia_shootdensity)

###########################################################################
#SBMP_wooramel shoot density

SBMP_wooramel_shootdensity <- ddply(SBMP_wooramel, .(Year), summarise,
                                    N    = length(!is.na(Posidonia_australis)),
                                    mean = mean(Posidonia_australis, na.rm=TRUE),
                                    sd   = sd(Posidonia_australis, na.rm=TRUE),
                                    se   = sd(Posidonia_australis, na.rm=TRUE) / sqrt(length(!is.na(Posidonia_australis)) ))

SBMP_wooramel_shootdensity_plot<-ggplot(SBMP_wooramel_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_wooramel_shootdensity$Year-0.125), max(SBMP_wooramel_shootdensity$Year+0.125)), breaks=min(SBMP_wooramel_shootdensity$Year):max(SBMP_wooramel_shootdensity$Year)) +
  scale_y_continuous(limits=c(min(0), max(25)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("d) Wooramel")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se = FALSE, fullrange=TRUE) +
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_wooramel_shootdensity_plot

attach(SBMP_wooramel_shootdensity)
MannKendall(mean)
detach(SBMP_wooramel_shootdensity)

####################################################################################
#MAXIMUM CANOPY HEIGHT
####################################################################################

#Overall max height
SBMP_maxheight <- plyr::ddply(SBMP, .(Year), summarise,
                              N    = length(!is.na(Maximum_height_mm)),
                              mean = mean(Maximum_height_mm, na.rm=TRUE),
                              sd   = sd(Maximum_height_mm, na.rm=TRUE),
                              se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

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

##################################################################################
#SBMP_western gulf max height
SBMP_westerngulf_maxheight <- plyr::ddply(SBMP_westerngulf, .(Year), summarise,
                                          N    = length(!is.na(Maximum_height_mm)),
                                          mean = mean(Maximum_height_mm, na.rm=TRUE),
                                          sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                          se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SBMP_westerngulf_maxheight_plot <- ggplot(SBMP_westerngulf_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd)+
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black")+
  scale_x_continuous(limits=c(min(SBMP_westerngulf_maxheight$Year-0.125), max(SBMP_westerngulf_maxheight$Year+0.125)), breaks=min(SBMP_westerngulf_maxheight$Year):max(SBMP_westerngulf_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year")+
  ylab(expression(paste("Mean maximum canopy height (mm)", sep = ""))) +
  ggtitle("a) Western Gulf")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  # facet_wrap(~ Zone, nrow = 2)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_westerngulf_maxheight_plot

attach(SBMP_westerngulf_maxheight)
MannKendall(mean)
detach(SBMP_westerngulf_maxheight)

#############################################################
#Peron max height

SBMP_peron_maxheight <- ddply(SBMP_peron, .(Year), summarise,
                              N    = length(!is.na(Maximum_height_mm)),
                              mean = mean(Maximum_height_mm, na.rm=TRUE),
                              sd   = sd(Maximum_height_mm, na.rm=TRUE),
                              se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SBMP_peron_maxheight_plot<-ggplot(SBMP_peron_maxheight, aes(x=Year, y=mean))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd)+
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black")+ # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_peron_maxheight$Year-0.125), max(SBMP_peron_maxheight$Year+0.125)), breaks=min(SBMP_peron_maxheight$Year):max(SBMP_peron_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year")+
  ylab(expression(paste("Mean maximum canopy height (mm)", sep = ""))) +
  ggtitle("b) Peron")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_peron_maxheight_plot

attach(SBMP_peron_maxheight)
MannKendall(mean)
detach(SBMP_peron_maxheight)

#################################################################
#SBMP_Monkey Mia max height

SBMP_monkeymia_maxheight <- ddply(SBMP_monkeymia, .(Year), summarise,
                                  N    = length(!is.na(Maximum_height_mm)),
                                  mean = mean(Maximum_height_mm, na.rm=TRUE),
                                  sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                  se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SBMP_monkeymia_maxheight_plot <- ggplot(SBMP_monkeymia_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_monkeymia_maxheight$Year-0.125), max(SBMP_monkeymia_maxheight$Year+0.125)), breaks=min(SBMP_monkeymia_maxheight$Year):max(SBMP_monkeymia_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year") +
  ylab(expression(paste("Mean maximum canopy height (mm)", sep = ""))) +
  ggtitle("c) Monkey Mia")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_monkeymia_maxheight_plot

attach(SBMP_monkeymia_maxheight)
MannKendall(mean)
detach(SBMP_monkeymia_maxheight)

###########################################################################
#SBMP_wooramel max height

SBMP_wooramel_maxheight <- ddply(SBMP_wooramel, .(Year), summarise,
                                 N    = length(!is.na(Maximum_height_mm)),
                                 mean = mean(Maximum_height_mm, na.rm=TRUE),
                                 sd   = sd(Maximum_height_mm, na.rm=TRUE),
                                 se   = sd(Maximum_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Maximum_height_mm)) ))

SBMP_wooramel_maxheight_plot<-ggplot(SBMP_wooramel_maxheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_wooramel_maxheight$Year-0.125), max(SBMP_wooramel_maxheight$Year+0.125)), breaks=min(SBMP_wooramel_maxheight$Year):max(SBMP_wooramel_maxheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year") +
  ylab(expression(paste("Mean maximum canopy height (mm)", sep = ""))) +
  ggtitle("d) Wooramel")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_wooramel_maxheight_plot

attach(SBMP_wooramel_maxheight)
MannKendall(mean)
detach(SBMP_wooramel_maxheight)


####################################################################################
#MEAN CANOPY HEIGHT
####################################################################################

#Overall mean height
SBMP_meanheight <- plyr::ddply(SBMP, .(Year), summarise,
                               N    = length(!is.na(Mean_height_mm)),
                               mean = mean(Mean_height_mm, na.rm=TRUE),
                               sd   = sd(Mean_height_mm, na.rm=TRUE),
                               se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

SBMP_meanheight_plot <- ggplot(SBMP_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_meanheight$Year-0.125), max(SBMP_meanheight$Year+0.125)), breaks=min(SBMP_meanheight$Year):max(SBMP_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year") +
  ylab(expression(paste("Mean canopy height (mm)", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, linetype=3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(axis.text.x=element_text(angle=45, size = 10, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
        axis.title.x=element_text( size = 15), #removes x axis title
        axis.title.y=element_text(size = 15), #removes y axis title
        axis.text.y=element_text(size = 10))

SBMP_meanheight_plot

attach(SBMP_meanheight)
MannKendall(mean)
detach(SBMP_meanheight)

##################################################################################
#SBMP_western gulf mean height
SBMP_westerngulf_meanheight <- plyr::ddply(SBMP_westerngulf, .(Year), summarise,
                                           N    = length(!is.na(Mean_height_mm)),
                                           mean = mean(Mean_height_mm, na.rm=TRUE),
                                           sd   = sd(Mean_height_mm, na.rm=TRUE),
                                           se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

SBMP_westerngulf_meanheight_plot <- ggplot(SBMP_westerngulf_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd)+
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black")+
  scale_x_continuous(limits=c(min(SBMP_westerngulf_meanheight$Year-0.125), max(SBMP_westerngulf_meanheight$Year+0.125)), breaks=min(SBMP_westerngulf_meanheight$Year):max(SBMP_westerngulf_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year")+
  ylab(expression(paste("Mean canopy height (mm)", sep = ""))) +
  ggtitle("a) Western Gulf")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  # facet_wrap(~ Zone, nrow = 2)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_westerngulf_meanheight_plot

attach(SBMP_westerngulf_meanheight)
MannKendall(mean)
detach(SBMP_westerngulf_meanheight)

#############################################################
#Peron mean height

SBMP_peron_meanheight <- ddply(SBMP_peron, .(Year), summarise,
                               N    = length(!is.na(Mean_height_mm)),
                               mean = mean(Mean_height_mm, na.rm=TRUE),
                               sd   = sd(Mean_height_mm, na.rm=TRUE),
                               se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

SBMP_peron_meanheight_plot<-ggplot(SBMP_peron_meanheight, aes(x=Year, y=mean))+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd)+
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black")+ # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_peron_meanheight$Year-0.125), max(SBMP_peron_meanheight$Year+0.125)), breaks=min(SBMP_peron_meanheight$Year):max(SBMP_peron_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year")+
  ylab(expression(paste("Mean canopy height (mm)", sep = ""))) +
  ggtitle("b) Peron")+
  # geom_smooth(method=lm, colour = 1, linetype = 3,se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_peron_meanheight_plot

attach(SBMP_peron_meanheight)
MannKendall(mean)
detach(SBMP_peron_meanheight)

#################################################################
#SBMP_Monkey Mia mean height

SBMP_monkeymia_meanheight <- ddply(SBMP_monkeymia, .(Year), summarise,
                                   N    = length(!is.na(Mean_height_mm)),
                                   mean = mean(Mean_height_mm, na.rm=TRUE),
                                   sd   = sd(Mean_height_mm, na.rm=TRUE),
                                   se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

SBMP_monkeymia_meanheight_plot <- ggplot(SBMP_monkeymia_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_monkeymia_meanheight$Year-0.125), max(SBMP_monkeymia_meanheight$Year+0.125)), breaks=min(SBMP_monkeymia_meanheight$Year):max(SBMP_monkeymia_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year") +
  ylab(expression(paste("Mean canopy height (mm)", sep = ""))) +
  ggtitle("c) Monkey Mia")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_monkeymia_meanheight_plot

attach(SBMP_monkeymia_meanheight)
MannKendall(mean)
detach(SBMP_monkeymia_meanheight)

###########################################################################
#SBMP_wooramel mean height

SBMP_wooramel_meanheight <- ddply(SBMP_wooramel, .(Year), summarise,
                                  N    = length(!is.na(Mean_height_mm)),
                                  mean = mean(Mean_height_mm, na.rm=TRUE),
                                  sd   = sd(Mean_height_mm, na.rm=TRUE),
                                  se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

SBMP_wooramel_meanheight_plot<-ggplot(SBMP_wooramel_meanheight, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  #geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(SBMP_wooramel_meanheight$Year-0.125), max(SBMP_wooramel_meanheight$Year+0.125)), breaks=min(SBMP_wooramel_meanheight$Year):max(SBMP_wooramel_meanheight$Year)) +
  scale_y_continuous(limits=c(min(0), max(500)))+
  xlab("Year") +
  ylab(expression(paste("Mean canopy height (mm)", sep = ""))) +
  ggtitle("d) Wooramel")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(plot.title = element_text(size = 25))

SBMP_wooramel_meanheight_plot

attach(SBMP_wooramel_meanheight)
MannKendall(mean)
detach(SBMP_wooramel_meanheight)


#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

#Shoot density

png(png_SBMP_overall_shoot_density_fn, width=500, height=300)
grid.arrange(SBMP_shootdensity_plot)
dev.off()

png(png_SBMP_shoot_density_fn, width=1000, height=800)
grid.arrange(SBMP_westerngulf_shootdensity_plot, SBMP_peron_shootdensity_plot, SBMP_monkeymia_shootdensity_plot, SBMP_wooramel_shootdensity_plot,ncol=2)
dev.off()

#Maximum canopy height
png(png_SBMP_overall_max_height_fn, width=500, height=300)
grid.arrange(SBMP_maxheight_plot)
dev.off()

png(png_SBMP_max_height_fn, width=1000, height=800)
grid.arrange(SBMP_westerngulf_maxheight_plot, SBMP_peron_maxheight_plot, SBMP_monkeymia_maxheight_plot, SBMP_wooramel_maxheight_plot,ncol=2)
dev.off()

#Mean canopy height
png(png_SBMP_overall_mean_height_fn, width=500, height=300)
grid.arrange(SBMP_meanheight_plot)
dev.off()

png(png_SBMP_mean_height_fn, width=1000, height=800)
grid.arrange(SBMP_westerngulf_meanheight_plot, SBMP_peron_meanheight_plot, SBMP_monkeymia_meanheight_plot, SBMP_wooramel_meanheight_plot,ncol=2)
dev.off()


#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_SBMP_overall_shoot_density_rid,png_SBMP_overall_shoot_density_fn)
ckanr::resource_update(png_SBMP_shoot_density_rid, png_SBMP_shoot_density_fn)

ckanr::resource_update(png_SBMP_overall_max_height_rid, png_SBMP_overall_max_height_fn)
ckanr::resource_update(png_SBMP_max_height_rid, png_SBMP_max_height_fn)

ckanr::resource_update(png_SBMP_overall_mean_height_rid, png_SBMP_overall_mean_height_fn)
ckanr::resource_update(png_SBMP_mean_height_rid, png_SBMP_mean_height_fn)

ckanr::resource_update(txt_rid, "SBMP_shoot_density_and_canopy_height_code.R")

#####################################################################################
#set workdir to main report location
setwd("~/projects")
######################################################################################
