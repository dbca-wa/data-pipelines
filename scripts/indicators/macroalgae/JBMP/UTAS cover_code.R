setwd("~/projects/data-pipelines/scripts/indicators/macroalgae/JBMP")
# source("~/projects/data-pipelines/setup/ckan.R")

library(gridExtra)
library(ggplot2)
library (dplyr)
library(Kendall)
######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "619d13a7-5df5-46e3-8391-50a2390e8df2"
txt_rid <- "a25672bd-15d8-4644-933f-3eaa9fe6b320"

#percent cover plots
# png_JBMP_canopycover_rid <- "f98ef2b7-2c97-4d7b-a0d7-10d3001a6db"
png_JBMP_canopycover_fn <-"JBMP_canopycover_subset.png"
# png_JBMP_eckcover_rid <-"70afbaf6-33d5-4e4c-9b9b-933ca251d36"
png_JBMP_eckcover_fn <-"JBMP_ecloniacover_subset.png"
png_JBMP_scycover_fn <-"JBMP_scycover_subset.png"
png_JBMP_sargcover_fn <-"JBMP_sargcover_subset.png"

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

###################################################################################################
#Load data
###################################################################################################

c<-Canopy
c =c %>% dplyr::filter(Year %in% c("1999", "2000", "2003", "2004","2006","2009", "2011", "2013", "2016"))
canopy_sites <- c %>% filter(Site %in% c("3","4","5","6","7","10","13","15","17","19","20","22","23"))#,"26","32","35","36","38","42"))
canopy_shallow = canopy_sites %>% dplyr::filter(Depth %in% c("Shallow"))
canopy_deep = canopy_sites %>% dplyr::filter(Depth %in% c("Deep"))

eck_sites <- c %>% filter(Site %in% c("3","4","5","6","7","10","13","15","17","19","23"))#,"26","32","36","38","42"))
eck_shallow = eck_sites %>% dplyr::filter(Depth %in% c("Shallow"))
eck_deep = eck_sites %>% dplyr::filter(Depth %in% c("Deep"))

c_shallow = c %>% dplyr::filter(Depth %in% c("Shallow"))
c_deep = c %>% dplyr::filter(Depth %in% c("Deep"))

##################################################################################
# ALL CANOPY
##################################################################################
make_canopy <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(canopy)),
      mean = mean(canopy, na.rm = TRUE),
      sd   = sd(canopy, na.rm = TRUE),
      se   = sd(canopy, na.rm = TRUE) / sqrt(N)
    )
}

######################################################################################

canopy <- make_canopy(canopy_sites)

canopy_plot <- ggplot(canopy, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(canopy$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle("Total canopy_all sites")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
canopy_plot

attach(canopy)
MannKendall(mean)
detach(canopy)

#######################################################################################
canopyshallow <- make_canopy(canopy_shallow)

canopyshallow_plot <- ggplot(canopyshallow, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(canopy$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle("b) Total canopy cover_shallow sites")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
canopyshallow_plot

attach(canopyshallow)
MannKendall(mean)
detach(canopyshallow)

##########################################################################################
canopydeep <- make_canopy(canopy_deep)

canopydeep_plot <- ggplot(canopydeep, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(canopy$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle("c) Total canopy cover_deep sites")+
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
canopydeep_plot

attach(canopydeep)
MannKendall(mean)
detach(canopydeep)

##################################################################################
# ECKLONIA
##################################################################################
make_eck <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(ecklonia)),
      mean = mean(ecklonia, na.rm = TRUE),
      sd   = sd(ecklonia, na.rm = TRUE),
      se   = sd(ecklonia, na.rm = TRUE) / sqrt(N)
    )
}

############################################################################################

eck <- make_eck(eck_sites)

eck_plot <- ggplot(eck, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(eck$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("a) ", italic("Ecklonia radiata "), "cover_all sites")))+
  geom_smooth(method=glm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
eck_plot

attach(eck)
MannKendall(mean)
detach(eck)

###############################################################################

eckshallow <- make_eck(eck_shallow)

eckshallow_plot <- ggplot(eckshallow, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(eck$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("b) ", italic("Ecklonia radiata "), "cover_shallow sites")))+
  geom_smooth(method=glm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
eckshallow_plot

attach(eckshallow)
MannKendall(mean)
detach(eckshallow)

###############################################################################

eckdeep <- make_eck(eck_deep)

eckdeep_plot <- ggplot(eckdeep, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(eck$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("c) ", italic("Ecklonia radiata "), "cover_deep sites")))+
  geom_smooth(method=glm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
eckdeep_plot

attach(eckdeep)
MannKendall(mean)
detach(eckdeep)

##################################################################################
# SCYTOTHALIA
##################################################################################
make_scy <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(scytothalia)),
      mean = mean(scytothalia, na.rm = TRUE),
      sd   = sd(scytothalia, na.rm = TRUE),
      se   = sd(scytothalia, na.rm = TRUE) / sqrt(N)
    )
}

######################################################################################

scy <- make_scy(canopy_sites)

scy_plot <- ggplot(scy, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(scy$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("a) ", italic("Scytothalia dorycarpa "), "cover_all sites")))+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
scy_plot

attach(scy)
MannKendall(mean)
detach(scy)

######################################################################################

scy_shallow <- make_scy(canopy_shallow)

scy_shallow_plot <- ggplot(scy_shallow, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(scy_shallow$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("b) ", italic("Scytothalia dorycarpa "), "cover_shallow sites")))+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
scy_shallow_plot

attach(scy_shallow)
MannKendall(mean)
detach(scy_shallow)

######################################################################################

scy_deep <- make_scy(canopy_deep)

scy_deep_plot <- ggplot(scy_deep, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(scy_deep$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("c) ", italic("Scytothalia dorycarpa "), "cover_deep sites")))+
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
scy_deep_plot

attach(scy_deep)
MannKendall(mean)
detach(scy_deep)

##################################################################################
# SARGASSUM
##################################################################################
make_sarg <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(Sargassaceae)),
      mean = mean(Sargassaceae, na.rm = TRUE),
      sd   = sd(Sargassaceae, na.rm = TRUE),
      se   = sd(Sargassaceae, na.rm = TRUE) / sqrt(N)
    )
}

######################################################################################

sarg <- make_sarg(canopy_sites)

sarg_plot <- ggplot(sarg, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(sarg$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("c) ", italic("Sargassum"), " spp. cover_all sites")))+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
sarg_plot

attach(sarg)
MannKendall(mean)
detach(sarg)

#######################################################################################
sargshallow <- make_sarg(canopy_shallow)

sargshallow_plot <- ggplot(sargshallow, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(sarg$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("c) ", italic("Sargassum"), " spp. cover_shallow sites")))+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
sargshallow_plot

attach(sargshallow)
MannKendall(mean)
detach(sargshallow)

##########################################################################################
sargdeep <- make_sarg(canopy_deep)

sargdeep_plot <- ggplot(sargdeep, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(sarg$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle(expression(paste("c) ", italic("Sargassum"), " spp. cover_deep sites")))+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
sargdeep_plot

attach(sargdeep)
MannKendall(mean)
detach(sargdeep)

################################################################################
#Create figures (will be saved to current workdir)
################################################################################

#Canopy cover

png(png_JBMP_canopycover_fn, width=500, height=900)
grid.arrange(canopy_plot,canopyshallow_plot, canopydeep_plot, ncol = 1)
dev.off()

#Ecklonia cover
png(png_JBMP_eckcover_fn, width=500, height=900)
grid.arrange(eck_plot, eckshallow_plot, eckdeep_plot, ncol=1)
dev.off()

#scytothalia
png(png_JBMP_scycover_fn, width=500, height=900)
grid.arrange(scy_plot,scy_shallow_plot, scy_deep_plot, ncol = 1)
dev.off()

#sargassum
png(png_JBMP_sargcover_fn, width=500, height=900)
grid.arrange(sarg_plot,sargshallow_plot, sargdeep_plot, ncol = 1)
dev.off()
