setwd("~/projects/data-pipelines/scripts/indicators/macroalgae/MMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(dplyr)

######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "a5c77770-0019-43fb-96be-348e17e1a69e"

png_MMP_overall_density_fn <-"MMP_overall_density.png"
png_MMP_density_fn <-"MMP_density.png"

png_MMP_eckjuv_overall_fn <-"MMP_eckloniajuv_overall.png"
png_MMP_eckjuv_fn <-"MMP_eckloniajuv.png"

################################################################################
#Load data
################################################################################

d <- all_data
names(d)[names(d) == 'Site name'] <- 'Site'###Changes column name

d$eck_adult <- (d$Ecklonia_adult_density * 4) # Scales seagrass data to 1m
d$eck_juv <- (d$Ecklonia_juvenile_density * 4) # Scales seagrass data to 1m

################################################################################
#Define graphic properties
################################################################################

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
#MMP_subsets

MMP = subset(d, Park %in% c("MMP"))
MMP_south = subset(d, Site %in% c("Watermans outer", "Watermans Reef"))
MMP_centre = subset(d, Site %in% c("The Lumps" , "Little Island", "Three Mile Reef outer"))
MMP_north = subset(d, Site %in% c("Three Mile Reef inner","Burns Rocks", "Burns Rocks offshore"))

####################################################################################
# Ecklonia adult desnity
####################################################################################

make_density <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(eck_adult)),
      mean = mean(eck_adult, na.rm = TRUE),
      sd   = sd(eck_adult, na.rm = TRUE),
      se   = sd(eck_adult, na.rm = TRUE) / sqrt(N)
    )
}

######################################################################################
#Overall density

MMP_cover <- make_density(MMP)

MMP_plot <- ggplot(MMP_cover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(MMP_cover$Year-0.125),
                                                          max(MMP_cover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
MMP_plot

######################################################################################
#South density

MMP_s <- make_density(MMP_south)

MMP_s_plot <- ggplot(MMP_s, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(MMP_s$Year-0.125),
                                                          max(MMP_s$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("c)  South")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
MMP_s_plot

#################################################################
#MMP_centre density

MMP_c <- make_density(MMP_centre)

MMP_c_plot <- ggplot(MMP_c, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(MMP_c$Year-0.125),
                                                          max(MMP_c$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("b)  Centre")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
MMP_c_plot

###########################################################################
#MMP_north density

MMP_n <- make_density(MMP_north)

MMP_n_plot <- ggplot(MMP_n, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(MMP_n$Year-0.125),
                                                          max(MMP_n$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("a)  North")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
MMP_n_plot

####################################################################################
# Ecklonia juvenile desnity
####################################################################################

make_juvdensity <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(eck_juv)),
      mean = mean(eck_juv, na.rm = TRUE),
      sd   = sd(eck_juv, na.rm = TRUE),
      se   = sd(eck_juv, na.rm = TRUE) / sqrt(N)
    )
}

######################################################################################
#Overall density

MMP_juvcover <- make_juvdensity(MMP)

MMP_juv_plot <- ggplot(MMP_juvcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(MMP_juvcover$Year-0.125),
                                                          max(MMP_juvcover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
MMP_juv_plot

######################################################################################
# South juvenile density

MMP_s_juv <- make_juvdensity(MMP_south)

MMP_s_juv_plot <- ggplot(MMP_s_juv, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(MMP_s_juv$Year-0.125),
                                                          max(MMP_s_juv$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("c)  South")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
MMP_s_juv_plot

#################################################################
#Centre juvenile density

MMP_c_juv <- make_juvdensity(MMP_centre)

MMP_c_juv_plot <- ggplot(MMP_c_juv, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(MMP_c_juv$Year-0.125),
                                                          max(MMP_c_juv$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("b)  Shoalwater")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
MMP_c_juv_plot

###########################################################################
#North juvenile density

MMP_n_juv <- make_juvdensity(MMP_north)

MMP_n_juv_plot <- ggplot(MMP_n_juv, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(MMP_n_juv$Year-0.125),
                                                          max(MMP_n_juv$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("a)  North")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
MMP_n_juv_plot

#####################################################################################
# Create figures
#####################################################################################

#Ecklonia density_overall
png(png_MMP_overall_density_fn, width=500, height=300)
grid.arrange(MMP_plot, ncol = 1)
dev.off()

#Ecklonia density
png(png_MMP_density_fn, width=500, height=900)
grid.arrange(MMP_n_plot, MMP_c_plot,MMP_s_plot, ncol=1)
dev.off()

#Ecklonia juvenile_density_overall
png(png_MMP_eckjuv_overall_fn, width=500, height=300)
grid.arrange(MMP_juv_plot, ncol = 1)
dev.off()

#Ecklonia juvenile_density
png(png_MMP_eckjuv_fn, width=500, height=900)
grid.arrange(MMP_n_juv_plot, MMP_c_juv_plot, MMP_s_juv_plot, ncol = 1)
dev.off()
