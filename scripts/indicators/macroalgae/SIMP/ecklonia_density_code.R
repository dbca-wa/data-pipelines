setwd("~/projects/data-pipelines/scripts/indicators/macroalgae/SIMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(dplyr)

######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "a5c77770-0019-43fb-96be-348e17e1a69e"

png_SIMP_overall_density_fn <-"SIMP_overall_density.png"
png_SIMP_density_fn <-"SIMP_density.png"

png_SIMP_eckjuv_overall_fn <-"SIMP_eckloniajuv_overall.png"
png_SIMP_eckjuv_fn <-"SIMP_eckloniajuv.png"

################################################################################
#Load data
################################################################################

d <- all_data
names(d)[names(d) == 'Site name'] <- 'Site'###Changes column name

d$eck_adult <- (d$Ecklonia_adult_density * 4) # Scales seagrass data to 1m
d$eck_juv <- (d$Ecklonia_juvenile_density * 4) # Scales seagrass data to 1m

c<-subset(d, select=c("eck_adult", "eck_juv"))

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
#SIMP_subsets

SIMP = subset(d, Park %in% c("SIMP"))
SIMP_south = subset(d, Site %in% c("Becher Point SZ", "The Sisters"))
SIMP_shoalwater = subset(d, Site %in% c("Penguin Island" , "Seal Island", "Second Rock SZ"))
SIMP_north = subset(d, Site %in% c("Bird Island","John Point", "Point Peron north"))

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

SIMP_cover <- make_density(SIMP)

SIMP_plot <- ggplot(SIMP_cover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(SIMP_cover$Year-0.125),
                                                          max(SIMP_cover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_plot

######################################################################################

SIMP_s <- make_density(SIMP_south)

SIMP_s_plot <- ggplot(SIMP_s, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(SIMP_s$Year-0.125),
                                                          max(SIMP_s$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("c)  South")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_s_plot

#################################################################
#SIMP_shoalwater Bay shoot density

SIMP_shoal <- make_density(SIMP_shoalwater)

SIMP_shoal_plot <- ggplot(SIMP_shoal, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(SIMP_shoal$Year-0.125),
                                                          max(SIMP_shoal$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("b)  Shoalwater")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_shoal_plot


###########################################################################
#SIMP_north shoot density

SIMP_n <- make_density(SIMP_north)

SIMP_n_plot <- ggplot(SIMP_n, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(SIMP_n$Year-0.125),
                                                          max(SIMP_n$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("a)  North")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_n_plot

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

SIMP_juvcover <- make_juvdensity(SIMP)

SIMP_juv_plot <- ggplot(SIMP_juvcover, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(SIMP_juvcover$Year-0.125),
                                                          max(SIMP_juvcover$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_juv_plot

######################################################################################

SIMP_s_juv <- make_density(SIMP_south)

SIMP_s_juv_plot <- ggplot(SIMP_s_juv, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(SIMP_s_juv$Year-0.125),
                                                          max(SIMP_s_juv$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("c)  South")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_s_juv_plot


#################################################################
#SIMP_shoalwater Bay shoot density

SIMP_shoal_juv <- make_density(SIMP_shoalwater)

SIMP_shoal_juv_plot <- ggplot(SIMP_shoal_juv, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(SIMP_shoal_juv$Year-0.125),
                                                          max(SIMP_shoal_juv$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("b)  Shoalwater")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_shoal_juv_plot


###########################################################################
#SIMP_north shoot density

SIMP_n_juv <- make_density(SIMP_north)

SIMP_n_juv_plot <- ggplot(SIMP_n_juv, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,1), limits=c(min(SIMP_n_juv$Year-0.125),
                                                          max(SIMP_n_juv$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","m"^-2,")", sep = ""))) +
  ggtitle ("a)  North")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_n_juv_plot


#####################################################################################
# Createfigures
#####################################################################################

#Ecklonia density_overall
png(png_SIMP_overall_density_fn, width=500, height=300)
grid.arrange(SIMP_plot, ncol = 1)
dev.off()

#Ecklonia density
png(png_SIMP_density_fn, width=500, height=900)
grid.arrange(SIMP_n_plot, SIMP_shoal_plot,SIMP_s_plot, ncol=1)
dev.off()

#Ecklonia juvenile_density_overall
png(png_SIMP_eckjuv_overall_fn, width=500, height=300)
grid.arrange(SIMP_juv_plot, ncol = 1)
dev.off()

#Ecklonia juvenile_density
png(png_SIMP_eckjuv_fn, width=500, height=900)
grid.arrange(SIMP_n_juv_plot, SIMP_shoal_juv_plot, SIMP_s_juv_plot, ncol = 1)
dev.off()

png_SIMP_eckjuv_overall_fn <-"SIMP_eckloniajuv_overall.png"
png_SIMP_eckjuv_fn <-"SIMP_eckloniajuv.png"

