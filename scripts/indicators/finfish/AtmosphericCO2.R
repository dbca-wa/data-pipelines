setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)

#Load CSV

dat <- read.csv("CapeGrim_CO2_data_download.csv")
dat.yearly <- subset(dat, MM %in% c("12"))
dat.yearly$YYYY=as.character(dat.yearly$YYYY)
dat.yearly$YYYY=as.integer(dat.yearly$YYYY)

pd <- position_dodge(0.25)
#limits <- aes(ymax = mean + se, ymin = mean - se)
CO2Fig <- ggplot(dat.yearly, aes(x=YYYY, y=CO2ppm)) +
  geom_point(position=pd, size=2) +             # points
  geom_smooth(method=lm, se=FALSE, color="black") + # line
  xlab("Year") +
  ylab("Atmospheric CO2 Concentration (ppm)") +
  scale_x_continuous(limits=c(1976, 2016))+
  theme_bw() +
  theme(axis.text.y=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=75,hjust=1,size=12),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        legend.position="none")

CO2Fig

png(filename = "AtmosphericCO2.png",
    width = 600, height = 400, units = "px", pointsize = 6)
CO2Fig
dev.off()