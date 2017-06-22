setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)

#Load CSV

dat <- read.csv("SBMP_RecFishingEffort.csv")

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
RecEffortSBMPFig <- ggplot(dat, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("No. Boat Days/Fishing Events (+/- SE)", sep = ""))) +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(25000))) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,0.4), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

RecEffortSBMPFig

png(filename = "RecEffort_SBMP.png",
    width = 600, height = 400, units = "px", pointsize = 6)
RecEffortSBMPFig
dev.off()