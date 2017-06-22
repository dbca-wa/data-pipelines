setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)

#Load CSV

dat <- read.csv("NMP_Charter fishing effort.csv")

pd <- position_dodge(0.25)
#limits <- aes(ymax = mean + se, ymin = mean - se)
NMPCharterEffortFig <- ggplot(dat, aes(x=Year, y=Lines_divers_fishing, group=1)) +
  geom_point(position=pd, size=2) +             # points
  #geom_smooth(method=lm, se=FALSE, color="black") + # line
  geom_line(position=pd) +
  xlab("Year") +
  ylab("Number of lines/divers used for fishing") +
  expand_limits(y=0) +
  scale_x_continuous(limits=c(2007, 2015))+
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

NMPCharterEffortFig

png(filename = "NMPCharterEffortFig.png",
    width = 600, height = 400, units = "px", pointsize = 6)
NMPCharterEffortFig
dev.off()