setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)

#Load CSV

dat <- read.csv("NMP_Recruit_BR_Abund.csv")
dat2 <- read.csv("NMP_Recruit_Leth_Abund.csv")

#Obtain mean and SE values for recruits at zone level

BRAbund <- ddply(dat, .(Year, Zone), summarise,
                   N    = length(Abund),
                   mean = mean(Abund),
                   sd   = sd(Abund),
                   se   = sd(Abund) / sqrt(length(Abund)) )

LethAbund <- ddply(dat2, .(Year, Zone), summarise,
                   N    = length(Lethrinids),
                   mean = mean(Lethrinids),
                   sd   = sd(Lethrinids),
                   se   = sd(Lethrinids) / sqrt(length(Lethrinids)) )

#Create figure for both Lethrinids and Back Reef Associates

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
BRAbundFig <- ggplot(BRAbund, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=5), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 30 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Coral Associates (Sanctuary n = 96-108, Recreational n = 30-36)") +
  scale_x_continuous(limits=c(min(BRAbund$Year-0.25),max(BRAbund$Year+0.25)), breaks=min(BRAbund$Year):max(BRAbund$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,0.9), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
BRAbundFig

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
LethAbundFig <- ggplot(LethAbund, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=6), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 30 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Lethrinids (Sanctuary n = 63, Recreational n = 33-36)") +
  scale_x_continuous(limits=c(min(LethAbund$Year-0.25),max(LethAbund$Year+0.25)), breaks=min(LethAbund$Year):max(LethAbund$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
LethAbundFig

#Create Panel Plot of Locations of interest

png(filename = "NMPRecruitAbund.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(BRAbundFig, LethAbundFig)
dev.off()

