setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)

#Load CSV

dat <- read.csv("SBMP_RecFishingCatch.csv")

dat.Cauratus <- subset(dat, Species %in% c("Chrysophrys auratus"))

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
CauratusFig <- ggplot(dat.Cauratus, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean No. Fish Retained/Released (+/- SE)", sep = ""))) +
  ggtitle("Chrysophrys auratus") +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(95000))) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=2,face="bold", colour="white"),                #removes x axis title
        axis.title.y=element_text(size=2,face="bold", colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=12,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,0.4), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

CauratusFig

#############################
dat.llaticaudis <- subset(dat, Species %in% c("Lethrinus laticaudis"))

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
LlaticaudisFig <- ggplot(dat.llaticaudis, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean No. Fish Retained/Released (+/- SE)", sep = ""))) +
  ggtitle("Lethrinus laticaudis") +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(22000))) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=2,face="bold", colour="white"),                #removes x axis title
        axis.title.y=element_text(size=10,face="bold", colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=12,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,0.4), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

LlaticaudisFig

#############################
dat.Crubescens <- subset(dat, Species %in% c("Choerodon rubescens"))

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
CrubescensFig <- ggplot(dat.Crubescens, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean No. Fish Retained/Released (+/- SE)", sep = ""))) +
  ggtitle("Choerodon rubescens") +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(14000))) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=2,face="bold", colour="white"),                #removes x axis title
        axis.title.y=element_text(size=9.5,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=12,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,0.4), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

CrubescensFig
#############################
dat.Scommerson <- subset(dat, Species %in% c("Scomberomorous commerson"))

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
ScommersonFig <- ggplot(dat.Scommerson, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean No. Fish Retained/Released (+/- SE)", sep = ""))) +
  ggtitle("Scomberomorous commerson") +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(2000))) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=2,face="bold", colour="white"),                #removes x axis title
        axis.title.y=element_text(size=10,face="bold", colour = "white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=12,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,0.4), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

ScommersonFig
#############################
dat.Emultinotatus <- subset(dat, Species %in% c("Epinephelus multinotatus"))

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
EmultinotatusFig <- ggplot(dat.Emultinotatus, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean No. Fish Retained/Released (+/- SE)", sep = ""))) +
  ggtitle("Epinephelus multinotatus") +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(900))) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=14,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=10,face="bold", colour ="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=12,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,0.4), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

EmultinotatusFig
#############################
dat.Lnebulosus <- subset(dat, Species %in% c("Lethrinus nebulosus"))

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
LnebulosusFig <- ggplot(dat.Lnebulosus, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean No. Fish Retained/Released (+/- SE)", sep = ""))) +
  ggtitle("Lethrinus nebulosus") +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(6000))) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=2,face="bold", colour="white"),                #removes x axis title
        axis.title.y=element_text(size=10,face="bold", colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=12,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,0.4), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

LnebulosusFig
#############################
dat.Emalabaricus <- subset(dat, Species %in% c("Epinephelus malabaricus"))

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
EmalabaricusFig <- ggplot(dat.Emalabaricus, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean No. Fish Retained/Released (+/- SE)", sep = ""))) +
  ggtitle("Epinephelus malabaricus") +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(2200))) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=2,face="bold", colour="white"),                #removes x axis title
        axis.title.y=element_text(size=2,face="bold", colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=12,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,0.4), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

EmalabaricusFig
#############################
dat.Ecoioides <- subset(dat, Species %in% c("Epinephelus coioides"))

pd <- position_dodge(0.25)
limits <- aes(ymax = Mean + SE, ymin = Mean - SE)
EcoioidesFig <- ggplot(dat.Ecoioides, aes(x=Year, y=Mean, group=Category, linetype=Category, shape=Category)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean No. Fish Retained/Released (+/- SE)", sep = ""))) +
  ggtitle("Epinephelus coioides") +
  #scale_x_continuous(limits=c(min(dat$Year-0.25),max(dat$Year+0.25)), breaks=min(dat$Year):max(dat$Year)) +
  scale_y_continuous(limits=c(min(0),max(1300))) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(vjust=0.4),
        axis.title.x=element_text(size=14,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=9.5,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=12,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.5,0.7), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

EcoioidesFig

#####################################

#Create Panel Plot of all trophic groups

png(filename = "RecCatchSBMP.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(ncol=2, CauratusFig, LlaticaudisFig, CrubescensFig, LnebulosusFig, EmalabaricusFig, ScommersonFig, EcoioidesFig, EmultinotatusFig)
dev.off()