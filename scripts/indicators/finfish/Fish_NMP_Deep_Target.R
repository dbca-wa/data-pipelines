setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)

#Load CSV

dat <- read.csv("NMP_BRUV_Deep_All Data.csv")

#Check column names

colnames (dat)

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Period", "Genus.Species", "MaxN"))
dat.ab$MaxN <- as.character(dat.ab$MaxN)
dat.ab$MaxN <- as.integer(dat.ab$MaxN)

#Remove any 'NA' values from dataset

dat.ab<-dat.ab[!is.na(dat.ab$Period),]
dat.ab = droplevels(dat.ab)

#Sum abundance values for individual species within site and period

dat.ab.total <- ddply(dat.ab, .(Year, Zone, Site, Period, Genus.Species), summarise,
                      total = sum(MaxN))

#Add 0 values to dataset for transects where fish species weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site + Period ~ Genus.Species, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site", "Period")))

#Subset dataset to highly targeted fish species at Ningaloo and sum together so that they represent 'combined MaxN for target species'

dat.ab.target <- subset(dat.ab.total2, Genus.Species %in% c("Lethrinus nebulosus", "Lethrinus laticaudis", "Lethrinus miniatus", "Lethrinus punctulatus", "Gymnocranius grandoculis", "Lutjanus sebae", "Lutjanus argentimaculatus", "Lutjanus lemniscatus", "Pristipomoides multidens", "Pagrus auratus", "Epinephelus rivulatus", "Epinephelus multinotatus", "Epinephelus areolatus", "Epinephelus coioides", "Cephalopholis sonnerati", "Plectropomus spp", "Plectropomus leopardus", "Plectropomus maculatus", "Variola albimarginata", "Variola louti", "Variola spp", "Rachycentron canadus"))
dat.ab.target2 <-ddply(dat.ab.target, .(Year, Zone, Site, Period), summarise,
                       total = sum(value))

#Obtain mean and SE values for targeted fishes at zone level

TargetNMP <- ddply(dat.ab.target2, .(Year, Zone), summarise,
                   N    = length(total),
                   mean = mean(total),
                   sd   = sd(total),
                   se   = sd(total) / sqrt(length(total)) )

#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetNMPFig <- ggplot(TargetNMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean Combined MaxN per BRUV drop +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetNMP$Year-0.25),max(TargetNMP$Year+0.25)), breaks=min(TargetNMP$Year):max(TargetNMP$Year)) +
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
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

png(filename = "Target_DeepNMP.png",
    width = 600, height = 400, units = "px", pointsize = 6)
TargetNMPFig
dev.off()

#Create figures for individual Sanctuary Zones

dat.ab.target.mand <- subset(dat.ab.target2, Site %in% c("Mandu SZ", "Mandu Ref Nth"))

TargetMand <- ddply(dat.ab.target.mand, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetMandFig <- ggplot(TargetMand, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Mean Combined MaxN per BRUV drop +/- SE", sep = ""))) +
  ggtitle("Mandu") +
  scale_x_continuous(limits=c(min(TargetMand$Year-0.25),max(TargetMand$Year+0.25)), breaks=min(TargetMand$Year):max(TargetMand$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=14,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

TargetMandFig
#####################################

dat.ab.target.osp <- subset(dat.ab.target2, Site %in% c("Osprey SZ", "Osprey Ref Nth", "Osprey Ref Sth"))

TargetOsp <- ddply(dat.ab.target.osp, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetOspFig <- ggplot(TargetOsp, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Mean Combined MaxN per BRUV drop +/- SE", sep = ""))) +
  ggtitle("Osprey") +
  scale_x_continuous(limits=c(min(TargetOsp$Year-0.25),max(TargetOsp$Year+0.25)), breaks=min(TargetOsp$Year):max(TargetOsp$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_blank(),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

TargetOspFig
#####################################

dat.ab.target.cloat <- subset(dat.ab.target2, Site %in% c("Cloates SZ", "Cloates Ref Nth", "Cloates Ref Sth"))

TargetCloat <- ddply(dat.ab.target.cloat, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetCloatFig <- ggplot(TargetCloat, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Mean Combined MaxN per BRUV drop +/- SE", sep = ""))) +
  ggtitle("Cloates") +
  scale_x_continuous(limits=c(min(TargetCloat$Year-0.25),max(TargetCloat$Year+0.25)), breaks=min(TargetCloat$Year):max(TargetCloat$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=14,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

TargetCloatFig
#####################################

dat.ab.target.pel <- subset(dat.ab.target2, Site %in% c("Pelican SZ", "Pelican Ref Nth", "Pelican Ref Sth"))

TargetPel <- ddply(dat.ab.target.pel, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetPelFig <- ggplot(TargetPel, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Mean Combined MaxN per BRUV drop +/- SE", sep = ""))) +
  ggtitle("Pelican") +
  scale_x_continuous(limits=c(min(TargetPel$Year-0.25),max(TargetPel$Year+0.25)), breaks=min(TargetPel$Year):max(TargetPel$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_blank(),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

TargetPelFig
#####################################

#Create Panel Plot of Locations of interest

png(filename = "Target_DeepNMPLoc.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(TargetMandFig, TargetOspFig, TargetCloatFig, TargetPelFig)
dev.off()
