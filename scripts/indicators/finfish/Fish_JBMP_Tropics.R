setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)
library(mgcv)
#library(gam)
library(MASS)
library(MuMIn)

#Load CSV

dat <- read.csv("JBMP_All Data.csv")

#Check column names

colnames (dat)

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Transect", "Genus.Species", "Number"))
dat.ab$Number <- as.character(dat.ab$Number)
dat.ab$Number <- as.integer(dat.ab$Number)

#Remove any 'NA' values from dataset

dat.ab<-dat.ab[!is.na(dat.ab$Transect),]
dat.ab = droplevels(dat.ab)

#Sum abundance values for individual species within site and period

dat.ab.total <- ddply(dat.ab, .(Year, Zone, Site, Transect, Genus.Species), summarise,
                      total = sum(Number))

#Add 0 values to dataset for transects where fish species weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site + Transect ~ Genus.Species, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site", "Transect")))

#Limit to those sites and years where we have a continuous time series
dat.ab.total3 <- subset(dat.ab.total2, Site %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25"))

#Subset dataset to tropicals at Jurien and some together so that they represent 'total target fish' per replicate

dat.ab.tropical <- subset(dat.ab.total3, Genus.Species %in% c("Chaetodon assarius", "Epinephelus rivulatus", "Stethojulis bandanensis", "Parupeneus spilurus"))

#Obtain mean and SE values for targeted fishes at zone level

TropicalJBMP <- ddply(dat.ab.tropical, .(Year, Genus.Species), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )


#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TropicalJBMPFig <- ggplot(TropicalJBMP, aes(x=Year, y=mean, group=Genus.Species, linetype=Genus.Species, shape=Genus.Species)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=8), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 500 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(TropicalJBMP$Year-0.25),max(TropicalJBMP$Year+0.25)), breaks=min(TropicalJBMP$Year):max(TropicalJBMP$Year)) +
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
        legend.justification=c(1,1), legend.position=c(0.75,1), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
TropicalJBMPFig

#Subset dataset to P milleri at Jurien and some together so that they represent 'total target fish' per replicate

dat.ab.milleri <- subset(dat.ab.total3, Genus.Species %in% c("Pomacentrus milleri"))

#Obtain mean and SE values for targeted fishes at zone level

MilleriJBMP <- ddply(dat.ab.milleri, .(Year, Genus.Species), summarise,
                      N    = length(value),
                      mean = mean(value),
                      sd   = sd(value),
                      se   = sd(value) / sqrt(length(value)) )


#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
MilleriJBMPFig <- ggplot(MilleriJBMP, aes(x=Year, y=mean, group=Genus.Species, linetype=Genus.Species, shape=Genus.Species)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=8), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 500 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(MilleriJBMP$Year-0.25),max(MilleriJBMP$Year+0.25)), breaks=min(MilleriJBMP$Year):max(MilleriJBMP$Year)) +
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
        legend.justification=c(1,1), legend.position=c(0.75,1), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
MilleriJBMPFig

#Create Panel Plot of all tropicals groups

png(filename = "TropicalJBMP.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(TropicalJBMPFig, MilleriJBMPFig)
dev.off()

###########################################

#Subset dataset to temperates at Jurien

#dat.ab.temperate <- subset(dat.ab.total3, Genus.Species %in% c("Austrolabrus maculatus", "Pictilabrus laticlavius", "Pictilabrus viridis"))
dat.ab.laticlavius <- subset(dat.ab.total3, Genus.Species %in% c("Pictilabrus laticlavius"))

#Obtain mean and SE values for targeted fishes at zone level

LaticlaviusJBMP <- ddply(dat.ab.laticlavius, .(Year, Genus.Species), summarise,
                     N    = length(value),
                     mean = mean(value),
                     sd   = sd(value),
                     se   = sd(value) / sqrt(length(value)) )


#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
LaticlaviusJBMPFig <- ggplot(LaticlaviusJBMP, aes(x=Year, y=mean, group=Genus.Species, linetype=Genus.Species, shape=Genus.Species)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=8), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 500 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Pictilabrus laticlavius") +
  scale_x_continuous(limits=c(min(LaticlaviusJBMP$Year-0.25),max(LaticlaviusJBMP$Year+0.25)), breaks=min(LaticlaviusJBMP$Year):max(LaticlaviusJBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold", color = "white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none")
        #legend.justification=c(1,1), legend.position=c(0.75,1), # Positions legend (x,y) in this case removes it from the graph
        #legend.title=element_blank(),
        #legend.key=element_blank(),
        #legend.text=element_text(size=18),
        #legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
LaticlaviusJBMPFig

#Subset dataset to temperates at Jurien

#dat.ab.temperate <- subset(dat.ab.total3, Genus.Species %in% c("Austrolabrus maculatus", "Pictilabrus laticlavius", "Pictilabrus viridis"))
dat.ab.maculatus <- subset(dat.ab.total3, Genus.Species %in% c("Austrolabrus maculatus"))

#Obtain mean and SE values for targeted fishes at zone level

MaculatusJBMP <- ddply(dat.ab.maculatus, .(Year, Genus.Species), summarise,
                         N    = length(value),
                         mean = mean(value),
                         sd   = sd(value),
                         se   = sd(value) / sqrt(length(value)) )


#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
MaculatusJBMPFig <- ggplot(MaculatusJBMP, aes(x=Year, y=mean, group=Genus.Species, linetype=Genus.Species, shape=Genus.Species)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=8), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 500 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Austrolabrus maculatus") +
  scale_x_continuous(limits=c(min(MaculatusJBMP$Year-0.25),max(MaculatusJBMP$Year+0.25)), breaks=min(MaculatusJBMP$Year):max(MaculatusJBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none")
        #legend.justification=c(1,1), legend.position=c(0.75,1), # Positions legend (x,y) in this case removes it from the graph
        #legend.title=element_blank(),
        #legend.key=element_blank(),
        #legend.text=element_text(size=18),
        #legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
MaculatusJBMPFig

#Subset dataset to temperates at Jurien

#dat.ab.temperate <- subset(dat.ab.total3, Genus.Species %in% c("Austrolabrus maculatus", "Pictilabrus laticlavius", "Pictilabrus viridis"))
dat.ab.viridis <- subset(dat.ab.total3, Genus.Species %in% c("Pictilabrus viridis"))

#Obtain mean and SE values for targeted fishes at zone level

ViridisJBMP <- ddply(dat.ab.viridis, .(Year, Genus.Species), summarise,
                       N    = length(value),
                       mean = mean(value),
                       sd   = sd(value),
                       se   = sd(value) / sqrt(length(value)) )


#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
ViridisJBMPFig <- ggplot(ViridisJBMP, aes(x=Year, y=mean, group=Genus.Species, linetype=Genus.Species, shape=Genus.Species)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=8), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 500 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Pictilabrus viridis") +
  scale_x_continuous(limits=c(min(ViridisJBMP$Year-0.25),max(ViridisJBMP$Year+0.25)), breaks=min(ViridisJBMP$Year):max(ViridisJBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold", color = "white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none")
        #legend.justification=c(1,1), legend.position=c(0.75,1), # Positions legend (x,y) in this case removes it from the graph
        #legend.title=element_blank(),
        #legend.key=element_blank(),
        #legend.text=element_text(size=18),
        #legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
ViridisJBMPFig

#Create Panel Plot of all temperates groups

png(filename = "TemperateJBMP.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(ViridisJBMPFig, MaculatusJBMPFig, LaticlaviusJBMPFig)
dev.off()
