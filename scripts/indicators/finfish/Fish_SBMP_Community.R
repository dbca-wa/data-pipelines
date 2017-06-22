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

dat <- read.csv("SBMP_DOV_All Data.csv")

#Check column names

colnames (dat)

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Period", "Feeding.Guild", "Number"))
dat.ab$Number <- as.character(dat.ab$Number)
dat.ab$Number <- as.integer(dat.ab$Number)
dat.ab <- dat.ab[!is.na(dat.ab$Number),]

#Remove any 'NA' values from dataset

dat.ab <- dat.ab[!is.na(dat.ab$Period),]
dat.ab = droplevels(dat.ab)

#ADD STEP HERE TO RENAME SCRAPER/EXCAVATOR AND MOBILE HERBIVORE AS LARGE HERBIVORE
dat.ab$Feeding.Guild <- as.character(dat.ab$Feeding.Guild)
dat.ab <- dat.ab[!(dat.ab$Feeding.Guild == "" | is.na(dat.ab$Feeding.Guild)), ]
dat.ab$Feeding.Guild[dat.ab$Feeding.Guild %in% "Large Cropper"] <- "Large Herbivore"
dat.ab$Feeding.Guild[dat.ab$Feeding.Guild %in% "Scraper/excavator"] <- "Large Herbivore"
dat.ab$Feeding.Guild <- as.factor(dat.ab$Feeding.Guild)

#Sum abundance values for trophic groups within site and period

dat.ab.total <- ddply(dat.ab, .(Year, Zone, Site, Period, Feeding.Guild), summarise,
                      total = sum(Number))

#Add 0 values to dataset for transects where trophic groups weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site + Period ~ Feeding.Guild, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site", "Period")))

#Subset dataset to trophic groups of interest (Piscivore, Large Herbivore, Mobile Invertivore, Coralivore)at Shark Bay and limit to those sites and years where we have a continuous time series

dat.ab.pisc <- subset(dat.ab.total2, Feeding.Guild %in% c("Piscivore"))
dat.ab.pisc1 <- subset(dat.ab.pisc, Site %in% c("SB204", "SB205", "SB213", "SBBFlat", "SBSPT"))
dat.ab.pisc2 <- subset(dat.ab.pisc1, Year %in% c("2010", "2015"))

dat.ab.herb <- subset(dat.ab.total2, Feeding.Guild %in% c("Large Herbivore"))
dat.ab.herb1 <- subset(dat.ab.herb, Site %in% c("SB204", "SB205", "SB213", "SBBFlat", "SBSPT"))
dat.ab.herb2 <- subset(dat.ab.herb1, Year %in% c("2010", "2015"))

dat.ab.coral <- subset(dat.ab.total2, Feeding.Guild %in% c("Corallivore"))
dat.ab.coral1 <- subset(dat.ab.coral, Site %in% c("SB204", "SB205", "SB213", "SBBFlat", "SBSPT"))
dat.ab.coral2 <- subset(dat.ab.coral1, Year %in% c("2010", "2015"))

dat.ab.invert <- subset(dat.ab.total2, Feeding.Guild %in% c("Mobile invertivore"))
dat.ab.invert1 <- subset(dat.ab.invert, Site %in% c("SB204", "SB205", "SB213", "SBBFlat", "SBSPT"))
dat.ab.invert2 <- subset(dat.ab.invert1, Year %in% c("2010", "2015"))

#Obtain mean and SE values for Piscivore, Large Herbivore Mobile Invertivore and Coralivore and create figures for all SBMP

PiscSBMP <- ddply(dat.ab.pisc2, .(Year, Zone), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
PiscSBMPFig <- ggplot(PiscSBMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Piscivores") +
  scale_x_continuous(limits=c(min(PiscSBMP$Year-0.25),max(PiscSBMP$Year+0.25)), breaks=min(PiscSBMP$Year):max(PiscSBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.75,1), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

PiscSBMPFig
#####################################

HerbSBMP <- ddply(dat.ab.herb2, .(Year, Zone), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
HerbSBMPFig <- ggplot(HerbSBMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Large Herbivores") +
  scale_x_continuous(limits=c(min(HerbSBMP$Year-0.25),max(HerbSBMP$Year+0.25)), breaks=min(HerbSBMP$Year):max(HerbSBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold", colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbSBMPFig
#####################################

CoralSBMP <- ddply(dat.ab.coral2, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
CoralSBMPFig <- ggplot(CoralSBMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Coralivores") +
  scale_x_continuous(limits=c(min(CoralSBMP$Year-0.25),max(CoralSBMP$Year+0.25)), breaks=min(CoralSBMP$Year):max(CoralSBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold",colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralSBMPFig
#####################################

InvertSBMP <- ddply(dat.ab.invert2, .(Year, Zone), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
InvertSBMPFig <- ggplot(InvertSBMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mobile Invertivores") +
  scale_x_continuous(limits=c(min(InvertSBMP$Year-0.25),max(InvertSBMP$Year+0.25)), breaks=min(InvertSBMP$Year):max(InvertSBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
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
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

InvertSBMPFig
#####################################

#Create Panel Plot of all trophic groups

png(filename = "CommunitySBMP.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(PiscSBMPFig, HerbSBMPFig, InvertSBMPFig, CoralSBMPFig)
dev.off()
