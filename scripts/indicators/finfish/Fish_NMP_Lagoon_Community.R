install.packages("gridExtra")
install.packages("reshape")

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

dat <- read.csv("NMP_DOV_All Data.csv")

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

#Subset dataset to trophic groups of interest (Piscivore, Large Herbivore, Coralivore)at Ningaloo and limit to those sites and years where we have a continuous time series

dat.ab.pisc <- subset(dat.ab.total2, Feeding.Guild %in% c("Piscivore"))
dat.ab.pisc1 <- subset(dat.ab.pisc, Site %in% c("FishCB1", "FishCB3", "FishM1", "FishM3", "FishO3", "FishO4"))
dat.ab.pisc2 <- subset(dat.ab.pisc1, Year %in% c("1999", "2000", "2011", "2014", "2015"))

dat.ab.herb <- subset(dat.ab.total2, Feeding.Guild %in% c("Large Herbivore"))
dat.ab.herb1 <- subset(dat.ab.herb, Site %in% c("FishCB1", "FishCB3", "FishM1", "FishM3", "FishO3", "FishO4"))
dat.ab.herb2 <- subset(dat.ab.herb1, Year %in% c("1999", "2000", "2011", "2014", "2015"))

dat.ab.coral <- subset(dat.ab.total2, Feeding.Guild %in% c("Coralivore"))
dat.ab.coral1 <- subset(dat.ab.coral, Site %in% c("FishCB1", "FishCB3", "FishM1", "FishM3", "FishO3", "FishO4"))
dat.ab.coral2 <- subset(dat.ab.coral1, Year %in% c("1999", "2000", "2011", "2014", "2015"))

dat.ab.invert <- subset(dat.ab.total2, Feeding.Guild %in% c("Mobile Invertivore"))
dat.ab.invert1 <- subset(dat.ab.invert, Site %in% c("FishCB1", "FishCB3", "FishM1", "FishM3", "FishO3", "FishO4"))
dat.ab.invert2 <- subset(dat.ab.invert1, Year %in% c("1999", "2000", "2014", "2015"))

#Obtain mean and SE values for Piscivore, Large Herbivore and Coralivore and create figures for all NMP

PiscNMP <- ddply(dat.ab.pisc2, .(Year, Zone), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
PiscNMPFig <- ggplot(PiscNMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Piscivores") +
  scale_x_continuous(limits=c(min(PiscNMP$Year-0.25),max(PiscNMP$Year+0.25)), breaks=min(PiscNMP$Year):max(PiscNMP$Year)) +
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
        legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

PiscNMPFig
#####################################

HerbNMP <- ddply(dat.ab.herb2, .(Year, Zone), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
HerbNMPFig <- ggplot(HerbNMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Large Herbivores") +
  scale_x_continuous(limits=c(min(HerbNMP$Year-0.25),max(HerbNMP$Year+0.25)), breaks=min(HerbNMP$Year):max(HerbNMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_blank(),               #removes y axis title
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

HerbNMPFig
#####################################

CoralNMP <- ddply(dat.ab.coral2, .(Year, Zone), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
CoralNMPFig <- ggplot(CoralNMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  #ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Coralivores") +
  scale_x_continuous(limits=c(min(CoralNMP$Year-0.25),max(CoralNMP$Year+0.25)), breaks=min(CoralNMP$Year):max(CoralNMP$Year)) +
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
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralNMPFig
#####################################

InvertNMP <- ddply(dat.ab.invert2, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
InvertNMPFig <- ggplot(InvertNMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mobile Invertivores") +
  scale_x_continuous(limits=c(min(InvertNMP$Year-0.25),max(InvertNMP$Year+0.25)), breaks=min(InvertNMP$Year):max(InvertNMP$Year)) +
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

InvertNMPFig
#####################################

#Create Panel Plot of all trophic groups

png(filename = "CommunityNMP.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(PiscNMPFig, HerbNMPFig, InvertNMPFig, CoralNMPFig)
dev.off()


#Examine Large Herbivore graphs for all locations and present interesting ones

dat.ab.herb.bund <- subset(dat.ab.herb, Site %in% c("FishB1", "FishB2", "FishB3", "FishB4"))

HerbBund <- ddply(dat.ab.herb.bund, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbBundFig <- ggplot(HerbBund, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=4), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Bundegi (n = 12)") +
  scale_x_continuous(limits=c(min(HerbBund$Year-0.25),max(HerbBund$Year+0.25)), breaks=min(HerbBund$Year):max(HerbBund$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold", colour = "white"),               #removes y axis title
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

HerbBundFig
#####################################

dat.ab.herb.muir <- subset(dat.ab.herb, Site %in% c("FishMI1", "FishMI2", "FishMI3", "FishMI4"))

HerbMuir <- ddply(dat.ab.herb.muir, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbMuirFig <- ggplot(HerbMuir, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("North Muirons (n = 12)") +
  scale_x_continuous(limits=c(min(HerbMuir$Year-0.25),max(HerbMuir$Year+0.25)), breaks=min(HerbMuir$Year):max(HerbMuir$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbMuirFig
#####################################

dat.ab.herb.light <- subset(dat.ab.herb, Site %in% c("FishL1", "FishL2"))

HerbLight <- ddply(dat.ab.herb.light, .(Year, Zone), summarise,
                     N    = length(value),
                     mean = mean(value),
                     sd   = sd(value),
                     se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbLightFig <- ggplot(HerbLight, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Lighthouse Bay (n = 6)") +
  scale_x_continuous(limits=c(min(HerbLight$Year-0.25),max(HerbLight$Year+0.25)), breaks=min(HerbLight$Year):max(HerbLight$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbLightFig
#####################################

dat.ab.herb.tant <- subset(dat.ab.herb, Site %in% c("FishT1", "FishT2"))

HerbTant <- ddply(dat.ab.herb.tant, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbTantFig <- ggplot(HerbTant, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Tantabiddi (n = 6)") +
  scale_x_continuous(limits=c(min(HerbTant$Year-0.25),max(HerbTant$Year+0.25)), breaks=min(HerbTant$Year):max(HerbTant$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbTantFig
#####################################

dat.ab.herb.mangbr <- subset(dat.ab.herb, Site %in% c("FishMB1", "FishMB2"))

HerbMangbr <- ddply(dat.ab.herb.mangbr, .(Year, Zone), summarise,
                      N    = length(value),
                      mean = mean(value),
                      sd   = sd(value),
                      se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbMangbrFig <- ggplot(HerbMangbr, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mangrove Bay - Back Reef (n = 6)") +
  scale_x_continuous(limits=c(min(HerbMangbr$Year-0.25),max(HerbMangbr$Year+0.25)), breaks=min(HerbMangbr$Year):max(HerbMangbr$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbMangbrFig
#####################################

dat.ab.herb.mangbom <- subset(dat.ab.herb, Site %in% c("FishMB3", "FishMB4"))

HerbMangbom <- ddply(dat.ab.herb.mangbom, .(Year, Zone), summarise,
                       N    = length(value),
                       mean = mean(value),
                       sd   = sd(value),
                       se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbMangbomFig <- ggplot(HerbMangbom, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mangrove Bay - Bommie (n = 6)") +
  scale_x_continuous(limits=c(min(HerbMangbom$Year-0.25),max(HerbMangbom$Year+0.25)), breaks=min(HerbMangbom$Year):max(HerbMangbom$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbMangbomFig
#####################################

dat.ab.herb.mand <- subset(dat.ab.herb, Site %in% c("FishM1", "FishM3"))

HerbMand <- ddply(dat.ab.herb.mand, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbMandFig <- ggplot(HerbMand, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mandu (n = 6)") +
  scale_x_continuous(limits=c(min(HerbMand$Year-0.25),max(HerbMand$Year+0.25)), breaks=min(HerbMand$Year):max(HerbMand$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold",colour="white"),               #removes y axis title
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

HerbMandFig
#####################################

dat.ab.herb.osp <- subset(dat.ab.herb, Site %in% c("FishO3", "FishO4"))

HerbOsp <- ddply(dat.ab.herb.osp, .(Year, Zone), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbOspFig <- ggplot(HerbOsp, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Osprey (n = 6-8)") +
  scale_x_continuous(limits=c(min(HerbOsp$Year-0.25),max(HerbOsp$Year+0.25)), breaks=min(HerbOsp$Year):max(HerbOsp$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),         #removes x axis title
        axis.title.y=element_text(size=16,face="bold", colour = "white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbOspFig
#####################################

dat.ab.herb.wind <- subset(dat.ab.herb, Site %in% c("FishW1", "FishW2"))

HerbWind <- ddply(dat.ab.herb.wind, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbWindFig <- ggplot(HerbWind, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Winderabandi (n = 6)") +
  scale_x_continuous(limits=c(min(HerbWind$Year-0.25),max(HerbWind$Year+0.25)), breaks=min(HerbWind$Year):max(HerbWind$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbWindFig
#####################################

dat.ab.herb.cloat <- subset(dat.ab.herb, Site %in% c("FishCL1", "FishCL2"))

HerbCloat <- ddply(dat.ab.herb.cloat, .(Year, Zone), summarise,
                     N    = length(value),
                     mean = mean(value),
                     sd   = sd(value),
                     se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbCloatFig <- ggplot(HerbCloat, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Cloates (n = 6)") +
  scale_x_continuous(limits=c(min(HerbCloat$Year-0.25),max(HerbCloat$Year+0.25)), breaks=min(HerbCloat$Year):max(HerbCloat$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbCloatFig
#####################################

dat.ab.herb.maud <- subset(dat.ab.herb, Site %in% c("FishCB1", "FishCB3"))

HerbMaud <- ddply(dat.ab.herb.maud, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbMaudFig <- ggplot(HerbMaud, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Maud (n = 6)") +
  scale_x_continuous(limits=c(min(HerbMaud$Year-0.25),max(HerbMaud$Year+0.25)), breaks=min(HerbMaud$Year):max(HerbMaud$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
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
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbMaudFig
#####################################

dat.ab.herb.pel <- subset(dat.ab.herb, Site %in% c("FishP1", "FishP3"))

HerbPel <- ddply(dat.ab.herb.pel, .(Year, Zone), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbPelFig <- ggplot(HerbPel, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Pelican (n = 6)") +
  scale_x_continuous(limits=c(min(HerbPel$Year-0.25),max(HerbPel$Year+0.25)), breaks=min(HerbPel$Year):max(HerbPel$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbPelFig
#####################################

dat.ab.herb.farq <- subset(dat.ab.herb, Site %in% c("FishCF1", "FishCF2", "FishCF3"))

HerbFarq <- ddply(dat.ab.herb.farq, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

HerbFarqFig <- ggplot(HerbFarq, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Cape Farquhar (n = 6)") +
  scale_x_continuous(limits=c(min(HerbFarq$Year-0.25),max(HerbFarq$Year+0.25)), breaks=min(HerbFarq$Year):max(HerbFarq$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbFarqFig
#####################################

#Create Panel Plot of Locations of interest

png(filename = "HerbNMPLoc.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(HerbBundFig, HerbMuirFig, HerbOspFig)
dev.off()

#Examine Coralivore graphs for all locations and present interesting ones

dat.ab.coral.bund <- subset(dat.ab.coral, Site %in% c("FishB1", "FishB2", "FishB3", "FishB4"))

CoralBund <- ddply(dat.ab.coral.bund, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralBundFig <- ggplot(CoralBund, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Bundegi (n = 12)") +
  scale_x_continuous(limits=c(min(CoralBund$Year-0.25),max(CoralBund$Year+0.25)), breaks=min(CoralBund$Year):max(CoralBund$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
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
        legend.justification=c(1,1), legend.position=c(1,0.8), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=20),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

CoralBundFig
#####################################

dat.ab.coral.muir <- subset(dat.ab.coral, Site %in% c("FishMI1", "FishMI2", "FishMI3", "FishMI4"))

CoralMuir <- ddply(dat.ab.coral.muir, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralMuirFig <- ggplot(CoralMuir, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=4), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("North Muirons (n = 12)") +
  scale_x_continuous(limits=c(min(CoralMuir$Year-0.25),max(CoralMuir$Year+0.25)), breaks=min(CoralMuir$Year):max(CoralMuir$Year)) +
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
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralMuirFig
#####################################

dat.ab.coral.light <- subset(dat.ab.coral, Site %in% c("FishL1", "FishL2"))

CoralLight <- ddply(dat.ab.coral.light, .(Year, Zone), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralLightFig <- ggplot(CoralLight, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Lighthouse Bay (n = 6)") +
  scale_x_continuous(limits=c(min(CoralLight$Year-0.25),max(CoralLight$Year+0.25)), breaks=min(CoralLight$Year):max(CoralLight$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralLightFig
#####################################

dat.ab.coral.tant <- subset(dat.ab.coral, Site %in% c("FishT1", "FishT2"))

CoralTant <- ddply(dat.ab.coral.tant, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralTantFig <- ggplot(CoralTant, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Tantabiddi (n = 6)") +
  scale_x_continuous(limits=c(min(CoralTant$Year-0.25),max(CoralTant$Year+0.25)), breaks=min(CoralTant$Year):max(CoralTant$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralTantFig
#####################################

dat.ab.coral.mangbr <- subset(dat.ab.coral, Site %in% c("FishMB1", "FishMB2"))

CoralMangbr <- ddply(dat.ab.coral.mangbr, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralMangbrFig <- ggplot(CoralMangbr, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mangrove Bay - Back Reef (n = 6)") +
  scale_x_continuous(limits=c(min(CoralMangbr$Year-0.25),max(CoralMangbr$Year+0.25)), breaks=min(CoralMangbr$Year):max(CoralMangbr$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralMangbrFig
#####################################

dat.ab.coral.mangbom <- subset(dat.ab.coral, Site %in% c("FishMB3", "FishMB4"))

CoralMangbom <- ddply(dat.ab.coral.mangbom, .(Year, Zone), summarise,
                     N    = length(value),
                     mean = mean(value),
                     sd   = sd(value),
                     se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralMangbomFig <- ggplot(CoralMangbom, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mangrove Bay - Bommie (n = 6)") +
  scale_x_continuous(limits=c(min(CoralMangbom$Year-0.25),max(CoralMangbom$Year+0.25)), breaks=min(CoralMangbom$Year):max(CoralMangbom$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralMangbomFig
#####################################

dat.ab.coral.mand <- subset(dat.ab.coral, Site %in% c("FishM1", "FishM3"))

CoralMand <- ddply(dat.ab.coral.mand, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralMandFig <- ggplot(CoralMand, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ x, se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mandu (n = 6-8)") +
  scale_x_continuous(limits=c(min(CoralMand$Year-0.25),max(CoralMand$Year+0.25)), breaks=min(CoralMand$Year):max(CoralMand$Year)) +
  theme_bw() +
  theme(axis.text.y=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(size=10, angle=45, vjust=0.4),
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

CoralMandFig
#####################################

dat.ab.coral.osp <- subset(dat.ab.coral, Site %in% c("FishO3", "FishO4"))

CoralOsp <- ddply(dat.ab.coral.osp, .(Year, Zone), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralOspFig <- ggplot(CoralOsp, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Osprey (n = 6)") +
  scale_x_continuous(limits=c(min(CoralOsp$Year-0.25),max(CoralOsp$Year+0.25)), breaks=min(CoralOsp$Year):max(CoralOsp$Year)) +
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
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralOspFig
#####################################

dat.ab.coral.wind <- subset(dat.ab.coral, Site %in% c("FishW1", "FishW2"))

CoralWind <- ddply(dat.ab.coral.wind, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralWindFig <- ggplot(CoralWind, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Winderabandi (n = 6)") +
  scale_x_continuous(limits=c(min(CoralWind$Year-0.25),max(CoralWind$Year+0.25)), breaks=min(CoralWind$Year):max(CoralWind$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralWindFig
#####################################

dat.ab.coral.cloat <- subset(dat.ab.coral, Site %in% c("FishCL1", "FishCL2"))

CoralCloat <- ddply(dat.ab.coral.cloat, .(Year, Zone), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralCloatFig <- ggplot(CoralCloat, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Cloates (n = 6)") +
  scale_x_continuous(limits=c(min(CoralCloat$Year-0.25),max(CoralCloat$Year+0.25)), breaks=min(CoralCloat$Year):max(CoralCloat$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralCloatFig
#####################################

dat.ab.coral.maud <- subset(dat.ab.coral, Site %in% c("FishCB1", "FishCB3"))

CoralMaud <- ddply(dat.ab.coral.maud, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralMaudFig <- ggplot(CoralMaud, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Maud (n = 6)") +
  scale_x_continuous(limits=c(min(CoralMaud$Year-0.25),max(CoralMaud$Year+0.25)), breaks=min(CoralMaud$Year):max(CoralMaud$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
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
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralMaudFig
#####################################

dat.ab.coral.pel <- subset(dat.ab.coral, Site %in% c("FishP1", "FishP3"))

CoralPel <- ddply(dat.ab.coral.pel, .(Year, Zone), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralPelFig <- ggplot(CoralPel, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Pelican (n = 6)") +
  scale_x_continuous(limits=c(min(CoralPel$Year-0.25),max(CoralPel$Year+0.25)), breaks=min(CoralPel$Year):max(CoralPel$Year)) +
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
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralPelFig
#####################################

dat.ab.coral.farq <- subset(dat.ab.coral, Site %in% c("FishCF1", "FishCF2", "FishCF3"))

CoralFarq <- ddply(dat.ab.coral.farq, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

CoralFarqFig <- ggplot(CoralFarq, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Cape Farquhar (n = 6)") +
  scale_x_continuous(limits=c(min(CoralFarq$Year-0.25),max(CoralFarq$Year+0.25)), breaks=min(CoralFarq$Year):max(CoralFarq$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

CoralFarqFig
#####################################

#Create Panel Plot of Locations of interest

png(filename = "CoralNMPLoc.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(CoralBundFig, CoralMuirFig, CoralMandFig, CoralPelFig)
dev.off()

#Examine Piscivore graphs for all locations and present interesting ones

dat.ab.pisc.bund <- subset(dat.ab.pisc, Site %in% c("FishB1", "FishB2", "FishB3", "FishB4"))

PiscBund <- ddply(dat.ab.pisc.bund, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscBundFig <- ggplot(PiscBund, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=4), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Bundegi (n = 12)") +
  scale_x_continuous(limits=c(min(PiscBund$Year-0.25),max(PiscBund$Year+0.25)), breaks=min(PiscBund$Year):max(PiscBund$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold", colour = "white"),               #removes y axis title
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

PiscBundFig
#####################################

dat.ab.pisc.muir <- subset(dat.ab.pisc, Site %in% c("FishMI1", "FishMI2", "FishMI3", "FishMI4"))

PiscMuir <- ddply(dat.ab.pisc.muir, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscMuirFig <- ggplot(PiscMuir, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("North Muirons (n = 12)") +
  scale_x_continuous(limits=c(min(PiscMuir$Year-0.25),max(PiscMuir$Year+0.25)), breaks=min(PiscMuir$Year):max(PiscMuir$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscMuirFig
#####################################

dat.ab.pisc.light <- subset(dat.ab.pisc, Site %in% c("FishL1", "FishL2"))

PiscLight <- ddply(dat.ab.pisc.light, .(Year, Zone), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscLightFig <- ggplot(PiscLight, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Lighthouse Bay (n = 6)") +
  scale_x_continuous(limits=c(min(PiscLight$Year-0.25),max(PiscLight$Year+0.25)), breaks=min(PiscLight$Year):max(PiscLight$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscLightFig
#####################################

dat.ab.pisc.tant <- subset(dat.ab.pisc, Site %in% c("FishT1", "FishT2"))

PiscTant <- ddply(dat.ab.pisc.tant, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscTantFig <- ggplot(PiscTant, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Tantabiddi (n = 6)") +
  scale_x_continuous(limits=c(min(PiscTant$Year-0.25),max(PiscTant$Year+0.25)), breaks=min(PiscTant$Year):max(PiscTant$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscTantFig
#####################################

dat.ab.pisc.mangbr <- subset(dat.ab.pisc, Site %in% c("FishMB1", "FishMB2"))

PiscMangbr <- ddply(dat.ab.pisc.mangbr, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscMangbrFig <- ggplot(PiscMangbr, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mangrove Bay - Back Reef (n = 6)") +
  scale_x_continuous(limits=c(min(PiscMangbr$Year-0.25),max(PiscMangbr$Year+0.25)), breaks=min(PiscMangbr$Year):max(PiscMangbr$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscMangbrFig
#####################################

dat.ab.pisc.mangbom <- subset(dat.ab.pisc, Site %in% c("FishMB3", "FishMB4"))

PiscMangbom <- ddply(dat.ab.pisc.mangbom, .(Year, Zone), summarise,
                     N    = length(value),
                     mean = mean(value),
                     sd   = sd(value),
                     se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscMangbomFig <- ggplot(PiscMangbom, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mangrove Bay - Bommie (n = 6)") +
  scale_x_continuous(limits=c(min(PiscMangbom$Year-0.25),max(PiscMangbom$Year+0.25)), breaks=min(PiscMangbom$Year):max(PiscMangbom$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscMangbomFig
#####################################

dat.ab.pisc.mand <- subset(dat.ab.pisc, Site %in% c("FishM1", "FishM3"))

PiscMand <- ddply(dat.ab.pisc.mand, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscMandFig <- ggplot(PiscMand, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mandu (n = 6)") +
  scale_x_continuous(limits=c(min(PiscMand$Year-0.25),max(PiscMand$Year+0.25)), breaks=min(PiscMand$Year):max(PiscMand$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold",colour="white"),               #removes y axis title
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

PiscMandFig
#####################################

dat.ab.pisc.osp <- subset(dat.ab.pisc, Site %in% c("FishO3", "FishO4"))

PiscOsp <- ddply(dat.ab.pisc.osp, .(Year, Zone), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscOspFig <- ggplot(PiscOsp, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Osprey (n = 6-8)") +
  scale_x_continuous(limits=c(min(PiscOsp$Year-0.25),max(PiscOsp$Year+0.25)), breaks=min(PiscOsp$Year):max(PiscOsp$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),         #removes x axis title
        axis.title.y=element_text(size=16,face="bold", colour = "white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscOspFig
#####################################

dat.ab.pisc.wind <- subset(dat.ab.pisc, Site %in% c("FishW1", "FishW2"))

PiscWind <- ddply(dat.ab.pisc.wind, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscWindFig <- ggplot(PiscWind, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Winderabandi (n = 6)") +
  scale_x_continuous(limits=c(min(PiscWind$Year-0.25),max(PiscWind$Year+0.25)), breaks=min(PiscWind$Year):max(PiscWind$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscWindFig
#####################################

dat.ab.pisc.cloat <- subset(dat.ab.pisc, Site %in% c("FishCL1", "FishCL2"))

PiscCloat <- ddply(dat.ab.pisc.cloat, .(Year, Zone), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscCloatFig <- ggplot(PiscCloat, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Cloates (n = 6)") +
  scale_x_continuous(limits=c(min(PiscCloat$Year-0.25),max(PiscCloat$Year+0.25)), breaks=min(PiscCloat$Year):max(PiscCloat$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscCloatFig
#####################################

dat.ab.pisc.maud <- subset(dat.ab.pisc, Site %in% c("FishCB1", "FishCB3"))

PiscMaud <- ddply(dat.ab.pisc.maud, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscMaudFig <- ggplot(PiscMaud, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Maud (n = 6)") +
  scale_x_continuous(limits=c(min(PiscMaud$Year-0.25),max(PiscMaud$Year+0.25)), breaks=min(PiscMaud$Year):max(PiscMaud$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
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
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscMaudFig
#####################################

dat.ab.pisc.pel <- subset(dat.ab.pisc, Site %in% c("FishP1", "FishP3"))

PiscPel <- ddply(dat.ab.pisc.pel, .(Year, Zone), summarise,
                 N    = length(value),
                 mean = mean(value),
                 sd   = sd(value),
                 se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscPelFig <- ggplot(PiscPel, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Pelican (n = 6)") +
  scale_x_continuous(limits=c(min(PiscPel$Year-0.25),max(PiscPel$Year+0.25)), breaks=min(PiscPel$Year):max(PiscPel$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscPelFig
#####################################

dat.ab.pisc.farq <- subset(dat.ab.pisc, Site %in% c("FishCF1", "FishCF2", "FishCF3"))

PiscFarq <- ddply(dat.ab.pisc.farq, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

PiscFarqFig <- ggplot(PiscFarq, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Cape Farquhar (n = 6)") +
  scale_x_continuous(limits=c(min(PiscFarq$Year-0.25),max(PiscFarq$Year+0.25)), breaks=min(PiscFarq$Year):max(PiscFarq$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

PiscFarq
#####################################

#Create Panel Plot of Locations of interest

png(filename = "PiscNMPLoc.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(HerbBundFig, HerbMuirFig, HerbOspFig)
dev.off()


