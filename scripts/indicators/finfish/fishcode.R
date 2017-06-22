install.packages("gridExtra")
install.packages("reshape")

setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)

#Load CSV

dat <- read.csv("NMP_DOV_All Data.csv")

#Check column names

colnames (dat)

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Period", "Genus.Species", "Number"))
dat.ab$Number <- as.character(dat.ab$Number)
dat.ab$Number <- as.integer(dat.ab$Number)

#Remove any 'NA' values from dataset

dat.ab<-dat.ab[!is.na(dat.ab$Period),]
dat.ab = droplevels(dat.ab)

#Sum abundance values for individual species within site and period

dat.ab.total <- ddply(dat.ab, .(Year, Zone, Site, Period, Genus.Species), summarise,
            total = sum(Number))

#Add 0 values to dataset for transects where fish species weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site + Period ~ Genus.Species, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site", "Period")))

#Subset dataset to highly targeted fish species at Ningaloo and some together so that they represent 'total target fish' per replicate

dat.ab.target <- subset(dat.ab.total2, Genus.Species %in% c("Lethrinus nebulosus", "Lethrinus laticaudis", "Lethrinus miniatus", "Epinephelus rivulatus", "Epinephelus multinotatus", "Plectropomus spp", "Plectropomus leopardus", "Plectropomus maculatus"))
dat.ab.target2 <-ddply(dat.ab.target, .(Year, Zone, Site, Period), summarise,
                       total = sum(value))

#Limit to those sites and years where we have a continuous time series
dat.ab.target3 <- subset(dat.ab.target2, Site %in% c("FishCB1", "FishCB3", "FishM1", "FishM3", "FishO3", "FishO4"))
dat.ab.target4 <- subset(dat.ab.target3, Year %in% c("1999", "2000", "2011", "2014", "2015"))

#Obtain mean and SE values for targeted fishes at zone level

TargetNMP <- ddply(dat.ab.target4, .(Year, Zone), summarise,
                N    = length(total),
                mean = mean(total),
                sd   = sd(total),
                se   = sd(total) / sqrt(length(total)) )


#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetNMPFig <- ggplot(TargetNMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetNMP$Year-0.25),max(TargetNMP$Year+0.25)), breaks=min(TargetNMP$Year):max(TargetNMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
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

png(filename = "TargetNMP.png",
    width = 600, height = 400, units = "px", pointsize = 6)
TargetNMPFig
dev.off()

#Create figures for individual Sanctuary Zones

dat.ab.target.bund <- subset(dat.ab.target2, Site %in% c("FishB1", "FishB2", "FishB3", "FishB4"))

TargetBund <- ddply(dat.ab.target.bund, .(Year, Zone), summarise,
                N    = length(total),
                mean = mean(total),
                sd   = sd(total),
                se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetBundFig <- ggplot(TargetBund, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Bundegi (n = 12)") +
  scale_x_continuous(limits=c(min(TargetBund$Year-0.25),max(TargetBund$Year+0.25)), breaks=min(TargetBund$Year):max(TargetBund$Year)) +
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

TargetBundFig
#####################################

dat.ab.target.muir <- subset(dat.ab.target2, Site %in% c("FishMI1", "FishMI2", "FishMI3", "FishMI4"))

TargetMuir <- ddply(dat.ab.target.muir, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetMuirFig <- ggplot(TargetMuir, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("North Muirons (n = 12)") +
  scale_x_continuous(limits=c(min(TargetMuir$Year-0.25),max(TargetMuir$Year+0.25)), breaks=min(TargetMuir$Year):max(TargetMuir$Year)) +
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

TargetMuirFig
#####################################

dat.ab.target.light <- subset(dat.ab.target2, Site %in% c("FishL1", "FishL2"))

TargetLight <- ddply(dat.ab.target.light, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetLightFig <- ggplot(TargetLight, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Lighthouse Bay (n = 6)") +
  scale_x_continuous(limits=c(min(TargetLight$Year-0.25),max(TargetLight$Year+0.25)), breaks=min(TargetLight$Year):max(TargetLight$Year)) +
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

TargetLightFig
#####################################

dat.ab.target.tant <- subset(dat.ab.target2, Site %in% c("FishT1", "FishT2"))

TargetTant <- ddply(dat.ab.target.tant, .(Year, Zone), summarise,
                     N    = length(total),
                     mean = mean(total),
                     sd   = sd(total),
                     se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetTantFig <- ggplot(TargetTant, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Tantabiddi (n = 6)") +
  scale_x_continuous(limits=c(min(TargetTant$Year-0.25),max(TargetTant$Year+0.25)), breaks=min(TargetTant$Year):max(TargetTant$Year)) +
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

TargetTantFig
#####################################

dat.ab.target.mangbr <- subset(dat.ab.target2, Site %in% c("FishMB1", "FishMB2"))

TargetMangbr <- ddply(dat.ab.target.mangbr, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetMangbrFig <- ggplot(TargetMangbr, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mangrove Bay - Back Reef (n = 6)") +
  scale_x_continuous(limits=c(min(TargetMangbr$Year-0.25),max(TargetMangbr$Year+0.25)), breaks=min(TargetMangbr$Year):max(TargetMangbr$Year)) +
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

TargetMangbrFig
#####################################

dat.ab.target.mangbom <- subset(dat.ab.target2, Site %in% c("FishMB3", "FishMB4"))

TargetMangbom <- ddply(dat.ab.target.mangbom, .(Year, Zone), summarise,
                      N    = length(total),
                      mean = mean(total),
                      sd   = sd(total),
                      se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetMangbomFig <- ggplot(TargetMangbom, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mangrove Bay - Bommie (n = 6)") +
  scale_x_continuous(limits=c(min(TargetMangbom$Year-0.25),max(TargetMangbom$Year+0.25)), breaks=min(TargetMangbom$Year):max(TargetMangbom$Year)) +
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

TargetMangbomFig
#####################################

dat.ab.target.mand <- subset(dat.ab.target2, Site %in% c("FishM1", "FishM3"))

TargetMand <- ddply(dat.ab.target.mand, .(Year, Zone), summarise,
                       N    = length(total),
                       mean = mean(total),
                       sd   = sd(total),
                       se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetMandFig <- ggplot(TargetMand, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mandu (n = 6)") +
  scale_x_continuous(limits=c(min(TargetMand$Year-0.25),max(TargetMand$Year+0.25)), breaks=min(TargetMand$Year):max(TargetMand$Year)) +
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

TargetMandFig
#####################################

dat.ab.target.osp <- subset(dat.ab.target2, Site %in% c("FishO3", "FishO4"))

TargetOsp <- ddply(dat.ab.target.osp, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetOspFig <- ggplot(TargetOsp, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Osprey (n = 6)") +
  scale_x_continuous(limits=c(min(TargetOsp$Year-0.25),max(TargetOsp$Year+0.25)), breaks=min(TargetOsp$Year):max(TargetOsp$Year)) +
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

TargetOspFig
#####################################

dat.ab.target.wind <- subset(dat.ab.target2, Site %in% c("FishW1", "FishW2"))

TargetWind <- ddply(dat.ab.target.wind, .(Year, Zone), summarise,
                   N    = length(total),
                   mean = mean(total),
                   sd   = sd(total),
                   se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetWindFig <- ggplot(TargetWind, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Winderabandi (n = 6)") +
  scale_x_continuous(limits=c(min(TargetWind$Year-0.25),max(TargetWind$Year+0.25)), breaks=min(TargetWind$Year):max(TargetWind$Year)) +
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

TargetWindFig
#####################################

dat.ab.target.cloat <- subset(dat.ab.target2, Site %in% c("FishCL1", "FishCL2"))

TargetCloat <- ddply(dat.ab.target.cloat, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetCloatFig <- ggplot(TargetCloat, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Cloates (n = 6)") +
  scale_x_continuous(limits=c(min(TargetCloat$Year-0.25),max(TargetCloat$Year+0.25)), breaks=min(TargetCloat$Year):max(TargetCloat$Year)) +
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

TargetCloatFig
#####################################

dat.ab.target.maud <- subset(dat.ab.target2, Site %in% c("FishCB1", "FishCB3"))

TargetMaud <- ddply(dat.ab.target.maud, .(Year, Zone), summarise,
                     N    = length(total),
                     mean = mean(total),
                     sd   = sd(total),
                     se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetMaudFig <- ggplot(TargetMaud, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Maud (n = 6)") +
  scale_x_continuous(limits=c(min(TargetMaud$Year-0.25),max(TargetMaud$Year+0.25)), breaks=min(TargetMaud$Year):max(TargetMaud$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold"),                #removes x axis title
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

TargetMaudFig
#####################################

dat.ab.target.pel <- subset(dat.ab.target2, Site %in% c("FishP1", "FishP3"))

TargetPel <- ddply(dat.ab.target.pel, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetPelFig <- ggplot(TargetPel, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Pelican (n = 6)") +
  scale_x_continuous(limits=c(min(TargetPel$Year-0.25),max(TargetPel$Year+0.25)), breaks=min(TargetPel$Year):max(TargetPel$Year)) +
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

TargetPelFig
#####################################

dat.ab.target.farq <- subset(dat.ab.target2, Site %in% c("FishCF1", "FishCF2", "FishCF3"))

TargetFarq <- ddply(dat.ab.target.farq, .(Year, Zone), summarise,
                   N    = length(total),
                   mean = mean(total),
                   sd   = sd(total),
                   se   = sd(total) / sqrt(length(total)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

TargetFarqFig <- ggplot(TargetFarq, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Cape Farquhar (n = 6)") +
  scale_x_continuous(limits=c(min(TargetFarq$Year-0.25),max(TargetFarq$Year+0.25)), breaks=min(TargetFarq$Year):max(TargetFarq$Year)) +
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

TargetFarqFig
#####################################

#Create Panel Plot of Locations of interest

png(filename = "TargetNMPLoc.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(TargetMandFig, TargetOspFig, TargetMaudFig)
dev.off()
