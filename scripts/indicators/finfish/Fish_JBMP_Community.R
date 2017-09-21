setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
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

colnames(dat)

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Transect", "Feeding.Guild", "Number"))
dat.ab$Number <- as.character(dat.ab$Number)
dat.ab$Number <- as.integer(dat.ab$Number)
dat.ab <- dat.ab[!is.na(dat.ab$Number),]

#Remove any 'NA' values from dataset

dat.ab <- dat.ab[!is.na(dat.ab$Transect),]
dat.ab = droplevels(dat.ab)

#ADD STEP HERE TO RENAME SCRAPER/EXCAVATOR AND LARGE CROPPER AS LARGE HERBIVORE
dat.ab$Feeding.Guild <- as.character(dat.ab$Feeding.Guild)
dat.ab <- dat.ab[!(dat.ab$Feeding.Guild == "" | is.na(dat.ab$Feeding.Guild)), ]
dat.ab$Feeding.Guild[dat.ab$Feeding.Guild %in% "Large Cropper"] <- "Large Herbivore"
dat.ab$Feeding.Guild[dat.ab$Feeding.Guild %in% "Scraper/excavator"] <- "Large Herbivore"
dat.ab$Feeding.Guild <- as.factor(dat.ab$Feeding.Guild)

#Sum abundance values for trophic groups within site and period

dat.ab.total <- ddply(dat.ab, .(Year, Zone, Site, Transect, Feeding.Guild), summarise,
                      total = sum(Number))

#Add 0 values to dataset for transects where trophic groups weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site + Transect ~ Feeding.Guild, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site", "Transect")))

#Limit to those sites and years where we have a continuous time series
dat.ab.total3 <- subset(dat.ab.total2, Site %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25"))

#Subset dataset to trophic groups of interest (Piscivore, Large Herbivore, Mobile Invertivore) at Jurien Bay and limit to those sites and years where we have a continuous time series

dat.ab.pisc <- subset(dat.ab.total3, Feeding.Guild %in% c("Piscivore"))

dat.ab.herb <- subset(dat.ab.total3, Feeding.Guild %in% c("Large Herbivore"))

dat.ab.invert <- subset(dat.ab.total3, Feeding.Guild %in% c("Mobile Inverivore"))


#Obtain mean and SE values for Piscivore, Large Herbivore and Mobile Invertivore and create figures for all SBMP

PiscJBMP <- ddply(dat.ab.pisc, .(Year), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
PiscJBMPFig <- ggplot(PiscJBMP, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=8), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 500 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Piscivores") +
  scale_x_continuous(limits=c(min(PiscJBMP$Year-0.25),max(PiscJBMP$Year+0.25)), breaks=min(PiscJBMP$Year):max(PiscJBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold", color = "white"),               #removes y axis title
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

PiscJBMPFig
#####################################

HerbJBMP <- ddply(dat.ab.herb, .(Year), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
HerbJBMPFig <- ggplot(HerbJBMP, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=8), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 500 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Large Herbivores") +
  scale_x_continuous(limits=c(min(HerbJBMP$Year-0.25),max(HerbJBMP$Year+0.25)), breaks=min(HerbJBMP$Year):max(HerbJBMP$Year)) +
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
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbJBMPFig
#####################################

InvertJBMP <- ddply(dat.ab.invert, .(Year), summarise,
                   N    = length(value),
                   mean = mean(value),
                   sd   = sd(value),
                   se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
InvertJBMPFig <- ggplot(InvertJBMP, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=8), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  #ylab(expression(paste("Abundance per 500 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mobile Invertivores") +
  scale_x_continuous(limits=c(min(InvertJBMP$Year-0.25),max(InvertJBMP$Year+0.25)), breaks=min(InvertJBMP$Year):max(InvertJBMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold", color = "white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

InvertJBMPFig
#####################################

#Create Panel Plot of all trophic groups

png(filename = "CommunityJBMP.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(PiscJBMPFig, HerbJBMPFig, InvertJBMPFig)
dev.off()
