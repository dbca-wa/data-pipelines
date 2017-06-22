install.packages("gridExtra")
install.packages("reshape")
install.packages("MuMIn")
install.packages("gam")

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

dat.ab.target <- subset(dat.ab.total2, Genus.Species %in% c("Lethrinus nebulosus", "Lethrinus laticaudis", "Pagrus auratus", "Choerodon rubescens", "Epinephelus multinotatus", "Scomberomorous commerson", "Lutjanus carponotatus", "Plectropomus spp", "Plectropomus leopardus", "Plectropomus maculatus"))
dat.ab.target2 <-ddply(dat.ab.target, .(Year, Zone, Site, Period), summarise,
                       total = sum(value))

#Limit to those sites and years where we have a continuous time series
dat.ab.target3 <- subset(dat.ab.target2, Site %in% c("SB204", "SB205", "SB213", "SBBFlat", "SBSPT"))

#extra stuff
#at.ab.target3 <- subset(dat.ab.target, Site %in% c("SB204", "SB205", "SB213", "SBBFlat", "SBSPT"))
#dat.ab.target4 <- ddply(dat.ab.target3, .(Year, Genus.Species), summarise,
                        #total = sum(value))

y = dat.ab.target3$total
x = as.integer(dat.ab.target3$Year)
gamtarget <- gam(data = dat.ab.target4, formula = y~x + poly(x,2) + poly(x,3), na.action = "na.fail", family = negbin())
gamtarget.compare <- dredge(trace = 1, gamtarget, m.lim = c(0,1), extra = c("adjR^2","R^2","AIC", "BIC"))


#Obtain mean and SE values for targeted fishes at zone level

TargetSBMP <- ddply(dat.ab.target3, .(Year, Zone), summarise,
                   N    = length(total),
                   mean = mean(total),
                   sd   = sd(total),
                   se   = sd(total) / sqrt(length(total)) )


#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetSBMPFig <- ggplot(TargetSBMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "gam", formula = y ~ poly(x,3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetSBMP$Year-0.25),max(TargetSBMP$Year+0.25)), breaks=min(TargetSBMP$Year):max(TargetSBMP$Year)) +
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

png(filename = "TargetSBMP.png",
    width = 600, height = 400, units = "px", pointsize = 6)
TargetSBMPFig
dev.off()
