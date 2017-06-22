setwd("~/projects/data-pipelines/scripts/indicators/finfish")
#source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)

#Load CSV

dat <- read.csv("SBMP_DOV_All Data.csv")

#Check column names

colnames (dat)

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Period", "Genus.Species", "Family", "Number"))
dat.ab$Number <- as.character(dat.ab$Number)
dat.ab$Number <- as.integer(dat.ab$Number)

#Remove any 'NA' values from dataset

dat.ab <- dat.ab[!is.na(dat.ab$Period),]
dat.ab = droplevels(dat.ab)

#Remove cryptic families from dataset

dat.ab.filter <- subset(dat.ab, Family != c("Apogonidae", "Blennidae", "Gobiidae"))
dat.ab.filter2 <- subset(dat.ab.filter, Family != c("Holocentridae", "Pempheridae", "Pseudochromidae"))
dat.ab.filter3 <- subset(dat.ab.filter2, Family != c("Pempherididae"))
dat.ab.filter4 <- subset(dat.ab.filter3, Family != c("Plotosidae"))

#Sum abundance values for individual species within site and period

dat.ab.total <- ddply(dat.ab.filter4, .(Year, Zone, Site, Period, Genus.Species), summarise,
                      total = sum(Number))

#Remove 0 values and sum unique Genus.species names to provide a Species Richness score for every transect

dat.ab.removezero <- subset(dat.ab.total, total > 0)
dat.ab.richness <- ddply(dat.ab.removezero, .(Year, Zone, Site, Period), summarise,
                         richness = length(unique(Genus.Species)))

#Limit to those sites and years where we have a continuous time series

dat.ab.richness2 <- subset(dat.ab.richness, Year %in% c("2010", "2015"))
dat.ab.richness3 <- subset (dat.ab.richness2, Site %in% c("SB204", "SB205", "SB213", "SBBFlat", "SBSPT"))

#Obtain mean and SE values for species Richness at zone level

RichnessSBMP <- ddply(dat.ab.richness3, .(Year, Zone), summarise,
                     N    = length(richness),
                     mean = mean(richness),
                     sd   = sd(richness),
                     se   = sd(richness) / sqrt(length(richness)) )

#Create figure for all NMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
RichnessSBMPFig <- ggplot(RichnessSBMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Species Richness per 250 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(RichnessSBMP$Year-0.25),max(RichnessSBMP$Year+0.25)), breaks=min(RichnessSBMP$Year):max(RichnessSBMP$Year)) +
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
        legend.justification=c(1,1), legend.position=c(1,0.3), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

png(filename = "RichnessSBMP.png",
    width = 600, height = 400, units = "px", pointsize = 6)
RichnessSBMPFig
dev.off()

