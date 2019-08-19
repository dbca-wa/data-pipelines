setwd("~/projects/data-pipelines/scripts/indicators/finfish")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)
library(mgcv)
#library(gam)
library(MASS)
library(MuMIn)

#Load CSV from the data catalogue
data_rid <- "20ec513f-5a75-46d3-96d8-ae99fa50b09f"
dat <- ckanr::resource_show(data_rid)$url %>% readr::read_csv(.)

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

#Subset dataset to highly targeted fish species at MBI and sum together so that they represent 'total target fish' per replicate

dat.ab.target <- subset(dat.ab.total2, Genus.Species %in% c("Lethrinus nebulosus", "Lethrinus laticaudis", "Lethrinus miniatus", "Lethrinus punctulatus", "Epinephelus multinotatus", "Epinephelus areolatus", "Epinephelus coioides", "Plectropomus spp", "Plectropomus leopardus", "Plectropomus maculatus", "Lutjanus argentimaculatus", "Lutjanus lemniscatus", "Lutjanus sebae", "Lutjanus erythropterus", "Lutjanus malabaricus"))
dat.ab.target2 <-ddply(dat.ab.target, .(Year, Zone, Site, Period), summarise,
                       total = sum(value))

#Limit to those sites and years where we have a continuous time series (three years or more). 2009 removed due to inconsistancy in DOV approach
dat.ab.target3 <- subset(dat.ab.target2, Site %in% c("MBI5","MBI6","MBI7","MBI8", "MBI11","MBI12","MBI19","MBI16","MBI17", "MBI20","MBI23","MBI27","MBI29","MBI31", "MBI26"))
dat.ab.target4 <- subset(dat.ab.target3, Year %in% c("2010", "2011", "2012", "2015", "2017"))

#Obtain mean and SE values for targeted fishes at site level for each zone across years

TargetMBI <- ddply(dat.ab.target4, .(Year, Zone,Site), summarise,
                   N    = length(total),
                   mean = mean(total),
                   sd   = sd(total),
                   se   = sd(total) / sqrt(length(total)) )


#Create figure for all MBI Abundance

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetMBIFig <- ggplot(TargetMBI, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Target abundance per 250 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetMBI$Year-0.25),max(TargetMBI$Year+0.25)), breaks=min(TargetMBI$Year):max(TargetMBI$Year)) +
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

png(filename = "TargetMBI.png",
    width = 600, height = 400, units = "px", pointsize = 6)
TargetMBIFig+facet_grid(Zone+Site~.,scales="free_y")
dev.off()

##############################
#Biomass

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Period", "Genus.Species", "Adjusted.Biomass..g."))
dat.ab$Adjusted.Biomass..g. <- as.character(dat.ab$Adjusted.Biomass..g.)
dat.ab$Adjusted.Biomass..g. <- as.integer(dat.ab$Adjusted.Biomass..g.)

#Remove any 'NA' values from dataset

dat.ab<-dat.ab[!is.na(dat.ab$Period),]
dat.ab = droplevels(dat.ab)

#Sum abundance values for individual species within site and period

dat.ab.total <- ddply(dat.ab, .(Year, Zone, Site, Period, Genus.Species), summarise,
                      total = sum(Adjusted.Biomass..g.))

#Add 0 values to dataset for transects where fish species weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site + Period ~ Genus.Species, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site", "Period")))

#Subset dataset to highly targeted fish species at Ningaloo and some together so that they represent 'total target fish' per replicate

dat.ab.target <- subset(dat.ab.total2, Genus.Species %in% c("Lethrinus nebulosus", "Lethrinus laticaudis", "Lethrinus miniatus", "Lethrinus punctulatus", "Epinephelus multinotatus", "Epinephelus areolatus", "Epinephelus coioides", "Plectropomus spp", "Plectropomus leopardus", "Plectropomus maculatus", "Lutjanus argentimaculatus", "Lutjanus lemniscatus", "Lutjanus sebae", "Lutjanus erythropterus", "Lutjanus malabaricus", "Rachycentron canadus"))
dat.ab.target2 <-ddply(dat.ab.target, .(Year, Zone, Site, Period), summarise,
                       total = sum(value))

#Limit to those sites and years where we have a continuous time series
dat.ab.target3 <- subset(dat.ab.target2, Site %in% c("MBI8", "MBI11", "MBI16", "MBI17", "MBI19", "MBI20", "MBI23", "MBI26", "MBI27", "MBI29"))
#dat.ab.target5 <- subset(dat.ab.target3, Zone %in% c("Reef Fished"))
dat.ab.target4 <- subset(dat.ab.target3, Year %in% c("2010", "2011", "2012", "2015"))

#y = dat.ab.target4$total
#x = as.integer(dat.ab.target4$Year)
#gamtarget <- gam(data = dat.ab.target4, formula = y~x + poly(x,2) + poly(x,3), na.action = "na.fail", family = negbin())
#gamtarget.compare <- dredge(trace = 1, gamtarget, m.lim = c(0,1), extra = c("adjR^2","R^2","AIC", "BIC"))


#Obtain mean and SE values for targeted fishes at zone level

TargetBioMBI <- ddply(dat.ab.target4, .(Year, Zone), summarise,
                      N    = length(total),
                      mean = mean(total),
                      sd   = sd(total),
                      se   = sd(total) / sqrt(length(total)) )


#Create figure for all MBI Abundance

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetBioMBIFig <- ggplot(TargetBioMBI, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Biomass per 250 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetBioMBI$Year-0.25),max(TargetBioMBI$Year+0.25)), breaks=min(TargetBioMBI$Year):max(TargetBioMBI$Year)) +
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

png(filename = "TargetBioMBI.png",
    width = 600, height = 400, units = "px", pointsize = 6)
TargetBioMBIFig
dev.off()
