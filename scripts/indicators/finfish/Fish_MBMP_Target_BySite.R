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

dat.ab.target <- subset(dat.ab.total2, Genus.Species %in% c("Lethrinus nebulosus", "Lethrinus laticaudis","Epinephelus coioides","Epinephelus malabaricus", "Plectropomus spp", "Plectropomus leopardus", "Plectropomus maculatus","Lutjanus carponotatus","Lutjanus argentimaculatus","Choerodon cyanodus","Choerodon schoenleinii"))

dat.ab.target2 <-ddply(dat.ab.target, .(Year, Zone, Site, Period), summarise,
                       total = sum(value))

#Limit to those sites and years where we have a continuous time series (three years or more). 2009 removed due to inconsistancy in DOV approach
dat.ab.target3 <- subset(dat.ab.target2, Site %in% c("MBI8","MBI7","MBI11","MBI12","MBI19","MBI16","MBI5","MBI6","MBI20","MBI26"))
dat.ab.target4 <- subset(dat.ab.target3, Year %in% c("2010", "2011", "2012", "2015", "2017"))

#Obtain mean and SE values for targeted fishes at site level for each zone across years

TargetMBI <- ddply(dat.ab.target4, .(Year, Zone, Site), summarise,
                   N    = length(total),
                   mean = mean(total),
                   sd   = sd(total),
                   se   = sd(total) / sqrt(length(total)) )

#Subset By Zone
TargetMBI <- subset(TargetMBI, Zone %in% c("Recreation"))

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
  ggtitle("Recreation Zone")+
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
        plot.title=element_text(hjust=0.5,size=14,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

png(filename = "TargetRecreationMBI.png",
   width = 600, height = 400, units = "px", pointsize = 6)
TargetMBIFig+facet_grid(Site~.,scales="free_y")
dev.off()

#Update plot on data catalogue
#ckanr::resource_update("e2dcbac6-c127-4b89-87e5-850faf77319e", "TargetSanctuaryMBI.png")
#ckanr::resource_update("bc9d125b-49f8-4be9-aaf4-4410c3a50215", "TargetGeneralMBI.png")
ckanr::resource_update("878c20d3-f8e3-408b-a558-43fc10146a83", "TargetRecreationMBI.png")
#ckanr::resource_update("aec5b10b-9d66-4687-b9df-c8ece0feff39", "TargetOutsideMBI.png")


##############################
#Biomass

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Period", "Genus.Species", "adjusted.biomass..g."))
dat.ab$adjusted.biomass..g. <- as.character(dat.ab$adjusted.biomass..g.)
dat.ab$adjusted.biomass..g. <- as.integer(dat.ab$adjusted.biomass..g.)

#Remove any 'NA' values from dataset

dat.ab<-dat.ab[!is.na(dat.ab$Period),]
dat.ab = droplevels(dat.ab)

#Sum abundance values for individual species within site and period

dat.ab.total <- ddply(dat.ab, .(Year, Zone, Site, Period, Genus.Species), summarise,
                      total = sum(adjusted.biomass..g.))

#Add 0 values to dataset for transects where fish species weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site + Period ~ Genus.Species, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site", "Period")))

#Subset dataset to highly targeted fish species at Ningaloo and some together so that they represent 'total target fish' per replicate

dat.ab.target <- subset(dat.ab.total2, Genus.Species %in% c("Lethrinus nebulosus", "Lethrinus laticaudis","Epinephelus coioides","Epinephelus malabaricus", "Plectropomus spp", "Plectropomus leopardus", "Plectropomus maculatus","Lutjanus carponotatus","Lutjanus argentimaculatus","Choerodon cyanodus","Choerodon schoenleinii"))
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
