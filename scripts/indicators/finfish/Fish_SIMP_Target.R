#setwd("~/projects/data-pipelines/scripts/indicators/finfish")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)
library(mgcv)
library(MASS)
library(MuMIn)

#Load CSV

#dat <- reall.csv")

# or from the data catalogue
data_rid <- "c80224c2-8ee6-49ae-aef4-4fabace94ac6"
dat <- ckanr::resource_show(data_rid)$url %>% readr::read_csv(.)


#Check column namesd.csv("SIMP_DOV_A

colnames (dat)

############################
#ABUNDANCE
#Limit to those columns that are important for this analysis

dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "GenusSpecies", "Number"))
dat.ab$Number <- as.character(dat.ab$Number)
dat.ab$Number <- as.integer(dat.ab$Number)

#Remove any 'NA' values from dataset

dat.ab<-dat.ab[!is.na(dat.ab$Site),]
dat.ab = droplevels(dat.ab)

#Sum abundance values for individual species within site and period

dat.ab.total <- ddply(dat.ab, .(Year, Zone, Site, GenusSpecies), summarise,
                      total = sum(Number))

#ADD STEP HERE TO RENAME ZONES TO FISHED AND PROTECTED
dat.ab.total$Zone <- as.character(dat.ab.total$Zone)
dat.ab.total <- dat.ab.total[!(dat.ab.total$Zone == "" | is.na(dat.ab.total$Zone)), ]
#dat.ab.total$Zone[dat.ab.total$Zone %in% "Special Purpose Wildlife"] <- "Fished"
dat.ab.total$Zone[dat.ab.total$Zone %in% "Sanctuary"] <- "Protected"
dat.ab.total$Zone[dat.ab.total$Zone %in% "Special Purpose Scientific"] <- "Protected"
dat.ab.total$Zone[dat.ab.total$Zone %in% "Fished"] <- "General Use"
dat.ab.total$Zone <- as.factor(dat.ab.total$Zone)

#Add 0 values to dataset for transects where fish species weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site ~ GenusSpecies, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site")))

#Subset dataset to highly targeted fish species at Shoalwater and some together so that they represent 'total target fish' per replicate

dat.ab.target <- subset(dat.ab.total2, GenusSpecies %in% c("Arripis georgianus", "Epinephelides armatus", "Sillaginodes punctatus", "Pagrus auratus", "Sillago vittata", "Sillago schomburgkii", "Sillago bassensis", "Pseudocaranx dentex", "Pseudocaranx spp", "Glaucosoma hebraicum", "Choerodon rubescens"))
dat.ab.target2 <- subset(dat.ab.target,Site %in% c("Becher Point SZ 1", "First Rock 1", "Murray Reef Control 2", "Murray Reef Special Purpose 1", "Passage Rock 1", "Penguin Island 2", "Peron Control 2", "Peron SZ 1", "Seal Island SZ 2", "Second Rock SZ 1", "Shoalwater Bay 1B"))

#Remove single large school from analysis
dat.ab.target2[5,4] = 0

dat.ab.target3 <-ddply(dat.ab.target2, .(Year, Zone, Site), summarise,
                       total = sum(value))


#Obtain mean and SE values for targeted fishes at zone level

TargetSIMP <- ddply(dat.ab.target3, .(Year, Zone), summarise,
                    N    = length(total),
                    mean = mean(total),
                    sd   = sd(total),
                    se   = sd(total) / sqrt(length(total)) )


#Create figure for all SIMP Abundance

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetSIMPFig <- ggplot(TargetSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Year") +
  ylab(expression(paste("Mean abundance (1500",m^2, "", "±SE)", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetSIMP$Year-0.25),max(TargetSIMP$Year+0.25)), breaks=min(TargetSIMP$Year):max(TargetSIMP$Year)) +
  #scale_y_continuous(limits=c(min(0),max(0.75))) +
  #ggtitle("a)") +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_rect(colour="black"),                #adds border around the graph
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=28,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.5,1), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(2,"cm"),
        legend.key.height=unit(0.6,"cm"),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
TargetSIMPFig

png(filename = "TargetSIMPFig.png",
    width = 600, height = 300, units = "px", pointsize = 6)
TargetSIMPFig
dev.off()

#Update plot on data catalogue
ckanr::resource_update("14f57359-aaf2-497d-9e50-7b307ea375d3", "TargetSIMPFig.png")

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetSIMPFig2 <- ggplot(TargetSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean abundance per 125 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetSIMP$Year-0.25),max(TargetSIMP$Year+0.25)), breaks=min(TargetSIMP$Year):max(TargetSIMP$Year)) +
  scale_y_continuous(limits=c(min(0),max(0.8))) +
  ggtitle("b)") +
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
        plot.title=element_text(hjust=1,size=28,face="bold"),
        legend.position="none")
TargetSIMPFig2

png(filename = "TargetSIMP.png",
    width = 600, height = 400, units = "px", pointsize = 6)
TargetSIMPFig
dev.off()


############################
#BIOMASS

dat.bio <- subset(dat, select=c("Year", "Zone", "Site", "GenusSpecies", "Biomass"))
dat.bio$Biomass <- as.character(dat.bio$Biomass)
dat.bio$Biomass <- as.integer(dat.bio$Biomass)

#Remove any 'NA' values from dataset

dat.bio<-dat.bio[!is.na(dat.bio$Site),]
dat.bio = droplevels(dat.bio)

#Sum abundance values for individual species within site and period

dat.bio.total <- ddply(dat.bio, .(Year, Zone, Site, GenusSpecies), summarise,
                       total = sum(Biomass))

#Convert Biomass to KG
dat.bio.total$BiomassKG <- dat.bio.total$total/1000

#ADD STEP HERE TO RENAME ZONES TO FISHED AND PROTECTED
dat.bio.total$Zone <- as.character(dat.bio.total$Zone)
dat.bio.total <- dat.bio.total[!(dat.bio.total$Zone == "" | is.na(dat.bio.total$Zone)), ]
#dat.ab.total$Zone[dat.ab.total$Zone %in% "Special Purpose Wildlife"] <- "Fished"
dat.bio.total$Zone[dat.bio.total$Zone %in% "Sanctuary"] <- "Protected"
dat.bio.total$Zone[dat.bio.total$Zone %in% "Special Purpose Scientific"] <- "Protected"
dat.bio.total$Zone[dat.bio.total$Zone %in% "Fished"] <- "General Use"
dat.bio.total$Zone <- as.factor(dat.bio.total$Zone)



#Add 0 values to dataset for transects where fish species weren't counted

dat.bio.total1 <- cast(dat.bio.total, Year + Zone + Site ~ GenusSpecies, value = "BiomassKG")
dat.bio.total1[is.na(dat.bio.total1)] = 0
dat.bio.total1 = droplevels(dat.bio.total1)
dat.bio.total2 = melt(dat.bio.total1, id.vars=(c("Year", "Zone", "Site")))

#Subset dataset to highly targeted fish species at Shoalwater and some together so that they represent 'total target fish' per replicate

dat.bio.target <- subset(dat.bio.total2, GenusSpecies %in% c("Arripis georgianus", "Epinephelides armatus", "Sillaginodes punctatus", "Pagrus auratus", "Sillago vittata", "Sillago schomburgkii", "Sillago bassensis", "Pseudocaranx dentex", "Pseudocaranx spp", "Glaucosoma hebraicum", "Choerodon rubescens"))
#dat.bio.target <- subset(dat.bio.total2, GenusSpecies %in% c("Epinephelides armatus", "Sillaginodes punctatus", "Pagrus auratus", "Sillago vittata", "Sillago schomburgkii", "Sillago bassensis", "Pseudocaranx dentex", "Pseudocaranx spp", "Glaucosoma hebraicum", "Choerodon rubescens"))
dat.bio.target2 <- subset(dat.bio.target,Site %in% c("Becher Point SZ 1", "First Rock 1", "Murray Reef Control 2", "Murray Reef Special Purpose 1", "Passage Rock 1", "Penguin Island 2", "Peron Control 2", "Peron SZ 1", "Seal Island SZ 2", "Second Rock SZ 1", "Shoalwater Bay 1B"))

#Remove single large school from analysis
dat.bio.target2[5,4] = 0

dat.bio.target3 <-ddply(dat.bio.target2, .(Year, Zone, Site), summarise,
                        total = sum(value))
#dat.bio.target3 <-ddply(dat.bio.target2, .(Year, Zone, Site, Period, GenusSpecies), summarise,
#total = sum(value))


#Obtain mean and SE values for targeted fishes at zone level

TargetBioSIMP <- ddply(dat.bio.target3, .(Year, Zone), summarise,
                       N    = length(total),
                       mean = mean(total),
                       sd   = sd(total),
                       se   = sd(total) / sqrt(length(total)) )


#Create figure for all SIMP Abundance

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetBioSIMPFig <- ggplot(TargetBioSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Year") +
  ylab(expression(paste("Mean biomass (kg.1500",m^2, "", "±SE)", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetBioSIMP$Year-0.25),max(TargetBioSIMP$Year+0.25)), breaks=min(TargetBioSIMP$Year):max(TargetBioSIMP$Year)) +
  #scale_y_continuous(limits=c(min(0),max(0.75))) +
  #ggtitle("a)") +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_rect(colour="black"),                #adds the border around graph
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=28,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(2,"cm"),
        legend.key.height=unit(0.6,"cm"),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))
TargetBioSIMPFig

png(filename = "TargetBioSIMPFig.png",
    width = 600, height = 300, units = "px", pointsize = 6)
TargetBioSIMPFig
dev.off()

#Update plot on data catalogue
ckanr::resource_update("3ba5bde4-681e-4636-94cd-37206c093efd", "TargetBioSIMPFig.png")


pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
TargetBioSIMPFig2 <- ggplot(TargetBioSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Mean abundance per 125 ", m^2, "", " +/- SE", sep = ""))) +
  scale_x_continuous(limits=c(min(TargetBioSIMP$Year-0.25),max(TargetBioSIMP$Year+0.25)), breaks=min(TargetBioSIMP$Year):max(TargetBioSIMP$Year)) +
  scale_y_continuous(limits=c(min(0),max(50))) +
  ggtitle("b)") +
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
        plot.title=element_text(hjust=1,size=28,face="bold"),
        legend.position="none")
TargetBioSIMPFig2

png(filename = "TargetBioSIMP.png",
    width = 600, height = 300, units = "px", pointsize = 6)
TargetBioSIMPFig
dev.off()
