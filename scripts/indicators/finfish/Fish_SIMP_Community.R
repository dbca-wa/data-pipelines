#setwd("~/projects/data-pipelines/scripts/indicators/finfish")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)
library(mgcv)
library(MASS)
library(MuMIn)

#Load CSV

#dat <- read.csv("SIMP_DOV_All.csv")

# or from the data catalogue
data_rid <- "c80224c2-8ee6-49ae-aef4-4fabace94ac6"
dat <- ckanr::resource_show(data_rid)$url %>% readr::read_csv(.)

#Check column names

colnames (dat)

##############################
#ABUNDANCE

#Remove cryptic families and sharks/rays from dataset

dat.filter <- subset(dat, Family != c("Apogonidae"))
dat.filter1 <- subset(dat.filter, Family != c("Blennidae"))
dat.filter2 <- subset(dat.filter1, Family != c("Gobiidae"))
dat.filter3 <- subset(dat.filter2, Family != c("Holocentridae"))
dat.filter4 <- subset(dat.filter3, Family != c("Plotosidae"))
dat.filter5 <- subset(dat.filter4, Family != c("Carcharhinidae"))
dat.filter6 <- subset(dat.filter5, Family != c("Dasyatidae"))
dat.filter7 <- subset(dat.filter6, Family != c("Ginglymostomatidae"))
dat.filter8 <- subset(dat.filter7, Family != c("Gobiesocidae"))
dat.filter9 <- subset(dat.filter8, Family != c("Hemiscylliidae"))
dat.filter10 <- subset(dat.filter9, Family != c("Muraenidae"))
dat.filter11 <- subset(dat.filter10, Family != c("Myliobatidae"))
dat.filter12 <- subset(dat.filter11, Family != c("Pleuronectidae"))
dat.filter13 <- subset(dat.filter12, Family != c("Stegostomatidae"))
dat.filter14 <- subset(dat.filter13, Family != c("Pseudochromidae"))
dat.filter15 <- subset(dat.filter14, Family != c("Heterodontidae"))

#Limit to those columns that are important for this analysis

dat.ab <- subset(dat.filter15, select=c("Year", "Zone", "Site", "FeedingGuild", "Number"))
dat.ab.limitsites <- subset(dat.ab, Site %in% c("Becher Point SZ 1", "First Rock 1", "Murray Reef Control 2", "Murray Reef Special Purpose 1", "Passage Rock 1", "Penguin Island 2", "Peron Control 2", "Peron SZ 1", "Seal Island SZ 2", "Second Rock SZ 1", "Shoalwater Bay 1B"))
dat.ab.limitsites$Number <- as.character(dat.ab.limitsites$Number)
dat.ab.limitsites$Number <- as.integer(dat.ab.limitsites$Number)
dat.ab.limitsites <- dat.ab.limitsites[!is.na(dat.ab.limitsites$Number),]

#Remove any 'NA' values from dataset

dat.ab.limitsites <- dat.ab.limitsites[!is.na(dat.ab.limitsites$Site),]
dat.ab.limitsites = droplevels(dat.ab.limitsites)

#ADD STEP HERE TO RENAME SCRAPER/EXCAVATOR AND MOBILE HERBIVORE AS LARGE HERBIVORE
dat.ab.limitsites$FeedingGuild <- as.character(dat.ab.limitsites$FeedingGuild)
dat.ab.limitsites <- dat.ab.limitsites[!(dat.ab.limitsites$FeedingGuild == "" | is.na(dat.ab.limitsites$FeedingGuild)), ]
dat.ab.limitsites$FeedingGuild[dat.ab.limitsites$FeedingGuild %in% "Large Cropper"] <- "Large Herbivore"
dat.ab.limitsites$FeedingGuild[dat.ab.limitsites$FeedingGuild %in% "Scraper/excavator"] <- "Large Herbivore"
dat.ab.limitsites$FeedingGuild <- as.factor(dat.ab.limitsites$FeedingGuild)

#RENAME ZONES TO FISHED AND PROTECTED
dat.ab.limitsites$Zone <- as.character(dat.ab.limitsites$Zone)
dat.ab.limitsites <- dat.ab.limitsites[!(dat.ab.limitsites$Zone == "" | is.na(dat.ab.limitsites$Zone)), ]
dat.ab.limitsites$Zone[dat.ab.limitsites$Zone %in% "Sanctuary"] <- "Protected"
dat.ab.limitsites$Zone[dat.ab.limitsites$Zone %in% "Special Purpose Scientific"] <- "Protected"
dat.ab.limitsites$Zone[dat.ab.limitsites$Zone %in% "Fished"] <- "General Use"
dat.ab.limitsites$Zone <- as.factor(dat.ab.limitsites$Zone)

#Sum abundance values for trophic groups within site and period

dat.ab.total <- ddply(dat.ab.limitsites, .(Year, Zone, Site, FeedingGuild), summarise,
                      total = sum(Number))

#Add 0 values to dataset for transects where trophic groups weren't counted

dat.ab.total1 <- cast(dat.ab.total, Year + Zone + Site ~ FeedingGuild, value = "total")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone", "Site")))

#Subset dataset to trophic groups of interest (Piscivore, Large Herbivore, Mobiel Invertivore)at SIMP and limit to those sites and years where we have a continuous time series

dat.ab.pisc <- subset(dat.ab.total2, FeedingGuild %in% c("Piscivore"))

dat.ab.herb <- subset(dat.ab.total2, FeedingGuild %in% c("Large Herbivore"))

dat.ab.invert <- subset(dat.ab.total2, FeedingGuild %in% c("Mobile Invertivore"))

#Obtain mean and SE values for Piscivore, Large Herbivore and Mobile Invertivore and create figures for all SIMP

PiscSIMP <- ddply(dat.ab.pisc, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
PiscSIMPFig <- ggplot(PiscSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "lm", formula = y ~ x, se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 125 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Piscivores") +
  scale_x_continuous(limits=c(min(PiscSIMP$Year-0.25),max(PiscSIMP$Year+0.25)), breaks=min(PiscSIMP$Year):max(PiscSIMP$Year)) +
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
        plot.title=element_text(hjust=1,size=18,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(2,"cm"),
        legend.key.height=unit(0.6,"cm"),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

PiscSIMPFig
#####################################

HerbSIMP <- ddply(dat.ab.herb, .(Year, Zone), summarise,
                  N    = length(value),
                  mean = mean(value),
                  sd   = sd(value),
                  se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
HerbSIMPFig <- ggplot(HerbSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "lm", formula = y ~ x, se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 125 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Large Herbivores") +
  scale_x_continuous(limits=c(min(HerbSIMP$Year-0.25),max(HerbSIMP$Year+0.25)), breaks=min(HerbSIMP$Year):max(HerbSIMP$Year)) +
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
        plot.title=element_text(hjust=1,size=18,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbSIMPFig
#####################################

InvertSIMP <- ddply(dat.ab.invert, .(Year, Zone), summarise,
                    N    = length(value),
                    mean = mean(value),
                    sd   = sd(value),
                    se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
InvertSIMPFig <- ggplot(InvertSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "lm", formula = y ~ x, se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Survey Year") +
  ylab(expression(paste("Abundance per 125 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mobile Invertivores") +
  scale_x_continuous(limits=c(min(InvertSIMP$Year-0.25),max(InvertSIMP$Year+0.25)), breaks=min(InvertSIMP$Year):max(InvertSIMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=16,face="bold", colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=18,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

InvertSIMPFig
#####################################

#Create Panel Plot of all trophic groups

png(filename = "CommunityAbSIMP.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(PiscSIMPFig, HerbSIMPFig, InvertSIMPFig)
dev.off()

##############################
#BIOMASS

#Remove the row with 500 herring in line with target plot
dat.filter15 = dat.filter15[-c(28),]
#
dat.bio <- subset(dat.filter15, select=c("Year", "Zone", "Site", "FeedingGuild", "Biomass"))
dat.bio.limitsites <- subset(dat.bio, Site %in% c("Becher Point SZ 1", "First Rock 1", "Murray Reef Control 2", "Murray Reef Special Purpose 1", "Passage Rock 1", "Penguin Island 2", "Peron Control 2", "Peron SZ 1", "Seal Island SZ 2", "Second Rock SZ 1", "Shoalwater Bay 1B"))
dat.bio.limitsites$Biomass <- as.character(dat.bio.limitsites$Biomass)
dat.bio.limitsites$Biomass <- as.integer(dat.bio.limitsites$Biomass)
dat.bio.limitsites <- dat.bio.limitsites[!is.na(dat.bio.limitsites$Biomass),]

#Remove any 'NA' values from dataset

dat.bio.limitsites <- dat.bio.limitsites[!is.na(dat.bio.limitsites$Site),]
dat.bio.limitsites = droplevels(dat.bio.limitsites)

#RENAME SCRAPER/EXCAVATOR AND MOBILE HERBIVORE AS LARGE HERBIVORE
dat.bio.limitsites$FeedingGuild <- as.character(dat.bio.limitsites$FeedingGuild)
dat.bio.limitsites <- dat.bio.limitsites[!(dat.bio.limitsites$FeedingGuild == "" | is.na(dat.bio.limitsites$FeedingGuild)), ]
dat.bio.limitsites$FeedingGuild[dat.bio.limitsites$FeedingGuild %in% "Large Cropper"] <- "Large Herbivore"
dat.bio.limitsites$FeedingGuild[dat.bio.limitsites$FeedingGuild %in% "Scraper/excavator"] <- "Large Herbivore"
dat.bio.limitsites$FeedingGuild <- as.factor(dat.bio.limitsites$FeedingGuild)

#RENAME ZONES TO FISHED AND PROTECTED
dat.bio.limitsites$Zone <- as.character(dat.bio.limitsites$Zone)
dat.bio.limitsites <- dat.bio.limitsites[!(dat.bio.limitsites$Zone == "" | is.na(dat.bio.limitsites$Zone)), ]
dat.bio.limitsites$Zone[dat.bio.limitsites$Zone %in% "Sanctuary"] <- "Protected"
dat.bio.limitsites$Zone[dat.bio.limitsites$Zone %in% "Special Purpose Scientific"] <- "Protected"
dat.bio.limitsites$Zone[dat.bio.limitsites$Zone %in% "Fished"] <- "General Use"
dat.bio.limitsites$Zone <- as.factor(dat.bio.limitsites$Zone)

#Sum abundance values for trophic groups within site and period

dat.bio.total <- ddply(dat.bio.limitsites, .(Year, Zone, Site, FeedingGuild), summarise,
                       total = sum(Biomass))

#Convert Biomass to KG
dat.bio.total$BiomassKG <- dat.bio.total$total/1000

#Add 0 values to dataset for transects where trophic groups weren't counted

dat.bio.total1 <- cast(dat.bio.total, Year + Zone + Site ~ FeedingGuild, value = "BiomassKG")
dat.bio.total1[is.na(dat.bio.total1)] = 0
dat.bio.total1 = droplevels(dat.bio.total1)
#dat.bio.total2 = melt(dat.bio.total1, id.vars=(c("Year", "Zone", "Site", "Period")))
dat.bio.total2 = melt(dat.bio.total1, id.vars=(c("Year", "Zone", "Site")))

#Subset dataset to trophic groups of interest (Piscivore, Large Herbivore, Mobile Invertivore) at SIMP and limit to those sites and years where we have a continuous time series

dat.bio.pisc <- subset(dat.bio.total2, FeedingGuild %in% c("Piscivore"))

dat.bio.herb <- subset(dat.bio.total2, FeedingGuild %in% c("Large Herbivore"))

dat.bio.invert <- subset(dat.bio.total2, FeedingGuild %in% c("Mobile Invertivore"))

#Obtain mean and SE values for Piscivore, Large Herbivore and Mobile Invertivore and create figures for all SIMP

PiscBioSIMP <- ddply(dat.bio.pisc, .(Year, Zone), summarise,
                     N    = length(value),
                     mean = mean(value),
                     sd   = sd(value),
                     se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
PiscBioSIMPFig <- ggplot(PiscBioSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "lm", formula = y ~ x, se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Year") +
  ylab(expression(paste("Biomass (g) per 1500 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Piscivores") +
  scale_x_continuous(limits=c(min(PiscBioSIMP$Year-0.25),max(PiscBioSIMP$Year+0.25)), breaks=min(PiscBioSIMP$Year):max(PiscBioSIMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=14,face="bold", colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_rect(colour="black"),                #adds border around graph
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=18,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.8,1), # Positions legend (x,y)
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(2,"cm"),
        legend.key.height=unit(0.6,"cm"),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

PiscBioSIMPFig
#####################################

HerbBioSIMP <- ddply(dat.bio.herb, .(Year, Zone), summarise,
                     N    = length(value),
                     mean = mean(value),
                     sd   = sd(value),
                     se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
HerbBioSIMPFig <- ggplot(HerbBioSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "lm", formula = y ~ x, se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Year") +
  ylab(expression(paste("Mean biomass (kg.1500",m^2, "", "±SE)", sep = ""))) +
  ggtitle("Large Herbivores") +
  scale_x_continuous(limits=c(min(HerbBioSIMP$Year-0.25),max(HerbBioSIMP$Year+0.25)), breaks=min(HerbBioSIMP$Year):max(HerbBioSIMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=15,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_rect(colour="black"),                #adds border around graph
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=18,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

HerbBioSIMPFig
#####################################

InvertBioSIMP <- ddply(dat.bio.invert, .(Year, Zone), summarise,
                       N    = length(value),
                       mean = mean(value),
                       sd   = sd(value),
                       se   = sd(value) / sqrt(length(value)) )

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
InvertBioSIMPFig <- ggplot(InvertBioSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "lm", formula = y ~ x, se=F, size = 0.5,col="black") +
  #geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Year") +
  ylab(expression(paste("Biomass (g) per 1500",m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Mobile Invertivores") +
  scale_x_continuous(limits=c(min(InvertBioSIMP$Year-0.25),max(InvertBioSIMP$Year+0.25)), breaks=min(InvertBioSIMP$Year):max(InvertBioSIMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=12),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=14,face="bold", colour="white"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_rect(colour="black"),                #adds border arond graph
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=18,face="bold"),
        legend.position="none",
        #legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

InvertBioSIMPFig
#####################################

#Create Panel Plot of all trophic groups

png(filename = "CommunityBioSIMP.png",
    width = 600, height = 700, units = "px", pointsize = 6)
grid.arrange(PiscBioSIMPFig, HerbBioSIMPFig, InvertBioSIMPFig)
dev.off()

#Update plot on data catalogue
ckanr::resource_update("4d758eb8-6608-4110-b077-d4dca0290a8a", "CommunityBioSIMP.png")

dat.gam.pisc.GU <- subset(dat.bio.pisc, Zone %in% c("General Use"))
dat.gam.pisc.protected <- subset(dat.bio.pisc, Zone %in% c("Protected"))
dat.gam.pisc.SPW <- subset(dat.bio.pisc, Zone %in% c("Special Purpose Wildlife"))
dat.gam.herb.GU <- subset(dat.bio.herb, Zone %in% c("General Use"))
dat.gam.herb.protected <- subset(dat.bio.herb, Zone %in% c("Protected"))
dat.gam.herb.SPW <- subset(dat.bio.herb, Zone %in% c("Special Purpose Wildlife"))
dat.gam.invert.GU <- subset(dat.bio.invert, Zone %in% c("General Use"))
dat.gam.invert.protected <- subset(dat.bio.invert, Zone %in% c("Protected"))
dat.gam.invert.SPW <- subset(dat.bio.invert, Zone %in% c("Special Purpose Wildlife"))
gam.pisc.GU <- gam(data=dat.gam.pisc.GU, formula=Year ~ s(value,k=3), bs="ts")
gam.pisc.protected <- gam(data=dat.gam.pisc.protected, formula=Year ~ s(value,k=3), bs="ts")
gam.pisc.SPW <- gam(data=dat.gam.pisc.SPW, formula=Year ~ s(value,k=3), bs="ts")
gam.herb.GU <- gam(data=dat.gam.herb.GU, formula=Year ~ s(value,k=3), bs="ts")
gam.herb.protected <- gam(data=dat.gam.herb.protected, formula=Year ~ s(value,k=3), bs="ts")
gam.herb.SPW <- gam(data=dat.gam.herb.SPW, formula=Year ~ s(value,k=3), bs="ts")
gam.invert.GU <- gam(data=dat.gam.invert.GU, formula=Year ~ s(value,k=3), bs="ts")
gam.invert.protected <- gam(data=dat.gam.invert.protected, formula=Year ~ s(value,k=3), bs="ts")
gam.invert.SPW <- gam(data=dat.gam.invert.SPW, formula=Year ~ s(value,k=3), bs="ts")

summary(gam.pisc.GU)
summary(gam.pisc.protected)
summary(gam.pisc.SPW)
summary(gam.herb.GU)
summary(gam.herb.protected)
summary(gam.herb.SPW)
summary(gam.invert.GU)
summary(gam.invert.protected)
summary(gam.invert.SPW)
