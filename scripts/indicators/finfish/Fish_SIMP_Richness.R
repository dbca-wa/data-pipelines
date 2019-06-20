#setwd("~/projects/data-pipelines/scripts/indicators/finfish")
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape)
library(mgcv)

#Load CSV

#dat <- read.csv("SIMP_DOV_All.csv")

# or from the data catalogue
data_rid <- "c80224c2-8ee6-49ae-aef4-4fabace94ac6"
dat <- ckanr::resource_show(data_rid)$url %>% readr::read_csv(.)

#Check column names

colnames (dat)

#Limit to those columns that are important for this analysis

#dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "Period", "GenusSpecies", "Family", "Number"))
dat.ab <- subset(dat, select=c("Year", "Zone", "Site", "GenusSpecies", "Family", "Number"))
dat.ab$Number <- as.character(dat.ab$Number)
dat.ab$Number <- as.integer(dat.ab$Number)

#Remove any 'NA' values from dataset

#dat.ab <- dat.ab[!is.na(dat.ab$Period),]
dat.ab <- dat.ab[!is.na(dat.ab$Site),]
dat.ab = droplevels(dat.ab)

#Remove cryptic families and sharks/rays from dataset

dat.ab.filter <- subset(dat.ab, Family != c("Apogonidae", "Blennidae", "Gobiidae"))
dat.ab.filter3 <- subset(dat.ab.filter, Family != c("Holocentridae"))
dat.ab.filter4 <- subset(dat.ab.filter3, Family != c("Plotosidae"))
dat.ab.filter5 <- subset(dat.ab.filter4, Family != c("Carcharhinidae"))
dat.ab.filter6 <- subset(dat.ab.filter5, Family != c("Dasyatidae"))
dat.ab.filter7 <- subset(dat.ab.filter6, Family != c("Ginglymostomatidae"))
dat.ab.filter8 <- subset(dat.ab.filter7, Family != c("Gobiesocidae"))
dat.ab.filter9 <- subset(dat.ab.filter8, Family != c("Hemiscylliidae"))
dat.ab.filter10 <- subset(dat.ab.filter9, Family != c("Muraenidae"))
dat.ab.filter11 <- subset(dat.ab.filter10, Family != c("Myliobatidae"))
dat.ab.filter12 <- subset(dat.ab.filter11, Family != c("Pleuronectidae"))
dat.ab.filter13 <- subset(dat.ab.filter12, Family != c("Stegostomatidae"))
dat.ab.filter14 <- subset(dat.ab.filter13, Family != c("Pseudochromidae"))


#Sum abundance values for individual species within site and period

dat.ab.total <- ddply(dat.ab.filter14, .(Year, Zone, Site, GenusSpecies), summarise,
                      total = sum(Number))

#ADD STEP HERE TO RENAME ZONES TO FISHED AND PROTECTED
dat.ab.total$Zone <- as.character(dat.ab.total$Zone)
dat.ab.total <- dat.ab.total[!(dat.ab.total$Zone == "" | is.na(dat.ab.total$Zone)), ]
dat.ab.total$Zone[dat.ab.total$Zone %in% "Sanctuary"] <- "Protected"
dat.ab.total$Zone[dat.ab.total$Zone %in% "Special Purpose Scientific"] <- "Protected"
dat.ab.total$Zone[dat.ab.total$Zone %in% "Fished"] <- "General Use"
dat.ab.total$Zone <- as.factor(dat.ab.total$Zone)

#Remove 0 values and sum unique Genus.species names to provide a Species Richness score for every transect

dat.ab.limitsites <-subset(dat.ab.total, Site %in% c("Becher Point SZ 1", "First Rock 1", "Murray Reef Control 2", "Murray Reef Special Purpose 1", "Passage Rock 1", "Penguin Island 2", "Peron Control 2", "Peron SZ 1", "Seal Island SZ 2", "Second Rock SZ 1", "Shoalwater Bay 1B"))
dat.ab.removezero <- subset(dat.ab.limitsites, total > 0)
dat.ab.richness <- ddply(dat.ab.removezero, .(Year, Zone, Site), summarise,
                         richness = length(unique(GenusSpecies)))

#Add 0 values to dataset for transects where fish species weren't counted

dat.ab.total1 <- cast(dat.ab.richness, Year + Zone ~ Site, value = "richness")
dat.ab.total1[is.na(dat.ab.total1)] = 0
dat.ab.total1 = droplevels(dat.ab.total1)
dat.ab.total2 = melt(dat.ab.total1, id.vars=(c("Year", "Zone")))

#Obtain mean and SE values for species Richness at zone level

RichnessSIMP <- ddply(dat.ab.total2, .(Year, Zone), summarise,
                      N    = length(value),
                      mean = mean(value),
                      sd   = sd(value),
                      se   = sd(value) / sqrt(length(value)) )

#Create figure for all SIMP

pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)
RichnessSIMPFig <- ggplot(RichnessSIMP, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  #geom_smooth(method = "gam", formula = y ~ s(x,k=3), se=F, size = 0.5,col="black") +
  #geom_smooth(method = "lm", formula = y ~ x, se=F, size = 0.5,col="black") +#geom_line(position=pd) +                      # line
  geom_point(position=pd, size=4) +             # points
  #stat_smooth(method = "lm", colour = "black", se = FALSE) +
  xlab("Year") +
  ylab(expression(paste("Mean species richness (1500", m^2, "", "± SE)", sep = ""))) +
  scale_x_continuous(limits=c(min(RichnessSIMP$Year-0.25),max(RichnessSIMP$Year+0.25)), breaks=min(RichnessSIMP$Year):max(RichnessSIMP$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=14),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=16,face="bold", vjust=-1),                #removes x axis title
        axis.title.y=element_text(size=14,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_rect(colour="black"),                #adds border around graph
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c(0.52,1), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.key.width=unit(2,"cm"),
        legend.key.height=unit(0.6,"cm"),
        legend.text=element_text(size=18),
        legend.background=element_rect(size=0.5, linetype="solid", colour="black"))

summary(RichnessSIMPFig)

png(filename = "RichnessSIMP.png",
    width = 600, height = 300, units = "px", pointsize = 6)
RichnessSIMPFig
dev.off()

#Update plot on data catalogue
ckanr::resource_update("0422d319-5b93-4406-83f6-26b5e402c77c", "RichnessSIMP.png")

dat.gam.zone.GU <- subset(dat.ab.total2, Zone %in% c("General Use"))
dat.gam.zone.protected <- subset(dat.ab.total2, Zone %in% c("Protected"))
dat.gam.zone.SPW <- subset(dat.ab.total2, Zone %in% c("Special Purpose Wildlife"))
gam.GU.richness <- gam(data=dat.gam.zone.GU, formula=Year ~ s(value,k=3), bs="ts")
gam.protected.richness <- gam(data=dat.gam.zone.protected, formula=Year ~ s(value,k=3), bs="ts")
gam.SPW.richness <- gam(data=dat.gam.zone.SPW, formula=Year ~ s(value,k=3), bs="ts")

summary(gam.GU.richness)
summary(gam.protected.richness)
summary(gam.SPW.richness)
