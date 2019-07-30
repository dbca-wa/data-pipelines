source("~/projects/data-pipelines/setup/ckan.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
## Settings
## Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/coral/coral_cover")


# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "e6446677-aff3-4910-aab4-04772ca787a3"
csv_rid_md <- "11d90582-53e3-4495-8014-b96a021630cf"
csv_rid_hist <- "50cb3b52-0aa9-42bc-b120-bb80feaf17e3"
# pdf_rid <- ""
pdf_rid_sector2 <- "f332a5e1-19a1-42e0-882f-d100169044ee"
txt_rid <- "7bf45f88-104c-42a4-b506-bc09d76d3695"

pdf_fn_sector2 <- "coral_cover_by_sector.pdf"
txt_fn <- "code.R"
txt_rid <- "7bf45f88-104c-42a4-b506-bc09d76d3695"

#------------------------------------------------------------------------------#
## Analysis - your code
d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))
d_md <- load_ckan_csv(csv_rid_md) # your data as data.frame
d_hist <- load_ckan_csv(csv_rid_hist, date_colnames = c('date', 'Date'))

# pd <- position_dodge(0.1)
# out <- ggplot(d, aes(x=Year, y=Mean1)) +
#   geom_point(aes(x=Year, y=Mean1, group=Site, shape=Site, col=Site), position=pd, size=2) +
#   geom_line(aes(x=Year, y=Mean1, group=Site, shape=Site, col=Site), position=pd) +
#   labs(title='Y axis metric name', x='Time', y='') +
#   scale_x_datetime(
#     labels=scales::date_format('%Y-%m'),
#     #limits=c(as.POSIXct('2000-11-28 08:00:00'), as.POSIXct('2015-06-18 08:00:00')),
#     breaks='1 year', minor_breaks='6 months') +
#   theme(
#     axis.text.x = element_text(size=10, angle=0),
#     axis.text.y = element_text(size=10),
#     axis.title.x = element_text(size=10),
#     axis.title.y = element_text(size=10),
#     legend.title = element_text(size=10),
#     legend.text = element_text(size=10),
#     legend.position = 'right'
#   )

####Notes####
#If you have data that has the wrong year, site, transect, etc; you MUST fix this in the Fix problems section. This means if the data is re-exported in the future it doesn't cause problems due to no changes in the data. These scripts are made to work on data straight from export!
#Update graph end year to match newest data on rows 206, 277, 338, 400, 461, 546, 603, 662. Make sure when extra code is updated that these row numbers are too!!
####Create LVL1 dataframe from raw EcoPAAS data####
nmpcoral=d

library(plyr) #needed for count

d$Year <- as.factor(d$Year)
#####Fix problems in original EcoPAAS data#########
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-BRZ-B5-B5A-2013124103206"] <- 2012
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-BRZ-B5-B5B-2013124104234"] <- 2012
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-BRZ-B5-B5C-2013124105143"] <- 2012
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6A-2013124123718"] <- 2012
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6B-2013124124621"] <- 2012
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6C-2013124125656"] <- 2012
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-TSZ-N1-N1A-20091219143521"] <- 2010
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-TSZ-N1-N1B-20091219145035"] <- 2010
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-TSZ-N1-N1C-20091219150516"] <- 2010
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6A-201517141422"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6B-201517142154"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-BUN-MSZ-B6-B6C-201517143154"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-MUSZ-N2-N2A-201516123326"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-MUSZ-N2-N2B-201516124543"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-MUSZ-N2-N2C-201516125635"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-OSZ-N3-N3A-201516143440"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-OSZ-N3-N3B-201516144509"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-OSZ-N3-N3C-201516145437"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6A-2015111122755"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6B-2015111121604"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-WRZ-N6-N6C-2015111120340"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5A-2015111135822"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5B-2015111140952"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-NTH-WSZ-N5-N5C-2015111142018"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7A-201519085254"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7B-201519090027"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7C-201519090851"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-STH-CFSZ-S5-S5A-201519131117"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-STH-CFSZ-S5-S5B-201519132131"] <- 2014
nmpcoral$Year[nmpcoral$Survey == "NIN-STH-CFSZ-S5-S5C-201519133042"] <- 2014
nmpcoral$Replicate[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7C-2010126093153"] <- 'S7B'
nmpcoral$ReplicateCode[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7C-2010126093153"] <- 'NIN-STH-3MSZ-S7-S7B'
levels(nmpcoral$Survey) = c(levels(nmpcoral$Survey), "NIN-STH-3MSZ-S7-S7B-2010126093153")
nmpcoral$Survey[nmpcoral$Survey == "NIN-STH-3MSZ-S7-S7C-2010126093153"] <-'NIN-STH-3MSZ-S7-S7B-2010126093153'

#write.csv(nmpcoral, "C:/temp/nmpcoral.csv")

####################################

nmpcoral_raw=count(nmpcoral, c("Survey", "Level2Class")) #counts number of observations of LVL1 per site, per year
nmpsite=count(nmpcoral_raw, c("Survey"), "freq") #counts number of observsations made at each site per year
nmpLVL2_replicate <- join(nmpcoral_raw, nmpsite, by = "Survey") #adds count of site observations agains the right site/year to allow percentage calculation
names(nmpLVL2_replicate)[3] <- "LVL2_Count" #Rename column to make more sense
names(nmpLVL2_replicate)[4] <- "Replicate_Count" #Rename column to make more sense
########Leave out for now######## nmpLVL2_replicate$percentcover = nmpLVL2_replicate$LVL2_Count/nmpLVL2_replicate$Replicate_Count *100 #Calculate percent cover
nmpsector=nmpcoral[,c("Zone", "Sector", "Site", "Replicate", "Survey", "Year")] #Create a list of sectors, zones and sites
nmpsectors = unique(nmpsector) #Makes the list unique values only
nmpLVL2 = join(nmpLVL2_replicate, nmpsectors, by = "Survey") #adds the site, sector and zone information to percent information
##End##

##Create Coral data.frame for Coral cover plot across entire NMP and to be separated into Sector and Zone##
nmpLVL2coral = subset(nmpLVL2, Level2Class %in% c("Hard coral","Octocorals - Hard")) #Extracts coral cover imformation only


nmpLVL2corala = join (nmpsectors, nmpLVL2coral, by = "Survey")
nmphardcoral = nmpLVL2corala[,c("Zone", "Sector", "Site", "Replicate", "Survey", "Year", "Level2Class",  "LVL2_Count")]
nmphardcoral = join (nmphardcoral, nmpsite, by = "Survey")
names(nmphardcoral)[9] <- "Replicate_Count"
nmphardcoral$LVL2_Count[is.na(nmphardcoral$LVL2_Count)] <- 0
nmphardcoral$Level2Class[is.na(nmphardcoral$Level2Class)] <- 'Hard coral'
#nmphardcoral$percentcover = nmphardcoral$LVL2_Count/nmphardcoral$Replicate_Count *100 #Calculate percent cover

# write.csv(nmpcoral, file = "nmpcoral_rawcheck.csv") #So non-EcoPAAS data can be appended, now automated in next row
Old_nmpLVL2coral=d_hist #read.table("L:/benthic/!results/NIN/R/preEcoPAASdataNINT.csv", sep=",", header=TRUE) #Reads in .csv of non-EcoPAAS data (format must be correct)

###############
# names(nmpLVL2coral)[2] <- "Level2Class" #Rename column to make more sense
# nmpLVL2coral$ReplicateY = paste(nmpLVL2coral$Replicate, nmpLVL2coral$Year, '')
nmphardcoralb = tapply(nmphardcoral$LVL2_Count,nmphardcoral$Survey,sum)
Coral=data.frame(nmphardcoralb)
Coral$Survey=rownames(Coral)
nmphardcorals=join(nmphardcoral,Coral,"Survey")
# nmphardcorals$ReplicateY = NULL
nmphardcorals$LVL2_Count = NULL
# nmphardcorals$percentcover = NULL
names(nmphardcorals)[9] = "LVL2_Count"
nmphardcorald=subset(nmphardcoral, Level2Class == "Hard coral")
nmphardcorald$percentcover = nmphardcorald$LVL2_Count/nmphardcorald$Replicate_Count *100
################
nmpcoralcover_all=rbind(nmphardcorald, Old_nmpLVL2coral)#joins EcoPAAS data and other data togetherinto one data.frame
delete<-data.frame(unique(nmpcoralcover_all$Site))
nmpmetadata2=d_md #read.table("L:/benthic/!results/NIN/R/nmp_metadata_2.csv",sep=",",header=TRUE,)
nmpcoralcover_all1 = join (nmpmetadata2, nmpcoralcover_all, by = "Site")
##End##


##Create Sector data.frame for Sector plots##
nmpLVL1coral_allMurion = subset(nmpcoralcover_all, Site %in% c("North Muiron 1", "North Muiron 2", "North Muiron 3", "Muiron Island North", "Muiron Island South"))
nmpLVL1coral_allBundegi = subset(nmpcoralcover_all, Site %in% c("Bundegi 1", "Bundegi 2", "Bundegi 3","BUNDEGI", "Bundegi", "N1 Bundegi", "N19 Bundegi Sanctuary", "Bundegi North", "Bundegi South", "Bundegi SZ North", "Bundegi SZ South", "Murat"))
nmpLVL1coral_allNorth = subset(nmpcoralcover_all, Sector %in% c("Ningaloo-North", "North"))
nmpLVL1coral_allSouth = subset(nmpcoralcover_all, Sector %in% c("Ningaloo-South", "South", "South "))
##End##

##Create Zone data.frame for Zone plots##
nmpLVL1coral_allSanctuary = subset(nmpcoralcover_all, Zone %in% c("Sanctuary", "Sanctuary Zone - Bundegi", "Sanctuary Zone - Cape Farquhar", "Sanctuary Zone - Cloates", "Sanctuary Zone - Gnarraloo Bay", "Sanctuary Zone - Mandu", "Sanctuary Zone - Maud", "Sanctuary Zone - Murat", "Sanctuary Zone - Osprey", "Sanctuary Zone - Pelican", "Sanctuary Zone - Tantabiddi", "Sanctuary Zone - Three Mile", "Sanctuary Zone - Turtles", "Sanctuary Zone - Winderabandi"))
nmpLVL1coral_allRecreation = subset(nmpcoralcover_all, Zone %in% c("Recreation", "Recreation Zone - Bundegi", "Recreation Zone - Cloates", "Recreation Zone - Winderabandi"))
nmpLVL1coral_allConservation = subset(nmpcoralcover_all, Zone %in% c("Conservation Area", "Conservation Area - Muiron Islands North", "Management Area - Muiron Islands"))
##End##



#################################################################
###################All Coral Cover Plots#########################
#################################################################
##Code from here is used to creat all plots##
####Sector facet wrap####

nmpcoralcover_mean=rbind(nmpLVL1coral_allMurion, nmpLVL1coral_allBundegi, nmpLVL1coral_allNorth, nmpLVL1coral_allSouth)

nmpcoralcover_mean$Sector[nmpcoralcover_mean$Sector == "Ningaloo-North"] <-"North"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Sector == "Ningaloo-South"] <-"South"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Sector == "South "] <-"South"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Site == "Bundegi SZ South"] <-"Bundegi"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Site == "Bundegi SZ North"] <-"Bundegi"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Site == "Bundegi"] <-"Bundegi"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Site == "Murat"] <-"Bundegi"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Site == "Muiron Island South"] <-"Muiron"
nmpcoralcover_mean$Sector[nmpcoralcover_mean$Site == "Muiron Island North"] <-"Muiron"
nmpcoralcover_mean<- droplevels(nmpcoralcover_mean)

nmpcoralcover_means <- ddply(nmpcoralcover_mean, .(Year, Sector), .inform=TRUE, summarise,
                             N    = length(Sector),
                             Mean = mean(percentcover),
                             sd   = sd(percentcover),
                             SE   = sd(percentcover) / sqrt(length(Sector)))

Ning<-ggplot(nmpcoralcover_means, aes(x = Year, y = Mean)) +
  geom_point(size=3) +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), se=FALSE, size = 1,col="black",linetype="dashed") +
  geom_smooth(method = "lm", se=TRUE, size = 1.5,colour="black",fill="grey20") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
  geom_point(colour="black",size=3) +
  facet_wrap(~ Sector, ncol=1) +
  theme_bw() + theme(axis.title.x = element_text(size = 20, vjust = .3),axis.line = element_line(colour = "black"),
                     axis.title.y = element_text(size = 20, vjust = .3,angle=90),axis.line = element_line(colour = "black"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank())+
  scale_y_continuous(limits=c(0,60)) +
  scale_x_continuous(limits=c(1990,2015))+
  #scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +
  xlab("Year") + ylab("Mean coral cover % (+/- SE)")
Ning


#################################################################
###################All Coral Cover Plots#########################
#################################################################
##Code from here was borrowed from George Shedrawi rather than recreate the wheel##

#######New Facet wrap and models for new plots NOT WORKING PROP ########

NMPCORCOVZN=ddply(nmpcoralcover_all1, .(Sector2, Year), summarise,
                  N    = length(percentcover),
                  mean = mean(percentcover),
                  sd   = sd(percentcover),
                  se   = sd(percentcover) / sqrt(length((percentcover)) ))

#windows(7,5)

PlotNMPCORCOVZN=ggplot(NMPCORCOVZN, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.025) + # error bars
  geom_smooth(method = "lm", se=FALSE, size = 1,colour="black",fill="white") +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), se=FALSE, size = 1,col="black",linetype="dashed") +
  #geom_line() +                                                # line
  geom_point(size=2) +                 # points
  xlab("") +
  ylab("Mean coral cover % (+/- SE)") +
  facet_wrap(~ Sector2) +                  #this is what puts them in a grid
  scale_y_continuous(limits=c(0,80)) +
  #scale_x_continuous(limits=c(min(NMPCORCOVZN$Year-0.125), max(NMPCORCOVZN$Year+0.125)), breaks=min(NMPCORCOVZN$Year):max(NMPCORCOVZN$Year)) +
  scale_x_continuous(breaks=c(1990, 1995, 2000, 2005, 2010, 2015), limits=c(1990,2015))+
  #scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12"), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  #scale_x_discrete(limits=c("1","2","3","4","5","6","7","8"), labels=c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov")) +
  theme_bw() +                                    #grid colour in this case black and white
  theme(
    strip.text.x = element_text(size=11),           #sets the size of the title to each grid section as 10
    strip.background = element_blank(),             #removes the grey background in the title of each grid section
    axis.text.x = theme_text(size=10, angle=45, hjust=1),               #sets the x axis tick text size to 8
    axis.text.y = theme_text(size=10),               #sets the y axis tick text size to 8
    axis.title.x=theme_blank(),                     #removes the x axis title
    axis.title.y=theme_text(size=11),               #sets the y axis title to size 10
    axis.line=theme_segment(colour="black"),        #sets the axis lines
    panel.grid.minor = theme_blank(),               #removes minor grid lines
    panel.grid.major = theme_blank(),               #removes major grid lines
    panel.border=theme_blank(),                     #removes the border around the graph
    panel.background=theme_blank(),                 #removes the grey background of the plot
    legend.justification=c(1,1), legend.position=c(1,0.3), # Position legend in top right
    legend.title = theme_blank(),                   #removes the legend title
    legend.key = theme_blank())                     #removes the legend key
PlotNMPCORCOVZN

################

library(ggplot2)
library(gridExtra)
library(plyr)

##Create function that calculates standard errors##
stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
Mean=tapply(nmpcoralcover_all$percentcover,nmpcoralcover_all$Year, mean) #group coral into means by year
SE=tapply(nmpcoralcover_all$percentcover,nmpcoralcover_all$Year, stderr) #Calculate standard errors for coral by year
D=data.frame(Mean,SE) #create dataframe so ggplot can read data
D$year=rownames(D) #change year into a variable
rownames(D)=NULL #get rid of rownames

##plot out means with SE for coral cover by year##
# Ning_means=ggplot(D, aes(x=D$year, y=D$Mean,)) +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
#   geom_line() +
#   geom_point(colour="black",size=3) +
#   theme_bw() + theme(axis.title.x = element_text(size = 20, vjust = .3),axis.line = element_line(colour = "black"),
#                      axis.title.y = element_text(size = 20, vjust = .3,angle=90),axis.line = element_line(colour = "black"),
#                      panel.grid.major = element_blank(),
#                      panel.grid.minor = element_blank(),
#                      panel.border = element_blank(),
#                      panel.background = element_blank())+
#   xlab("") + ylab("Mean coral cover (+/- SE)")
# Ning_means

##300 dpi outputs for Journal##
#tiff("NingMeans.tiff", width = 8, height = 6, units = 'in', res = 300)
#Ning_means
#dev.off()

#---------------------------------------------------------------------------------------------------------------------------------------------
################################## Exercise 7 - Run a linear model through means  ##############################################################
Intyear=as.integer(D$year)
Ning_Mod1<-ggplot(D, aes(x = Intyear, y = D$Mean)) +
  geom_point(size=3) +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), se=FALSE, size = 1,col="black",linetype="dashed") +
  geom_smooth(method = "lm", se=TRUE, size = 1.5,colour="black",fill="grey20") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
  geom_point(colour="black",size=3) +
  theme_bw() + theme(axis.title.x = element_text(size = 20, vjust = .3),axis.line = element_line(colour = "black"),
                     axis.title.y = element_text(size = 20, vjust = .3,angle=90),axis.line = element_line(colour = "black"),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     panel.background = element_blank())+
  scale_y_continuous(limits=c(0,60)) +
  scale_x_continuous(limits=c(1990,2014))+
  #scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +
  xlab("Year") + ylab("Mean coral cover % (+/- SE)")
Ning_Mod1



Allparklm<- lm(D$Mean~Intyear)
summary(Allparklm)

fm1.poly.D <- lm(D$Mean ~ poly(Intyear, 4))
summary(fm1.poly.D)

##300 dpi outputs for Journal##
#tiff("NingMod1.tiff", width = 8), height = 6, units = 'in', res = 300)
#Ning_Mod1
#dev.off()

###############################Sector Coral Plots###################################
library(ggplot2)
library(gridExtra)
library(plyr)

##Create function that calculates standard errors##
stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
Mean=tapply(nmpLVL1coral_allBundegi$percentcover,nmpLVL1coral_allBundegi$Year, mean) #group coral into means by year
SE=tapply(nmpLVL1coral_allBundegi$percentcover,nmpLVL1coral_allBundegi$Year, stderr) #Calculate standard errors for coral by year
DSB=data.frame(Mean,SE) #create dataframe so ggplot can read data
DSB$year=rownames(DSB) #change year into a variable
rownames(DSB)=NULL #get rid of rownames

# #plot out means with SE for coral cover by year##
# Ning_means=ggplot(DSB, aes(x=DSB$year, y=DSB$Mean,)) +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
#   geom_line() +
#   geom_point(colour="black",size=3) +
#   theme_bw() + opts(axis.title.x = theme_text(size = 20, vjust = .3),axis.line = theme_segment(colour = "black"),
#                     axis.title.y = theme_text(size = 20, vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
#                     panel.grid.major = theme_blank(),
#                     panel.grid.minor = theme_blank(),
#                     panel.border = theme_blank(),
#                     panel.background = theme_blank())+
#   xlab("") + ylab("Mean coral cover (+/- SE)")
# Ning_means
#
# #300 dpi outputs for Journal##
# #tiff("NingMeans.tiff", width = 8, height = 6, units = 'in', res = 300)
# # Ning_means
# dev.off()

#---------------------------------------------------------------------------------------------------------------------------------------------#
## A linear model through means  ##
IntyearDSB=as.integer(DSB$year)
Ning_ModDSB<-ggplot(DSB, aes(x = IntyearDSB, y = DSB$Mean)) +
  geom_point(size=3) +
  stat_smooth(method = "lm", formula = y ~ poly(x,4), se=FALSE, size = 1,col="black",linetype="dashed") +
  geom_smooth(method = "lm", se=FALSE, size = 1.5,colour="black",fill="grey20") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
  geom_point(colour="black",size=3) +
  ggtitle("Bundegi") +
  xlab("Year") + ylab("Mean coral cover % (+/- SE)")+
  theme_bw() + opts(#axis.title.x = theme_text(vjust = .3),
    axis.line = theme_segment(colour = "black"),
    #axis.title.y = theme_text(vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
    axis.title.y = theme_blank(),
    axis.title.x = theme_blank(),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.border = theme_blank(),
    panel.background = theme_blank())+
  scale_y_continuous(limits=c(0,80)) +
  scale_x_continuous(limits=c(1990,2014))
#scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +

Ning_ModDSB

RegDSB<- lm(DSB$Mean~IntyearDSB)
summary(RegDSB)

fm1.poly.DSB <- lm(DSB$Mean ~ poly(IntyearDSB, 3))
summary(fm1.poly.DSB)
#---------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------#



##Create function that calculates standard errors##
stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
Mean=tapply(nmpLVL1coral_allMurion$percentcover,nmpLVL1coral_allMurion$Year, mean) #group coral into means by year
SE=tapply(nmpLVL1coral_allMurion$percentcover,nmpLVL1coral_allMurion$Year, stderr) #Calculate standard errors for coral by year
DSM=data.frame(Mean,SE) #create dataframe so ggplot can read data
DSM$year=rownames(DSM) #change year into a variable
rownames(DSM)=NULL #get rid of rownames

##plot out means with SE for coral cover by year##
# Ning_means=ggplot(DSM, aes(x=DSM$year, y=DSM$Mean,)) +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
#   geom_line() +
#   geom_point(colour="black",size=3) +
#   theme_bw() + opts(axis.title.x = theme_text(size = 20, vjust = .3),axis.line = theme_segment(colour = "black"),
#                     axis.title.y = theme_text(size = 20, vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
#                     panel.grid.major = theme_blank(),
#                     panel.grid.minor = theme_blank(),
#                     panel.border = theme_blank(),
#                     panel.background = theme_blank())+
#   xlab("") + ylab("Mean coral cover (+/- SE)")
# Ning_means

##300 dpi outputs for Journal##
#tiff("NingMeans.tiff", width = 8, height = 6, units = 'in', res = 300)
#Ning_means
#dev.off()

#---------------------------------------------------------------------------------------------------------------------------------------------#
## A linear model through means  ##
IntyearDSM=as.integer(DSM$year)
Ning_ModDSM<-ggplot(DSM, aes(x = IntyearDSM, y = DSM$Mean)) +
  geom_point(size=3) +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), se=FALSE, size = 1,col="black",linetype="dashed") +
  geom_smooth(method = "lm", se=FALSE, size = 1.5,colour="black",fill="grey20") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
  geom_point(colour="black",size=3) +
  ggtitle("Muiron") +
  theme_bw() + opts(#axis.title.x = theme_text(vjust = .3),axis.line = theme_segment(colour = "black"), #size=20
    axis.title.y = theme_text(vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
    #axis.title.y = theme_blank(),
    axis.title.x = theme_blank(),
    panel.grid.major = theme_blank(),
    panel.grid.minor = theme_blank(),
    panel.border = theme_blank(),
    panel.background = theme_blank())+
  scale_y_continuous(limits=c(0,80)) +
  scale_x_continuous(limits=c(1990,2015))+ # breaks=2005:2015,
  #scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +
  xlab("Year") + ylab("Mean coral cover % (+/- SE)")
Ning_ModDSM

RegDSM<- lm(DSM$Mean~IntyearDSM)
summary(RegDSM)

fm1.poly.DSM <- lm(DSM$Mean ~ poly(IntyearDSM, 3))
summary(fm1.poly.DSM)

#---------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------#


##Create function that calculates standard errors##
stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
Mean=tapply(nmpLVL1coral_allNorth$percentcover,nmpLVL1coral_allNorth$Year, mean) #group coral into means by year
SE=tapply(nmpLVL1coral_allNorth$percentcover,nmpLVL1coral_allNorth$Year, stderr) #Calculate standard errors for coral by year
DSN=data.frame(Mean,SE) #create dataframe so ggplot can read data
DSN$year=rownames(DSN) #change year into a variable
rownames(DSN)=NULL #get rid of rownames

##plot out means with SE for coral cover by year##
# Ning_means=ggplot(DSN, aes(x=DSN$year, y=DSN$Mean,)) +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
#   geom_line() +
#   geom_point(colour="black",size=3) +
#   theme_bw() + opts(axis.title.x = theme_text(size = 20, vjust = .3),axis.line = theme_segment(colour = "black"),
#                     axis.title.y = theme_text(size = 20, vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
#                     panel.grid.major = theme_blank(),
#                     panel.grid.minor = theme_blank(),
#                     panel.border = theme_blank(),
#                     panel.background = theme_blank())+
#   xlab("") + ylab("Mean coral cover (+/- SE)")
# Ning_means

##300 dpi outputs for Journal##
#tiff("NingMeans.tiff", width = 8, height = 6, units = 'in', res = 300)
#Ning_means
#dev.off()

#---------------------------------------------------------------------------------------------------------------------------------------------#
## A linear model through means  ##
IntyearDSN=as.integer(DSN$year)
Ning_ModDSN<-ggplot(DSN, aes(x = IntyearDSN, y = DSN$Mean)) +
  geom_point(size=3) +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), se=FALSE, size = 1,col="black",linetype="dashed") +
  geom_smooth(method = "lm", se=FALSE, size = 1.5,colour="black",fill="grey20") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
  geom_point(colour="black",size=3) +
  ggtitle("North") +
  theme_bw() + opts(axis.title.x = theme_text(vjust = .3),
                    axis.line = theme_segment(colour = "black"),
                    axis.title.y = theme_text(vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
                    #axis.title.y = theme_blank(),
                    #axis.title.x = theme_blank(),
                    panel.grid.major = theme_blank(),
                    panel.grid.minor = theme_blank(),
                    panel.border = theme_blank(),
                    panel.background = theme_blank())+
  scale_y_continuous(limits=c(0,80)) +
  scale_x_continuous(limits=c(1990,2014))+
  #scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +
  xlab("Year") + ylab("Mean coral cover % (+/- SE)")
Ning_ModDSN

RegDSN<- lm(DSN$Mean~IntyearDSN)
summary(RegDSN)

fm1.poly.DSN <- lm(DSN$Mean ~ poly(IntyearDSN, 3))
summary(fm1.poly.DSN)

#---------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------#


##Create function that calculates standard errors##
stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
Mean=tapply(nmpLVL1coral_allSouth$percentcover,nmpLVL1coral_allSouth$Year, mean) #group coral into means by year
SE=tapply(nmpLVL1coral_allSouth$percentcover,nmpLVL1coral_allSouth$Year, stderr) #Calculate standard errors for coral by year
DSS=data.frame(Mean,SE) #create dataframe so ggplot can read data
DSS$year=rownames(DSS) #change year into a variable
rownames(DSS)=NULL #get rid of rownames

##plot out means with SE for coral cover by year##
# Ning_means=ggplot(DSS, aes(x=DSS$year, y=DSS$Mean,)) +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
#   geom_line() +
#   geom_point(colour="black",size=3) +
#   theme_bw() + opts(axis.title.x = theme_text(size = 20, vjust = .3),axis.line = theme_segment(colour = "black"),
#                     axis.title.y = theme_text(size = 20, vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
#                     panel.grid.major = theme_blank(),
#                     panel.grid.minor = theme_blank(),
#                     panel.border = theme_blank(),
#                     panel.background = theme_blank())+
#   xlab("") + ylab("Mean coral cover (+/- SE)")
# Ning_means

##300 dpi outputs for Journal##
#tiff("NingMeans.tiff", width = 8, height = 6, units = 'in', res = 300)
#Ning_means
#dev.off()

#---------------------------------------------------------------------------------------------------------------------------------------------#
## A linear model through means  ##
IntyearDSS=as.integer(DSS$year)
Ning_ModDSS<-ggplot(DSS, aes(x = IntyearDSS, y = DSS$Mean)) +
  geom_point(size=3) +
  stat_smooth(method = "lm", formula = y ~ poly(x,3), se=FALSE, size = 1,col="black",linetype="dashed") +
  geom_smooth(method = "lm", se=FALSE, size = 1.5,colour="black",fill="grey20") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
  geom_point(colour="black",size=3) +
  ggtitle("South") +
  theme_bw() + opts(axis.title.x = theme_text(vjust = .3),axis.line = theme_segment(colour = "black"),
                    #axis.title.y = theme_text(vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
                    axis.title.y = theme_blank(),
                    #axis.title.x = theme_blank(),
                    panel.grid.major = theme_blank(),
                    panel.grid.minor = theme_blank(),
                    panel.border = theme_blank(),
                    panel.background = theme_blank())+
  scale_y_continuous(limits=c(0,80)) +
  scale_x_continuous(limits=c(1990,2014))+
  #scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +
  xlab("Year") + ylab("Mean coral cover % (+/- SE)")
Ning_ModDSS

RegDSS<- lm(DSS$Mean~IntyearDSS)
summary(RegDSS)

fm1.poly.DSS <- lm(DSS$Mean ~ poly(IntyearDSS, 3))
summary(fm1.poly.DSS)

#-----------------------------------------------------------------------------------------------------------------------------------#

library(gridExtra)  #package used to make panel plots

##Create a panel plot using previous 3 plots
Panelplot<-grid.arrange(Ning_ModDSM, Ning_ModDSB, Ning_ModDSN, Ning_ModDSS)
#Panelplot<-grid.arrange(PlotBundegi, PlotCloates, PlotCoralBay, PlotLighthouse, PlotMandu, PlotMangrove, PlotMuiron, PlotOsprey, PlotPelican, PlotTantabiddi)

#-----------------------------------------------------------------------------------------------------------------------------------#


# DSS<- lm(D$Mean~Intyear)
# summary(DSS)
# # plot(D$Mean~Intyear,pch=19)
# # abline(Reg1)
#
# ##  Polynomial Regression - 4 knots  ##
# Ning_ModDSS <- lm(D$Mean ~ poly(Intyear, 4))
# summary(Ning_ModDSS)
# xy<-1990:2012
# yz<-predict(Ning_ModDSS,list(Intyear=xy))
# lines(xy, yz)

#################################Zone Coral Plots####################################
# library(ggplot2)
# library(gridExtra)
# library(plyr)
# ##Create function that calculates standard errors##
# stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
# Mean=tapply(nmpLVL1coral_allRecreation$percentcover,nmpLVL1coral_allRecreation$Year, mean) #group coral into means by year
# SE=tapply(nmpLVL1coral_allRecreation$percentcover,nmpLVL1coral_allRecreation$Year, stderr) #Calculate standard errors for coral by year
# RZ=data.frame(Mean,SE) #create dataframe so ggplot can read data
# RZ$year=rownames(RZ) #change year into a variable
# rownames(RZ)=NULL #get rid of rownames
#
# ##plot out means with SE for coral cover by year##
# # Ning_means=ggplot(RZ, aes(x=RZ$year, y=RZ$Mean,)) +
# #   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
# #   geom_line() +
# #   geom_point(colour="black",size=3) +
# #   theme_bw() + opts(axis.title.x = theme_text(size = 20, vjust = .3),axis.line = theme_segment(colour = "black"),
# #                     axis.title.y = theme_text(size = 20, vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
# #                     panel.grid.major = theme_blank(),
# #                     panel.grid.minor = theme_blank(),
# #                     panel.border = theme_blank(),
# #                     panel.background = theme_blank())+
# #   xlab("") + ylab("Mean coral cover (+/- SE)")
# # Ning_means
#
# ##300 dpi outputs for Journal##
# #tiff("NingMeans.tiff", width = 8, height = 6, units = 'in', res = 300)
# #Ning_means
# #dev.off()
#
# #---------------------------------------------------------------------------------------------------------------------------------------------#
# ## A linear model through means  ##
# IntyearRZ=as.integer(RZ$year)
# Ning_ModRZ<-ggplot(RZ, aes(x = IntyearRZ, y = RZ$Mean)) +
#   geom_point(size=3) +
#   stat_smooth(method = "lm", formula = y ~ poly(x,4), se=FALSE, size = 1,col="black",linetype="dashed") +
#   geom_smooth(method = "lm", se=FALSE, size = 1.5,colour="black",fill="grey20") +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
#   geom_point(colour="black",size=3) +
#   ggtitle("Recreation Zone") +
#   theme_bw() + opts(#axis.title.x = theme_text(size = 20, vjust = .3),
#     axis.line = theme_segment(colour = "black"),
#     #axis.title.y = theme_text(size = 20, vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
#     axis.title.y = theme_blank(),
#     axis.title.x = theme_blank(),
#     panel.grid.major = theme_blank(),
#     panel.grid.minor = theme_blank(),
#     panel.border = theme_blank(),
#     panel.background = theme_blank())+
#   scale_y_continuous(limits=c(0,80)) +
#   scale_x_continuous(limits=c(1990,2014))+
#   #scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +
#   xlab("Year") + ylab("Mean coral cover % (+/- SE)")
# Ning_ModRZ
#
# #---------------------------------------------------------------------------------------------------------------------------------------------#
# #---------------------------------------------------------------------------------------------------------------------------------------------#
#
#
# ##Create function that calculates standard errors##
# stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
# Mean=tapply(nmpLVL1coral_allSanctuary$percentcover,nmpLVL1coral_allSanctuary$Year, mean) #group coral into means by year
# SE=tapply(nmpLVL1coral_allSanctuary$percentcover,nmpLVL1coral_allSanctuary$Year, stderr) #Calculate standard errors for coral by year
# SZ=data.frame(Mean,SE) #create dataframe so ggplot can read data
# SZ$year=rownames(SZ) #change year into a variable
# rownames(SZ)=NULL #get rid of rownames
#
# ##plot out means with SE for coral cover by year##
# # Ning_means=ggplot(SZ, aes(x=SZ$year, y=SZ$Mean,)) +
# #   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
# #   geom_line() +
# #   geom_point(colour="black",size=3) +
# #   theme_bw() + opts(axis.title.x = theme_text(size = 20, vjust = .3),axis.line = theme_segment(colour = "black"),
# #                     axis.title.y = theme_text(size = 20, vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
# #                     panel.grid.major = theme_blank(),
# #                     panel.grid.minor = theme_blank(),
# #                     panel.border = theme_blank(),
# #                     panel.background = theme_blank())+
# #   xlab("") + ylab("Mean coral cover (+/- SE)")
# # Ning_means
#
# ##300 dpi outputs for Journal##
# #tiff("NingMeans.tiff", width = 8, height = 6, units = 'in', res = 300)
# #Ning_means
# #dev.off()
#
# #---------------------------------------------------------------------------------------------------------------------------------------------#
# ## A linear model through means  ##
# IntyearSZ=as.integer(SZ$year)
# Ning_ModSZ<-ggplot(SZ, aes(x = IntyearSZ, y = SZ$Mean)) +
#   geom_point(size=3) +
#   stat_smooth(method = "lm", formula = y ~ poly(x,3), se=FALSE, size = 1,col="black",linetype="dashed") +
#   geom_smooth(method = "lm", se=FALSE, size = 1.5,colour="black",fill="grey20") +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
#   geom_point(colour="black",size=3) +
#   ggtitle("Sanctuary Zone") +
#   theme_bw() + opts(#axis.title.x = theme_text(size = 20, vjust = .3),
#     axis.line = theme_segment(colour = "black"),
#     axis.title.y = theme_text(size = 13, vjust = 0.1,angle=90),
#     #axis.title.y = theme_blank(),
#     axis.title.x = theme_blank(),
#     axis.line = theme_segment(colour = "black"),
#     panel.grid.major = theme_blank(),
#     panel.grid.minor = theme_blank(),
#     panel.border = theme_blank(),
#     panel.background = theme_blank())+
#   scale_y_continuous(limits=c(0,80)) +
#   scale_x_continuous(limits=c(1990,2014))+
#   #scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +
#   xlab("Year") + ylab("Mean coral cover % (+/- SE)")
# Ning_ModSZ
#
#
# #---------------------------------------------------------------------------------------------------------------------------------------------#
# #---------------------------------------------------------------------------------------------------------------------------------------------#
#
#
#
# ##Create function that calculates standard errors##
# stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
# Mean=tapply(nmpLVL1coral_allConservation$percentcover,nmpLVL1coral_allConservation$Year, mean) #group coral into means by year
# SE=tapply(nmpLVL1coral_allConservation$percentcover,nmpLVL1coral_allConservation$Year, stderr) #Calculate standard errors for coral by year
# CZ=data.frame(Mean,SE) #create dataframe so ggplot can read data
# CZ$year=rownames(CZ) #change year into a variable
# rownames(CZ)=NULL #get rid of rownames
#
# ##plot out means with SE for coral cover by year##
# # Ning_means=ggplot(CZ, aes(x=CZ$year, y=CZ$Mean,)) +
# #   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
# #   geom_line() +
# #   geom_point(colour="black",size=3) +
# #   theme_bw() + opts(axis.title.x = theme_text(size = 20, vjust = .3),axis.line = theme_segment(colour = "black"),
# #                     axis.title.y = theme_text(size = 20, vjust = .3,angle=90),axis.line = theme_segment(colour = "black"),
# #                     panel.grid.major = theme_blank(),
# #                     panel.grid.minor = theme_blank(),
# #                     panel.border = theme_blank(),
# #                     panel.background = theme_blank())+
# #   xlab("") + ylab("Mean coral cover (+/- SE)")
# # Ning_means
#
# ##300 dpi outputs for Journal##
# #tiff("NingMeans.tiff", width = 8, height = 6, units = 'in', res = 300)
# #Ning_means
# #dev.off()
#
# #---------------------------------------------------------------------------------------------------------------------------------------------#
# ## A linear model through means  ##
# IntyearCZ=as.integer(CZ$year)
# Ning_ModCZ<-ggplot(CZ, aes(x = IntyearCZ, y = CZ$Mean)) +
#   geom_point(size=3) +
#   stat_smooth(method = "lm", formula = y ~ poly(x,3), se=FALSE, size = 1,col="black",linetype="dashed") +
#   geom_smooth(method = "lm", se=FALSE, size = 1.5,colour="black",fill="grey20") +
#   geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.1,colour="black") +
#   geom_point(colour="black",size=3) +
#   ggtitle("Muiron Islands Conservation Area") +
#   theme_bw() + opts(axis.title.x = theme_text(size = 20, vjust = .3),
#                     axis.line = theme_segment(colour = "black"),
#                     #axis.title.y = theme_text(size = 20, vjust = .3,angle=90),
#                     axis.title.y = theme_blank(),
#                     #axis.title.x = theme_blank(),
#                     axis.line = theme_segment(colour = "black"),
#                     panel.grid.major = theme_blank(),
#                     panel.grid.minor = theme_blank(),
#                     panel.border = theme_blank(),
#                     panel.background = theme_blank())+
#   scale_y_continuous(limits=c(0,80)) +
#   scale_x_continuous(limits=c(1990,2014))+
#   #scale_x_continuous limits=c(min(D.Means$year-0.125), max(D.Means$year+0.125)), breaks=min(D.Means$year):max(D.Means$year))  +
#   xlab("Year") + ylab("Mean coral cover % (+/- SE)")
# Ning_ModCZ
#
#
# #-----------------------------------------------------------------------------------------------------------------------------------#
#
# library(gridExtra)  #package used to make panel plots
#
# ##Create a panel plot using previous 3 plots
#
# Panelplot<-grid.arrange(Ning_ModRZ, Ning_ModSZ, Ning_ModCZ)
#Panelplot<-grid.arrange(PlotBundegi, PlotCloates, PlotCoralBay, PlotLighthouse, PlotMandu, PlotMangrove, PlotMuiron, PlotOsprey, PlotPelican, PlotTantabiddi)

#-----------------------------------------------------------------------------------------------------------------------------------#


CZ<- lm(D$Mean~Intyear)
summary(CZ)
plot(D$Mean~Intyear,pch=19)
abline(CZ)

##  Polynomial Regression - 3 knots  ##
Ning_ModCZ <- lm(D$Mean ~ poly(Intyear, 3))
summary(Ning_ModCZ)
xy<-1990:2012
yz<-predict(Ning_ModCZ,list(Intyear=xy))
lines(xy, yz)
###

# out

#------------------------------------------------------------------------------#
## Save outputs and upload to CKAN, restore workdir
# pdf(pdf_fn_sector2, height = 10, width = 7); out; dev.off()

# ckanr::resource_update(pdf_resource_id, pdf_fn)

# ckanr::resource_update(pdf_rid_sector2, pdf_fn_sector2)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")
