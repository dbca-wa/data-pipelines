#This script is design to created an automated annual summary report for seagrass monitoring sites -biomass cores only
#Created by Molly Moustaka June 2019
#Dept. Biodiversity, Conservation and Attractions, Marine Science Program
#Email: molly.moustaka@dbbca.wa.gov.au

###-----Get set up----####
#Clear environment
rm(list=ls())

#Install & Load packages
#install.packages(c("ggplot2", "devtools", "dplyr","tidyr", "plotrix","gridExtra","RColorBrewer","gplots", "ggpubr"),dependencies = T)
#install.packages("devtools")
library(ggplot2) # make plots
library(dplyr) #data wrangling
library(plotrix) #package to calculate standard error
library(gridExtra) #grid layouts for multiple plots
library(RColorBrewer) #colours for plots.
library(gplots) #text to plot tools
library(ggpubr)#another arranging tool
filter = dplyr::filter #make sure R uses the DPLYR version of filter

#I'm also playing with the new tools in tidyverse for reshaping data which aren't formally in the package yet
#devtools::install_github("tidyverse/tidyr") #select 3 in first option menu
library(tidyr)

#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Seagrass")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/")
data=paste(work.dir,"Data",sep="/")

setwd(data)

#View files in the directory
dir()

#### OPTIONAL: Pull seagrass biomass mastersheet off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "7315491d-8d83-45b0-9211-426538538d85"
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)

###-----Explore the data----####

#Load data
dat<-read.csv("DMP_Seagrass_Biomass_AllData.csv", header=T) #read in the data in CSV form, noting that the top row is column labels

#Check out the data
head(dat) #show me the top six rows
names(dat) #show me the column names
str(dat) #show me the data structure
unique(dat$Species) #tell me which species I have present in the dataset

#These column names are too long, lets correct them
dat<- dat%>%
  rename(Total=Total.Biomass.dry.weight..g.,
         Above=Above.ground.dry.weight..g.,
         Below=Below.Ground.dry.weight..g.) #NOTE: New column name on the left, old on right
names(dat)

#Something strange is going on with the header for the Year column so lets fix that
names(dat)[1]<-"Year" #just another method of doing what I did above for simple changes

#Lets also make Replicate into a factor to simplify later data wrangling
dat$Replicate<-as.factor(dat$Replicate)

#We also want to get rid of spaces in species names
dat$Species <- gsub(" ", "_", dat$Species)

#and create a column for A:B (A/B) biomass
dat$ABR<-(dat$Above/dat$Below)

#Makes A:B NaN & inf = 0
dat$ABR[is.infinite(dat$ABR)]<-0
dat$ABR[is.na(dat$ABR)]<-0

###-----Get sum and mean biomass per site and standard error----####
dat2<- dat %>%
  filter(Replicate!="")%>% #remove rows where a sample wasn't taken
  group_by(Year,SiteName) %>% #you can do this whatever level you want (location,site, transect, just add/remove levels of resolution)
  summarise(
    sumTotal=sum(Total),
    sumAbove=sum(Above),
    sumBelow=sum(Below),
    meanTotal=mean(Total),
    meanAbove=mean(Above),
    meanBelow=mean(Below),
    meanABR=mean(ABR),
    sdTotal=sd(Total),
    sdAbove=sd(Above),
    sdBelow=sd(Below),
    sdABR=sd(ABR),
    Obs=n_distinct(Replicate), #this creates a column containing how many cores we took at each site
    seTotal=(sdTotal)/(sqrt(Obs)), #this calcs standard error
    seAbove=(sdAbove)/(sqrt(Obs)),
    seBelow=(sdBelow)/(sqrt(Obs)),
    seABR=(sdABR)/(sqrt(Obs)))


###-----Get sum and mean biomass per site and standard error for each species ----####
#Another way to calculate standard error is using a function which is below
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

#Now we reshape the data and add our summary variables & get rid of unused columns

dat3<- dat%>%
  filter(Replicate!="")%>%  #Remove rows from the data where there is no replicate number - i.e. we couldn't take a core (NOT that the core was empty of seagrass)
  tidyr::pivot_wider(              #The next few lines make the data in to wide format.NOTE this uses the devTools version of TidyR not on CRAN yet. See install above
    names_from=Species,     #split column 'Species'
    values_from= c(Total, Above, Below, ABR), #A seperate one of these columns for every species
    values_fill=list(Total=0,Above=0, Below=0, ABR=0))%>%  #make NA = 0
  group_by(Year,SiteName,Replicate)%>% #these next lines merge the replicates into one row (if you look at the data with and without running them it makes sense)
  summarise_if(is.numeric ,sum) %>%
  select(-c(Above_, Below_, Total_,ABR_,Transect, Shoot.Count, Fruit,Flowers,Seeds))%>%  #remove empty columns made from samples that contained no seagrass but still keeping the rows, also removing coumns we don't need
  group_by(Year,SiteName)%>%
  summarise_if(is.numeric,funs(mean,st.err)) #gives us the mean and SD for each variable


###-----Get sum and mean biomass for the whole marine park and standard error----####
dat4<- dat %>%
  filter(Replicate!="")%>% #remove rows where a sample wasn't taken
  filter(SiteName!="Keast")%>%
  group_by(Year, SiteName) %>%
  summarise(
    meanTotal=mean(Total),
    meanAbove=mean(Above),
    meanBelow=mean(Below),
    meanABR=mean(ABR),
    sdTotal=sd(Total),
    sdAbove=sd(Above),
    sdBelow=sd(Below),
    sdABR=sd(ABR),
    seTotal=st.err(Total), #this calcs standard error
    seAbove=st.err(Above),
    seBelow=st.err(Below),
    seABR=st.err(ABR))

#Alt method
meanNK<- dat %>%
  filter(Replicate!="")%>% #remove rows where a sample wasn't taken
  filter(SiteName!="Keast")%>%
  group_by(Year, SiteName) %>%
  summarise(
    meanTotal=mean(Total),
    meanABR=mean(ABR),
    seTotal=st.err(Total), #this calcs standard error
    seABR=st.err(ABR))%>%
  group_by(Year)%>%
  summarise(MmeanTotal=mean(meanTotal),
            MmeanABR=mean(meanABR),
            MseTotal=st.err(meanTotal), #this calcs standard error
            MseABR=st.err(meanABR))

#Alt method
meanK<- dat %>%
  filter(Replicate!="")%>% #remove rows where a sample wasn't taken
  filter(SiteName=="Keast")%>%
  group_by(Year, SiteName) %>%
  summarise(
    meanTotal=mean(Total),
    meanABR=mean(ABR),
    seTotal=st.err(Total), #this calcs standard error
    seABR=st.err(ABR))%>%
  group_by(Year)%>%
  summarise(MmeanTotal=mean(meanTotal),
            MmeanABR=mean(meanABR),
            MseTotal=st.err(meanTotal), #this calcs standard error
            MseABR=st.err(meanABR))

###-----Time for some data analysis----####
#Cores and even transect are a very small sample to use as a replicate
#So for biomass (at this point) we will restrict ourselves to Archipelago-level analysis
#I.E. differences between years with site as replicate. We will do it for total and by species

#First for total seagrass
sum1<-summary(aov(meanTotal~Year, data=dat2))
names(sum1)<-paste("ANOVA Total seagrass_DampierArchipelago")
sum2<-summary(aov(meanAbove~Year, data=dat2))
names(sum2)<-paste("ANOVA Total above ground seagrass_DampierArchipelago")
sum3<-summary(aov(meanBelow~Year, data=dat2))
names(sum3)<-paste("ANOVA Total below ground seagrass_DampierArchipelago")
sum4<-summary(aov(meanABR~Year, data=dat2))
names(sum4)<-paste("ANOVA Total A:B ground seagrass_DampierArchipelago")

TotalDampierArchipelago<-list(c(sum1,sum2,sum3,sum4)) #combine all ANOVA outputs into one list

#Now by species
sum1<-summary(aov(Total_Halophila_ovalis_mean~Year, data=dat3))
names(sum1)<-paste("ANOVA Total Halophila_ovalis_DampierArchipelago")
sum2<-summary(aov(Total_Halodule_uninervis_mean~Year, data=dat3))
names(sum2)<-paste("ANOVA Total Halodule_uninervis_DampierArchipelago")
sum3<-summary(aov(Total_Syringodium_isoetifolium_mean~Year, data=dat3))
names(sum3)<-paste("ANOVA Total Syringodium_isoetifolium_DampierArchipelago")
sum4<-summary(aov(Total_Halophila_minor_mean~Year, data=dat3))
names(sum4)<-paste("ANOVA Tota lHalophila_minor_DampierArchipelago")
sum5<-summary(aov(Total_Halophila_decipiens_mean~Year, data=dat3))
names(sum5)<-paste("ANOVA Total Halophila_decipiens_DampierArchipelago")

SpeciesDampierArchipelago<-list(c(sum1,sum2,sum3,sum4,sum5)) #combine all ANOVA outputs into one list


###-----Set up plots----####
#IMPORTANT NOTE: Lack of standard error bars in these plots DO NOT ALWAYS = LOW STANDARD ERROR
#This is because some sites only have one replicate containing a particular species therefore you cannot calc SE

#Set up the jitter so points dont overlap
jitter <- position_jitter(width = 0.2, height = 0) #this is so points don't overlap, increase values to spread out more
jitter2<- position_jitter(width = 0.1, height = 0) #this is so the A:B point doesn't overlap

#Set up the colours to be used for above/below/total biomass
cols <- c("Total"="black","Above ground"="lightgreen","Below ground"="brown", "A:B" = "pink") #change the colours you want here

#Set up loop for site names
sitenames<-unique(dat2$SiteName)

#set up a loop for species names
speciesnames<- unique(dat$Species) #get unique species
speciesnames<-speciesnames[speciesnames != ""] #remove blank for empty samples

for (i in sitenames) {

  for (s in speciesnames) { #open a species loop

    ###-----Set up plots of total biomass (not divided by species at each site) ----####
plotall<-dat2%>%
    filter(SiteName==(paste(i)))%>%
    ggplot(aes(x=Year,y=meanTotal)) +
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust=0.5, size=10), panel.border = element_rect(colour="black", fill=NA))+
    scale_x_discrete(name="Year", limits=c((min(dat2$Year)),(max(dat2$Year))))+
    coord_cartesian(ylim=c(0,2))+ #set Y axis limits
    labs(y = "Mean dry biomass per core (g)", x="Year")+
    geom_errorbar(aes(ymin=meanTotal-seTotal,ymax=meanTotal+seTotal),width=NA, position=jitter, colour="light grey")+
    geom_point(position=jitter, aes(colour="Total"), size=2)+
    geom_errorbar(aes(ymin=meanAbove-seAbove,ymax=meanAbove+seAbove),width=NA, colour="light grey")+
    geom_point(aes(y=meanAbove,colour="Above ground"),size=2)+
    geom_errorbar(aes(ymin=meanBelow-seBelow,ymax=meanBelow+seBelow),width=NA, colour="light grey")+
    geom_point(aes(y=meanBelow,colour="Below ground"),size=2)+
    geom_errorbar(aes(ymin=meanABR-seABR,ymax=meanABR+seABR),width=NA, position=jitter2, colour="light grey")+
    geom_point(aes(y=meanABR,colour="A:B"),size=2, position=jitter2)+
    scale_colour_manual(name="", values=c(cols))+
    ggtitle("Total biomass")


    ###-----Set up plots of total biomass by species ----####
#this is some wrangeling to select columns in the loop
total=paste("Total",s,"mean", sep="_")
totalse=paste("Total",s,"st.err", sep="_")
above=paste("Above",s,"mean", sep="_")
abovese=paste("Above",s,"st.err", sep="_")
below=paste("Below",s,"mean", sep="_")
belowse=paste("Below",s,"st.err", sep="_")
abr=paste("ABR",s,"mean", sep="_")
abrse=paste("ABR",s,"st.err", sep="_")

plot1<-dat3%>%
  filter(SiteName==(paste(i)))%>%
  rename(
    "total"=total,
    "totalse"=totalse,
    "above"=above,
    "abovese"=abovese,
    "below"=below,
    "belowse"=belowse,
    "abr"=abr,
    "abrse"=abrse)  %>%
  ggplot(aes(x=Year,y=total))+
    theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5,size=10), panel.border = element_rect(colour="black", fill=NA))+
  scale_x_discrete(name="Year", limits=c((min(dat2$Year)),(max(dat2$Year))))+
  coord_cartesian(ylim=c(0,2))+
  labs(y = "Mean dry biomass per core (g)", x="Year")+
  geom_point(position=jitter, aes(colour="Total"), size=2)+
  geom_errorbar(aes(ymin=total-totalse,ymax=total+totalse),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter, aes(colour="Total"), size=2)+
  geom_errorbar(aes(ymin=above-abovese,ymax=above+abovese),width=NA, colour="light grey")+
  geom_point(aes(y=above,colour="Above ground"),size=2)+
  geom_errorbar(aes(ymin=below-belowse,ymax=below+belowse),width=NA, colour="light grey")+
  geom_point(aes(y=below,colour="Below ground"),size=2)+
  scale_colour_manual(name="", values=c(cols))+
  geom_errorbar(aes(ymin=abr-abrse,ymax=abr+abrse),width=NA, position=jitter2, colour="light grey")+
  geom_point(aes(y=abr,colour="A:B"),size=2, position=jitter2)+
  ggtitle(paste(s),)
print(plot1)

pltName<-paste("bm", s, sep= "") #name the plot
assign(pltName, plot1)


  } #close species loop


#Print all plots for one site onto one page and
#save as an object in the environment

#Print all plots for one site onto one page
pltName<-paste("bm", i, sep= "") #name the plot
assign(pltName, (ggarrange(plotall,
                            bmHalophila_ovalis,
                            bmHalophila_decipiens,
                            bmHalophila_minor,
                            bmSyringodium_isoetifolium,
                            bmCymodocea_angustata,
                            nrow=2,ncol=3,
                            common.legend =TRUE, legend="bottom") %>%
                            annotate_figure(top=paste(i))))


} #close site loop


###-----Set up plots for for the whole Archipelago ----####
#First for total biomass
plottot<-dat2%>%
  ggplot(aes(x=Year,y=meanTotal)) +
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5, size=10), panel.border = element_rect(colour="black", fill=NA))+
  scale_x_discrete(name="Year", limits=c((min(dat2$Year)),(max(dat2$Year))))+
  coord_cartesian(ylim=c(0,2))+ #set Y axis limits
  labs(y = "Mean dry biomass per core (g)", x="Year")+
  geom_errorbar(aes(ymin=meanTotal-seTotal,ymax=meanTotal+seTotal),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName), size=2)+
  ggtitle("Total biomass")+
  scale_color_brewer(type="qual", palette = "Paired")
plottot <- plottot +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data=dat4,aes(ymin=meanTotal-seTotal,ymax=meanTotal+seTotal),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = meanTotal),
             size=4,shape="cross",colour="black",show.legend=FALSE)
plottot

#Above biomass
plotabo<-dat2%>%
  ggplot(aes(x=Year,y=meanAbove)) +
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5, size=10), panel.border = element_rect(colour="black", fill=NA))+
  scale_x_discrete(name="Year", limits=c((min(dat2$Year)),(max(dat2$Year))))+
  coord_cartesian(ylim=c(0,2))+ #set Y axis limits
  labs(y = "Mean dry biomass per core (g)", x="Year")+
  geom_errorbar(aes(ymin=meanAbove-seAbove,ymax=meanAbove+seAbove),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName), size=2)+
  ggtitle("Above ground biomass")+
  scale_color_brewer(type="qual", palette = "Paired")
plotabo <- plotabo +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data=dat4,aes(ymin=meanAbove-seAbove,ymax=meanAbove+seAbove),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = meanAbove),
             size=4,shape="cross",colour="black",show.legend=FALSE)
plotabo

#Below biomass
plotbel<-dat2%>%
  ggplot(aes(x=Year,y=meanBelow)) +
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5, size=10), panel.border = element_rect(colour="black", fill=NA))+
  scale_x_discrete(name="Year", limits=c((min(dat2$Year)),(max(dat2$Year))))+
  coord_cartesian(ylim=c(0,2))+ #set Y axis limits
  labs(y = "Mean dry biomass per core (g)", x="Year")+
  geom_errorbar(aes(ymin=meanBelow-seBelow,ymax=meanBelow+seBelow),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName), size=2)+
  ggtitle("Below ground biomass")+
  scale_color_brewer(type="qual", palette = "Paired")
plotbel <- plotbel +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data=dat4,aes(ymin=meanBelow-seBelow,ymax=meanBelow+seBelow),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = meanBelow),
             size=4,shape="cross",colour="black",show.legend=FALSE)
plotbel

#A:B ratio biomass
plotabr<-dat2%>%
  ggplot(aes(x=Year,y=meanABR)) +
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5, size=10), panel.border = element_rect(colour="black", fill=NA))+
  scale_x_discrete(name="Year", limits=c((min(dat2$Year)),(max(dat2$Year))))+
  coord_cartesian(ylim=c(0,2))+ #set Y axis limits
  labs(y = "Mean dry biomass per core (g)", x="Year")+
  geom_errorbar(aes(ymin=meanABR-seABR,ymax=meanABR+seABR),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName), size=2)+
  ggtitle("Above:Below ground biomass")+
  scale_color_brewer(type="qual", palette = "Paired")
plotabr <- plotabr +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data=dat4,aes(ymin=meanABR-seABR,ymax=meanABR+seABR),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = meanABR),
             size=4,shape="cross",colour="black",show.legend=FALSE)
plotabr

#stich them
pltName<-paste("bmDampierArchipelago") #name the plot
assign(pltName, (ggarrange(plottot,
                           plotabo,
                           plotbel,
                           plotabr,
                           nrow=2,ncol=2,
                           common.legend =TRUE, legend="bottom") %>%
                   annotate_figure(top="Dampier Archipelago")))


##And lets do the same for total biomass per species

for (s in speciesnames) { #open a species loop

#This portion of the code makes the by species plots
#this is some wrangeling to select columns in the loop
total=paste("Total",s,"mean", sep="_")
totalse=paste("Total",s,"st.err", sep="_")
above=paste("Above",s,"mean", sep="_")
abovese=paste("Above",s,"st.err", sep="_")
below=paste("Below",s,"mean", sep="_")
belowse=paste("Below",s,"st.err", sep="_")
abr=paste("ABR",s,"mean", sep="_")
abrse=paste("ABR",s,"st.err", sep="_")

plot1<-dat3%>%
  rename(
    "total"=total,
    "totalse"=totalse,
    "above"=above,
    "abovese"=abovese,
    "below"=below,
    "belowse"=belowse,
    "abr"=abr,
    "abrse"=abrse)  %>%
  ggplot(aes(x=Year,y=total))+
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5,size=10), panel.border = element_rect(colour="black", fill=NA))+
  scale_x_discrete(name="Year", limits=c((min(dat2$Year)),(max(dat2$Year))))+
  coord_cartesian(ylim=c(0,2))+
  labs(y = "Mean dry biomass per core (g)", x="Year")+
  geom_errorbar(aes(ymin=total-totalse,ymax=total+totalse),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter, aes(colour=SiteName), size=2)+
  scale_color_brewer(type="qual", palette = "Paired")+
  ggtitle(paste(s),)
print(plot1)

pltName<-paste("tbm", s, sep= "") #name the plot
assign(pltName, plot1)


} #close species loop

#And stitch them
pltName<-paste("bmDampierArchipelagoSpecies") #name the plot
assign(pltName, (ggarrange(tbmHalophila_ovalis,
                           tbmHalophila_decipiens,
                           tbmHalophila_minor,
                           tbmHalodule_uninervis,
                           tbmSyringodium_isoetifolium,
                           tbmCymodocea_angustata,
                           nrow=2,ncol=3,
                           common.legend =TRUE, legend="bottom") %>%
                   annotate_figure(top="Dampier Archipelago Total Biomass")))



#Print all plots for one site onto one page and
#save as an object in the environment

#Print all plots for one site onto one page
pltName<-paste("bm", i, sep= "") #name the plot
assign(pltName, (ggarrange(plotall,
                           bmHalophila_ovalis,
                           bmHalophila_decipiens,
                           bmHalophila_minor,
                           bmSyringodium_isoetifolium,
                           bmCymodocea_angustata,
                           nrow=2,ncol=3,
                           common.legend =TRUE, legend="bottom") %>%
                   annotate_figure(top=paste(i))))


###-----Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder

st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date
pdf(paste("DMP_Seagrass_Biomass_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Seagrass biomass monitoring summary
         Dampier Archipelago", halign="center", fixed.width=FALSE)            #set your title page as you please
plot(bmDampierArchipelago)
textplot(capture.output(TotalDampierArchipelago), cex=0.5) #this prints your ANOVA list as an image in your PDF
plot(bmDampierArchipelagoSpecies)
textplot(capture.output(SpeciesDampierArchipelago), cex=0.5)
plot(bmConzinc) #You will need to go through and put your own site names here
plot(bmEastLewis)
plot(bmEnderbyBay)
plot(bmEnderbyIsland)
plot(bmKeast)
plot(bmMalus)
plot(bmSWRegnard)
plot(bmVictoriaRocks)
plot(bmWhitnell)
dev.off()

jitter <- position_jitter(width = 0.00, height = 0.00) #this is so points don't overlap, increase values to spread out more

###Report
biomass <- ggplot(data =meanNK,aes(x=Year, y=MmeanTotal))+
  geom_errorbar(aes(ymin=MmeanTotal-MseTotal,ymax=MmeanTotal+MseTotal), width=0.1, colour="light grey")+
  geom_point(size=3,shape="square",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = expression ("Mean biomass (g/core)"),x="Year")+
  expand_limits(y=0)
biomass<-biomass+
  geom_errorbar(data=meanK,aes(ymin=MmeanTotal-MseTotal,ymax=MmeanTotal+MseTotal), width=0.1, colour="light grey", position=jitter)+
  geom_point(data=meanK,size=3, shape = "circle", aes (Year, MmeanTotal), position = jitter)
biomass

abr <- ggplot(data =meanNK,aes(x=Year, y=MmeanABR))+
  labs(y = expression ("Above:below ground biomass"),x="Year")+
  geom_errorbar(aes(ymin=MmeanABR-MseABR,ymax=MmeanABR+MseABR), width=0.1, colour="light grey", position=jitter)+
  geom_point(size=2, shape = "square", aes (Year, MmeanABR), position = jitter)+
  expand_limits(y=0)
abr<-abr+
  geom_errorbar(data=meanK,aes(ymin=MmeanABR-MseABR,ymax=MmeanABR+MseABR), width=0.1, colour="light grey", position=jitter)+
  geom_point(data=meanK,size=3, shape = "circle", aes (Year, MmeanABR), position = jitter)
abr


ggsave(biomass, device="png", units="cm", height = 15, filename = "biomass.png")


