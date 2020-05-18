#This script is designed to created an automated annual summary report for fish from DOV data
#Created by Molly Moustaka July 2019
#Dept. Biodiversity, Conservation and Attractions, Marine Science Program
#Email: molly.moustaka@dbbca.wa.gov.au

###-----1. Get set up----####
#Clear environment
rm(list=ls())

#Install & Load packages
#install.packages(c("ggplot2", "dplyr","plotrix","gridExtra","RColorBrewer","gplots", "ggpubr","vegan"),dependencies = T)
library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(magrittr)
library(googlesheets)
library(ggplot2)
library(httpuv)
library(RCurl) # needed to download data from GitHub
library(stringr)
library(RColorBrewer)
library(gplots)
library(tidyr)
library(ggpubr)

#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Fish")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/")
data=paste(work.dir,"Data",sep="/")
plots=paste(work.dir,"Data/Plots",sep="/")
tidy=paste(work.dir,"Data/Concatenated, Cleaned and Checked Data",sep="/")

#Set up study name
st=format(Sys.time(), "%Y-%m") #make an object with todays date
study<-paste("DMP_Monitoring",st, sep="_")  ##change this to inc. date and change subsequent outputs

#Set up a function to calculate SE
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

###-----2. Read in the data----####
setwd(tidy)
dir()

abun<-read.csv("DMP_Monitoring_2019-10_complete.sumcount.csv",header=T ,fileEncoding="UTF-8-BOM")
length<-read.csv("DMP_Monitoring_2019-10_complete.length.number.csv",header=T ,fileEncoding="UTF-8-BOM")
biom<-read.csv("DMP_Monitoring_2019-10_complete.length.number.mass.csv",header=T ,fileEncoding="UTF-8-BOM")

##Note: The mastersheet of data is available on DBCA Data Catalogue (CKAN) but not the cleaned and formated files.
#This is because data should be cleaned/checked prior to any new analysis.

###-----3. Rename some columns and make year a factor----####
abun<-abun%>%
  dplyr::rename(feeding=rls.trophic.group,
                target=target.code)%>%
  dplyr::mutate(year=as.factor(year))

length<-length%>%
  dplyr::rename(feeding=rls.trophic.group,
                target=target.code)%>%
  dplyr::mutate(year=as.factor(year))

biom<-biom%>%
  dplyr::rename(feeding=rls.trophic.group,
                target=target.code)%>%
  dplyr::mutate(year=as.factor(year))

###-----4. Data checking----####
str(abun)
unique(abun$site) #corect number of sites no duplicates
unique(abun$feeding) #check unique guilds
unique(abun$target) #check target classifications
unique(abun$year)


###-----5. Make datasets----####

###-----5.1 Abundance data----####

###Total abundance###
#Mean sum total abundance MP level
mptotalabun<-abun%>%
  group_by(year,site,transect)%>%
dplyr::summarise(sumcount=sum(sumcount))%>%
group_by(year,site)%>%
  dplyr::summarise(averagecount=mean(sumcount))%>%
group_by(year)%>%
dplyr::summarise(meancount=mean(averagecount),
                 se = st.err(averagecount))

###Total abundance for sites sampled in all years###
#Mean sum total abundance MP level
mptotalabun2<-abun%>%
  filter(site==c("Hammersley Shoal", "Legendre Gen", "NE Regnard", "Sailfish"))%>%
  group_by(year,site,transect)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,site)%>%
  dplyr::summarise(averagecount=mean(sumcount),
                   se=st.err(sumcount))%>%
  group_by(year)%>%
  dplyr::summarise(meancount=mean(averagecount),
                   se = st.err(averagecount))

plot<-mptotalabun2%>%
  ggplot(aes(x=year, y=averagecount))+
  geom_point(aes(colour=site))
plot

#Mean sum total abundance site level
sitetotalabun<-abun%>%
  group_by(year,site,transect)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,site)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

###Family abundance###
#Mean sum total abundance mp and family level
mptotalfamabun<-abun%>%
  group_by(year,site,transect,family)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,family)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

#Mean sum total abundance site and family level
sitetotalfamabun<-abun%>%
  group_by(year,site,transect,family)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,site,family)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

###Feeding guild abundance###
#Mean sum total abundance mp and feeding level
mptotalfeedabun<-abun%>%
  dplyr::filter(feeding.guild!="NA")%>% #remove rows without feeding guild data
  group_by(year,site,transect,feeding.guild)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,feeding.guild)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

#Mean sum total abundance site and feeding level
sitetotalfeedabun<-abun%>%
  dplyr::filter(feeding.guild!="NA")%>%
  group_by(year,site,transect,feeding.guild)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,site,feeding.guild)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

###EXAMPLE: the same data for a species/genera/family/guild of interest###
examplespecies<-abun%>% #name your dataset and feed in the raw abundance data
  dplyr::filter(genus=="Pomacentrus"|species=="moluccensis")%>% #genus/species can be changed to family or feeding, the "|" means within, you don't need two commands if you only want one filter
  group_by(year,site,transect)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year)%>% #This is marine park level. To make it site level use (year,site)
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

###-----5.2 Biomass data----####

###Total biomass###
#Mean sum total biomdance MP level
mptotalbiom<-biom%>%
  filter(site==c( "Hammersley Shoal", "Legendre Gen", "NE Regnard", "Sailfish"))%>%
  group_by(year,site,transect)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%  ##Note the use of na.rm =T. This removes any rows where biomass could not be calculated rather than treating them as a '0'
  group_by(year,site)%>%
  dplyr::summarise(mass.g2=mean(mass.g))%>%
  group_by(year)%>%
  dplyr::summarise(meancount=mean(mass.g2),
                   se = st.err(mass.g2))


#Mean sum total biomass site level
sitetotalbiom<-biom%>%
  group_by(year,site,transect)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year,site)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

###Family biomass###
#Mean sum total biomass mp and family level
mptotalfambiom<-biom%>%
  group_by(year,site,transect,family)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year,family)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

#Mean sum total biomass site and family level
sitetotalfambiom<-biom%>%
  group_by(year,site,transect,family)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year,site,family)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

###Feeding guild biomass###
#Mean sum total biomass mp and feeding level
mptotalfeedbiom<-biom%>%
  dplyr::filter(feeding.guild!="NA")%>% #remove rows without feeding guild data
  group_by(year,site,transect,feeding.guild)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year,feeding.guild)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

#Mean sum total biomass site and feeding level
sitetotalfeedbiom<-biom%>%
  dplyr::filter(feeding.guild!="NA")%>%
  group_by(year,site,transect,feeding.guild)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year,site,feeding.guild)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

###EXAMPLE: the same data for a species/genera/family/guild of interest###
examplespeciesbiom<-biom%>% #name your dataset and feed in the raw biomass data
  dplyr::filter(genus=="Pomacentrus"|species=="moluccensis")%>% #genus/species can be changed to family or feeding, the "|" means within, you don't need two commands if you only want one filter
  group_by(year,site,transect)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year)%>% #This is marine park level. To make it site level use (year,site)
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

###-----5.3 Length data----####
#I haven't done anything with this yet because we don't use it in any common analyses yet - if you have something you want to do let me know :)

###-----5.4 Species Richness data----####
#Marine park level species richness
mpsr<-abun%>%
  dplyr::filter(species!=c("NA","spp","sp1","sp3"))%>%
  dplyr::filter(sumcount>0)%>%
  dplyr::group_by(year)%>%
  dplyr::summarise(richness=n_distinct(scientific))

#Site level species richness
sitesr<-abun%>%
  dplyr::filter(species!=c("NA","spp","sp1","sp3"))%>%
  dplyr::filter(sumcount>0)%>%
  dplyr::group_by(year,site)%>%
  dplyr::summarise(richness=n_distinct(scientific))

###-----5.4 Target species data----####

###Target species abundance###
#Mean sum total abundance MP level for pooled target species
mptotaltargetabun<-abun%>%
  dplyr::filter(target.code==c("HT", "T"))%>% #filter out target species
  group_by(year,site,transect)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

#Mean sum total abundance MP level for individual target species
mptotaltargetspeciesabun<-abun%>%
  dplyr::filter(target.code==c("HT", "T"))%>% #filter out target species
  group_by(year,site,transect,scientific)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,scientific)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

#Mean sum total abundance site level for pooled target species
sitetotaltargetabun<-abun%>%
  dplyr::filter(target.code==c("HT", "T"))%>% #filter out target species
  group_by(year,site,transect)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,site)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

#Mean sum total abundance site level for individual target species
sitetotaltargetspeciesabun<-abun%>%
  dplyr::filter(target.code==c("HT", "T"))%>% #filter out target species
  group_by(year,site,transect,scientific)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  group_by(year,site,scientific)%>%
  dplyr::summarise(meancount=mean(sumcount),
                   se = st.err(sumcount))

###Target species biomass###
#Mean sum total biomass MP level for pooled target species
mptotaltargetbiom<-biom%>%
  dplyr::filter(target.code==c("HT", "T"))%>% #filter out target species
  group_by(year,site,transect)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

#Mean sum total biomass MP level for individual target species
mptotaltargetspeciesbiom<-biom%>%
  dplyr::filter(target.code==c("HT", "T"))%>% #filter out target species
  group_by(year,site,transect,scientific)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year,scientific)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

#Mean sum total biomass site level for pooled target species
sitetotaltargetbiom<-biom%>%
  dplyr::filter(target.code==c("HT", "T"))%>% #filter out target species
  group_by(year,site,transect)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year,site)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

#Mean sum total biomass site level for individual target species
sitetotaltargetspeciesbiom<-biom%>%
  dplyr::filter(target.code==c("HT", "T"))%>% #filter out target species
  group_by(year,site,transect,scientific)%>%
  dplyr::summarise(mass.g=sum(mass.g, na.rm=T))%>%
  group_by(year,site,scientific)%>%
  dplyr::summarise(meancount=mean(mass.g),
                   se = st.err(mass.g))

###-----6. Stats----####


###-----7. Plotting----####

###-----7.1 Plotting - get set up----####
#First lets set a theme up. Note you cant set the point colours as part of this
custtheme<-theme_grey()+
  theme(panel.grid.major = element_blank(), #get rid of grid
        panel.grid.minor = element_blank(), #get rid of grid
        panel.background = element_blank(), #get rid of background (blank),
        axis.line = element_line(colour = "black"), #make axis lines black
        plot.title = element_text(hjust=0.5)) #move the plot title

theme_set(custtheme) # apply the theme

#make a poisition jitter
jitter <- position_jitter(width = 0.1, height = 0) #this is so points don't overlap, increase values to spread out more

#create a colour palette
nb.cols <- 16
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

###-----7.2 Abundance plots----####
#Total abundance
totalabun<-sitetotalabun%>%
  ggplot(aes(x=year,y=meancount)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Mean abundance (1500m2)", x="Year")+
  ggtitle("Mean abundance of fish in the Dampier Archipelago")+
  scale_color_manual(values=mycolors)
totalabun <- totalabun +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mptotalabun,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(data = mptotalabun, aes(x = year, y = meancount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
totalabun


#Family abundance - this is a loop for multiple plots
families<-unique(sitetotalfamabun$family)

for (f in families){

family<-sitetotalfamabun%>%
  filter(family==paste(f))%>%
  ggplot(aes(x=year,y=meancount)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Mean abundance (1500m2)", x="Year")+
  scale_color_manual(values=mycolors)+
  ggtitle(paste(f))

mpdat<- mptotalfamabun%>%
  filter(family==paste(f))

family <- family +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mpdat,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(data = mpdat, aes(x = year, y = meancount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
print(family)

pltName<-paste("abun", f, sep= "")
assign(pltName,family)


}

#Feeding guild abundance - this is a loop for multiple plots
feeding<-unique(sitetotalfeedabun$feeding.guild)

for (f in feeding){

  feeding<-sitetotalfeedabun%>%
    filter(feeding.guild==paste(f))%>%
    ggplot(aes(x=year,y=meancount)) + #select variable to plot
    geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
    labs(y = "Mean abundance (1500m2)", x="Year")+
    scale_color_manual(values=mycolors)+
    ggtitle(paste(f))

  mpdat<- mptotalfeedabun%>%
    filter(feeding.guild==paste(f))

  feeding <- feeding +  #this line adds a archipelago-wide point to the plot as a cross
    geom_errorbar(data = mpdat,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(data = mpdat, aes(x = year, y = meancount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
               size=4,shape="cross",colour="black",show.legend=FALSE)
  print(feeding)

  pltName<-paste("abun", f, sep= "")
  assign(pltName,feeding)
}

###-----7.3 Biomass plots----####
#Total Biomass
totalbiom<-sitetotalbiom%>%
  ggplot(aes(x=year,y=meancount)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Mean Biomass (g/1500m2)", x="Year")+
  ggtitle("Mean biomass of fish in the Dampier Archipelago")+
  scale_color_manual(values=mycolors)
totalbiom <- totalbiom +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mptotalbiom,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(data = mptotalbiom, aes(x = year, y = meancount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
totalbiom


#Family abundance - this is a loop for multiple plots
families<-unique(sitetotalfambiom$family)

for (f in families){

  familybiom<-sitetotalfambiom%>%
    filter(family==paste(f))%>%
    ggplot(aes(x=year,y=meancount)) + #select variable to plot
    geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
    labs(y = "Mean biomass (g/1500m2)", x="Year")+
    scale_color_manual(values=mycolors)+
    ggtitle(paste(f))

  mpdat<- mptotalfambiom%>%
    filter(family==paste(f))

  familybiom <- familybiom +  #this line adds a archipelago-wide point to the plot as a cross
    geom_errorbar(data = mpdat,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(data = mpdat, aes(x = year, y = meancount),
               size=4,shape="cross",colour="black",show.legend=FALSE)
  print(familybiom)

  pltName<-paste("biomass", f, sep= "")
  assign(pltName,familybiom)

}

#Feeding guild biomass - this is a loop for multiple plots
feeding<-unique(sitetotalfeedbiom$feeding.guild)

for (f in feeding){

  feedingbiom<-sitetotalfeedbiom%>%
    filter(feeding.guild==paste(f))%>%
    ggplot(aes(x=year,y=meancount)) + #select variable to plot
    geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
    labs(y = "Mean biomass (g/1500m2)", x="Year")+
    scale_color_manual(values=mycolors)+
    ggtitle(paste(f))

  mpdat<- mptotalfeedbiom%>%
    filter(feeding.guild==paste(f))

  feedingbiom <- feedingbiom +  #this line adds a archipelago-wide point to the plot as a cross
    geom_errorbar(data = mpdat,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(data = mpdat, aes(x = year, y = meancount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
               size=4,shape="cross",colour="black",show.legend=FALSE)
  print(feedingbiom)

  pltName<-paste("biomass", f, sep= "")
  assign(pltName,feedingbiom)
}


###-----7.4 Richness plots----####
richness<-sitesr%>%
  ggplot(aes(x=year,y=richness)) + #select variable to plot
  geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Species richness", x="Year")+
  ggtitle("Species richness of fish in the Dampier Archipelago")+
  scale_color_manual(values=mycolors)
richness <- richness +  #this line adds a archipelago-wide point to the plot as a cross
  geom_point(data = mpsr, aes(x = year, y = richness), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
richness

###-----7.5 Target species plots----####
#Pooled target species abundance
totaltarget<-sitetotaltargetabun%>%
  ggplot(aes(x=year,y=meancount)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Mean Abundance (1500m2)", x="Year")+
  ggtitle("Mean abundance of target fish species in the Dampier Archipelago")+
  scale_color_manual(values=mycolors)
totaltarget <- totaltarget +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mptotaltargetabun,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(data = mptotaltargetabun, aes(x = year, y = meancount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
totaltarget

#Pooled target species biomass
totaltargetbiom<-sitetotaltargetbiom%>%
  ggplot(aes(x=year,y=meancount)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Mean Biomass (g/1500m2)", x="Year")+
  ggtitle("Mean biomass of target fish species in the Dampier Archipelago")+
  scale_color_manual(values=mycolors)
totaltargetbiom <- totaltargetbiom +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mptotaltargetbiom,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(data = mptotaltargetbiom, aes(x = year, y = meancount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
totaltargetbiom

#Individual target species abundance
targetspecies<-unique(sitetotaltargetspeciesabun$scientific)

for (f in targetspecies){

  targetabun<-sitetotaltargetspeciesabun%>%
    filter(scientific==paste(f))%>%
    ggplot(aes(x=year,y=meancount)) + #select variable to plot
    geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
    labs(y = "Mean abundance (1500m2)", x="Year")+
    scale_color_manual(values=mycolors)+
    ggtitle(paste(f))

  mpdat<- mptotaltargetspeciesabun%>%
    filter(scientific==paste(f))

  targetabun <- targetabun +  #this line adds a archipelago-wide point to the plot as a cross
    geom_errorbar(data = mpdat,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(data = mpdat, aes(x = year, y = meancount),
               size=4,shape="cross",colour="black",show.legend=FALSE)
  print(targetabun)

  pltName<-paste("targetabundance", f, sep= "")
  assign(pltName,targetabun)

}

#Individual target species biomass
targetspecies<-unique(sitetotaltargetspeciesbiom$scientific)

for (f in targetspecies){

  targetbiom<-sitetotaltargetspeciesbiom%>%
    filter(scientific==paste(f))%>%
    ggplot(aes(x=year,y=meancount)) + #select variable to plot
    geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(position=jitter,aes(colour=site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
    labs(y = "Mean biomass (g/1500m2)", x="Year")+
    scale_color_manual(values=mycolors)+
    ggtitle(paste(f))

  mpdat<- mptotaltargetspeciesbiom%>%
    filter(scientific==paste(f))

  targetbiom <- targetbiom +  #this line adds a archipelago-wide point to the plot as a cross
    geom_errorbar(data = mpdat,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(data = mpdat, aes(x = year, y = meancount),
               size=4,shape="cross",colour="black",show.legend=FALSE)
  print(targetbiom)

  pltName<-paste("targetbiomass", f, sep= "")
  assign(pltName,targetbiom)

}

###-----7.6 Report plots----####

#Total abundance and biomass
totalabunrep <- ggplot(data =mptotalabun2,aes(x=year, y=meancount))+
  geom_errorbar(aes(ymin=meancount-se,ymax=meancount+se), width=0.1, colour="light grey")+
  geom_point(size=3, shape="square",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = expression ("Mean abundance (1500"~m^2~")"),x="Year")+
  expand_limits(y=0)
totalabunrep


totalbiomrep <- ggplot(data =mptotalbiom ,aes(x=year, y=meancount))+
  geom_errorbar(aes(ymin=meancount-se,ymax=meancount+se), width=0.1, colour="light grey")+
  geom_point(size=3,shape="square",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = expression ("Mean biomass (g/1500"~m^2~")"),x="Year")+
  expand_limits(y=0)
totalbiomrep

totalplots<-ggarrange(totalabunrep,totalbiomrep,nrow=1,ncol=2)

#Target abundance and biomass
totaltarget <- ggplot(data =mptotaltargetabun ,aes(x=year, y=meancount))+
  geom_errorbar(aes(ymin=meancount-se,ymax=meancount+se), width=0.1, colour="light grey")+
  geom_point(size=4,shape="cross",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = expression ("Mean abundance (1500"~m^2~")"),x="Year")+
  expand_limits(y=0)
totaltarget

totaltargetbiom <- ggplot(data =mptotaltargetbiom ,aes(x=year, y=meancount))+
  geom_errorbar(aes(ymin=meancount-se,ymax=meancount+se), width=0.1, colour="light grey")+
  geom_point(size=4,shape="cross",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = expression ("Mean biomass (g/1500"~m^2~")"),x="Year")+
  expand_limits(y=0)
totaltargetbiom

#make into one plot
targetplots<-ggarrange(totaltarget,totaltargetbiom,nrow=1,ncol=2)

#Species Richness plot
richnessplot <- ggplot(data=mpsr, aes(x=year, y=richness)) +
      geom_point(size=4,shape="cross",colour="black",show.legend=FALSE)+
  labs(y = expression ("Total species richness"),x="Year")+
  expand_limits(y=0)
richnessplot


#Trophic plots
feeding<-unique(mptotalfeedabun$feeding.guild)
feeding

for (f in feeding){

  feedingbiom<-mptotalfeedbiom%>% ##SET TO BIOMASS NOW
    filter(feeding.guild==paste(f))%>%
    ggplot(aes(x=year,y=meancount)) + #select variable to plot
    geom_errorbar(aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
    geom_point(aes(x = year, y = meancount), size=4,shape="cross",colour="black",show.legend=FALSE)+
    labs(y = expression ("Mean biomass (g/1500"~m^2~")"),x="Year")+
    expand_limits(y=0)+
    ggtitle(paste(f))

  print(feedingbiom)

  pltName<-paste("repabunfeed", f, sep= "")
  assign(pltName,feedingbiom)
}


gfeedingabunplots1<-ggarrange(repabunfeedCorallivore,repabunfeedDetritivore,repabunfeedInvertivore,
                            `repabunfeedLarge cropper`, `repabunfeedMobile Invertivore`, repabunfeedOmnivore,
                            repabunfeedPiscivore, repabunfeedPlanktivore, `repabunfeedScraper/excavator`,
                             nrow=3,ncol=3)

gfeedingabunplots2<-ggarrange(`repabunfeedSessile Invertebrates`, `repabunfeedSmall Cropper`, `repabunfeedSmall Invertivore`,
                              `repabunfeedSmall Omnivore`, repabunfeedZooplanktivore,
                              nrow=2,ncol=3)

###-----8. Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder (Note: you need to make this folder within your WD first)

st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date
pdf(paste("DMP_Fish_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Fish monitoring summary for
         the Dampier Archipelago", halign="center", fixed.width=FALSE)            #set your title page as you please
#add a sequence of plots and text plots, each will print on a new page of the pdf

plot(totalabun)
plot(totalbiom)
plot(richness)
plot(totaltarget)
plot(totaltargetbiom)

textplot("Family Abundance", halign="center",cex=1)
plot(abunAcanthuridae)
plot(abunApogonidae)
plot(abunBalistidae)
plot(abunBlenniidae)
plot(abunCaesionidae)
plot(abunCarangidae)
plot(abunCarcharhinidae)
plot(abunChaetodontidae)
plot(abunDasyatidae)
plot(abunEcheneidae)
plot(abunEphippidae)
plot(abunFistulariidae)
plot(abunGobiidae)
plot(abunGrammistidae)
plot(abunHaemulidae)
plot(abunHolocentridae)
plot(abunLabridae)
plot(abunLethrinidae)
plot(abunLutjanidae)
plot(abunMicrodesmidae)
plot(abunMyliobatidae)
plot(abunMicrodesmidae)
plot(abunNemipteridae)
plot(abunOdacidae)
plot(abunOstraciidae)
plot(abunPomacanthidae)
plot(abunPomacentridae)
plot(abunPseudochromidae)
plot(abunScaridae)
plot(abunScombridae)
plot(abunSerranidae)
plot(abunSiganidae)
plot(abunSphyraenidae)
plot(abunTetraodontidae)
plot(abunZanclidae)

textplot("Family Biomass", halign="center", cex=1)
plot(biomassAcanthuridae)
plot(biomassApogonidae)
plot(biomassBalistidae)
plot(biomassBlenniidae)
plot(biomassCaesionidae)
plot(biomassCarangidae)
plot(biomassCarcharhinidae)
plot(biomassChaetodontidae)
plot(biomassDasyatidae)
plot(biomassEcheneidae)
plot(biomassEphippidae)
plot(biomassFistulariidae)
plot(biomassGobiidae)
plot(biomassGrammistidae)
plot(biomassHaemulidae)
plot(biomassHolocentridae)
plot(biomassLabridae)
plot(biomassLethrinidae)
plot(biomassLutjanidae)
plot(biomassMicrodesmidae)
plot(biomassMyliobatidae)
plot(biomassMicrodesmidae)
plot(biomassNemipteridae)
plot(biomassOdacidae)
plot(biomassOstraciidae)
plot(biomassPomacanthidae)
plot(biomassPomacentridae)
plot(biomassPseudochromidae)
plot(biomassScaridae)
plot(biomassScombridae)
plot(biomassSerranidae)
plot(biomassSiganidae)
plot(biomassSphyraenidae)
plot(biomassTetraodontidae)
plot(biomassZanclidae)

textplot("Feeding Abundance", halign="center", fixed.width=FALSE, cex=1)
plot(`abunLarge cropper`)
plot(abunCorallivore)
plot(`abunMobile invertivore`)
plot(abunDetritivore)
plot(`abunOmnivore`)
plot(abunPiscivore)
plot(`abunSmall omnivore`)
plot(`abunSmall invertivore`)
plot(`abunSesssile invertebrates`)
plot(`abunSmall Cropper`)
plot(`abunScraper/excavator`)

textplot("Feeding Biomass", halign="center", fixed.width=FALSE,cex=1)
plot(`biomassLarge cropper`)
plot(biomassCorallivore)
plot(`biomassMobile invertivore`)
plot(biomassDetritivore)
plot(`biomassOmnivore`)
plot(biomassPiscivore)
plot(`biomassSmall omnivore`)
plot(`biomassSmall invertivore`)
plot(`biomassSesssile invertebrates`)
plot(`biomassSmall Cropper`)
plot(`biomassScraper/excavator`)

textplot("Target species abundance", halign="center", fixed.width=FALSE,cex=1)
plot(`targetabundanceCarangidae Carangoides fulvoguttatus`)
plot(`targetabundanceLabridae Choerodon schoenleinii`)
plot(`targetabundanceLethrinidae Lethrinus atkinsoni`)
plot(`targetabundanceLethrinidae Lethrinus laticaudis`)
plot(`targetabundanceLethrinidae Lethrinus nebulosus`)
plot(`targetabundanceLutjanidae Lutjanus argentimaculatus`)
plot(`targetabundanceLutjanidae Lutjanus carponotatus`)
plot(`targetabundanceSerranidae Epinephelus fasciatus`)
plot(`targetabundanceSerranidae Epinephelus rivulatus`)
plot(`targetabundanceSerranidae Plectropomus leopardus`)
plot(`targetabundanceSerranidae Plectropomus maculatus`)

textplot("Target species biomass", halign="center", fixed.width=FALSE,cex=1)
plot(`targetbiomassCarangidae Carangoides fulvoguttatus`)
plot(`targetbiomassLabridae Choerodon schoenleinii`)
plot(`targetbiomassLethrinidae Lethrinus atkinsoni`)
plot(`targetbiomassLethrinidae Lethrinus laticaudis`)
plot(`targetbiomassLethrinidae Lethrinus nebulosus`)
plot(`targetbiomassLutjanidae Lutjanus argentimaculatus`)
plot(`targetbiomassLutjanidae Lutjanus carponotatus`)
plot(`targetbiomassSerranidae Epinephelus fasciatus`)
plot(`targetbiomassSerranidae Epinephelus rivulatus`)
plot(`targetbiomassSerranidae Plectropomus leopardus`)
plot(`targetbiomassSerranidae Plectropomus maculatus`)

dev.off()

