#This script is designed to created an automated annual summary report for coral monitoring sites - total cover, family & genera level cover, and diversity
#Created by Molly Moustaka July 2019
#Dept. Biodiversity, Conservation and Attractions, Marine Science Program
#Email: molly.moustaka@dbbca.wa.gov.au

#Run sections 1-4 to create all the data - you need to work through this as there are several sections where you need to spec
#certain commands yoursef.
#Then you can run parts of sections 5/6/7 as you require


###-----1. Get set up----####
#Clear environment
rm(list=ls())

#Install & Load packages
#install.packages(c("ggplot2", "dplyr","plotrix","gridExtra","RColorBrewer","gplots", "ggpubr","vegan"),dependencies = T)
library(ggplot2) # make plots
library(plotrix) #package to calculate standard error
library(gridExtra) #grid layouts for multiple plots
library(RColorBrewer) #colours for plots.
library(gplots) #text to plot tools
library(ggpubr)#another arranging tool
library(vegan) #multivariate stats
library(dplyr) #data wrangling.
library(devtools)
filter = dplyr::filter #make sure R uses the DPLYR version of filter
summarise = dplyr::summarise
mutate = dplyr::mutate
library(tidyr)


#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Coral")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/") #spit out monitoring summary pdfs to here
data=paste(work.dir,"Data/Benthic",sep="/") #Spit out an updated concatenated mastersheet here
ecopaasdata=paste(work.dir,"Data/Benthic/Raw EcoPaas Outputs",sep="/") #Read in the raw ecopaas data from here

#Make an object with todays date
st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date

####----2. Merge EcoPass outputs----####
setwd(ecopaasdata)

#View files in the directory
dir()

#Merge files
files<-list.files(ecopaasdata, pattern = ".csv") #make a list of all file names in the data directory ending in CSV

dat<- NA # make a blank data frame

for (f in files) {
  x<-read.csv(f,header=TRUE,fileEncoding="UTF-8-BOM")
  dat<-rbind(dat,x)

}

#remove the bank row at the top thats made by the importing process
dat<-dat[-1,]

#Filter out the re-done 2019 High Point data
dat<-dat%>%
  dplyr::filter(Site!="High Point"|Year!="2019"|Month!="May")

#Write the updated Master CSV to the T drive asset folder
setwd(data)
write.csv(dat,file=paste("DMP_Coral_Benthic_Master",st,".csv"))

####2.1 OPTIONAL: Push updated benthic coral mastersheet onto CKAN (DBCA data catalogue)####
setwd("~/projects/data-pipelines/scripts/indicators/coral/Dampier_allmetrics/temp") #this is a temporary file to write a copy of the data to before it gets pushed to CKAN
source("~/projects/data-pipelines/setup/ckan.R")
csv_rid <- "57b442ff-46fe-4a20-ab26-5d3f7179f1a8"
write_delim(dat, path = "data.csv", delim = ",")
r <- ckanr::resource_update(csv_rid, "data.csv")

####2.2 OPTIONAL: Pull benthic coral mastersheet off CKAN (DBCA data catalogue)####
source("~/projects/data-pipelines/setup/ckan.R")
csv_rid <- "57b442ff-46fe-4a20-ab26-5d3f7179f1a8"
d <- load_ckan_csv(csv_rid)
dplyr::glimpse(d)
dat<-d

####----3. Data checking----####
#Drop columns we wont use
names(dat)
dat<-select(dat,c(-RegionCode,-ZoneCode,-LocationCode,-SiteCode,-ReplicateCode,
                  -Survey,-Date,-Time,-CameraSide))

#Check all site names match - you could check for marine park here too
unique(dat$Site)

#Check which sites were sampled in each year
table(dat$Site,dat$Year) #This data is a bit munted so we'll make datasets for plotting and stats seperately

#Make year into a factor for analysis
dat$Year<-as.factor(dat$Year)

#If you have a master dataset and just want to look at one marine park you can filter this here
#dat<-filter(dat, MarinePark=="Marine Park Name")


#If you want % cover of all classes (i.e. %turf, % coral etc.) you will need to add more rows inside the summarise command
#Two rows for each asset - an 'classcount' column and a 'classpercent' column.
#you could pipe the data through to drop the 'classcount' columns if you wanted.
#Ask Molly if you can't figure it out :)

####---4. Make data into hard coral % cover----####

#First lets set up a function to calculate SE
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

#Now make % hard coral cover data

trandat<- dat%>% #per transect
  group_by(Year,Site,Replicate)%>%
  summarise(
    count=n(),
    coralcount=sum(Level2Class=="Hard coral"),
    percent=(coralcount/count)*100
  )

sitedat<- dat%>% #per site (points pooled to site level)
  group_by(Year,Site)%>%
  summarise(
    count=n(),
    coralcount=sum(Level2Class=="Hard coral"),
    percent=(coralcount/count)*100
  )

#Lets do an alternative site level method where we take the mean of the transect so we have a standard error
meantrandat<- dat%>% #per transect
  group_by(Year,Site,Replicate)%>%
  summarise(
    count=n(),
    coralcount=sum(Level2Class=="Hard coral"),
    percent=(coralcount/count)*100) %>%
  group_by(Year,Site)%>%
  summarise(
    meancover=mean(percent),
    se=st.err(percent)
  )


mpdat<- dat%>% #for the whole dataset (i.e. marine park)
  group_by(Year)%>%
  summarise(
    count=n(),
    coralcount=sum(Level2Class=="Hard coral"),
    percent=(coralcount/count)*100
  )

#And an alternative mp level method where we take the mean of the transect so we have a standard error
meansitedat<- dat%>% #per transect
  filter(Site!="Nelson Rocks")%>%    #Anything you don't want included in the site level mean needs to be removed here
  filter(Year!="2016")%>%
  group_by(Year,Site,Replicate)%>%
  summarise(
    count=n(),
    coralcount=sum(Level2Class=="Hard coral"),
    percent=(coralcount/count)*100) %>%
  group_by(Year)%>%
  summarise(
    meancover=mean(percent),
    se=st.err(percent)
  )

####----4. Make data into family level% cover-----####


#first make a loop of the coral family names
fams<-filter(dat, Level2Class=="Hard coral") #make a subset of only coral data
fams<-unique(fams$Level3Class) #make a character vector of all the coral families

#make a blank data frames for each level of resolution - don't worry about the warnings here
tranfamdat<-trandat[FALSE,] #transect level- take the column heads from the trandat dataset
sitefamdat<-sitedat[FALSE,] #site level
mpfamdat<-mpdat[FALSE,] #marine park level

#make a loop
for (f in fams) {

  ##Make the data for each transect
  x<- dat%>% #per transect
   # filter(Level2Class=="Hard coral")%>% #pull out only rows that are observations of hard coral
    group_by(Year,Site,Replicate)%>% #this is your data resolution i.e. to replicate level
    summarise(
      count=n(),                    #count the number of rows in each replicate (remebering that this is only rows containing coral)
      coralcount=sum(Level3Class==paste(f)), #count the number of those rows that equal a single family
      percent=(coralcount/count)*100)    #calculate the % of hard coral for that family

  x$family<-paste(f) #add column with the coral family name

  tranfamdat<-rbind(tranfamdat,x) #bind the data for this family to the whole dataset


  ##Make the data for each Site
  y<- dat%>%
    filter(Level2Class=="Hard coral")%>%
    group_by(Year,Site)%>%
    summarise(
      count=n(),
      coralcount=sum(Level3Class==paste(f)),
      percent=(coralcount/count)*100)

  y$family<-paste(f) #add column with the coral family name

  sitefamdat<-rbind(sitefamdat,y) #bind the data for this family to the whole dataset


  ##Make the data for each marine park
  z<- dat%>%
    filter(Level2Class=="Hard coral")%>%
    group_by(Year)%>%
    summarise(
      count=n(),
      coralcount=sum(Level3Class==paste(f)),
      percent=(coralcount/count)*100)

  z$family<-paste(f) #add column with the coral family name

  mpfamdat<-rbind(mpfamdat,z) #bind the data for this family to the whole dataset

}

mpdat<-tranfamdat%>%
  filter(Site!="Nelson Rocks")%>%    #Anything you don't want included in the site level mean needs to be removed here
  filter(Year!="2016")%>%
  group_by(Year,family)%>%
  dplyr::summarise(
    meancover=mean(percent),
    se=st.err(percent)
  )

####----4. Make data into genera level% cover-----####
#This code calculates the % of each genera WITHIN hard coral (i.e. will total 100%)
#If you want the % of each genera from the whole bentos you need to remove the "filter(Level2Class=="Hard coral")%>%" line


#first make a loop of the coral genera names
gens<-filter(dat, Level2Class=="Hard coral") #make a subset of only coral data
gens<-unique(gens$Level4Class) #make a character vector of all the coral families

#make a blank data frames for each level of resolution- don't worry about the warnings here
trangendat<-trandat[FALSE,] #transect level- take the column heads from the trandat dataset
sitegendat<-sitedat[FALSE,] #site level
mpgendat<-mpdat[FALSE,] #marine park level

#make a loop
for (g in gens) {

  ##Make the data for each transect
  x<- dat%>% #per transect
    filter(Level2Class=="Hard coral")%>% #pull out only rows that are observations of hard coral
    group_by(Year,Site,Replicate)%>% #this is your data resolution i.e. to replicate level
    summarise(
      count=n(),                    #count the number of rows in each replicate (remebering that this is only rows containing coral)
      coralcount=sum(Level4Class==paste(g)), #count the number of those rows that equal a single family
      percent=(coralcount/count)*100)    #calculate the % of hard coral for that family

  x$genera<-paste(g) #add column with the coral family name

  trangendat<-rbind(trangendat,x) #bind the data for this family to the whole dataset


  ##Make the data for each site
  y<- dat%>% #per site
    filter(Level2Class=="Hard coral")%>%
    group_by(Year,Site)%>%
    summarise(
      count=n(),
      coralcount=sum(Level4Class==paste(g)),
      percent=(coralcount/count)*100)

  y$genera<-paste(g) #add column with the coral family name

  sitegendat<-rbind(sitegendat,y) #bind the data for this family to the whole dataset


  ##Make the data for each marine park
  z<- dat%>% #per mp
    filter(Level2Class=="Hard coral")%>%
    group_by(Year)%>%
    summarise(
      count=n(),
      coralcount=sum(Level4Class==paste(g)),
      percent=(coralcount/count)*100)

  z$genera<-paste(g) #add column with the coral family name

  mpgendat<-rbind(mpgendat,z) #bind the data for this family to the whole dataset

}


####----4. Make genera-richness data-----####

divtrandat<- dat%>% #per transect
  filter(Level2Class=="Hard coral")%>% #subset data to only hard coral data
  filter(Level4Class!="Hard coral")%>% #remove rows where we don't know what genera it is
  group_by(Year,Site,Replicate)%>%
  summarise(
    diversity = n_distinct(Level4Class))

divsitedat<- dat%>% #per site
  filter(Level2Class=="Hard coral")%>% #subset data to only hard coral data
  filter(Level4Class!="Hard coral")%>% #remove rows where we don't know what genera it is
  group_by(Year,Site)%>%
  summarise(
    diversity = n_distinct(Level4Class))

divmpdat<- dat%>% #for the whole dataset (i.e. marine park)
  filter(Level2Class=="Hard coral")%>% #subset data to only hard coral data
  filter(Level4Class!="Hard coral")%>% #remove rows where we don't know what genera it is
  group_by(Year)%>%
  summarise(
    diversity = n_distinct(Level4Class))


####----4. Make data for statistics (correct for missing years) YOU NEED TO EDIT HERE -----####
#Lets see what is missing
table(trandat$Year,trandat$Site)

#we are missing most sites in 2016 & most data for Nelson Rocks so lets remove those
#You will need to analyse this and work our what you want to remove yourself
#Do we need to correct for missing transects i.e. 2 vs 3 transects? Do it here

####Transect level
trandatstat<-trandat%>%          #for total percent cover
  filter(Year != "2016")%>%     # filter (keep) rows where data does not equal 2016
  filter(Site !="Nelson Rocks")

tranfamdatstat<-tranfamdat%>%          #for family percent cover
  filter(Year != "2016")%>%
  filter(Site !="Nelson Rocks")

trangendatstat<-trangendat%>%          #for genera percent cover
  filter(Year != "2016")%>%
  filter(Site !="Nelson Rocks")

trandivdatstat<-divtrandat%>%        #for diversity
  filter(Year != "2016")%>%
  filter(Site !="Nelson Rocks")


####site level
sitedatstat<-sitedat%>%          #for total percent cover
  filter(Year != "2016")%>%     # filter (keep) rows where data does not equal 2016
  filter(Site !="Nelson Rocks")

sitefamdatstat<-sitefamdat%>%          #for family percent cover
  filter(Year != "2016")%>%
  filter(Site !="Nelson Rocks")

sitegendatstat<-sitegendat%>%          #for genera percent cover
  filter(Year != "2016")%>%
  filter(Site !="Nelson Rocks")

sitedivdatstat<-divsitedat%>%        #for diversity
  filter(Year != "2016")%>%
  filter(Site !="Nelson Rocks")


###marine park level
mpdatstat<- dat%>% #for the whole dataset (i.e. marine park)
  filter(Year != "2016")%>%
  filter(Site !="Nelson Rocks")%>%
  group_by(Year)%>%
  summarise(
    count=n(),
    coralcount=sum(Level2Class=="Hard coral"),
    percent=(coralcount/count)*100
  )


mpfamdatstat<-mpdat[FALSE,] #family at marine park level minus rows to be removed
  for (f in fams){
    z<- dat%>%
    filter(Year != "2016")%>%
    filter(Site !="Nelson Rocks")%>%
    filter(Level2Class=="Hard coral")%>%
    group_by(Year)%>%
    summarise(
        count=n(),
        coralcount=sum(Level3Class==paste(f)),
        percent=(coralcount/count)*100)

    z$family<-paste(f) #add column with the coral family name

    mpfamdatstat<-rbind(mpfamdatstat,z) #bind the data for this family to the whole dataset
}


mpgendatstat <- mpdat[FALSE,]  #genera at marine park level minus rows to be removed
  for (g in gens) {

    z<- dat%>%
      filter(Year != "2016")%>%
      filter(Site !="Nelson Rocks")%>%
      filter(Level2Class=="Hard coral")%>%
      group_by(Year)%>%
      summarise(
        count=n(),
        coralcount=sum(Level4Class==paste(g)),
        percent=(coralcount/count)*100)
    z$genera<-paste(g) #add column with the coral family name

    mpgendatstat<-rbind(mpgendatstat,z) #bind the data for this family to the whole dataset
  }


mpdivdatstat<- dat%>% #diversity for marine park
  filter(Year != "2016")%>%
  filter(Site !="Nelson Rocks")%>%
  filter(Level2Class=="Hard coral")%>% #subset data to only hard coral data
  filter(Level4Class!="Hard coral")%>% #remove rows where we don't know what genera it is
  group_by(Year)%>%
  summarise(
    diversity = n_distinct(Level4Class))


####----4. Make data for stacked bar plots (corrected for missing years) -----####

#Make % of benthos by family at marine park level
mpfamstack <-mpdat[FALSE,]
sitefamstack <-sitedat[FALSE,]

for (f in fams){
  s<- dat%>%
    filter(Year != "2016")%>%
    filter(Site !="Nelson Rocks")%>%
    group_by(Year)%>%
    summarise(
      count=n(),
      famcount=sum(Level3Class==paste(f)),
      percent=(famcount/count)*100)
  s$family<-paste(f)
  mpfamstack<-rbind(mpfamstack,s)

  t<- dat%>%
    filter(Year != "2016")%>%
    filter(Site !="Nelson Rocks")%>%
    group_by(Year,Site)%>%
    summarise(
      count=n(),
      famcount=sum(Level3Class==paste(f)),
      percent=(famcount/count)*100)
  t$family<-paste(f)
  sitefamstack<-rbind(sitefamstack,t)
}

####----4. Summary of the datasets we have made so far----####
#Choose which dataset you want to use for subsequent stats and plotting based on your question
#Use head() or str() to check them out

#Total hard coral cover (% of benthic substrate) - does not total 100%
mpdat #marine park level
meansitedat #alterntive site level taking the mean of all transects rather than pooling the points + with certain sites removed
sitedat #site level
meantrandat #alterntive site level taking the mean of three transects rather than pooling the points
transdat #transect level

# % of hard coral coral cover by family (% of hard coral cover) - totals 100%
mpfamdat #marine park level
sitefamdat  #site level
tranfamdat #transect level

# % of hard coral coral cover by genera (% of hard coral cover) - totals 100%
mpgendat #marine park level
sitegendat #site level
trangendat  #transect level

#Diversity (genera level) data
divmpdat #marine park level
divsitedat #site level
divtrandat #transect level

#Datasets corrected for missing years/sites for stats - transect level
trandatstat #total % cover (does not = 100%)
tranfamdatstat # % of hard coral by family (=100%)
trangendatstat # % of hard coral by genera (=100%)
trandivdatstat # genera diversity

#Datasets corrected for missing years/sites for stats - site level
sitedatstat #total % cover (does not = 100%)
sitefamdatstat  # % of hard coral by family (=100%)
sitegendatstat # % of hard coral by genera (=100%)
sitedivdatstat # genera diversity

#Datasets corrected for missing years/sites for stats - marine park level
mpdatstat #total % cover (does not = 100%)
mpfamdatstat # % of hard coral by family (=100%)
mpgendatstat # % of hard coral by genera (=100%)
mpdivdatstat # genera diversity

#Datasets for stacked bar plots
mpfamstack #family level % of benthos
sitefamstack # family/site level % of benthos

####----5. Statistics - Univariate - Total % Cover -----####
#Marine park level
sum1<-summary(aov(percent~Year, data=trandatstat))
names(sum1)<-paste("ANOVATotalNoInt")

#Site interaction
sum2<-summary(aov(percent~Year*Site, data=trandatstat)) #Dataset choice = replicate level i.e. for this transect = replicate
names(sum2)<-paste("ANOVATotalInt")

#PostHoc
lm1<-aov(percent~Year*Site, data=trandatstat)
post1<-TukeyHSD(lm1, "Site") #Show me which sites are different. You can remove that to get every site in every year

#Transect level (i.e within sites)
sites<- unique(trandatstat$Site)
for (s in sites) {

  avdat<-filter(trandatstat,Site==(paste(s))) #filter the data by site
  sum3<-summary(aov(percent~Year, data=avdat)) #run ANOVA's for differences between years

  sumName<-paste("ano", s, sep= "") #create loop naming convention

  assign(sumName, sum3) #name combined summary list

} #close loop - don't worry about the error here - it is because one site only has one year of data

####----5. Statistics - Univariate - Diversity  -----####
#Marine park level
sum4<-summary(aov(diversity~Year, data=trandivdatstat))
names(sum4)<-paste("ANOVADivNoInt")

#Site level
sum5<-summary(aov(diversity~Year*Site, data=trandivdatstat)) #Dataset choice = replicate level i.e. for this transect = replicate
names(sum5)<-paste("ANOVADivInt")

#PostHocs
lm2<-aov(diversity~Year*Site, data=trandivdatstat)
post2<-TukeyHSD(lm2, "Site") #Show me which sites are different. You can remove that to get every site in every year

#Put all MP level together except post hocs for plotting
mpstat<-list(c(sum1,sum2,sum4,sum5)) #combine all ANOVA outputs into one list

####----5. Statistics - Multivariate - Family % Cover -----####
#Right now we are running our stats on the % family/genera of total coral cover.
#You may want to change this to % of benthos for plotting/stats
#so our question is: are relative coral assemblages different between year/sites REGARDLESS of total % cover

#First we need to get the datat into wide format
bray<- tranfamdatstat%>%
  pivot_wider(names_from=family,     #split column 'family'
              values_from= percent, #A seperate one of these columns for every species
              values_fill=list(percent=0)) %>%
  group_by(Year,Site,Replicate)%>% #these next lines merge the replicates into one row (if you look at the data with and without running them it makes sense)
  dplyr::  summarise_if(is.numeric ,sum)%>%
  dplyr::select(-`Hard coral`)%>% # remove 'hard coral column for unidentified samples
  dplyr::filter(Site != "Nelson Rocks")%>%
  dplyr::filter(Year !="2016")%>%
  dplyr::filter(count!="0") #remove transects with no coral as the matrix wont work on these

str(bray)
#Now lets transform the data
bray1<-log(bray[,6:17]+1) #logx +1 transformation


#Create Bray Curtis Matrix - Chosen because lots of zeros in data
braycurtisdists <- vegdist(bray1, method="bray")

#Do a permanova
permfam<-print(adonis(braycurtisdists~Year*Site, data=bray)) #NOTE: these outputs wont name and stitch like ANOVAS

#have a look at the MDS stress
#Note stress <0.05 is very good representation in reduced dimensions, <0.1 is great, <0.2 is ok, <0.3 is poor
fammds <- metaMDS(braycurtisdists, distance ="bray",trymax=50)
fammds
stressplot(fammds)

#NMDS plot
plot(fammds,type='n')
text(fammds$points,as.character(bray$Site), cex=0.8, col=as.numeric(bray$Year))
legend(-0.5,0.3, legend=c("2015","2017"),col=c("black","green"), cex=0.8, pch=15)

####----5. Statistics - Multivariate - Genera % Cover -----####
#Right now we are running our stats on the % family/genera of total coral cover.
#You may want to change this to % of benthos for plotting/stats
#so our question is: are relative coral assemblages different between year/sites REGARDLESS of total % cover

#First we need to get the datat into wide format
bray2<- trangendatstat%>%
  pivot_wider(names_from=genera,     #split column 'genera'
              values_from= percent, #A seperate one of these columns for every species
              values_fill=list(percent=0)) %>%
  group_by(Year,Site,Replicate)%>% #these next lines merge the replicates into one row (if you look at the data with and without running them it makes sense)
  dplyr::  summarise_if(is.numeric ,sum)%>%
  dplyr::select(-`Hard coral`)%>% # remove 'hard coral column for unidentified samples
  dplyr::filter(Site != "Nelson Rocks")%>%
  dplyr::filter(Year !="2016")%>%
  dplyr::filter(count!="0") #remove transects with no coral as the matrix wont work on these

#Now lets transform the data
bray3<-log(bray2[,6:42]+1) #logx +1 transformation - more columns than in family data so need to make sure we've only selected genera cols

#Create Bray Curtis Matrix - Chosen because lots of zeros in data
braycurtisdists1 <- vegdist(bray3, method="bray")

#Do a permanova
permgen<-adonis(braycurtisdists1~Year*Site, data=bray2)

#have a look at the MDS stress
#Note stress <0.05 is very good representation in reduced dimensions, <0.1 is great, <0.2 is ok, <0.3 is poor
genmds <- metaMDS(braycurtisdists1, distance ="bray",trymax=50)
genmds
stressplot(genusmds)

#NMDS plot
plot(genmds,type='n')
text(genmds$points,as.character(bray2$Site), cex=0.8, col=as.numeric(bray2$Year))
legend(-0.5,0.3, legend=c("2015","2017"),col=c("black","green"), cex=0.8, pch=15)


####----6. Plotting - setting up -----####
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

####----6. Plotting - total coral cover & whole marine park -----####

#Total coral cover
#note: we can use the data that includes the years/sites we removed for the stats by changing the data in the first row
total<-meantrandat%>%
  filter(Site != "Nelson Rocks")%>%
  filter(Year != "2016") %>%
  ggplot(aes(x=Year,y=meancover)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meancover-se,ymax=meancover+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,100))+ #set Y axis limits
  labs(y = "Total hard coral cover", x="Year")+
  scale_color_manual(values=mycolors)
total <- total +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = meansitedat,aes(ymin=meancover-se,ymax=meancover+se),width=NA, colour="light grey")+
  geom_point(data = meansitedat, aes(x = Year, y = meancover), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)

total

#Genera richness
rich<-sitedivdatstat%>%
  ggplot(aes(x=Year,y=diversity)) + #select variable to plot
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(divmpdat$diversity))))+ #set Y axis limits
  labs(y = "Genera richness", x="Year")+
  scale_color_manual(values=mycolors)
rich <- rich +  #this line adds a archipelago-wide point to the plot as a cross
  geom_point(data =mpdivdatstat, aes(x = Year, y = diversity),
             size=4,shape="cross",colour="black",show.legend=FALSE)
rich

#put them together (plot for a page)
totalrich<-ggarrange(total,rich,nrow=2,ncol=1,
                                common.legend =TRUE, legend="bottom") %>%
  annotate_figure(top="Dampier Archipelago")

####----6. Stacked bar plots (family) - whole marine park -----####
#Note you can do this by genera but you need to create another dataset - ask molly if you can't figure out how :)

stackmp<-ggplot(mpfamstack, aes(fill=family, y=percent, x=Year)) +
   geom_bar(stat="identity")+
    scale_fill_manual(values=mycolors)+    #set your pallete of choice but needs to have enough colours
    coord_cartesian(ylim=c(0,100))+ #change axis height if you want
   labs(y = "% Benthic Cover", x="Year", fill="Family")+
  ggtitle("Dampier Archipelago")

####----6. Stacked bar plots (family) -  per site -----####
sites<-unique(sitefamstack$Site)

for (s in sites) {
  stacksite<- sitefamstack%>%
    filter(Site==(paste(s)))%>%
    ggplot(aes(fill=family, y=percent, x=Year)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values=mycolors)+    #set your pallete of choice but needs to have enough colours
    coord_cartesian(ylim=c(0,100))+ #change axis height if you want
    labs(y = "% Benthic Cover", x="Year", fill="Family")+
    ggtitle(paste(s))

pltName<-paste("stack", s, sep= "")
  assign(pltName,stacksite)

}

###-----7. Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder (Note: you need to make this folder within your WD first)

pdf(paste("DMP_Coral_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Coral monitoring summary for
         the Dampier Archipelago", halign="center", fixed.width=FALSE)            #set your title page as you please
#add a sequence of plots and text plots, each will print on a new page of the pdf
plot(totalrich) #total cover by site + genera richness by site
  textplot(capture.output(mpstat), cex=0.6) #sig difs between year/site for total cover & richness
plot(stackmp)
  textplot(c("Family Level PERMANOVA
             Only coral data (=100%)",(capture.output(permfam))), cex=0.6)
  textplot(c("Genera Level PERMANOVA
             Only coral data =(100%)",(capture.output(permgen))), cex=0.6)
plot(stack407)
  textplot(capture.output(ano407), cex=0.6)
plot(stackConzinc)
  textplot(capture.output(anoConzinc), cex=0.6)
plot(stackDelambre)
  textplot(capture.output(anoDelambre), cex=0.6)
plot(stackDockrell)
  textplot(capture.output(anoDockrell), cex=0.6)
plot(`stackEaglehawk Island`) #apostrophes are for names with spaces
  textplot(capture.output(`anoEaglehawk Island`), cex=0.6)
plot(stackEnderby)
  textplot(capture.output(anoEnderby), cex=0.6)
plot(stackGidley)
  textplot(capture.output(anoGidley), cex=0.6)
plot(stackGoodwyn)
  textplot(capture.output(anoGoodwyn), cex=0.6)
plot(`stackHamersley Shoal`)
  textplot(capture.output(`anoHamersley Shoal`), cex=0.6)
plot(`stackHigh Point`)
  textplot(capture.output(`anoHigh Point`), cex=0.6)
plot(`stackLegendre General`)
  textplot(capture.output(`anoLegendre General`), cex=0.6)
plot(`stackLegendre SZ North`)
  textplot(capture.output(`anoLegendre SZ`), cex=0.6) #This wont work as we only have 1 year of data at present so couldn't do an ANOVA
plot(stackMalus)
  textplot(capture.output(anoMalus), cex=0.6)
plot(`stackNE Regnard`)
  textplot(capture.output(`anoNE Regnard`), cex=0.6)
plot(stackSailfish)
  textplot(capture.output(anoSailfish), cex=0.6)

dev.off()

###-----7. Report Plots ----####
dodge<-position_dodge(0)

#Total abundance and biomass
cover <- ggplot(data =meansitedat,aes(x=Year, y=meancover))+
  geom_errorbar(aes(ymin=meancover-se,ymax=meancover+se), width=0.1, colour="light grey")+
  geom_point(size=4,shape="square",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = expression ("Mean percent live coral cover (+/- SE)"),x="Year")+
  expand_limits(y=0)
cover

#family plots
family <- mpdat%>%
  dplyr::filter(family %in% c("Poritidae","Acroporidae", "Faviidae"))%>%
  ggplot(aes(x=Year, y=meancover))+
  facet_wrap(vars(family), nrow=3, ncol=1)+
  geom_errorbar(aes(ymin=meancover-se,ymax=meancover+se), width=0.1, colour="light grey", position=dodge)+
  geom_point(size=2,shape="square",colour="black",show.legend=FALSE, position=dodge)+
  labs(y = expression ("Mean percent live coral cover (+/- SE)"),x="Year")+
  expand_limits(y=0)
family

