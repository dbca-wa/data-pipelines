#This script is design to created an automated annual summary report for seagrass monitoring sites - shoot density only
#Created by Molly Moustaka June 2019
#Dept. Biodiversity, Conservation and Attractions, Marine Science Program
#Email: molly.moustaka@dbbca.wa.gov.au

###-----Get set up----####
#Clear environment
rm(list=ls())

#Install & Load packages
#install.packages(c("ggplot2", "dplyr","plotrix","gridExtra","RColorBrewer","gplots", "ggpubr"),dependencies = T)
library(ggplot2) # make plots
library(dplyr) #data wrangling
library(plotrix) #package to calculate standard error
library(gridExtra) #grid layouts for multiple plots
library(RColorBrewer) #colours for plots.
library(gplots) #text to plot tools
library(ggpubr)#another arranging tool


#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Seagrass")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/")
data=paste(work.dir,"Data",sep="/")

setwd(data)

#View files in the directory
dir()

#### OPTIONAL: Pull seagrass density mastersheet off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "8f1df364-1524-4aed-87a8-717664c863cc"
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)

###-----Explore the data----####

#Load data
dat<-read.csv("DMP_Seagrass_ShootCounts_AllData.csv", header=T)

#Check out the data
head(dat)
names(dat)

#Do I have X seagrass species in this dataset? in future scripts I will automate this but this can be used for reference
max(dat$Halophila.ovalis) #yes
max(dat$Halophila.decipiens) #yes
max(dat$Halophila.minor) #yes
max(dat$Halodule.uninervis) #yes
max(dat$Syringodium.isoetifolium) #yes
max(dat$Cymodocea.angustata) #no so lets remove this column

#Remove seagrasses that aren't present
dat$Cymodocea.angustata<-NULL

#Something strange is going on with the header for the Year column so lets fix that
names(dat)[1]<-"Year"

#Make sure Year & SiteName are factors and shoot counts are integer.
#while we are at it, lets transform the data from 0.5m2 to 1m2 (x4)
str(dat)
dat$Year<-as.factor(dat$Year)
dat$SiteName <- as.factor(dat$SiteName)
dat$Total<-as.integer(dat$Total*4)
dat$Halophila.ovalis<-as.integer(dat$Halophila.ovalis*4)
dat$Halophila.minor<-as.integer(dat$Halophila.minor*4)
dat$Halophila.decipiens<-as.integer(dat$Halophila.decipiens*4)
dat$Halodule.uninervis<-as.integer(dat$Halodule.uninervis*4)
dat$Syringodium.isoetifolium<- as.integer(dat$Syringodium.isoetifolium*4)
#dat$Cymodocea.angustata<-as.integer(dat$Cymodocea.angustata*4) #hashed out because its not in this dataset

###-----Get average values and standard error for transect----####
dat2<- dat %>%
  group_by(Year,SiteName,Transect) %>% #you can do this by quadrat if you want, just change "Transect" to "Quadrat"
  summarise(
    #this section calculates the mean for each seagrass species + total
    MeanTotal=mean(Total),
    MeanHalophila.ovalis = mean(Halophila.ovalis),
    MeanHalophila.minor = mean(Halophila.minor),
    MeanHalophila.decipiens = mean(Halophila.decipiens),
    MeanHalodule.uninervis = mean(Halodule.uninervis),
    MeanSyringodium.isoetifolium = mean(Syringodium.isoetifolium),
#    MeanCymodocea.angustata = mean(Cymodocea.angustata),#hashed out because its not in this dataset

    #this section calculates the SD for each seagrass species + total
    SDTotal=sd(Total),
    SDHalophila.ovalis = sd(Halophila.ovalis),
    SDHalophila.minor = sd(Halophila.minor),
    SDHalophila.decipiens = sd(Halophila.decipiens),
    SDHalodule.uninervis = sd(Halodule.uninervis),
    SDSyringodium.isoetifolium = sd(Syringodium.isoetifolium),
#    SDCymodocea.angustata = sd(Cymodocea.angustata), #hashed out because its not in this dataset

    #this section counts the number of quadrats (obs) in each transect
    Obs=n_distinct(Quadrat),

    #this section calculates the SE for each seagrass species + total
    SETotal=(SDTotal)/(sqrt(Obs)),
    SEHalophila.ovalis =(SDHalophila.ovalis)/(sqrt(Obs)),
    SEHalophila.minor =(SDHalophila.minor)/(sqrt(Obs)),
    SEHalophila.decipiens =(SDHalophila.decipiens)/(sqrt(Obs)),
    SEHalodule.uninervis =(SDHalodule.uninervis)/(sqrt(Obs)),
    SESyringodium.isoetifolium =(SDSyringodium.isoetifolium)/(sqrt(Obs))
#    SECymodocea.angustata =(SDCymodocea.angustata)/(sqrt(Obs))#hashed out because its not in this dataset
  )



###-----Get average values and standard error for site----####
dat3<- dat %>%
  group_by(Year,SiteName) %>%
  summarise(
    #this section calculates the mean for each seagrass species + total
    MeanTotal=mean(Total),
    MeanHalophila.ovalis = mean(Halophila.ovalis),
    MeanHalophila.minor = mean(Halophila.minor),
    MeanHalophila.decipiens = mean(Halophila.decipiens),
    MeanHalodule.uninervis = mean(Halodule.uninervis),
    MeanSyringodium.isoetifolium = mean(Syringodium.isoetifolium),
#    MeanCymodocea.angustata = mean(Cymodocea.angustata), #hashed out because its not in this dataset

    #this section calculates the SD for each seagrass species + total
    SDTotal=sd(Total),
    SDHalophila.ovalis = sd(Halophila.ovalis),
    SDHalophila.minor = sd(Halophila.minor),
    SDHalophila.decipiens = sd(Halophila.decipiens),
    SDHalodule.uninervis = sd(Halodule.uninervis),
    SDSyringodium.isoetifolium = sd(Syringodium.isoetifolium),
#    SDCymodocea.angustata = sd(Cymodocea.angustata), #hashed out because its not in this dataset

    #this section counts the number of quadrats (obs) in each transect
    Obs=n_distinct(Replicate),

    #this section calculates the SE for each seagrass species + total
    SETotal=(SDTotal)/(sqrt(Obs)),
    SEHalophila.ovalis =(SDHalophila.ovalis)/(sqrt(Obs)),
    SEHalophila.minor =(SDHalophila.minor)/(sqrt(Obs)),
    SEHalophila.decipiens =(SDHalophila.decipiens)/(sqrt(Obs)),
    SEHalodule.uninervis =(SDHalodule.uninervis)/(sqrt(Obs)),
    SESyringodium.isoetifolium =(SDSyringodium.isoetifolium)/(sqrt(Obs))
#    SECymodocea.angustata =(SDCymodocea.angustata)/(sqrt(Obs)) #hashed out because its not in this dataset
  )


###-----Get average values and standard error for whole marine park----####
dat4<- dat %>%
  group_by(Year) %>%
  summarise(
    #this section calculates the mean for each seagrass species + total
    MeanTotal=mean(Total),
    MeanHalophila.ovalis = mean(Halophila.ovalis),
    MeanHalophila.minor = mean(Halophila.minor),
    MeanHalophila.decipiens = mean(Halophila.decipiens),
    MeanHalodule.uninervis = mean(Halodule.uninervis),
    MeanSyringodium.isoetifolium = mean(Syringodium.isoetifolium),
    #    MeanCymodocea.angustata = mean(Cymodocea.angustata), #hashed out because its not in this dataset

    #this section calculates the SD for each seagrass species + total
    SDTotal=sd(Total),
    SDHalophila.ovalis = sd(Halophila.ovalis),
    SDHalophila.minor = sd(Halophila.minor),
    SDHalophila.decipiens = sd(Halophila.decipiens),
    SDHalodule.uninervis = sd(Halodule.uninervis),
    SDSyringodium.isoetifolium = sd(Syringodium.isoetifolium),
    #    SDCymodocea.angustata = sd(Cymodocea.angustata), #hashed out because its not in this dataset

    #this section counts the number of quadrats (obs) in each transect
    Obs=n_distinct(Replicate),

    #this section calculates the SE for each seagrass species + total
    SETotal=(SDTotal)/(sqrt(Obs)),
    SEHalophila.ovalis =(SDHalophila.ovalis)/(sqrt(Obs)),
    SEHalophila.minor =(SDHalophila.minor)/(sqrt(Obs)),
    SEHalophila.decipiens =(SDHalophila.decipiens)/(sqrt(Obs)),
    SEHalodule.uninervis =(SDHalodule.uninervis)/(sqrt(Obs)),
    SESyringodium.isoetifolium =(SDSyringodium.isoetifolium)/(sqrt(Obs))
    #    SECymodocea.angustata =(SDCymodocea.angustata)/(sqrt(Obs)) #hashed out because its not in this dataset
  )

###-----Time for some data analysis----####

#First, site level analysis (i.e. difference between years, transect as replicate).This is just a basic ANOVA.
#You could expand this to include post-hoc tests pretty easily
#Set up loop
sitenames<-unique(dat2$SiteName)

for (i in sitenames) {

avdat<-filter(dat2,SiteName==(paste(i))) #filter the data by site
sum1<-summary(aov(MeanTotal~Year, data=avdat)) #run ANOVA's for differences between years
names(sum1)<-paste("ANOVA Total seagrass", i, sep= "") #this line names the ANOVA output by species and site (i) so that when we stitch the list together we know whats what
sum2<-summary(aov(MeanHalophila.ovalis~Year, data=avdat))
names(sum2)<-paste("ANOVA H. ovalis", i, sep= "")
sum3<-summary(aov(MeanHalophila.decipiens~Year, data=avdat))
names(sum3)<-paste("ANOVA H. decipiens ", i, sep= "")
sum4<-summary(aov(MeanHalophila.minor~Year, data=avdat))
names(sum4)<-paste("ANOVA H. minor", i, sep= "")
sum5<-summary(aov(MeanHalodule.uninervis~Year, data=avdat))
names(sum5)<-paste("ANOVA H. uninervis", i, sep= "")
sum6<-summary(aov(MeanSyringodium.isoetifolium~Year, data=avdat))
names(sum6)<-paste("ANOVA S. isoetifolium", i, sep= "")

sumall<-list(c(sum1,sum2,sum3,sum4,sum5,sum6)) #combine all ANOVA outputs into one list

sumName<-paste("sum", i, sep= "") #create loop naming convention

assign(sumName, sumall) #name combined summary list

} #close loop

#right now you will get an error message as Keast only has one year but it will still work for every other site

#Now we need a whole archipelago analsysis (difference between years AND sites- this is a CROSSED ANOVA)
#Here year is fixed and site is fixed. We are using dat2 so transect is our replicate
sum1<-summary(aov(MeanTotal~Year*SiteName, data=dat2))
names(sum1)<-paste("ANOVA Total seagrass_DampierArchipelago")
sum2<-summary(aov(MeanHalophila.ovalis~Year*SiteName, data=dat2))
names(sum2)<-paste("ANOVA H. ovalis_DampierArchipelago")
sum3<-summary(aov(MeanHalophila.decipiens~Year*SiteName, data=dat2))
names(sum3)<-paste("ANOVA H. decipiens_DampierArchipelago")
sum4<-summary(aov(MeanHalophila.minor~Year*SiteName, data=dat2))
names(sum4)<-paste("ANOVA H. minor_DampierArchipelago")
sum5<-summary(aov(MeanHalodule.uninervis~Year*SiteName, data=dat2))
names(sum5)<-paste("ANOVA H. uninervis_DampierArchipelago")
sum6<-summary(aov(MeanSyringodium.isoetifolium~Year*SiteName, data=dat2))
names(sum6)<-paste("ANOVA S. isoetifolium_DampierArchipelago")

sumDampierArchipelago<-list(c(sum1,sum2,sum3,sum4,sum5,sum6)) #combine all ANOVA outputs into one list

#If you wanted to you could then do a post-hoc test however that is more than we need at the moment. an example is below
# it can be printed into the PDF the same way that the ANOVA outputs are
#lm1<-aov(MeanTotal~Year*SiteName, data=dat2)
#post1<-TukeyHSD(x=lm1)

###-----Set up plots----####
#Set up jitter
jitter <- position_jitter(width = 0.2, height = 0.1) #this is so points don't overlap, increase values to spread out more

#Set up loop
sitenames<-unique(dat2$SiteName)
for (i in sitenames) {

#Make all plots for one site

  #Total shoot density
total<-dat2%>%
  filter(SiteName==(paste(i)))%>% #select site
  ggplot(aes(x=Year,y=MeanTotal)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanTotal-SETotal,ymax=MeanTotal+SETotal),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=Year),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          plot.title = element_text(hjust=0.5))+
  labs(y = bquote('Total shoot density'~(m^2)), x="Year")

  #Halophila ovalis
halo<-dat2%>%
  filter(SiteName==(paste(i)))%>% #select site
  ggplot(aes(x=Year,y=MeanHalophila.ovalis)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanHalophila.ovalis-SEHalophila.ovalis,ymax=MeanHalophila.ovalis+SEHalophila.ovalis),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=Year),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5))+
  labs(y = bquote('Halophila ovalis shoot density'~(m^2)), x="Year")

  #Halophila decipiens
hald<-dat2%>%
  filter(SiteName==(paste(i)))%>% #select site
  ggplot(aes(x=Year,y=MeanHalophila.decipiens)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanHalophila.decipiens-SEHalophila.decipiens,ymax=MeanHalophila.decipiens+SEHalophila.decipiens),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=Year),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5))+ #legend left on
  labs(y = bquote('Halophila decipiens shoot density'~(m^2)), x="Year")


  #Halophila minor
halm<-dat2%>%
  filter(SiteName==(paste(i)))%>% #select site
  ggplot(aes(x=Year,y=MeanHalophila.minor)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanHalophila.minor-SEHalophila.minor,ymax=MeanHalophila.minor+SEHalophila.minor),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=Year),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5))+
  labs(y = bquote('Halophila minor shoot density'~(m^2)), x="Year")


  #Halodule uninervis
halu<-dat2%>%
  filter(SiteName==(paste(i)))%>% #select site
  ggplot(aes(x=Year,y=MeanHalodule.uninervis)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanHalodule.uninervis-SEHalodule.uninervis,ymax=MeanHalodule.uninervis+SEHalodule.uninervis),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=Year),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5))+
  labs(y = bquote('Halodule uninervis shoot density'~(m^2)), x="Year")


  #Syringodium isoetifolium
syr<-dat2%>%
  filter(SiteName==(paste(i)))%>% #select site
  ggplot(aes(x=Year,y=MeanSyringodium.isoetifolium)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanSyringodium.isoetifolium-SESyringodium.isoetifolium,ymax=MeanSyringodium.isoetifolium+SESyringodium.isoetifolium),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=Year),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust=0.5))+
  labs(y = bquote('Syringodium isoetifolium shoot density'~(m^2)), x="Year")

#Print all plots for one site onto one page
plot<-grid.arrange(total,halo,hald,halm,halu,syr,nrow=2, top=paste(i))

#save as an object in the environment
pltName<-paste("SP", i, sep= "")
assign(pltName, (grid.arrange(total,halo,hald,halm,halu,syr,nrow=2, top=paste(i))))


} #close the loop


#I also want to make a set of plots for the whole archipelago
#First lets set a theme up. Note you cant set the point colours as part of this
custtheme<-theme_grey()+
           theme(panel.grid.major = element_blank(), #get rid of grid
                 panel.grid.minor = element_blank(), #get rid of grid
                 panel.background = element_blank(), #get rid of background (blank),
                 axis.line = element_line(colour = "black"), #make axis lines black
                 plot.title = element_text(hjust=0.5)) #move the plot title

theme_set(custtheme) # apply the theme

#Total shoot density
total<-dat3%>%
  ggplot(aes(x=Year,y=MeanTotal)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanTotal-SETotal,ymax=MeanTotal+SETotal),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  labs(y = bquote('Total shoot density'~(m^2)), x="Year")+
  scale_color_brewer(type="qual", palette = "Paired")
total <- total +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data=dat4,aes(ymin=MeanTotal-SETotal,ymax=MeanTotal+SETotal),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = MeanTotal),
             size=4,shape="cross",colour="black",show.legend=FALSE)
total
#Halophila ovalis
halo<-dat3%>%
  ggplot(aes(x=Year,y=MeanHalophila.ovalis)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanHalophila.ovalis-SEHalophila.ovalis,ymax=MeanHalophila.ovalis+SEHalophila.ovalis),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  labs(y = bquote('Halophila ovalis shoot density'~(m^2)), x="Year")+
  scale_color_brewer(type="qual", palette = "Paired")
halo <- halo +
  geom_errorbar(data=dat4,aes(ymin=MeanHalophila.ovalis-SEHalophila.ovalis,ymax=MeanHalophila.ovalis+SEHalophila.ovalis),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = MeanHalophila.ovalis),
             size=4,shape="cross",colour="black",show.legend=FALSE)

#Halophila decipiens
hald<-dat3%>%
  ggplot(aes(x=Year,y=MeanHalophila.decipiens)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanHalophila.decipiens-SEHalophila.decipiens,ymax=MeanHalophila.decipiens+SEHalophila.decipiens),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  labs(y = bquote('Halophila decipiens shoot density'~(m^2)), x="Year")+
    scale_color_brewer(type="qual", palette = "Paired")
hald <- hald +
  geom_errorbar(data=dat4,aes(ymin=MeanHalophila.decipiens-SEHalophila.decipiens,ymax=MeanHalophila.decipiens+SEHalophila.decipiens),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = MeanHalophila.decipiens),
             size=4,shape="cross",colour="black",show.legend=FALSE)

#Halophila minor
halm<-dat3%>%
  ggplot(aes(x=Year,y=MeanHalophila.minor)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanHalophila.minor-SEHalophila.minor,ymax=MeanHalophila.minor+SEHalophila.minor),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  labs(y = bquote('Halophila minor shoot density'~(m^2)), x="Year")+
    scale_color_brewer(type="qual", palette = "Paired")
halm <- halm +
  geom_errorbar(data=dat4,aes(ymin=MeanHalophila.minor-SEHalophila.minor,ymax=MeanHalophila.minor+SEHalophila.minor),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = MeanHalophila.minor),
             size=4,shape="cross",colour="black",show.legend=FALSE)

#Halodule uninervis
halu<-dat3%>%
  ggplot(aes(x=Year,y=MeanHalodule.uninervis)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanHalodule.uninervis-SEHalodule.uninervis,ymax=MeanHalodule.uninervis+SEHalodule.uninervis),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  labs(y = bquote('Halodule uninervis shoot density'~(m^2)), x="Year")+
    scale_color_brewer(type="qual", palette = "Paired")
halu <- halu +
  geom_errorbar(data=dat4,aes(ymin=MeanHalodule.uninervis-SEHalodule.uninervis,ymax=MeanHalodule.uninervis+SEHalodule.uninervis),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = MeanHalodule.uninervis),
             size=4,shape="cross",colour="black",show.legend=FALSE)

#Syringodium isoetifolium
syr<-dat3%>%
  ggplot(aes(x=Year,y=MeanSyringodium.isoetifolium)) + #select variable to plot
  geom_errorbar(aes(ymin=MeanSyringodium.isoetifolium-SESyringodium.isoetifolium,ymax=MeanSyringodium.isoetifolium+SESyringodium.isoetifolium),width=NA, position=jitter, colour="light grey")+
  geom_point(position=jitter,aes(colour=SiteName),size=3, show.legend=FALSE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,(max(dat2$MeanTotal))))+ #set Y axis limits
  labs(y = bquote('Syringodium isoetifolium shoot density'~(m^2)), x="Year")+
  scale_color_brewer(type="qual", palette = "Paired")
syr<-syr +
  geom_errorbar(data=dat4,aes(ymin=MeanSyringodium.isoetifolium-SESyringodium.isoetifolium,ymax=MeanSyringodium.isoetifolium+SESyringodium.isoetifolium),width=NA, colour="light grey")+
  geom_point(data = dat4, aes(x = Year, y = MeanSyringodium.isoetifolium),
             size=4,shape="cross",colour="black",show.legend=FALSE)

#Print all plots for one site onto one page
SPDampierArchipelago<-ggarrange(total,halo,hald,halm,halu,syr,nrow=2,ncol=3,
                                common.legend =TRUE, legend="bottom") %>%
                                annotate_figure(top="Dampier Archipelago")

SPDampierArchipelago
###-----Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder

st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date
pdf(paste("DMP_Seagrass_ShootDensity_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Seagrass shoot density monitoring summary
         Dampier Archipelago", halign="center", fixed.width=FALSE)            #set your title page as you please
plot(SPDampierArchipelago)
  textplot(capture.output(sumDampierArchipelago), cex=0.5) #this prints your ANOVA list as an image in your PDF
plot(SPConzinc) #You will need to go through and put your own site names here
  textplot(capture.output(sumConzinc), cex=0.6)
plot(SPEastLewis)
  textplot(capture.output(sumEastLewis), cex=0.6)
plot(SPEnderbyBay)
  textplot(capture.output(sumEnderbyBay), cex=0.6)
plot(SPEnderbyIsland)
  textplot(capture.output(sumEnderbyIsland), cex=0.6)
plot(SPKeast)
#  textplot(capture.output(sumKeast), cex=0.6) #this wont work right now coz Keast only has one year
plot(SPMalus)
  textplot(capture.output(sumMalus), cex=0.6)
plot(SPSWRegnard)
  textplot(capture.output(sumSWRegnard), cex=0.6)
plot(SPVictoriaRocks)
  textplot(capture.output(sumVictoriaRocks), cex=0.6)
plot(SPWhitnell)
  textplot(capture.output(sumWhitnell), cex=0.6)
dev.off()

