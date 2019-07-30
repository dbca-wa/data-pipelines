#This script is design to created an automated annual summary report for coral monitoring sites - Juvenile corals
#Created by Molly Moustaka July 2019
#Dept. Biodiversity, Conservation and Attractions, Marine Science Program
#Email: molly.moustaka@dbbca.wa.gov.au

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

#I'm also playing with the new tools in tidyverse for reshaping data which aren't formally in the package yet
#devtools::install_github("tidyverse/tidyr") #select 3 in first option menu
library(tidyr)

#And make a function to calc standard error
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Coral")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/")
data=paste(work.dir,"Data/Juvenile",sep="/")

setwd(data)

#View files in the directory
dir()

dat<-read.csv("DMP_Coral_Juvenile_2019.csv", header=T)


####1.2 OPTIONAL: Pull juvenile coral mastersheet off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "26542b7e-44c9-43ba-8293-f1bc90f40481"
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)

###-----2. Data checking----####
str(dat)

#make the date column into date format
names(dat)[1]<-"Date"
dat$Date<-as.Date(dat$Date, format="%d/%m/%Y")

#make a year column
dat$Year<- as.factor(format(dat$Date, "%Y"))

#Lets see how many genera we observed and make sure the names make sense
unique(dat$Final.Genera.species)

#Check out how many transects and quadrats we did
table(dat$Site,dat$Quadrat) #we are missing 15 quadrats at eaglehawk and 20 at Sailfish - so we will take 10 Eaglehawk quads and make 1 transect

#manipulation to fix the eaglehawk problem
  #1. Remove the last 5 quadrats
dat<- dat%>%
  filter(Site != "Eaglehawk" | Transect!="3")

  #2. Take the first 10 quadrats and make that T1
dat[dat$Site=="Eaglehawk",]$Transect<-"1"


####---3.1 Make data into number of juveniles----####
#per transect
count<-dat%>%
  filter(Final.Genera.species!="")%>% #remove rows without juveniles
  group_by(Year,Site,Transect)%>%
  summarise(
    juvcount=n()
  )

#Mean of transects per site
sitemeancount<-count%>%
  group_by(Year,Site)%>%
  summarise(
    meancount=mean(juvcount),
    se=st.err(juvcount)
  )

#Mean for the marine park
mpmeancount<-count%>%
  group_by(Year)%>%
  summarise(
    meancount=mean(juvcount),
    se=st.err(juvcount)
  )

####---3. Make data into genera diversity ----####
div<- dat%>% #per site
  filter(Final.Genera.species!="" & Final.Genera.species!="?")%>% #remove rows without juvs/unknown taxa
  group_by(Year,Site)%>%
  summarise(
    diversity = n_distinct(Final.Genera.species))

mpdiv<- dat%>% #per site
  filter(Final.Genera.species!="" & Final.Genera.species!="?")%>% #remove rows without juvs/unknown taxa
  group_by(Year)%>%
  summarise(
    diversity = n_distinct(Final.Genera.species))

####---3.2 Data for proportion of juveniles from each family (stacked) ----####
#make a loop
genera<-unique(dat$Final.Genera.species)
genera<-genera[genera != ""]

sitestack <-count[FALSE,]
mpstack <-count[FALSE,]

for (g in genera){

x<-dat%>%
  filter(Final.Genera.species!="")%>% #remove rows without juveniles
  filter(Final.Genera.species==paste(g))%>%
  group_by(Year,Site)%>%
  summarise(
    juvcount=n())

x$genera<-paste(g)
sitestack<-rbind(sitestack,x)

y<-dat%>%
  filter(Final.Genera.species!="")%>% #remove rows without juveniles
  filter(Final.Genera.species==paste(g))%>%
  group_by(Year)%>%
  summarise(
    juvcount=n())

y$genera<-paste(g)
mpstack<-rbind(mpstack,y)

}


####---3.3 Summary of the data made so far----####

#Count of individual corals per transect
count #per transect
sitemeancount #average of transects per site
mpmeancount #average of transects per marine park

#Diversity
div # count of genera present at each site
mpdiv # count of genera presemt in the marine park

#Data for stacked histograms
sitestack #site level
mpstack #marine park level


####---4.1 Stats - MP level---####
##CAN'T DO YET AS ONLY 1 YR DATA

#Marine park level - can only test within year as some sites only have 1 transect
#juv count
sum1<-summary(aov(juvcount~Year, data=count))
names(sum1)<-paste("CountANOVATotalNoInt")

#sum2<-summary(aov(juvcount~Year*Site, data=count))
#names(sum2)<-paste("CountANOVATotalInt")

#juv diversity
sum3<-summary(aov(diversity~Year, data=div))
names(sum3)<-paste("DiversityANOVATotalNoInt")

sum4<-summary(aov(diversity~Year, data=div))
names(sum4)<-paste("DiversityANOVATotalInt")

####---4.2 Stats - site level----####
##CAN'T DO YET AS ONLY 1 YR DATa

for (s in sites) {

  avdat<-filter(count,Site==(paste(s))) #filter the data by site
  sum5<-summary(aov(juvcount~Year, data=avdat)) #run ANOVA's for differences between years

  sumName<-paste("ano", s, sep= "") #create loop naming convention

  assign(sumName, sum5) #name combined summary list

} #

####----5. Plotting - setting up -----####
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

#create a colour palette - tell it how many colours you want
nb.cols <- 16
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
nb.cols2<-32
mycolors2 <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols2)
####----6. Plotting - Mean # Juveniles & diversity -----####

#Total coral cover
total<-sitemeancount%>%
  ggplot(aes(x=Year,y=meancount)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,max(sitemeancount$meancount)))+ #set Y axis limits
  labs(y = "Mean # Juv Corals", x="Year")+
  scale_color_manual(values=mycolors)
total <- total+  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mpmeancount,aes(ymin=meancount-se,ymax=meancount+se),width=NA, colour="light grey")+
  geom_point(data = mpmeancount, aes(x = Year, y = meancount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
total

rich<-div%>%
  ggplot(aes(x=Year,y=diversity)) + #select variable to plot
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,max(div$diversity)))+ #set Y axis limits
  labs(y = "Genera Diversity Juv Corals", x="Year")+
  scale_color_manual(values=mycolors)
rich <- rich +  #this line adds a archipelago-wide point to the plot as a cross
  geom_point(data = mpdiv, aes(x = Year, y = diversity), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
rich


####----6. Plotting - Stackplots -----####
#Whole Archipelago
mpstackplot<-ggplot(mpstack, aes(fill=genera, y=juvcount, x=Year)) +
  geom_bar(stat="identity")+
  labs(y = "Number of coral juveniles", x="Year", fill="Genera")+
  scale_fill_manual(values=mycolors2)+
  ggtitle("Dampier Archipelago")

#by site
#NOTE this code doesn't apply the same colours to every genera
sites<-unique(sitestack$Site)

for (s in sites) {
  sitestackplot<- sitestack%>%
    filter(Site==(paste(s)))%>%
    ggplot(aes(fill=genera, y=juvcount, x=Year)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values=mycolors)+    #set your pallete of choice but needs to have enough colours
    coord_cartesian(ylim=c(0,100))+ #change axis height if you want
    labs(y = "Number of coral juveniles", x="Year", fill="Genera")+
    ggtitle(paste(s))

  pltName<-paste("stack", s, sep= "")
  assign(pltName,sitestackplot)

}

###-----7. Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder (Note: you need to make this folder within your WD first)

st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date
pdf(paste("DMP_Coral_Juvenile_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Coral juvenile monitoring summary for
         the Dampier Archipelago", halign="center", fixed.width=FALSE)            #set your title page as you please
#add a sequence of plots and text plots, each will print on a new page of the pdf
#NOTE: right now there is no stats as there is only 1 year of data. Run sections 4 when collected
#And add outputs using the (textplot(capture.output(`anoLegendre SZ`), cex=0.6)) format
plot(total)
plot(rich)
plot(mpstackplot)
plot(stackConzinc)
plot(stackDelambre)
plot(stackDockrell)
plot(stackEaglehawk)
plot(stackEnderby)
plot(stackGidley)
plot(stackGoodwin)
plot(stackHammersley)
plot(`stackHigh Point`)
plot(`stackLegendre Island General`)
plot(`stackLegendre Island SZ`)
plot(stackMalus)
plot(`stackNE Regnard`)
plot(stackSailfish)
plot(stackUnnamed407)

dev.off()

