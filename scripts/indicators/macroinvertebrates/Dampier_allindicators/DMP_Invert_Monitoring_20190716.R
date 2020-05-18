#This script is design to created an automated annual summary report for Benthic Invertebrates
#Created by Molly Moustaka July 2019
#Dept. Biodiversity, Conservation and Attractions, Marine Science Program
#Email: molly.moustaka@dbbca.wa.gov.au

###-----1. Get set up----####
#Clear environment
rm(list=ls())

#Install & Load packages
#install.packages(c("ggplot2", "dplyr","RColorBrewer","gplots"),dependencies = T)
library(ggplot2) # make plots
library(RColorBrewer) #colours for plots.
library(gplots) #text to plot tools
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
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Invertebrates")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/")
data=paste(work.dir,"Data",sep="/")

setwd(data)

#View files in the directory
dir()

#Read in the data
dat<-read.csv("DMP_Invertebrate_Master.csv",header = T)

####1.2 OPTIONAL: Pull Invertebrate mastersheet off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "e1e2c62b-cffa-462f-89c4-51e71f78dc96"
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)

###-----2. Check out the data----####
str(dat)

#A couple of manipulations
dat$Year<-as.factor(dat$Year)
names(dat)[18]<-"Group"

###-----3. Make some group level count data----####
groupcount<-dat%>%
  group_by(Year,Site,Transect,Group)%>%
  summarise(
    groupcount=sum(Count)
  )

#And a mean + SE at site level
meangroupcount<-groupcount%>%
  group_by(Year,Site,Group)%>%
  summarise(
    meangroupcount=mean(groupcount),
    se=st.err(groupcount)
  )

#marine park level
#And a mean + SE at site level
mpmeangroupcount<-groupcount%>%
  group_by(Year,Group)%>%
  summarise(
    meangroupcount=mean(groupcount),
    se=st.err(groupcount)
  )

###-----3. Make some count data for species of interest----####
unique(dat$Species)

#COTS
COTcount<-dat%>%
  filter(Group=="A.planci..CoTS.")%>%
  group_by(Year,Site)%>%
  summarise(
    cotcount=sum(Count)
  )

#Lobsters
lobcount<-dat%>%
  filter(Species==c("P.versicolor","P.ornatus"))%>%
  group_by(Year,Site)%>%
  summarise(
    lobcount=sum(Count)
  )

#Drupella
drupcount<-dat%>%
  filter(Species=='D.cornus')%>%
  group_by(Year,Site)%>%
  summarise(
    drupcount=sum(Count)
  )

###-----4. Stats (not written yet as only one year of data)----####
#unique(groupcount$Group)

#Example below:

#avdat<-groupcount%>%
#  filter(Group=="Asteroidea")

#sum1<-summary(aov(groupcount~Year, data=avdat))
#names(sum1)<-paste("ANOVATotalNoInt")

###-----5. Plotting - Get set up ----####
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

###-----5. Plotting - Archipelago level ----####
groups<-unique(meangroupcount$Group)

for (g in groups) {

total<-meangroupcount%>%
    filter(Group==paste(g))%>%
    ggplot(aes(x=Year,y=meangroupcount)) + #select variable to plot
    geom_errorbar(position =jitter,aes(ymin=meangroupcount-se,ymax=meangroupcount+se),width=NA, colour="light grey")+
    geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
    labs(y = "Mean # Invertebrates", x="Year")+
    scale_color_manual(values=mycolors)+
  ggtitle(paste(g))
dat2<-filter(mpmeangroupcount,Group==paste(g))
total <- total+
  geom_errorbar(data = dat2,aes(ymin=meangroupcount-se,ymax=meangroupcount+se),width=NA, colour="light grey")+
  geom_point(data = dat2, aes(x = Year, y = meangroupcount), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
total

pltName<-paste("DA", g, sep= "")
assign(pltName,total)

}


###-----5. Plotting - Species of interest ----####
cot<-COTcount%>%
  ggplot(aes(x=Year,y=cotcount)) + #select variable to plot
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Total # COTS", x="Year")+
  scale_color_manual(values=mycolors)+
  ggtitle("Crown of Thorns")


lob<-lobcount%>%
  ggplot(aes(x=Year,y=lobcount)) + #select variable to plot
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Total # Lobsters", x="Year")+
  scale_color_manual(values=mycolors)+
  ggtitle("Lobsters")

drup<-drupcount%>%
  ggplot(aes(x=Year,y=drupcount)) + #select variable to plot
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Total # Drupella", x="Year")+
  scale_color_manual(values=mycolors)+
  ggtitle("Drupella")


###-----7. Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder (Note: you need to make this folder within your WD first)

st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date
pdf(paste("DMP_Invertebrate_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Invertebrate monitoring summary for
         the Dampier Archipelago", halign="center", fixed.width=FALSE)            #set your title page as you please

#add a sequence of plots and text plots, each will print on a new page of the pdf
plot(DAAsteroidea)
plot(DABivalves)
plot(DAcephalopod)
plot(DAChitons)
plot(DACrustacea)
plot(DAEchinoidia)
plot(DAGastropod)
plot(DAHolothuria)
plot(DANudibranch)
plot(DAWorms)
plot(cot)
plot(drup)
plot(lob)

dev.off()







