#This script is designed to created an automated annual summary report for water temperature from loggers and SST data
#Created by Molly Moustaka September 2019
#Dept. Biodiversity, Conservation and Attractions, Marine Science Program
#Email: molly.moustaka@dbbca.wa.gov.au

###-----1. Get set up----####

#Install & Load packages
library(tidyr)
library(ggplot2)
library(stringr)
library(rerddap)
library(dplyr)


#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Temperature")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/")
raw=paste(work.dir,"Data/Raw Data",sep="/")
data=paste(work.dir,"Data",sep="/")
clean=paste(work.dir,"Data/Cleaned and Concatenated Data",sep="/")

#Set up study name
st=format(Sys.time(), "%Y-%m") #make an object with todays date
study<-paste("DMP_Monitoring_WaterTemp",st, sep="_")  ##change this to inc. date and change subsequent outputs

###-----2. Read in logger control sheet----####
setwd(work.dir)
dir()
control<-read.csv("DMP_Logger_ControlSheet.csv", header=T,fileEncoding="UTF-8-BOM")%>%
  mutate(Deployment.Date=as.Date(Deployment.Date, format="%d/%m/%Y"),
         Retrieval.Date=as.Date(Retrieval.Date, format="%d/%m/%Y"))

###-----3. Merge data & format ----####
#Note: I have manually cropped & merged data for each site as I haven't had time to write code to do it yet. I crop it to the day after/ before
#deployment/retrieval and split out date/time
setwd(clean)
dir()

#Make list of files in directory
files<-list.files(clean, pattern = ".csv") #make a list of all file names in the data directory ending in CSV

#Make a blank data frame
dat<- NA

#Run a loop to bind files together
for (f in files) {
  x<-read.csv(f,header=TRUE,fileEncoding="UTF-8-BOM")%>%
    mutate(Date=as.Date(Date, format="%d/%m/%Y"), #set date format
           Time= as.POSIXct(Time, format="%H:%M:%S"), #set time format
           Logger= as.factor(Logger))%>% #set logger as factor
    mutate(Time=format(Time,"%H:%M:%S")) #Cut date from date/time so column is now only time
  dat<-rbind(dat,x) #bind to the bottom of the dataset

}

#Remove blank top row
dat<-dat[-1,]


#Write the updated Master CSV to the T drive asset folder
setwd(data)
dir()
write.csv(dat,file=paste("DMP_WaterTemperature_Master_AllSites",st,".csv"))

####2.1 OPTIONAL: Push updated mastersheet onto CKAN (DBCA data catalogue)####
#setwd("~/projects/data-pipelines/scripts/indicators/coral/Dampier_allmetrics/temp") #this is a temporary file to write a copy of the data to before it gets pushed to CKAN
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "9bc98943-5080-491f-929e-34736c7d0e42"
#write_delim(dat, path = "data.csv", delim = ",")
#r <- ckanr::resource_update(csv_rid, "data.csv")


###-----4. Calculate daily means ----####
daily<-dat%>%
  group_by(Site, Date)%>%
  dplyr::summarise(dailyave=mean(Temp.C))


###-----5. Pull in daily mean SST data ----####
#Pull out lat long and date ranges for data
latlong<-control%>%
  dplyr::filter(Cleaned.Merged!="Not collected")%>%
  group_by(Site)%>%
  summarise(datemin=min(Deployment.Date),
            datemax=max(Retrieval.Date),
            latitude=mean(Latitude),
            longitude=mean(Longitude))

#set NOAA dataset ID
datasetID="erdMH1sstd1day"

#set up the loop
sites<-unique(latlong$Site)

sst<-NA

for (s in sites){
  loc<-latlong%>%
    dplyr::filter(Site==paste(s))

x<-griddap(datasetID, latitude =c((loc$latitude),(loc$latitude)),
           longitude=c((loc$longitude), (loc$longitude)),
           time=c(loc$datemin, loc$datemax), fmt="csv")
x$Site<-paste(s)
sst<-rbind(sst,x)
}

sst<-sst[-1,]

#Pull out date column
sst$Date<-sapply(strsplit(sst[,1],split="T"), '[',1)

#remove time column
sst<-subset(sst, select=-(time))

#make date into date format
sst<- sst%>%
  dplyr::mutate(Date=as.Date(Date, format="%Y-%m-%d"))


###-----5. Pull in daily mean DHW data ----####
#set NOAA dataset ID
datasetID="NOAA_DHW_5km"
#browse("NOAA_DHW_5km")

dhw<-NA
x<-NA

#These grid cells are a different size to the SST ones and I get errors using this method for some
#So I did it on the website which returns data fine & changed the offending points to the ones returned
#by the ERDDAP website below
latlong2<-latlong
latlong2[2,4]<-as.numeric("-20.574999") #correct Enderby site to points I know have data from the NOAA server
latlong2[2,5]<-as.numeric("116.57502")#correct Enderby site to points I know have data from the NOAA server
latlong2[3,4]<-as.numeric("-20.375002	") #correct Hammersley site to points I know have data from the NOAA server
latlong2[3,5]<-as.numeric("116.82502")#correct Hammersley site to points I know have data from the NOAA server

sites<-as.character(unique(latlong2$Site))

for (s in sites){
  loc<-latlong2%>%
    dplyr::filter(Site==paste(s))

  x<-griddap(datasetID, latitude =c((loc$latitude),(loc$latitude)),
             longitude=c((loc$longitude), (loc$longitude)),
             time=c(loc$datemin, loc$datemax), fields="CRW_DHW", fmt="csv")
  x$Site<-paste(s)
dhw<- rbind(dhw,x)

  }

dhw<-dhw[-1,]

#Pull out date column
dhw$Date<-sapply(strsplit(dhw[,1],split="T"), '[',1)

#remove time column
dhw<-subset(dhw, select=-(time))

#make date into date format
dhw<- dhw%>%
  dplyr::mutate(Date=as.Date(Date, format="%Y-%m-%d"))


###-----6.Plotting ----####
#Set theme
custtheme<-theme_grey()+
  theme(panel.grid.major = element_blank(), #get rid of grid
        panel.grid.minor = element_blank(), #get rid of grid
        panel.background = element_blank(), #get rid of background (blank),
        axis.line = element_line(colour = "black"), #make axis lines black
        plot.title = element_text(hjust=0.5)) #move the plot title

theme_set(custtheme) # apply the theme

vars <- c("in situ"="pink", "SST"="lightblue", "DHW + 10"="lightgreen", "Cyclone"="grey")

#set up plot
plot<-daily%>%
  ggplot((aes(x=Date,y=dailyave)))+
  geom_line(aes(colour="in situ"))+
  #stat_smooth(data=daily,aes(Date, dailyave),method=lm, colour="black", size=0.5)+ #off because we don't have full years of data to start with so slants line
  labs(y = "Mean dailywater temperature (c)", x="Year")+
  ylim(10,35)+
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-03-24")), colour="Cyclone"), linetype="dotted")+ #cyclones
  geom_vline(aes(xintercept=as.numeric(as.Date("2019-01-26")),colour="Cyclone"), linetype="dotted")+ #cyclones
  geom_line(data=sst, aes(Date,sst, colour="SST"))+
  geom_line(data=dhw, aes(Date,(crw_dhw+10), colour="DHW + 10"))+
  scale_colour_manual(name="Measure", values=vars) +
  theme(legend.position="bottom")+
  facet_wrap(~Site)
plot

