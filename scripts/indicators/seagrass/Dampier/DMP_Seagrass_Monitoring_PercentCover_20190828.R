#This script is designed to created an automated annual summary report for seagrass monitoring sites - % cover, species/geneera % cover, and diversity
#Created by Molly Moustaka August 2019
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
library(dplyr) #data wrangling.
library(devtools)
filter = dplyr::filter #make sure R uses the DPLYR version of filter
summarise = dplyr::summarise
mutate = dplyr::mutate
#I'm also playing with the new tools in tidyverse for reshaping data which aren't formally in the package yet
devtools::install_github("tidyverse/tidyr") #select 3 in first option menu
library(tidyr)

#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Seagrass")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/") #spit out monitoring summary pdfs to here
data=paste(work.dir,"Data.",sep="/") #Spit out an updated concatenated mastersheet here

#Make an object with todays date
st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date

#Read in data
setwd(data)
dir()
dat<- read.csv("DUMMYDATA_PercentCover_RWriting.csv", header =T, fileEncoding="UTF-8-BOM")
head(dat)

####1.1 OPTIONAL: Push updated seagrass % cover onto CKAN (DBCA data catalogue)####
#setwd("~/projects/data-pipelines/scripts/indicators/seagrass/Dampier/temp") #this is a temporary file to write a copy of the data to before it gets pushed to CKAN
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "  " #FILL ID HERE - not currently on CKAN
#write_delim(dat, path = "data.csv", delim = ",")
#r <- ckanr::resource_update(csv_rid, "data.csv")

####1.2 OPTIONAL: Pull seagrass % cover off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "  " #FILL ID HERE - not currently on CKAN
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)


####----2. Data checking----####
#Check all site names match - you could check for marine park here too
unique(dat$Site)

#Change Date col aname to year
dat<- dat%>%
  dplyr::rename(Year=Date,
                Transect=Transect..)

#Check which sites were sampled in each year
table(dat$Site,dat$Year) 

#Make year into a factor for analysis
dat$Year<-as.factor(dat$Year)

####---3. Make data into seagrass % cover----####

#First lets set up a function to calculate SE
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

#Transect level
trandat<- dat%>% #per transect
  group_by(Year,Site,Transect)%>%
  summarise(
    count=n(),
    seagrass=sum(Seagrass!="None" ),
    percent=(seagrass/count)*100)

#Site level - mean of the transect so we have a standard error
sitemean<- dat%>% #per transect
  group_by(Year,Site,Transect)%>%
  summarise(
    count=n(),
    seagrass=sum(Seagrass!="None" ),
    percent=(seagrass/count)*100) %>%
  group_by(Year,Site)%>%
  summarise(
    meancover=mean(percent),
    se=st.err(percent)
  )

#Marine Park level - mean of the TRANSECTS so we have a standard error
mpmean<- dat%>% #per transect
  group_by(Year,Site,Transect)%>%  #If you want this to be the mean of sites (IE points pooled site level, remove "Transect here)
  summarise(
    count=n(),
    seagrass=sum(Seagrass!="None" ),
    percent=(seagrass/count)*100) %>%
  group_by(Year)%>%
  summarise(
    meancover=mean(percent),
    se=st.err(percent)
  )

####----4. Make data into species % cover-----####

#set up loop
specs<-unique(dat$Seagrass)

spectran<-trandat[FALSE,] #transect level- take the column heads from the trandat dataset
specmeansite<-sitemean[FALSE,] #site level
specmeanmp<-mpmean[FALSE,] #marine park level


#Get the loop going
for (s in specs) {
  ##Make the data for each transect
  x<- dat%>% #per transect
    group_by(Year,Site,Transect)%>% #this is your data resolution i.e. to replicate level
    summarise(
      count=n(),                    #count the number of rows in each replicate 
      seagrass=sum(Seagrass==paste(s)), #count the number of those rows that equal a single family
      percent=(seagrass/count)*100)    #calculate the % of hard coral for that family
  
  x$species<-paste(s) #add column with the coral family name
  
  spectran<-rbind(spectran,x) #bind the data for this family to the whole dataset
  
  y<- dat%>% #per transect
    group_by(Year,Site,Transect)%>%
    summarise(
      count=n(),
      seagrass=sum(Seagrass==paste(s)), #count the number of those rows that equal a single family
      percent=(seagrass/count)*100) %>%
    group_by(Year,Site)%>%
    summarise(
      meancover=mean(percent),
      se=st.err(percent))
  
  y$species<-paste(s) #add column with the coral family name
  
  specmeansite<-rbind(specmeansite,y) #bind the data for this family to the whole dataset
  
  z<- dat%>% #per transect
    group_by(Year,Site,Transect)%>%
    summarise(
      count=n(),
      seagrass=sum(Seagrass==paste(s)), #count the number of those rows that equal a single family
      percent=(seagrass/count)*100) %>%
    group_by(Year)%>%
    summarise(
      meancover=mean(percent),
      se=st.err(percent))
  
  z$species<-paste(s) #add column with the coral family name
  
  specmeanmp<-rbind(specmeanmp,z) #bind the data for this family to the whole dataset
  
}

####----5. Make genera-richness data-----####
#Site level
divsite<- dat%>% #per site
  filter(Seagrass!="None"&Seagrass !="Unknown Species Seagrass")%>% 
  group_by(Year,Site)%>%
  summarise(
    diversity = n_distinct(Seagrass))

#MP level
divmp<- dat%>% #per site
  filter(Seagrass!="None"&Seagrass !="Unknown Species Seagrass")%>% 
  group_by(Year)%>%
  summarise(
    diversity = n_distinct(Seagrass))


####----6. Stats -----####
#If you want to do multivariate or within site ask Molly to help

#Total % cover
  #Marine park level
sum1<-summary(aov(percent~Year, data=trandat)) #Dataset choice = replicate level i.e. for this transect = replicate
names(sum1)<-paste("ANOVATotalNoInt")

  #Site interaction
sum2<-summary(aov(percent~Year*Site, data=trandat)) #Dataset choice = replicate level i.e. for this transect = replicate
names(sum2)<-paste("ANOVATotalInt")


#Example for specific species % cover
  #Marine park level
stat<-trandat %>%
  filter(species=="Halophila spp.")

sum3<-summary(aov(percent~Year, data=stat)) 
names(sum3)<-paste("HalophilaANOVATotalNoInt")

  #Site interaction
sum4<-summary(aov(percent~Year*Site, data=stat)) 
names(sum4)<-paste("HalophilaANOVATotalInt")


#Put all MP level together for plotting
mpstat<-list(c(sum1,sum2,sum3,sum4)) #combine all ANOVA outputs into one list


####----7. Plotting - setting up -----####
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
nb.cols <- 10
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)


#Total seagrass cover
total<-sitemean%>%
  ggplot(aes(x=Year,y=meancover)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meancover-se,ymax=meancover+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,25))+ #set Y axis limits
  labs(y = "Total seagrass % cover", x="Year")+
  scale_color_manual(values=mycolors)
total <- total +  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mpmean,aes(ymin=meancover-se,ymax=meancover+se),width=NA, colour="light grey")+
  geom_point(data = mpmean, aes(x = Year, y = meancover), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)

total

#seagrass richness
rich<-divsite%>%
  ggplot(aes(x=Year,y=diversity)) + #select variable to plot
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,5))+ #set Y axis limits
  labs(y = "Seagrass diversity", x="Year")+
  scale_color_manual(values=mycolors)
rich <- rich +  #this line adds a archipelago-wide point to the plot as a cross
  geom_point(data = divmp, aes(x = Year, y = diversity), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)

rich

#Concatenate whole MP plots

totrich<-ggarrange(total,rich,nrow=2,ncol=1,
               common.legend =TRUE, legend="bottom") %>%
  annotate_figure(top="Dampier Archipelago")


#Stacked bar plots (species) - whole marine park
stackmp<-specmeanmp%>%
  filter(species!="None")%>%
  ggplot(aes(fill=species, y=meancover, x=Year)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=mycolors)+    #set your pallete of choice but needs to have enough colours
  coord_cartesian(ylim=c(0,25))+ #change axis height if you want
  labs(y = "Mean Seagrass % Cover", x="Year", fill="species")+
  ggtitle("Dampier Archipelago")
stackmp


####----6. Stacked bar plots (species) -  per site -----####
sites<-unique(dat$Site)

for (s in sites) {
  stacksite<- specmeansite%>%
    filter(Site==(paste(s)))%>%
    filter(species !="None")%>%
    ggplot(aes(fill=species, y=meancover, x=Year)) +
    geom_bar(stat="identity")+
    scale_fill_manual(values=mycolors)+    #set your pallete of choice but needs to have enough colours
    coord_cartesian(ylim=c(0,25))+ #change axis height if you want
    labs(y = "Seagrass Cover", x="Year", fill="Species")+
    ggtitle(paste(s))
  
  pltName<-paste("stack", s, sep= "")
  assign(pltName,stacksite)
  
}

###-----7. Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder (Note: you need to make this folder within your WD first)

pdf(paste("DMP_Seagrass_PercentCover_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Seagrass percent cover monitoring summary for
         the Dampier Archipelago", halign="center", fixed.width=FALSE)            #set your title page as you please

plot(totrich)
#textplot(c("ANOVAs - MPA level",(capture.output(mpstat))), cex=0.6) # no stats at present but example to intersperse
plot(stackmp)
plot(stackChookie)
plot(stackConzinc)
plot(stackElew)
plot(stackEndbay)
plot(stackEndIS)
plot(stackKeast)
plot(stackMalus)
plot(stackSWRegnard)

dev.off()

