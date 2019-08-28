#This script is designed to created an automated annual summary report for coral monitoring sites - recruits
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
library(vegan) #multivariate stats
library(dplyr) #data wrangling.
library(devtools)
filter = dplyr::filter #make sure R uses the DPLYR version of filter
summarise = dplyr::summarise
mutate = dplyr::mutate

#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Coral")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/") #spit out monitoring summary pdfs to here
data=paste(work.dir,"Data/Recruitment",sep="/") #Spit out an updated concatenated mastersheet here

#Make an object with todays date
st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date

###-----2. Read in the data----####
setwd(data)
dir()
dat<- read.csv("DummyData.csv", header=T, fileEncoding = "UTF-8-BOM")
head(dat)
str(dat)

####2.1 OPTIONAL: Push coral recuit data onto CKAN (DBCA data catalogue)####
#setwd("~/projects/data-pipelines/scripts/indicators/coral/Dampier_allmetrics/temp") #this is a temporary file to write a copy of the data to before it gets pushed to CKAN
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- ""  #fill here
#write_delim(dat, path = "data.csv", delim = ",")
#r <- ckanr::resource_update(csv_rid, "data.csv")

####2.2 OPTIONAL: Pull coral recuit data off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- ""  #fill here
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)

####----3. Data checking----####
#Check all site names match - you could check for marine park here too
unique(dat$Site)

#Check which sites were sampled in each year
table(dat$Site,dat$Year) #This data is a bit munted so we'll make datasets for plotting and stats seperately

#Make year into a factor for analysis
dat$Year<-as.factor(dat$Year)

####---4. Make data----####

#First lets set up a function to calculate SE
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

#recruits count data

#recruitss per site
sitecount<- dat%>%
  group_by(Year,Site)%>%
  summarise(
    recruitscount=sum(No..ind))

#mean recruitss per year
meanmpcount<-dat%>%
  group_by(Year,Site)%>%
  summarise(
    recruitscount=sum(No..ind))%>%
  group_by(Year)%>%
  summarise(
    se=st.err(recruitscount),
    recruitscount=mean(recruitscount)
    )

#recruits taxa level count data
taxa<-unique(dat$Tax.Group) #make a character vector of all the coral families

#make a blank data frames for each level of resolution - don't worry about the warnings here
sitetaxa<-sitecount[FALSE,] #transect level- take the column heads from the trandat dataset
meanmptaxa<-sitecount[FALSE,] #site level

for (t in taxa) {
  
  ##Make the data for each transect
  x<- dat%>% #per transect
    filter(Tax.Group==paste(t))%>%
    group_by(Year,Site)%>%
    summarise(
      recruitscount=sum(No..ind))
  
  x$taxa<-paste(t) #add column with the coral family name
  
  sitetaxa<-rbind(sitetaxa,x) #bind the data for this family to the whole dataset
  
  
  y<- dat%>% #per transect
    filter(Tax.Group==paste(t))%>%
    group_by(Year,Site)%>%
    summarise(
      recruitscount=sum(No..ind))%>%
    group_by(Year)%>%
    summarise(
      se=st.err(recruitscount),
      recruitscount=mean(recruitscount))
  
  
  y$taxa<-paste(t) #add column with the coral family name
  
  meanmptaxa<-rbind(meanmptaxa,y) #bind the data for this family to the whole dataset
  

}


####----5. Statistics - Univariate -----####
#Marine park level
sum1<-summary(aov(recruitscount~Year, data=sitecount))
names(sum1)<-paste("ANOVATotalNoInt")

#PostHoc
lm1<-aov(recruitscount~Year, data=sitecount)
post1<-TukeyHSD(lm1, "Year") #Show me which sites are different. You can remove that to get every site in every year


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
nb.cols <- 20
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)
coralcolors <- colorRampPalette(brewer.pal(8, "Paired"))(6)

#Total recruits count
total<-sitecount%>%
  ggplot(aes(x=Year,y=recruitscount)) + #select variable to plot
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  labs(y = "Coral recruit count", x="Year")+
  scale_color_manual(values=mycolors)
total <- total +  
  geom_errorbar(data = meanmpcount,aes(ymin=recruitscount-se,ymax=recruitscount+se),width=NA, colour="light grey")+
  geom_point(data = meanmpcount, aes(x = Year, y =recruitscount),size=4,shape="cross",colour="black",show.legend=FALSE)



#Stack plot by site
sites<-unique(dat$Site)

for (s in sites){
  
  plot<-sitetaxa%>%
      filter(Site==(paste(s)))%>%
      ggplot(aes(fill=taxa, y=recruitscount, x=Year)) +
      geom_bar(stat="identity")+
      scale_fill_manual(values=coralcolors)+    #set your pallete of choice but needs to have enough colours
      coord_cartesian(ylim=c(0,100))+ #change axis height if you want
      labs(y = "# Coral recuits", x="Year", fill="taxa")+
      ggtitle(paste(s))
    
  plot(plot)
    pltName<-paste("stack", s, sep= "")
    assign(pltName,plot)
}


###-----7. Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder (Note: you need to make this folder within your WD first)

pdf(paste("DMP_Coral_Recruit_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Coral recruit monitoring summary for
         the Dampier Archipelago", halign="center", fixed.width=FALSE)  

plot(total)
textplot(c("ANOVA Coral Recruits",(capture.output(sum1))), cex=0.6)
textplot(c("Tukey's Post-Hoc Coral Recruits",(capture.output(post1))), cex=0.6)
plot(stackAIR)
##etc.

dev.off()