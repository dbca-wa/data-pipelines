#Written by Molly Moustaka (molly.moustaka@dbca.wa.gov.au)

####----1. Get set up ----####
rm(list=ls()) #clear environment

#Load libraries
library("dplyr")
library("purrr")
library("ggplot2")
library("RColorBrewer")
#I'm also playing with the new tools in tidyverse for reshaping data which aren't formally in the package yet
#devtools::install_github("r-lib/vctrs") #select 3 in first option menu
#devtools::install_github("tidyverse/tidyr") #select 3 in first option menu
library(tidyr)

#And make a function to calc standard error
st.err <- function(x) {
  sd(x)/sqrt(length(x))
}

#set working drive
work.dir=("C:/Users/mollymoustaka/OneDrive - Department of Biodiversity, Conservation and Attractions/Desktop/T Drive/Pluto Offset 4 Monitoring_20200325/Assets/Macroalgae")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/") #spit out monitoring summary pdfs to here
data=paste(work.dir,"Data",sep="/") #Spit out an updated concatenated mastersheet her

####----2. Read in data ----####
setwd(data)
dir()
#data
dat<-read.csv("Macro_DAMP_LIT_AllData.csv", header = T, fileEncoding="UTF-8-BOM")

####1.2 OPTIONAL: Pull  mastersheet off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "f0bca518-de73-4bf5-b4f4-e1cda367a565"
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)

setwd(work.dir)
dir()
#categorisations
cats<-read.csv("Macroalgae_LIT_Classifications.csv", header = T, fileEncoding="UTF-8-BOM")

###-----3. Data checking----####
str(dat)

#make the date column into date format
dat$Date<-as.Date(dat$Date, format="%d/%m/%Y")

#make a year factor col
dat$Year<- as.factor(format(dat$Date, "%Y"))

#Check sites
unique(dat$Site)

#check # reps
table(dat$Site,dat$Year) #doubled # of transects

#Drop last columns as they will mess up the pivot
dat<-dat%>%
  select(-c("Total..cm.","Remain", "X"))
head(dat)

###-----4. Get data into long format----####
datlong<-dat%>%
  pivot_longer(cols=12:57,
               names_to="Taxa",
               values_to="Area")

head(datlong)

#And attach classifications
datlongclass<-left_join(datlong,cats, by="Taxa")
head(datlongclass)
###-----5. summarise data----####
#Cover for all recorded biota/substrate
taxacover<-datlongclass%>%
  dplyr::group_by(Year,Site,Taxa)%>%
  dplyr::summarise(meanarea=mean(Area),
                   se=st.err(Area))

#Cover by broad class e.g. substrate, algae, coral etc.
cat1cover<-datlongclass%>%
  dplyr::group_by(Year,Site,Transect,Sub.Transect,Level.1)%>%
  dplyr::summarise(sum=sum(Area))%>%
  dplyr::group_by(Year,Site, Level.1)%>%
  dplyr::summarise(meanarea=mean(sum),
                   se=st.err(sum))

#Algae only cover by taxa
algaecover<-datlongclass%>%
  dplyr::filter(Level.1=="Macroalgae")%>%
  dplyr::group_by(Year,Site,Taxa)%>%
  dplyr::summarise(meanarea=mean(Area),
                   se=st.err(Area))

#Algae only cover by taxa
mpalgaecover<-datlongclass%>%
  dplyr::filter(Level.1=="Macroalgae")%>%
  dplyr::group_by(Year,Taxa)%>%
  dplyr::summarise(meanarea=mean(Area),
                   se=st.err(Area))

#Algae cover by colour
algaecolcover<-datlongclass%>%
  dplyr::filter(Level.1=="Macroalgae")%>%
  dplyr::group_by(Year,Site, Transect,Sub.Transect, Level.2)%>%
  dplyr::summarise(sumcolour=sum(Area))%>%
  dplyr::group_by(Year,Level.2)%>%
  dplyr::summarise(meanarea=mean(sumcolour),
                   se=st.err(sumcolour))

#Canopy algae cover
canopycover<-datlongclass%>%
  dplyr::filter(Level.3=="Canopy")%>%
  dplyr::group_by(Year,Site)%>%
  dplyr::summarise(meanarea=mean(Area),
                   se=st.err(Area))

mpcanopy<-datlongclass%>%
  dplyr::filter(Level.3=="Canopy")%>%
  dplyr::group_by(Year)%>%
  dplyr::summarise(meanarea=mean(Area),
                   se=st.err(Area))



###-----6. Plotting----####
custtheme<-theme_grey()+
  theme(panel.grid.major = element_blank(), #get rid of grid
        panel.grid.minor = element_blank(), #get rid of grid
        panel.background = element_blank(), #get rid of background (blank),
        axis.line = element_line(colour = "black"), #make axis lines black
        plot.title = element_text(hjust=0.5)) #move the plot title

theme_set(custtheme) # apply the theme

#make a poisition jitter
jitter <- position_dodge(width = 0.4) #this is so points don't overlap, increase values to spread out more

#create a colour palette - tell it how many colours you want
nb.cols <- 8
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

#Canopy cover
canopy<-canopycover%>%
  ggplot(aes(x=Year,y=meanarea)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meanarea-se,ymax=meanarea+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,max(canopycover$meanarea)))+ #set Y axis limits
  labs(y = "Cover (cm)", x="Year")+
  scale_color_manual(values=mycolors)
canopy <- canopy+  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mpcanopy,aes(ymin=meanarea-se,ymax=meanarea+se),width=NA, colour="light grey")+
  geom_point(data = mpcanopy, aes(x = Year, y = meanarea), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
canopy

#By colour
colour<-algaecolcover%>%
  ggplot(aes(x=Year,y=meanarea)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meanarea-se,ymax=meanarea+se),width=NA, colour="light grey")+
  facet_wrap(algaecolcover$Level.2)+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,max(algaecolcover$meanarea)))+ #set Y axis limits
  labs(y = "Cover (cm)", x="Year")+
  scale_color_manual(values=mycolors)
colour

#Cat 1
cat1covercor<-na.omit(cat1cover)%>%
  dplyr::filter(Level.1!="Substrate")
cat1<-cat1covercor%>%
  ggplot(aes(x=Year,y=meanarea)) + #select variable to plot
  facet_wrap(cat1covercor$Level.1)+
  geom_errorbar(position =jitter,aes(ymin=meanarea-se,ymax=meanarea+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,max(cat1covercor$meanarea)))+ #set Y axis limits
  labs(y = "Cover (cm)", x="Year")+
  scale_color_manual(values=mycolors)
cat1


###-----6. Report plots----####
colourplot <- ggplot(data =algaecolcover,aes(x=Year, y=meanarea, shape=Level.2, group=Level.2))+
  geom_errorbar(aes(ymin=meanarea-se,ymax=meanarea+se), width=0.2, size=0.5, colour="light grey",position=jitter)+
  geom_point(position=jitter,size=3,colour="black",show.legend=FALSE)+
  labs(y = bquote('Area per transect (cm)'), x="Year")+
  expand_limits(y=0)
colourplot

unique(mpalgaecover$Taxa)
keytaxa<-c("MA_Dictyopteris","MA_Padina", "MA_Lobophora", "MA_Laurencia")
sarg<-"MA_Sargassum"

test<-mpalgaecover %>%
  filter(Taxa %in% keytaxa)
test$Taxa<-substring(test$Taxa,4)


taxaplot <- ggplot(data =test,aes(x=Year, y=meanarea))+
  geom_errorbar(position=jitter,aes(ymin=meanarea-se,ymax=meanarea+se), width=0.1, colour="light grey")+
  geom_point(position=jitter,size=2,shape="square",colour="black",show.legend=FALSE)+
  labs(y = bquote('Area per transect (cm)'), x="Year")+
  facet_wrap(vars(Taxa), nrow=3, ncol=2)+
  expand_limits(y=0)
taxaplot

sargdat<-mpalgaecover %>%
  filter(Taxa %in% sarg)
sargdat$Taxa<-substring(sargdat$Taxa,4)

sargplot<-ggplot(data =sargdat,aes(x=Year, y=meanarea))+
  geom_errorbar(position=jitter,aes(ymin=meanarea-se,ymax=meanarea+se), width=0.1, colour="light grey")+
  geom_point(position=jitter,size=2,shape="square",colour="black",show.legend=FALSE)+
  labs(y = bquote('Area per transect (cm)'), x="Year")+
  facet_wrap(vars(Taxa), nrow=3, ncol=2)+
  expand_limits(y=0)
sargplot


#Calculate taxa % of data recorded
test<-datlongclass%>%
  dplyr::filter(Level.1=="Macroalgae")%>%
  dplyr::group_by(Taxa)%>%
  dplyr::summarise(sum1=sum(Area),
                   percent=(sum1/31058)*100)
sum(test$sum1)

