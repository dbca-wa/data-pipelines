#Written by Molly Moustaka (molly.moustaka@dbca.wa.gov.au)

####----1. Get set up ----####
rm(list=ls()) #clear environment

#Load libraries
library("dplyr")
library("purrr")
library("ggplot2")
library("RColorBrewer")
library("gplots")

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

dat<-read.csv("Macro_DAMP_Holdfasts_AllData.csv", header = T, fileEncoding="UTF-8-BOM")


####1.2 OPTIONAL: Pull  mastersheet off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "666fac47-a3a2-41cc-b0f8-192d573cf4b2"
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)

###-----3. Data checking----####
str(dat)

#make the date column into date format
dat$Date<-as.Date(dat$Date, format="%d/%m/%Y")

#make a year factor col
dat$Year<- as.factor(format(dat$Date, "%Y"))

#Check sites
unique(dat$Site)

#check # reps
table(dat$Site,dat$Year)

###-----3. Data summarising----####
dat$X..Sargassum.holdfasts<-dat$X..Sargassum.holdfasts*4 #make it per m2

## Number holdfasts
statmeanhold<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanhf=mean(X..Sargassum.holdfasts))

plotmeanhold<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanhf=mean(X..Sargassum.holdfasts))%>%
  group_by(Year,Site)%>%
  dplyr::summarise(meanhold=mean(meanhf),
                   se=st.err(meanhf))

mphold<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanhf=mean(X..Sargassum.holdfasts))%>%
  group_by(Year)%>%
  dplyr::summarise(meanhold=mean(meanhf),
                   se=st.err(meanhf))

##Slack canopy height
statmeanheight<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanslack=mean(Slack.canopy.height..cm.))

plotmeanheight<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanslack=mean(Slack.canopy.height..cm.)) %>%
  group_by(Year,Site)%>%
  dplyr::summarise(meantransmax=mean(meanslack),
                   se=st.err(meanslack))

mpmeanheight<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanslack=mean(Slack.canopy.height..cm.)) %>%
  group_by(Year)%>%
  dplyr::summarise(meantransmax=mean(meanslack),
                   se=st.err(meanslack))

##Max canopy height
statmaxheight<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanmax=mean(Max.canopy.height..cm.))

plotmaxheight<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanmax=mean(Max.canopy.height..cm.)) %>%
  group_by(Year,Site)%>%
  dplyr::summarise(meantransmax=mean(meanmax),
                   se=st.err(meanmax))

mpmaxheight<-dat%>%
  group_by(Year,Site, Transect)%>%
  dplyr::summarise(meanmax=mean(Max.canopy.height..cm.)) %>%
  group_by(Year)%>%
  dplyr::summarise(meantransmax=mean(meanmax),
                   se=st.err(meanmax))

###-----4. Stats----####
#holdfasts
sum1<-summary(aov(meanhf~Year, data=statmeanhold))
names(sum1)<-paste("Holdfasts")

#slack canopy
sum2<-summary(aov(meanslack~Year, data=statmeanheight))
names(sum2)<-paste("Mean canopy height")

#slack canopy
sum3<-summary(aov(meanmax~Year, data=statmaxheight))
names(sum3)<-paste("Max canopy height")


###-----4. Plotting----####
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
nb.cols <- 8
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

#Holdfast count
total<-plotmeanhold%>%
  ggplot(aes(x=Year,y=meanhold)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meanhold-se,ymax=meanhold+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,max(plotmeanhold$meanhold)))+ #set Y axis limits
  labs(y = "Mean # Holdfasts", x="Year")+
  scale_color_manual(values=mycolors)
total <- total+  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mphold,aes(ymin=meanhold-se,ymax=meanhold+se),width=NA, colour="light grey")+
  geom_point(data = mphold, aes(x = Year, y = meanhold), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
total


#Slack canopy height
slack<-plotmeanheight%>%
  ggplot(aes(x=Year,y=meantranslack)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meantranslack-se,ymax=meantranslack+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,max(plotmeanheight$meantranslack)))+ #set Y axis limits
  labs(y = "Mean Canopy Height cm", x="Year")+
  scale_color_manual(values=mycolors)
slack <- slack+  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mpmeanheight,aes(ymin=meantranslack-se,ymax=meantranslack+se),width=NA, colour="light grey")+
  geom_point(data = mpmeanheight, aes(x = Year, y = meantranslack), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
slack


#Max canopy height
max<-plotmaxheight%>%
  ggplot(aes(x=Year,y=meantransmax)) + #select variable to plot
  geom_errorbar(position =jitter,aes(ymin=meantransmax-se,ymax=meantransmax+se),width=NA, colour="light grey")+
  geom_point(position=jitter,aes(colour=Site),size=3, show.legend=TRUE)+ #add points, set colour +size, jitter them, legend off
  coord_cartesian(ylim=c(0,max(plotmaxheight$meantransmax)))+ #set Y axis limits
  labs(y = "Max Canopy Height cm", x="Year")+
  scale_color_manual(values=mycolors)
max <- max+  #this line adds a archipelago-wide point to the plot as a cross
  geom_errorbar(data = mpmaxheight,aes(ymin=meantransmax-se,ymax=meantransmax+se),width=NA, colour="light grey")+
  geom_point(data = mpmaxheight, aes(x = Year, y = meantransmax), #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
             size=4,shape="cross",colour="black",show.legend=FALSE)
max

###-----5. Lets make this shit into a PDF ----####
setwd(pdf.out) #put the outputs into the 'Monitoring Summaries' folder (Note: you need to make this folder within your WD first)

st=format(Sys.time(), "%Y-%m-%d") #make an object with todays date
pdf(paste("DMP_MacroalgaeCanopy_MonitoringSummary",st, ".pdf", sep = ""), height = 8, width = 10) #Change this name to suit you

textplot("Macroalgae canopy monitoring summary for
         the Dampier Archipelago", halign="center", fixed.width=FALSE)            #set your title page as you please
#add a sequence of plots and text plots, each will print on a new page of the pdf

plot(total)
textplot(capture.output(sum1), cex=0.6)
plot(slack)
textplot(capture.output(sum2), cex=0.6)
plot(max)
textplot(sum3)
textplot(capture.output(sum3), cex=0.6)

dev.off()


###-----6. Report Plots ----####
dodge<-position_dodge(0)

#Total abundance and biomass
totalplot <- ggplot(data =mphold,aes(x=Year, y=meanhold))+
  geom_errorbar(aes(ymin=meanhold-se,ymax=meanhold+se), width=0.1, colour="light grey")+
  geom_point(size=3,shape="square",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = bquote('Mean holdfast density'~(m^2)), x="Year")+
  expand_limits(y=0)
totalplot

slackplot <- ggplot(data =mpmeanheight,aes(x=Year, y=meantranslack))+
  geom_errorbar(aes(ymin=meantranslack-se,ymax=meantranslack+se), width=0.1, colour="light grey")+
  geom_point(size=3,shape="square",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = bquote('Mean slack canopy height (cm)'), x="Year")+
  expand_limits(y=0)
slackplot

maxplot <- ggplot(data =mpmaxheight,aes(x=Year, y=meantransmax))+
  geom_errorbar(aes(ymin=meantransmax-se,ymax=meantransmax+se), width=0.1, colour="light grey")+
  geom_point(size=3,shape="square",colour="black",show.legend=FALSE)+ #change this line if you use the alternate dataset in notes above (i.e. stat/not stat)
  labs(y = bquote('Canopy height (cm)'), x="Year")+
  expand_limits(y=0)
maxplot

heightplot<-maxplot +
  geom_errorbar(data = mpmeanheight,aes(ymin=meantransmax-se,ymax=meantransmax+se), width=0.1, colour="light grey")+
  geom_point(data = mpmeanheight,size=3,shape="circle",colour="black",show.legend=FALSE)
heightplot
