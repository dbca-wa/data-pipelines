setwd("~/projects/data-pipelines/scripts/indicators/wave_height/NCMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "d2232a23-2eae-460e-a939-313315f92a46"
pdf_rid <- "43c1836e-f127-4fea-8db3-b9307be60321"
txt_rid <- "66da8688-ea9f-436f-b2fc-2d09d28ffd58"
pdf_fn <- "figure.pdf"


###################################
library(ggplot2)
library(plyr)

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))
pd <- position_dodge(0.1)
graphics = theme(axis.text.x=element_text(angle=45, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
                 axis.title.x=element_text(), #removes x axis title
                 axis.title.y=element_text(), #removes y axis title
                 axis.line=element_line(colour="black"), #sets axis lines 
                 plot.title =element_text(hjust = 0.05),
                 panel.grid.minor = element_blank(), #removes minor grid lines
                 panel.grid.major = element_blank(), #removes major grid lines
                 panel.border=element_blank(), #removes border
                 panel.background=element_blank(), #needed to ensure integrity of axis lines
                 legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank())

ncmp <- ggplot(d, aes(x=Year, y=Days, group=Type, linetype=Type, shape=Type)) +
  geom_line(position=pd) +
  geom_hline(aes(yintercept=Mean_mean), linetype="dashed") + #includes horizontal line
  geom_hline(aes(yintercept=Max_mean)) + #includes horizontal line
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d$Year-0.125), max(d$Year+0.125)), breaks=min(d$Year):max(d$Year)) +
  scale_y_continuous(limits=c(min(0), max(120)))+
  xlab("Year") +
  ylab(expression(paste("No of days >4m", sep = ""))) +
  ggtitle("Wave height")+
  theme_bw() + graphics

ncmp

#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(ncmp, ncol=1)
dev.off()

## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "wave height_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")

############################################################################################
############################################################################################
############################################################################################

# CINDY'S CONTROL CHART SCRIPT'
# 
# source("~/projects/data-pipelines/setup/ckan.R")
# source("~/projects/data-pipelines/scripts/ckan_secret.R")
# ## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")
# 
# #------------------------------------------------------------------------------#
# # Settings
# # Adjust the following path to the location of this file:
# setwd("~/projects/data-pipelines/scripts/indicators/wave_height/NCMP")
# 
# # Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
# csv_rid <- "581ce2e8-c5d0-4146-9894-3fb3f68225a8"
# pdf_rid <- ""
# txt_rid <- ""
# 
# pdf_fn <- "figure.pdf"
# txt_fn <- "code.R"
# 
# #------------------------------------------------------------------------------#
# # Analysis - your code
# # add date_colnames you wish to read as POSIXct date, remove to keep as factor
# wave <- load_ckan_csv(csv_rid)
# 
# wave1<- wave$Date_Time & Total_Hs_m
# wave1[,c(3,4)]
# 
# na.omit
# 
# #R Script used to obtain a indication of storm action in Ngari Capes Marine Park based on quality control charting
# #This code will use the Cape Naturaliste - wave action data provided by WA DOT; which is served on the MSP data catalogue.
# #Author: Cindy Bessey
# #Written: July 16,2014
# #Modified and Rerun: August 12, 2014 to include May and June 2014 data into graphic
# #Modified and Rerun: Aug.28, 2014 to ensure graphics are the correct dimensions
# #Modified and Rerun: Sept.5'2014, to ensure graphic had increased text size and the control period lines showed
# 
# #The following data manipulations require the R Cran package xts (extensible time series) and qcc (quality control charting), and MASS
# library(xts)
# library(qcc)
# library(MASS)
# library(Kendall)
# 
# 
# #Import the data from the specified location
# #wave<-read.csv("c://Users//CindyB//NgariCapes//NGARI_CAPES_DATA//Wave_Action2004-2014//CapeNaturalisteWave_2004_2014.csv", sep=",")
# 
# #Ensure that the wave height data is a vector
# wave$Total_Hs_m<-as.vector(wave$Total_Hs_m)
# 
# #The raw data file has some dates that were entered twice.  For example the data for "17/09/2012 0:19" shows as two seperate but identicle rows
# #Eliminate duplicate columns
# wave<-wave[!duplicated(wave$Date_Time),]
# 
# #Convert the 'Date_Time' character data to a time stamp
# wave$Date_Time<-strptime(wave$Date_Time,format="%d/%m/%Y %H:%M")
# 
# ##Exclude any data point where the Date_Time=NA
# wave<-wave[!is.na(wave$Date_Time),]
# 
# 
# #Convert to XTS format using the time column
# wave.xts<-xts(x=wave[,c(3:16)],order.by=wave[,"Date_Time"],frequency=24,unique=TRUE)
# 
# 
# #Determine the mean and maximum wave values per day
# height1<-wave.xts$Total_Hs_m
# wave.daily.mean1<-apply.daily(height1,function(x) apply(x,2,mean,na.rm=TRUE))
# height<-na.omit(wave.xts$Total_Hs_m)
# wave.daily.mean <- apply.daily(height1, function(x) apply(x, 2, mean))
# wave.daily.max <- apply.daily(height1, function(x) apply(x,2,max))
# 
# ##Produce a quality control chart; using the first 4 years of complete data (2005-2008) to set the control limits, and limits at 3 standard deviations
# height.qcc<-qcc(wave.daily.mean$Total_Hs_m[248:1672], type="xbar.one",sizes=24,digits=3, nsigmas=3,newdata=wave.daily.mean$Total_Hs_m[1673:dim(wave.daily.mean)[1]],plot=FALSE)
# 
# #Determine number of days in a year that wave height is > 4m
# wave.4count<-wave.daily.mean$Total_Hs_m[wave.daily.mean$Total_Hs_m>4]
# wave.4count.yearly<-apply.yearly(wave.4count, function(x) sum(table(x)))
# 
# wave.4countmax<-wave.daily.max$Total_Hs_m[wave.daily.max$Total_Hs_m>4]
# wave.4count.yearlymax<-apply.yearly(wave.4countmax, function(x) sum(table(x)))
# 
# 
# #Determine number of days in a year that wave height is > upper threshold
# 
# wave.Tcount<-wave.daily.mean$Total_Hs_m[wave.daily.mean$Total_Hs_m>height.qcc$limits[2]]
# wave.Tcount.yearly<-apply.yearly(wave.Tcount, function(x) sum(table(x)))
# 
# wave.Tcountmax<-wave.daily.max$Total_Hs_m[wave.daily.max$Total_Hs_m>height.qcc$limits[2]]
# wave.Tcount.yearlymax<-apply.yearly(wave.Tcountmax, function(x) sum(table(x)))
# 
# recent.period<-last(wave.daily.mean,'6 months')
# recent.period.daysabove4<-length(recent.period[recent.period$Total_Hs_m>height.qcc$limits[2]])
# 
# ###Make sure the graph is a in a .pdf format with appropriate dimentions
# pdf("c://Users//CindyB//NgariCapes//NGARI_CAPES_DATA//Wave_Action2004-2014//NCMP-Storms.pdf", width=9, height=5)
# ##Make a pretty plot that is consistent with others used by MSP
# tt<-length(wave.daily.mean$Total_Hs_m)
# par(bty="l")
# plot(wave.daily.mean1$Total_Hs_m, type='l', main="", ylab='Significant wave height (m)',auto.grid=FALSE, major.ticks="years",major.format="%Y", minor.ticks=FALSE, cex.axis=1.5,cex.lab=1.5)
# abline(height.qcc$limits[1],0,lty=2)
# abline(height.qcc$limits[2],0,lty=2)
# cperiod<-c(rep(index(wave.daily.mean1[249]),2),rep(index(wave.daily.mean1[1705]),2))
# yperiod<-c(0,8,8,0)
# polygon(cperiod,yperiod,border=TRUE,lty=2)
# text(index(wave.daily.mean1[972]),7.5,'control period', cex=1.5)
# 
# text(index(wave.daily.mean1[tt-80]),height.qcc$limits[1]-0.2,'LCL')
# text(index(wave.daily.mean1[tt-80]),height.qcc$limits[2]+0.2, 'UCL')
# points(wave.daily.mean1[wave.daily.mean1$Total_Hs_m>height.qcc$limits[2]],col='red',pch=16)
# 
# #Shade the Winter months June/July/August and indicate the number of days in the year above the threshold
# #Each year is done independently; in the future it would be useful to automate this process
# y.cords<-c(0,7,7,0)
# 
# #Year 2004
# winter<-c(rep(index(wave.daily.mean1[35]),2),rep(index(wave.daily.mean1[126]),2))
# polygon(winter,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[85]),7.2,wave.Tcount.yearly[1], cex=1.5)
# 
# 
# #Year 2005
# winter2<-c(rep(index(wave.daily.mean1[400]),2),rep(index(wave.daily.mean1[491]),2))
# polygon(winter2,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[450]),7.2,wave.Tcount.yearly[2], cex=1.5)
# 
# #Year 2006
# winter3<-c(rep(index(wave.daily.mean1[765]),2),rep(index(wave.daily.mean1[856]),2))
# polygon(winter3,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[815]),7.2,wave.Tcount.yearly[3], cex=1.5)
# 
# # Year 2007
# winter4<-c(rep(index(wave.daily.mean1[1130]),2),rep(index(wave.daily.mean1[1221]),2))
# polygon(winter4,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[1180]),7.2,wave.Tcount.yearly[4], cex=1.5)
# 
# # Year 2008
# winter5<-c(rep(index(wave.daily.mean1[1496]),2),rep(index(wave.daily.mean1[1587]),2))
# polygon(winter5,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[1546]),7.2,wave.Tcount.yearly[5], cex=1.5)
# 
# # Year 2009
# winter6<-c(rep(index(wave.daily.mean1[1855]),2),rep(index(wave.daily.mean1[1952]),2))
# polygon(winter6,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[1911]),7.2,wave.Tcount.yearly[6], cex=1.5)
# 
# # Year 2010
# winter7<-c(rep(index(wave.daily.mean1[2220]),2),rep(index(wave.daily.mean1[2311]),2))
# polygon(winter7,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[2270]),7.2,wave.Tcount.yearly[7], cex=1.5)
# 
# # Year 2011
# winter8<-c(rep(index(wave.daily.mean1[2585]),2),rep(index(wave.daily.mean1[2676]),2))
# polygon(winter8,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[2615]),7.2,wave.Tcount.yearly[8], cex=1.5)
# 
# # Year 2012
# winter9<-c(rep(index(wave.daily.mean1[2951]),2),rep(index(wave.daily.mean1[3042]),2))
# polygon(winter9,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[3001]),7.2,wave.Tcount.yearly[9], cex=1.5)
# 
# # Year 2013
# winter10<-c(rep(index(wave.daily.mean1[3316]),2),rep(index(wave.daily.mean1[3407]),2))
# polygon(winter10,y.cords,col=gray(0,alpha=0.2),border=NA)
# text(index(wave.daily.mean1[3366]),7.2,wave.Tcount.yearly[10], cex=1.5)
# 
# # Year 2014
# winter11<-c(rep(index(wave.daily.mean1[3681]),2),rep(index(wave.daily.mean1[3710]),2))
# polygon(winter11,y.cords,col=gray(0,alpha=0.2),border=NA)
# 
# dev.off()
# 
# ###Seasonal Mann-Kendal Test
# #Determine the mean and maximum wave values per month
# height<-na.omit(wave.xts$Total_Hs_m)
# wave.monthly.mean <- apply.monthly(height, function(x) apply(x, 2, mean))
# wave.monthly.max <- apply.monthly(height, function(x) apply(x,2,max))
# 
# 
# #Run a Seasonal Mann Kendall Test to determine if a seasonal trend exists
# monthly.Kendall<-SeasonalMannKendall(as.ts(wave.monthly.mean))
# summary(monthly.Kendall)
# 
# 
# 
# #------------------------------------------------------------------------------#
# # Save outputs and upload to CKAN, restore workdir
# pdf(pdf_fn, height = 5, width = 7)
# out
# dev.off()
# 
# ckanr::resource_update(pdf_rid, pdf_fn)
# ckanr::resource_update(txt_rid, txt_fn)
# 
# setwd("~/projects/data-pipelines")
