# R Script used to produce a qcc plot of whale shark 
# Written on 7/29/2015
# Modified on 8/17/2015 to include a 3-panel graphics with legends

source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

library(xts)
library(qcc)
library(MASS)

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/sharks")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
# sharks<-read.csv("C://DPaW//WhaleSharkControlChart//whalesharkdata.csv", sep=",")
csv_rid <- ""
pdf_rid <- ""
txt_rid <- ""

pdf_fn <- "figure.pdf"
txt_fn <- "whalesharkscript.R"


#' Produce the quality control statistics; 
#' using the first 5 years of data (2010-2014) to set the control limits, 
#' set limits at 3 standard deviations

small.qcc<-qcc(sharks$Small[1:15], type="xbar.one",sizes=3,digits=2, nsigmas=3,newdata=sharks$Small[16:18],plot=TRUE)
medium.qcc<-qcc(sharks$Medium[1:15], type="xbar.one",sizes=3,digits=2, nsigmas=3,newdata=sharks$Medium[16:18],plot=TRUE)
large.qcc<-qcc(sharks$Large[1:15], type="xbar.one",sizes=3,digits=2, nsigmas=3,newdata=sharks$Large[16:18],plot=TRUE)

##Produce a pretty graphic with years on the x-axis, proportion on the y, and the data
par(mfrow=c(3,1))

# open output file
pdf(pdf_fn, height = 5, width = 7)

plot(sharks$Small,ylim=c(0,20),ylab="Proportion",xaxt="n",xlab="Time",type="b",main="Length < 4 m")
lab<-as.character(sharks$Year)
axis(side = 1, at = c(1:18), labels = lab)
abline(small.qcc$limits[1],0,lty=2)
abline(small.qcc$limits[2],0,lty=2)
abline(v=15.5,lty=3)

plot(sharks$Medium,ylim=c(70,100),ylab="Proportion",xaxt="n",xlab="Time",type="b",main="Length 4 - 7.9 m")
lab<-as.character(sharks$Year)
axis(side = 1, at = c(1:18), labels = lab)
abline(medium.qcc$limits[1],0,lty=2)
abline(medium.qcc$limits[2],0,lty=2)
abline(v=15.5,lty=3)


plot(sharks$Large,ylim=c(0,20),ylab="Proportion",xaxt="n",xlab="Time",type="b",main="Length > 8 m")
lab<-as.character(sharks$Year)
axis(side = 1, at = c(1:18), labels = lab)
abline(large.qcc$limits[1],0,lty=2)
abline(large.qcc$limits[2],0,lty=2)
abline(v=15.5,lty=3)

# save and close output file
dev.off()

# upload code and figure to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")

