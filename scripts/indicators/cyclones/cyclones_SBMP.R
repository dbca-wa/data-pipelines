setwd("~/projects/data-pipelines/scripts/indicators/cyclones")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(dplyr)
library(rworldmap)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

#################################################################

#Make plots of cyclone activity

csv_rid <- "779cde13-b376-4edd-8ddd-2361fe1a22fd"
pdf_rid <- "70570eb7-11f2-446f-aa67-a2b7cf0783d0"
txt_rid <- "ddbbd5bc-1ce9-4da4-8185-c3596db1faf3"
pdf_fn = "final.pdf"

c_sum <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date')) #NOT READING ALL CELLS!

pd <- position_dodge(0.1)
graphics = theme(axis.text.x=element_text(angle=75, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
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
                 legend.key = element_blank()
)
##################################################################################
#Shark Bay number of cyclones

sbN = subset(c_sum, Location =="Shark Bay" & Radius == "200") 

c_sbN <- plyr::ddply(sbN, .(Year), summarise,
                     N    = length(!is.na(Time.in.radius)))

sbN_plot <- ggplot(c_sbN, aes(x=Year, y=N)) +
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_sbN$Year-0.125), max(c_sbN$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(3)))+
  xlab("Year") +
  ylab(expression(paste("Number of cyclones", sep = ""))) +
  ggtitle("a) Number of cyclones")+
  theme_bw() + graphics

sbN_plot

#sbley Shoals cyclones_200km

sb200 = subset(c_sum, Location =="Shark Bay" & Radius == "200") 

c_sb200 <- plyr::ddply(sb200, .(Year), summarise,
                     N    = length(!is.na(Time.in.radius)),
                     mean = mean(Time.in.radius, na.rm=TRUE),
                     sd   = sd(Time.in.radius, na.rm=TRUE),
                     se   = sd(Time.in.radius, na.rm=TRUE) / sqrt(length(!is.na(Time.in.radius)) ))

sb200_plot <- ggplot(c_sb200, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_sb200$Year-0.125), max(c_sb200$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Time in perimeter (hrs)", sep = ""))) +
  ggtitle("b) Time within 200km")+
  theme_bw() + graphics

sb200_plot

#Shark Bay cyclones_50km

sb50 = subset(c_sum, Location =="Shark Bay" & Radius == "50") 

c_sb50 <- plyr::ddply(sb50, .(Year), summarise,
                        N    = length(!is.na(Time.in.radius)),
                        mean = mean(Time.in.radius, na.rm=TRUE),
                        sd   = sd(Time.in.radius, na.rm=TRUE),
                        se   = sd(Time.in.radius, na.rm=TRUE) / sqrt(length(!is.na(Time.in.radius)) ))

sb50_plot <- ggplot(c_sb50, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_sb50$Year-0.125), max(c_sb50$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Time in perimeter (hrs)", sep = ""))) +
  ggtitle("c) Time within 50km")+
  theme_bw() + graphics

sb50_plot

#Shark Bay cyclones_pressure

c_sbp200 <- plyr::ddply(sb200, .(Year), summarise,
                     N    = length(!is.na(Mean.pressure)),
                     mean = mean(Mean.pressure, na.rm=TRUE),
                     sd   = sd(Mean.pressure, na.rm=TRUE),
                     se   = sd(Mean.pressure, na.rm=TRUE) / sqrt(length(!is.na(Mean.pressure)) ))

sbp200_plot <- ggplot(c_sbp200, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_sbp200$Year-0.5), max(c_sbp200$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(925), max(1050)))+
  xlab("Year") +
  ylab(expression(paste("Mean central pressure (hPa)", sep = ""))) +
  ggtitle("d) Central pressure")+
  theme_bw() + graphics

sbp200_plot

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(sbN_plot, sb200_plot, sb50_plot, sbp200_plot, ncol=2)
dev.off()

## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "cyclones_SBMP.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")

