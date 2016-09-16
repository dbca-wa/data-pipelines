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

csv_rid <- "779cde13-b376-4edd-8ddd-2361fe1a22fd" #This is the cyclone summary data
pdf_rid <- "b368ae5c-0bcd-42d6-ac16-9d39cbf7ac29"
txt_rid <- "d0031f11-aa63-449b-aee9-713e6cf2824b"
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
#Montes number of cyclones

montesN = subset(c_sum, Location =="Montes" & Radius == "200") 

c_montesN <- plyr::ddply(montesN, .(Year), summarise,
                     N    = length(!is.na(Time.in.radius)))

montesN_plot <- ggplot(c_montesN, aes(x=Year, y=N)) +
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_montesN$Year-0.125), max(c_montesN$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(6.5)))+
  xlab("Year") +
  ylab(expression(paste("Number of cyclones", sep = ""))) +
  ggtitle("a) Number of cyclones")+
  theme_bw() + graphics

montesN_plot

#montes cyclones_200km

montes200 = subset(c_sum, Location =="Montes" & Radius == "200") 

c_montes200 <- plyr::ddply(montes200, .(Year), summarise,
                       N    = length(!is.na(Time.in.radius)),
                       mean = mean(Time.in.radius, na.rm=TRUE),
                       sd   = sd(Time.in.radius, na.rm=TRUE),
                       se   = sd(Time.in.radius, na.rm=TRUE) / sqrt(length(!is.na(Time.in.radius)) ))

montes200_plot <- ggplot(c_montes200, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_montes200$Year-0.125), max(c_montes200$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Time in perimeter (hrs)", sep = ""))) +
  ggtitle("b) Time within 200km")+
  theme_bw() + graphics

montes200_plot

#Montes cyclones_50km

montes50 = subset(c_sum, Location =="Montes" & Radius == "50") 

c_montes50 <- plyr::ddply(montes50, .(Year), summarise,
                      N    = length(!is.na(Time.in.radius)),
                      mean = mean(Time.in.radius, na.rm=TRUE),
                      sd   = sd(Time.in.radius, na.rm=TRUE),
                      se   = sd(Time.in.radius, na.rm=TRUE) / sqrt(length(!is.na(Time.in.radius)) ))

montes50_plot <- ggplot(c_montes50, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_montes50$Year-0.125), max(c_montes50$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Time in perimeter (hrs)", sep = ""))) +
  ggtitle("c) Time within 50km")+
  theme_bw() + graphics

montes50_plot

#Shark Bay cyclones_pressure

c_montesp200 <- plyr::ddply(montes200, .(Year), summarise,
                        N    = length(!is.na(Mean.pressure)),
                        mean = mean(Mean.pressure, na.rm=TRUE),
                        sd   = sd(Mean.pressure, na.rm=TRUE),
                        se   = sd(Mean.pressure, na.rm=TRUE) / sqrt(length(!is.na(Mean.pressure)) ))

montesp200_plot <- ggplot(c_montesp200, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_montesp200$Year-0.5), max(c_montesp200$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(900), max(1050)))+
  xlab("Year") +
  ylab(expression(paste("Mean central pressure (hPa)", sep = ""))) +
  ggtitle("d) Central pressure")+
  theme_bw() + graphics

montesp200_plot

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(montesN_plot, montes200_plot, montes50_plot, montesp200_plot, ncol=2)
dev.off()

## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "cyclones_MBIMP.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")

