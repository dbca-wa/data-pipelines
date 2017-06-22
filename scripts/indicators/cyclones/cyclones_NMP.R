setwd("~/projects/data-pipelines/scripts/indicators/cyclones")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(dplyr)
library(rworldmap)
library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "779cde13-b376-4edd-8ddd-2361fe1a22fd"
pdf_rid <- "2f3f05c2-efc1-4bcd-8a2a-a9fc6731aea4"
txt_rid <- "3627cec1-79ba-480c-8fdf-7aa31f8009d9"
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
#Ningaloo number of cyclones

ninN = subset(c_sum, Location =="Ningaloo " & Radius == "200")

c_ninN <- plyr::ddply(ninN, .(Year), summarise,
                      N    = length(!is.na(Time.in.radius)))

ninN_plot <- ggplot(c_ninN, aes(x=Year, y=N)) +
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_ninN$Year-0.125), max(c_ninN$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(4)))+
  xlab("Year") +
  ylab(expression(paste("Number of cyclones", sep = ""))) +
  ggtitle("a) Number of cyclones")+
  theme_bw() + graphics

ninN_plot

#Ningaloo cyclones_200km

nin200 = subset(c_sum, Location =="Ningaloo " & Radius == "200")

c_nin200 <- plyr::ddply(nin200, .(Year), summarise,
                        N    = length(!is.na(Time.in.radius)),
                        mean = mean(Time.in.radius, na.rm=TRUE),
                        sd   = sd(Time.in.radius, na.rm=TRUE),
                        se   = sd(Time.in.radius, na.rm=TRUE) / sqrt(length(!is.na(Time.in.radius)) ))

nin200_plot <- ggplot(c_nin200, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_nin200$Year-0.125), max(c_nin200$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Time in perimeter (hrs)", sep = ""))) +
  ggtitle("b) Time within 200km")+
  theme_bw() + graphics

nin200_plot

#Ningaloo (Exmouth) cyclones_50km

nin50 = subset(c_sum, Location =="Ningaloo (Exmouth)" & Radius == "50")

c_nin50 <- plyr::ddply(nin50, .(Year), summarise,
                       N    = length(!is.na(Time.in.radius)),
                       mean = mean(Time.in.radius, na.rm=TRUE),
                       sd   = sd(Time.in.radius, na.rm=TRUE),
                       se   = sd(Time.in.radius, na.rm=TRUE) / sqrt(length(!is.na(Time.in.radius)) ))

nin50_e_plot <- ggplot(c_nin50, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_nin50$Year-0.125), max(c_nin50$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(8)))+
  xlab("Year") +
  ylab(expression(paste("Time in perimeter (hrs)", sep = ""))) +
  ggtitle("c) Time within 50km of Exmouth")+
  theme_bw() + graphics

nin50_e_plot

#Ningaloo (Coral Bay) cyclones_50km

nin_cb_50 = subset(c_sum, Location =="Ningaloo (Coral Bay)" & Radius == "50")

c_nin50_cb <- plyr::ddply(nin_cb_50, .(Year), summarise,
                       N    = length(!is.na(Time.in.radius)),
                       mean = mean(Time.in.radius, na.rm=TRUE),
                       sd   = sd(Time.in.radius, na.rm=TRUE),
                       se   = sd(Time.in.radius, na.rm=TRUE) / sqrt(length(!is.na(Time.in.radius)) ))

nin_cb_50_plot <- ggplot(c_nin50_cb, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  #geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_nin50_cb$Year-0.125), max(c_nin50_cb$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(15)))+
  xlab("Year") +
  ylab(expression(paste("Time in perimeter (hrs)", sep = ""))) +
  ggtitle("d) Time within 50km of Coral Bay")+
  theme_bw() + graphics

nin_cb_50_plot

#Ningaloo cyclones_pressure

c_ninp200 <- plyr::ddply(nin200, .(Year), summarise,
                         N    = length(!is.na(Mean.pressure)),
                         mean = mean(Mean.pressure, na.rm=TRUE),
                         sd   = sd(Mean.pressure, na.rm=TRUE),
                         se   = sd(Mean.pressure, na.rm=TRUE) / sqrt(length(!is.na(Mean.pressure)) ))

ninp200_plot <- ggplot(c_ninp200, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.7, binwidth=0) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(expand = c(0,0), limits=c(min(c_ninp200$Year-0.5), max(c_ninp200$Year+0.5)), breaks=seq(1960,2020,2)) +
  scale_y_continuous(expand = c(0,0), limits=c(min(900), max(1025)))+
  xlab("Year") +
  ylab(expression(paste("Mean central pressure (hPa)", sep = ""))) +
  ggtitle("e) Central pressure")+
  theme_bw() + graphics

ninp200_plot

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=10, height=10)
grid.arrange(ninN_plot, nin200_plot, nin_cb_50_plot, nin50_e_plot, ninp200_plot, ncol=2)
dev.off()

## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "cyclones_NMP.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")

