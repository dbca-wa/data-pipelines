setwd("~/projects/mpa-reporting/scripts/indicators/seagrass/JBMP")
source("~/projects/mpa-reporting/scripts/ckan.R")
source("~/projects/mpa-reporting/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"
pdf_rid <- "a387ccf5-8e06-4f35-b5b7-74fc5a0acec4"
txt_rid <- "6d05dddf-7d72-474c-9a66-b645f7335f6c"
pdf_fn = "final.pdf"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))

names(d)[names(d) == 'Park_name'] <- 'Park'###Changes column name
names(d)[names(d) == 'Sites'] <- 'Site'###Changes column name 
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
                 legend.key = element_blank()
)
##################################################################################
#JBMP south Maximum height

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
JBMP_s = subset(d, Site %in% c("Green Island", "Kangaroo Point", "Cervantes Island")) 

d_sum <- plyr::ddply(JBMP_s, .(Year, Zone), summarise,
                     N    = length(!is.na(Mean_height_mm)),
                     mean = mean(Mean_height_mm, na.rm=TRUE),
                     sd   = sd(Mean_height_mm, na.rm=TRUE),
                     se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

JBMP_s_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(700)))+
  xlab("Year") +
  ylab(expression(paste("Mean height (mm)", sep = ""))) +
  ggtitle("a) South")+
  theme_bw() + graphics

# Step 3: look at output - adjust Step 2, repeat until good enough
JBMP_s_plot

#############################################################
#JBMP Centre maximum height
JBMP_c = subset(d, Site %in% c("Jurien Impact Site 2.5" , "Boullanger Island 2.5", "Boullanger Island 3.5" , "Boullanger Island 5.5"))

d_sum_JBMPc <- ddply(JBMP_c, .(Year, Zone), summarise,
                     N    = length(!is.na(Mean_height_mm)),
                     mean = mean(Mean_height_mm, na.rm=TRUE),
                     sd   = sd(Mean_height_mm, na.rm=TRUE),
                     se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

JBMP_c_plot<-ggplot(d_sum_JBMPc, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_JBMPc$Year-0.125), max(d_sum_JBMPc$Year+0.125)), breaks=min(d_sum_JBMPc$Year):max(d_sum_JBMPc$Year)) +
  scale_y_continuous(limits=c(min(0), max(700)))+
  xlab("Year") +
  ylab(expression(paste("Mean height (mm)", sep = ""))) +
  ggtitle("b) Centre")+
  theme_bw() + graphics

JBMP_c_plot

###########################################################################
#JBMP_north shoot density

JBMP_n = subset(d, Site %in% c("Fishermans Island 2.5","Fishermans Island 3.5", "Fishermans Island 5.5"))
d_sum_JBMPn <- ddply(JBMP_n, .(Year, Zone), summarise,
                     N    = length(!is.na(Mean_height_mm)),
                     mean = mean(Mean_height_mm, na.rm=TRUE),
                     sd   = sd(Mean_height_mm, na.rm=TRUE),
                     se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

JBMP_n_plot<-ggplot(d_sum_JBMPn, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_JBMPn$Year-0.125), max(d_sum_JBMPn$Year+0.125)), breaks=min(d_sum_JBMPn$Year):max(d_sum_JBMPn$Year)) +
  scale_y_continuous(limits=c(min(0), max(700)))+
  xlab("Year") +
  ylab(expression(paste("Mean height (mm)", sep = ""))) +
  ggtitle("c) North")+
  theme_bw() + graphics

JBMP_n_plot

#####################################################################################


pdf(pdf_fn, width=8, height=7)
grid.arrange(JBMP_s_plot, JBMP_c_plot, JBMP_n_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "mean_height_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/mpa-reporting/reports")
