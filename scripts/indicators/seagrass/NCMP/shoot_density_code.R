setwd("~/projects/data-pipelines/scripts/indicators/seagrass/NCMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"
txt_rid <- "6b003575-fa64-4a48-8ae6-7f6988daa263"

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
#NCMP_Geographe Bay inshore Shoot density
#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area
NCMP_in = subset(d, Site %in% c("Dunsborough", "Buayanup", "Vasse Diversion", "Busselton Jetty", "Port Geographe", "Vasse-Wonnerup", "Forrest Beach"))

d_sum <- plyr::ddply(NCMP_in, .(Year, Zone), summarise,
          N    = length(!is.na(Pos_total)),
          mean = mean(Pos_total, na.rm=TRUE),
          sd   = sd(Pos_total, na.rm=TRUE),
          se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

NCMP_in_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.00, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("a) Geographe Bay_near shore")+
  theme_bw() + graphics

NCMP_in_plot

#############################################################
#NCMP Geographe Bay mid-shore shoot density


NCMP_m = subset(uwa, Site %in% c("MS1" , "MS2", "MS3","MS4" , "MS6"))

d_sum_m <- ddply(NCMP_m, .(Year, Zone), summarise,
          N    = length(!is.na(Density)),
          mean = mean(Density, na.rm=TRUE),
          sd   = sd(Density, na.rm=TRUE),
          se   = sd(Density, na.rm=TRUE) / sqrt(length(!is.na(Density)) ))

NCMP_m_plot<-ggplot(d_sum_m, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.00, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_m$Year-0.125), max(d_sum_m$Year+0.125)), breaks=min(d_sum_m$Year):max(d_sum_m$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("b) Geographe Bay_mid")+
  theme_bw() + graphics

NCMP_m_plot

#################################################################
#NCMP_Geographe Bay offshore shoot density

NCMP_o = subset(uwa, Site %in% c("OS1" , "OS2", "OS3", "OS4", "OS6"))

d_sum_o <- ddply(NCMP_o, .(Year, Zone), summarise,
          N    = length(!is.na(Density)),
          mean = mean(Density, na.rm=TRUE),
          sd   = sd(Density, na.rm=TRUE),
          se   = sd(Density, na.rm=TRUE) / sqrt(length(!is.na(Density)) ))

NCMP_o_plot <- ggplot(d_sum_o, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.00, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_o$Year-0.125), max(d_sum_o$Year+0.125)), breaks=min(d_sum_o$Year):max(d_sum_o$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("c) Geographe Bay_offshore")+
  theme_bw() + graphics

NCMP_o_plot

###########################################################################
#NCMP_West coast shoot density

NCMP_w = subset(d, Site %in% c("Cowaramup Bay"))

d_sum_w <- ddply(NCMP_w, .(Year, Zone), summarise,
          N    = length(!is.na(Pos_total)),
          mean = mean(Pos_total, na.rm=TRUE),
          sd   = sd(Pos_total, na.rm=TRUE),
          se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

NCMP_w_plot<-ggplot(d_sum_w, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.00, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_w$Year-0.125), max(d_sum_w$Year+0.125)), breaks=min(d_sum_w$Year):max(d_sum_w$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("d) west coast")+
  theme_bw() + graphics

NCMP_w_plot

#####################################################################################
# Step 4: Create PDF (will be saved to current workdir)


## Step 5: Upload to CKAN

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "shoot_density_code.R")

# Step 6: set workdir to main report location
setwd("~/projects")
