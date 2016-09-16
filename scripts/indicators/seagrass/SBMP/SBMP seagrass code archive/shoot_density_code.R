setwd("~/projects/mpa-reporting/scripts/indicators/seagrass/SBMP")
source("~/projects/mpa-reporting/scripts/ckan.R")
source("~/projects/mpa-reporting/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"
pdf_rid <- "263166f5-fbcc-4ce1-9b8e-5b64a47b69ae"
txt_rid <- "38fd849b-0ae9-4289-b8a2-4279bd220e6c"
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
#SBMP_east Shoot density

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
SBMP_e = subset(d, Site %in% c("Wooramel North", "Disappointment Reach")) 

d_sum <- plyr::ddply(SBMP_e, .(Year, Zone), summarise,
               N    = length(!is.na(Pos_total)),
               mean = mean(Pos_total, na.rm=TRUE),
               sd   = sd(Pos_total, na.rm=TRUE),
               se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

SBMP_e_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("a) Wooramel")+
  theme_bw() + graphics

SBMP_e_plot

#############################################################
#SBMP Monkey Mia shoot density
SBMP_m = subset(d, Site %in% c("East Peron" , "Monkey Mia inner bank", "Monkey Mia south outer" , "Monkey Mia south"))

d_sum_SBMPm <- ddply(SBMP_m, .(Year, Zone), summarise,
             N    = length(!is.na(Pos_total)),
             mean = mean(Pos_total, na.rm=TRUE),
             sd   = sd(Pos_total, na.rm=TRUE),
             se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

SBMP_m_plot<-ggplot(d_sum_SBMPm, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_SBMPm$Year-0.125), max(d_sum_SBMPm$Year+0.125)), breaks=min(d_sum_SBMPm$Year):max(d_sum_SBMPm$Year)) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("b) Monkey Mia")+
  theme_bw() + graphics

SBMP_m_plot

#################################################################
#SBMP Peron shoot density

SBMP_p = subset(d, Site %in% c("Big Lagoon" , "Denham", "Peron south"))

d_sum_p <- ddply(SBMP_p, .(Year, Zone), summarise,
             N    = length(!is.na(Pos_total)),
             mean = mean(Pos_total, na.rm=TRUE),
             sd   = sd(Pos_total, na.rm=TRUE),
             se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

SBMP_p_plot <- ggplot(d_sum_p, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_p$Year-0.125), max(d_sum_p$Year+0.125)), breaks=min(d_sum_p$Year):max(d_sum_p$Year)) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("c) Denham")+
  theme_bw() + graphics

SBMP_p_plot

###########################################################################
#SBMP west shoot density

SBMP_w = subset(d, Site %in% c("Sandy Point", "South Passage", "Useless Loop North", "Useless Loop South"))

d_sum_SBMPw <- ddply(SBMP_w, .(Year, Zone), summarise,
             N    = length(!is.na(Pos_total)),
             mean = mean(Pos_total, na.rm=TRUE),
             sd   = sd(Pos_total, na.rm=TRUE),
             se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

SBMP_w_plot<-ggplot(d_sum_SBMPw, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_SBMPw$Year-0.125), max(d_sum_SBMPw$Year+0.125)), breaks=min(d_sum_SBMPw$Year):max(d_sum_SBMPw$Year)) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("d) Western Gulf")+
  theme_bw() + graphics

SBMP_w_plot

#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(SBMP_e_plot, SBMP_m_plot, SBMP_p_plot, SBMP_w_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "shoot_density_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/mpa-reporting/reports")
