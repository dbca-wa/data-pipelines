setwd("~/projects/mpa-reporting/scripts/indicators/seagrass/SIMP")
source("~/projects/mpa-reporting/scripts/ckan.R")
source("~/projects/mpa-reporting/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"
pdf_rid <- "485457c3-a66f-4a6b-aa60-909736986ff8"
txt_rid <- "ef76d134-cc13-4c54-8864-0c441a36a127"
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
#Shoalwater_south Shoot density

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
SIMP_s = subset(d, Site %in% c("Becher Point", "Becher Point SZ", "Port Kennedy")) 

d_sum <- plyr::ddply(SIMP_s, .(Year, Zone), summarise,
                     N    = length(!is.na(Pos_total)),
                     mean = mean(Pos_total, na.rm=TRUE),
                     sd   = sd(Pos_total, na.rm=TRUE),
                     se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

SIMP_s_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("a) Becher Point") +
  theme_bw() + graphics

SIMP_s_plot

#############################################################
#Warnbro Sound shoot density
SIMP_w = subset(d, Site %in% c("Warnbro Sound 2.5m" , "Warnbro Sound 3.2m", "Warnbro Sound 5.2m" , "Warnbro Sound 7.0m", "Warnbro Sound 2.0m" , "Mersey Point"))

d_sum_simpw <- ddply(SIMP_w, .(Year, Zone), summarise,
                     N    = length(!is.na(Pos_total)),
                     mean = mean(Pos_total, na.rm=TRUE),
                     sd   = sd(Pos_total, na.rm=TRUE),
                     se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

SIMP_w_plot<-ggplot(d_sum_simpw, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_simpw$Year-0.125), max(d_sum_simpw$Year+0.125)), breaks=min(d_sum_simpw$Year):max(d_sum_simpw$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("b) Warnbro Sound")+
  theme_bw() + graphics

SIMP_w_plot

#################################################################
#SIMP_shoalwater Bay shoot density
SIMP_shoal = subset(d, Site %in% c("Penguin Island" , "Seal Island", "Bird Island"))

d_sum_shoal <- ddply(SIMP_shoal, .(Year, Zone), summarise,
                     N    = length(!is.na(Pos_total)),
                     mean = mean(Pos_total, na.rm=TRUE),
                     sd   = sd(Pos_total, na.rm=TRUE),
                     se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

SIMP_shoal_plot <- ggplot(d_sum_shoal, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_shoal$Year-0.125), max(d_sum_shoal$Year+0.125)), breaks=min(d_sum_shoal$Year):max(d_sum_shoal$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("c) Shoalwater Bay")+
  theme_bw() + graphics

SIMP_shoal_plot

###########################################################################
#SIMP_north shoot density

SIMP_n = subset(d, Site %in% c("Causeway"))
d_sum_simpn <- ddply(SIMP_n, .(Year, Zone), summarise,
                     N    = length(!is.na(Pos_total)),
                     mean = mean(Pos_total, na.rm=TRUE),
                     sd   = sd(Pos_total, na.rm=TRUE),
                     se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

SIMP_n_plot<-ggplot(d_sum_simpn, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_simpn$Year-0.125), max(d_sum_simpn$Year+0.125)), breaks=min(d_sum_simpn$Year):max(d_sum_simpn$Year)) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("d) Point Peron")+
  theme_bw() + graphics

SIMP_n_plot


#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(SIMP_s_plot, SIMP_w_plot, SIMP_shoal_plot, SIMP_n_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "shoot_density_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/mpa-reporting/reports")
