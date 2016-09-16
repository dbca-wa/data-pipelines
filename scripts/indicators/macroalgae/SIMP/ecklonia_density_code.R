setwd("~/projects/data-pipelines/scripts/indicators/macroalgae/SIMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "a5c77770-0019-43fb-96be-348e17e1a69e"
pdf_rid <- "a90cfb33-29c6-474b-be44-ab3435fb5552"
txt_rid <- "256de5ee-0a25-43d2-8104-21b9d388641a"
pdf_fn = "final.pdf"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))

names(d)[names(d) == 'Park_name'] <- 'Park'###Changes column name
names(d)[names(d) == 'Site.name'] <- 'Site'###Changes column name 
unique(d$Site)

d$eck_adult <- (d$Ecklonia.adult.density * 4) # Scales seagrass data to 1m
d$eck_juv <- (d$Ecklonia.juvenile.density * 4) # Scales seagrass data to 1m

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
#Shoalwater_south Ecklonia density

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
SIMP_s = subset(d, Site %in% c("Becher Point SZ", "The Sisters")) 

d_sum <- plyr::ddply(SIMP_s, .(Year, Zone), summarise,
                     N    = length(!is.na(Ecklonia.adult.density)),
                     mean = mean(eck_adult, na.rm=TRUE),
                     sd   = sd(eck_adult, na.rm=TRUE),
                     se   = sd(eck_adult, na.rm=TRUE) / sqrt(length(!is.na(eck_adult)) ))

SIMP_s_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (m"^-2,")", sep = ""))) +
  ggtitle("a) Becher Point")+
  theme_bw() + graphics

SIMP_s_plot

#################################################################
#SIMP_shoalwater Bay shoot density
SIMP_shoal = subset(d, Site %in% c("Penguin Island" , "Seal Island", "Second Rock SZ"))

d_sum_shoal <- ddply(SIMP_shoal, .(Year, Zone), summarise,
                     N    = length(!is.na(eck_adult)),
                     mean = mean(eck_adult, na.rm=TRUE),
                     sd   = sd(eck_adult, na.rm=TRUE),
                     se   = sd(eck_adult, na.rm=TRUE) / sqrt(length(!is.na(eck_adult)) ))

SIMP_shoal_plot <- ggplot(d_sum_shoal, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_shoal$Year-0.125), max(d_sum_shoal$Year+0.125)), breaks=min(d_sum_shoal$Year):max(d_sum_shoal$Year)) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (m"^-2,")", sep = ""))) +
  ggtitle("b) Shoalwater Bay")+
  theme_bw() + graphics

SIMP_shoal_plot

###########################################################################
#SIMP_north shoot density

SIMP_n = subset(d, Site %in% c("Bird Island","John Point", "Point Peron north"))
d_sum_simpn <- ddply(SIMP_n, .(Year, Zone), summarise,
                     N    = length(!is.na(eck_adult)),
                     mean = mean(eck_adult, na.rm=TRUE),
                     sd   = sd(eck_adult, na.rm=TRUE),
                     se   = sd(eck_adult, na.rm=TRUE) / sqrt(length(!is.na(eck_adult)) ))

SIMP_n_plot<-ggplot(d_sum_simpn, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_simpn$Year-0.125), max(d_sum_simpn$Year+0.125)), breaks=min(d_sum_simpn$Year):max(d_sum_simpn$Year)) +
  scale_y_continuous(limits=c(min(0), max(20)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (m"^-2,")", sep = ""))) +
  ggtitle("c) Point Peron")+
  theme_bw() + graphics

SIMP_n_plot

#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(SIMP_s_plot, SIMP_shoal_plot, SIMP_n_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "ecklonia_density_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
