setwd("~/projects/data-pipelines/scripts/indicators/macroalgae/MMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "a5c77770-0019-43fb-96be-348e17e1a69e"
pdf_rid <- "fc3d638f-5f16-449a-9c10-aeb9dfa9a500"
txt_rid <- "ffebe23b-6482-4e59-8ba9-ac24ff6ea527"
pdf_fn = "final.pdf"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))

names(d)[names(d) == 'Park_name'] <- 'Park'###Changes column name
names(d)[names(d) == 'Site.name'] <- 'Site'###Changes column name 

d$eck_juvenile <- (d$Ecklonia.juvenile.density * 4) # Scales seagrass data to 1m
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
#Marmion_south Ecklonia density

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
mmp_s = subset(d, Site %in% c("Watermans outer", "Watermans Reef")) 

d_sum <- plyr::ddply(mmp_s, .(Year, Zone), summarise,
                     N    = length(!is.na(Ecklonia.juvenile.density)),
                     mean = mean(eck_juvenile, na.rm=TRUE),
                     sd   = sd(eck_juvenile, na.rm=TRUE),
                     se   = sd(eck_juvenile, na.rm=TRUE) / sqrt(length(!is.na(eck_juvenile)) ))

mmp_s_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (m"^-2,")", sep = ""))) +
  ggtitle("a) South")+
  theme_bw() + graphics

mmp_s_plot

#################################################################
#Marmion centre Ecklonia density

mmp_c = subset(d, Site %in% c("The Lumps" , "Little Island", "Three Mile Reef outer"))

d_sum_c <- ddply(mmp_c, .(Year, Zone), summarise,
                     N    = length(!is.na(eck_juvenile)),
                     mean = mean(eck_juvenile, na.rm=TRUE),
                     sd   = sd(eck_juvenile, na.rm=TRUE),
                     se   = sd(eck_juvenile, na.rm=TRUE) / sqrt(length(!is.na(eck_juvenile)) ))

mmp_c_plot <- ggplot(d_sum_c, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_c$Year-0.125), max(d_sum_c$Year+0.125)), breaks=min(d_sum_c$Year):max(d_sum_c$Year)) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (m"^-2,")", sep = ""))) +
  ggtitle("b) Centre")+
  theme_bw() + graphics

mmp_c_plot

###########################################################################
#Marmion_north Ecklonia density

mmp_n = subset(d, Site %in% c("Three Mile Reef inner","Burns Rocks", "Burns Rocks offshore"))

d_sum_mmpn <- ddply(mmp_n, .(Year, Zone), summarise,
                     N    = length(!is.na(eck_juvenile)),
                     mean = mean(eck_juvenile, na.rm=TRUE),
                     sd   = sd(eck_juvenile, na.rm=TRUE),
                     se   = sd(eck_juvenile, na.rm=TRUE) / sqrt(length(!is.na(eck_juvenile)) ))

mmp_n_plot<-ggplot(d_sum_mmpn, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_mmpn$Year-0.125), max(d_sum_mmpn$Year+0.125)), breaks=min(d_sum_mmpn$Year):max(d_sum_mmpn$Year)) +
  scale_y_continuous(limits=c(min(0), max(10)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (m"^-2,")", sep = ""))) +
  ggtitle("c) North")+
  theme_bw() + graphics

mmp_n_plot

#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(mmp_s_plot, mmp_c_plot, mmp_n_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "juv_ecklonia_density_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
