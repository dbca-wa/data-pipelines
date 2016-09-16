setwd("~/projects/data-pipelines/scripts/indicators/seagrass/MMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"
pdf_rid <- "1cfb1c06-5f4b-4342-89e8-e480569efb17"
txt_rid <- "4f57358f-8a60-4e50-be29-70824df01736"
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
#MMP_south Shoot density

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
MMP_s = subset(d, Site %in% c("North Beach", "Sorrento")) 

d_sum <- plyr::ddply(MMP_s, .(Year, Zone), summarise,
                     N    = length(!is.na(Mean_height_mm)),
                     mean = mean(Mean_height_mm, na.rm=TRUE),
                     sd   = sd(Mean_height_mm, na.rm=TRUE),
                     se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

MMP_s_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(800)))+
  xlab("Year") +
  ylab(expression(paste("Mean height (mm)", sep = ""))) +
  ggtitle("a) South")+
  theme_bw() + graphics

MMP_s_plot

#############################################################
#MMP centre shoot density
MMP_c = subset(d, Site %in% c("Hillarys Channel" , "Wreck Rock", "Mullaloo"))

d_sum_MMPc <- ddply(MMP_c, .(Year, Zone), summarise,
                    N    = length(!is.na(Mean_height_mm)),
                    mean = mean(Mean_height_mm, na.rm=TRUE),
                    sd   = sd(Mean_height_mm, na.rm=TRUE),
                    se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

MMP_c_plot<-ggplot(d_sum_MMPc, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_MMPc$Year-0.125), max(d_sum_MMPc$Year+0.125)), breaks=min(d_sum_MMPc$Year):max(d_sum_MMPc$Year)) +
  scale_y_continuous(limits=c(min(0), max(800)))+
  xlab("Year") +
  ylab(expression(paste("Mean height (mm)", sep = ""))) +
  ggtitle("b) Centre")+
  theme_bw() + graphics

MMP_c_plot

#################################################################
#MMP_north shoot density

MMP_n = subset(d, Site %in% c("Ocean Reef Outer", "Ocean Reef Inner", "Burns Rocks"))

d_sum_MMPn <- ddply(MMP_n, .(Year, Zone), summarise,
                    N    = length(!is.na(Mean_height_mm)),
                    mean = mean(Mean_height_mm, na.rm=TRUE),
                    sd   = sd(Mean_height_mm, na.rm=TRUE),
                    se   = sd(Mean_height_mm, na.rm=TRUE) / sqrt(length(!is.na(Mean_height_mm)) ))

MMP_n_plot<-ggplot(d_sum_MMPn, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_MMPn$Year-0.125), max(d_sum_MMPn$Year+0.125)), breaks=min(d_sum_MMPn$Year):max(d_sum_MMPn$Year)) +
  scale_y_continuous(limits=c(min(0), max(800)))+
  xlab("Year") +
  ylab(expression(paste("Mean height (mm)", sep = ""))) +
  ggtitle("c) North")+
  theme_bw() + graphics

MMP_n_plot

#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(MMP_s_plot, MMP_c_plot, MMP_n_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "mean_height_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
