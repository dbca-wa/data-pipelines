setwd("~/projects/data-pipelines/scripts/indicators/macroalgae/MMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "2405012d-a1f8-4f0f-9cae-ff10fb292195"
pdf_rid <- "aa78b91d-78fb-4316-b99b-6b1f28a7d84b"
txt_rid <- "b3a0e05b-8d3a-4864-898d-cc429a7fe9da"
pdf_fn = "final.pdf"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))

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
                 legend.justification=c(10,10), legend.position=c(10,10), # canitions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank()
)

##################################################################################
#Percent cover calculations for all data

cover=count(d, c("Zone", "Site", "Year", "Cover")) #counts number of observations per site, per year
cover_obs=count(cover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
cover_add <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
can_cover = subset(cover_add, Cover %in% c("canopy")) #Extracts cover information only
can_total = count(can_cover, c("Zone", "Site", "Year", "freq.1"), "freq")
names(can_total)[4] <- "total_count" #Rename column to make more sense
names(can_total)[5] <- "can_count" #Rename column to make more sense
can_total$percent = can_total$can_count/can_total$total_count *100 #Calculate percent cover

##################################################################################
#Marmion_south percent cover

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
MMP_s = subset(can_total, Site %in% c("Watermans Reef", "Watermans outer")) 

d_sum <- plyr::ddply(MMP_s, .(Year, Zone), summarise,
                     N    = length(!is.na(percent)),
                     mean = mean(percent, na.rm=TRUE),
                     sd   = sd(percent, na.rm=TRUE),
                     se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

MMP_s_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", canition=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("a) South")+
  theme_bw() + graphics

MMP_s_plot

#############################################################
#MMP centre percent cover

MMP_w = subset(can_total, Site %in% c("Little Island" , "The Lumps", "Three Mile Reef outer"))

d_sum_MMPw <- plyr::ddply(MMP_w, .(Year, Zone), summarise,
                   N    = length(!is.na(percent)),
                   mean = mean(percent, na.rm=TRUE),
                   sd   = sd(percent, na.rm=TRUE),
                   se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

MMP_w_plot<-ggplot(d_sum_MMPw, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", canition=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_MMPw$Year-0.125), max(d_sum_MMPw$Year+0.125)), breaks=min(d_sum_MMPw$Year):max(d_sum_MMPw$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("b) Centre")+
  theme_bw() + graphics

MMP_w_plot

#######################################################################################
#MMP_north percent cover

MMP_n = subset(can_total, Site %in% c("Burns Rocks", "Burns Rocks outer", "Three Mile Reef inner"))

d_sum_MMPn <- plyr::ddply(MMP_n, .(Year, Zone), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

MMP_n_plot<-ggplot(d_sum_MMPn, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", canition=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_MMPn$Year-0.125), max(d_sum_MMPn$Year+0.125)), breaks=min(d_sum_MMPn$Year):max(d_sum_MMPn$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("c) North")+
  theme_bw() + graphics

MMP_n_plot
#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(MMP_s_plot, MMP_w_plot,MMP_n_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "percent_cover_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
