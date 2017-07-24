setwd("~/projects/data-pipelines/scripts/indicators/seagrass/MMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "b0b546ed-ab74-4592-8429-7175cd637de4"
txt_rid <- "2bb67f9b-c6d7-4328-83d2-efd49afee876"

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
                 legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank()
)

##################################################################################
#Percent cover calculations for all data

cover=count(d, c("Sector", "Zone", "Site", "Year", "Level5Class")) #counts number of observations per site, per year
cover_obs=count(cover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
cover_add <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
pos_cover = subset(cover_add, Level5Class %in% c("Posidonia sinuosa","Posidonia australis")) #Extracts cover information only
pos_total = count(pos_cover, c("Sector","Zone", "Site", "Year", "freq.1"), "freq")
names(pos_total)[5] <- "total_count" #Rename column to make more sense
names(pos_total)[6] <- "pos_count" #Rename column to make more sense
pos_total$percent = pos_total$pos_count/pos_total$total_count *100 #Calculate percent cover

##################################################################################
#Marmion_south percent cover

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area
MMP_s = subset(pos_total, Site %in% c("North Beach", "Sorrento"))

d_sum <- plyr::ddply(MMP_s, .(Year, Zone), summarise,
                     N    = length(!is.na(percent)),
                     mean = mean(percent, na.rm=TRUE),
                     sd   = sd(percent, na.rm=TRUE),
                     se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

MMP_s_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("a) South")+
  theme_bw() + graphics

MMP_s_plot

#################################################################
#MMP_shoalwater Bay percent cover

MMP_c = subset(pos_total, Site %in% c("Hillarys Channel" , "Wreck Rock"))

d_sum_c <- plyr::ddply(SIMP_c, .(Year, Zone), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))


MMP_c_plot <- ggplot(d_sum_c, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_c$Year-0.125), max(d_sum_c$Year+0.125)), breaks=min(d_sum_c$Year):max(d_sum_c$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("b) Centre")+
  theme_bw() + graphics

SIMP_c_plot

###########################################################################
#MMP_north percent cover

MMP_n = subset(pos_total, Site %in% c("Causeway"))

d_sum_n <- plyr::ddply(MMP_n, .(Year, Zone), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

MMP_n_plot<-ggplot(d_sum_n, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_n$Year-0.125), max(d_sum_n$Year+0.125)), breaks=min(d_sum_n$Year):max(d_sum_n$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean density (","0.04m", ")", sep = ""))) +
  ggtitle("d) North")+
  theme_bw() + graphics

MMP_n_plot
#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)



## Step 5: Upload to CKAN
ckanr::resource_update(txt_rid, "percent_cover_code.R")

# Step 6: set workdir to main report location
setwd("~/projects")
