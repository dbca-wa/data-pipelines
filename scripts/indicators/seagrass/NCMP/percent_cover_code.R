setwd("~/projects/data-pipelines/scripts/indicators/seagrass/NCMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "26781d5a-5792-429b-93a7-9e0f5d5ac290"
pdf_rid <- "85a589f5-13d5-40fa-b231-8f9298d3a4f6"
txt_rid <- "89ad20bc-6430-43d5-904b-891485894fdf"
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
                 legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank()
)

##################################################################################
#Percent cover calculations for all data

cover=count(d, c("Zone", "Site", "Year", "Level5Class")) #counts number of observations per site, per year
cover_obs=count(cover, c("Site", "Year"), "freq") #counts number of observations made at each site per year
cover_add <- join(cover, cover_obs, by = c("Site", "Year")) #adds total count of site observations agains the right site/year to allow percentage calculation
pos_cover = subset(cover_add, Level5Class %in% c("Posidonia sinuosa","Posidonia australis")) #Extracts cover information only
pos_total = count(pos_cover, c("Zone", "Site", "Year", "freq.1"), "freq")
names(pos_total)[4] <- "total_count" #Rename column to make more sense
names(pos_total)[5] <- "pos_count" #Rename column to make more sense
pos_total$percent = pos_total$pos_count/pos_total$total_count *100 #Calculate percent cover

##################################################################################
#Capes_inshore percent cover

#Creates a data frame summarised for the sites included. Repeat for each 'sector' or reporting area     
NCMP_in = subset(pos_total, Site %in% c("VW", "BUS", "PTG", "FB")) 

d_sum <- plyr::ddply(NCMP_in, .(Year, Zone), summarise,
                     N    = length(!is.na(percent)),
                     mean = mean(percent, na.rm=TRUE),
                     sd   = sd(percent, na.rm=TRUE),
                     se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

NCMP_in_plot <- ggplot(d_sum, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum$Year-0.125), max(d_sum$Year+0.125)), breaks=min(d_sum$Year):max(d_sum$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("a) Geographe Bay_near shore")+
  theme_bw() + graphics

NCMP_in_plot

#############################################################
#Capes Mid percent cover
NCMP_m = subset(pos_total, Site %in% c("MS1" , "MS2", "MS3" , "MS4", "MS6"))

d_sum_ncmpm <- plyr::ddply(NCMP_m, .(Year, Zone), summarise,
                   N    = length(!is.na(percent)),
                   mean = mean(percent, na.rm=TRUE),
                   sd   = sd(percent, na.rm=TRUE),
                   se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

NCMP_m_plot<-ggplot(d_sum_ncmpm, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_ncmpm$Year-0.125), max(d_sum_ncmpm$Year+0.125)), breaks=min(d_sum_ncmpm$Year):max(d_sum_ncmpm$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("b) Geographe Bay_mid")+
  theme_bw() + graphics

NCMP_m_plot

#################################################################
#NCMP_Geographe Bay our percent cover

NCMP_o = subset(pos_total, Site %in% c("OS2" , "OS3", "OS4", "OS6"))

d_sum_ncmpo <- plyr::ddply(NCMP_o, .(Year, Zone), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))


NCMP_o_plot <- ggplot(d_sum_ncmpo, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_ncmpo$Year-0.125), max(d_sum_ncmpo$Year+0.125)), breaks=min(d_sum_ncmpo$Year):max(d_sum_ncmpo$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("c) Geographe Bay_offshore")+
  theme_bw() + graphics

NCMP_o_plot

###########################################################################
#NCMP_west percent cover

NCMP_w = subset(pos_total, Site %in% c("Cowaramup Bay"))

d_sum_ncmpw <- plyr::ddply(NCMP_w, .(Year, Zone), summarise,
                           N    = length(!is.na(percent)),
                           mean = mean(percent, na.rm=TRUE),
                           sd   = sd(percent, na.rm=TRUE),
                           se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))

NCMP_w_plot<-ggplot(d_sum_ncmpw, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d_sum_ncmpw$Year-0.125), max(d_sum_ncmpw$Year+0.125)), breaks=min(d_sum_ncmpw$Year):max(d_sum_ncmpw$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean percent cover", sep = ""))) +
  ggtitle("d) Cowaramup Bay")+
  theme_bw() + graphics

NCMP_w_plot
#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(NCMP_in_plot, NCMP_m_plot, NCMP_o_plot, NCMP_w_plot, ncol=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "percent_cover_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
