setwd("~/projects/data-pipelines/scripts/indicators/macroalgae/JBMP")

library(gridExtra)
library(ggplot2)
library (dplyr)
library(Kendall)
######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

# csv_rid <- "619d13a7-5df5-46e3-8391-50a2390e8df2"
# txt_rid <- "a25672bd-15d8-4644-933f-3eaa9fe6b320"

#percent cover plots
png_JBMP_canopycover_rid <- "f98ef2b7-2c97-4d7b-a0d7-10d3001a6db"
png_JBMP_diversity_fn <-"JBMP_diversity_subset.png"

pd <- position_dodge(0.1)
graphics = theme(axis.text.x=element_text(size = 12, angle=45, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
                 axis.title.x=element_text(size=15,face="bold"),
                 axis.title.y=element_text(size=15,face="bold"), #removes y axis title
                 axis.text.y=element_text(size = 12),
                 axis.line=element_line(colour="black"), #sets axis lines
                 plot.title =element_text(size = 15, hjust = 0.05),
                 panel.grid.minor = element_blank(), #removes minor grid lines
                 panel.grid.major = element_blank(), #removes major grid lines
                 panel.border=element_blank(), #removes border
                 panel.background=element_blank(), #needed to ensure integrity of axis lines
                 legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank())

###################################################################################################
#Load data
###################################################################################################

d<-diversity
names(diversity)[names(diversity) == 'SURVEY_DATE'] <- 'Year'###Changes column name

shallow = d %>% dplyr::filter(Depth %in% c("Shallow"))
deep = d %>% dplyr::filter(Depth %in% c("Deep"))

######################################################################################

make_diverse <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(richness)),
      mean = mean(richness, na.rm = TRUE),
      sd   = sd(richness, na.rm = TRUE),
      se   = sd(richness, na.rm = TRUE) / sqrt(N)
    )
}


######################################################################################

diverse <- make_diverse(d)

diverse_plot <- ggplot(diverse, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(diverse$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) diversity", sep = ""))) +
  ggtitle("a) All sites")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
diverse_plot

attach(diverse)
MannKendall(mean)
detach(diverse)

######################################################################################

shallow <- make_diverse(shallow)

shallow_plot <- ggplot(shallow, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(shallow$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) diversity", sep = ""))) +
  ggtitle("b) Shallow sites")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
shallow_plot

attach(diverse)
MannKendall(mean)
detach(diverse)

######################################################################################

deep <- make_diverse(deep)

deep_plot <- ggplot(deep, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(deep$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) diversity", sep = ""))) +
  ggtitle("c) Deep sites")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
deep_plot

attach(diverse)
MannKendall(mean)
detach(diverse)





#######################################################################################
canopyshallow <- make_canopy(canopy_shallow)

canopyshallow_plot <- ggplot(canopyshallow, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(canopy$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle("b) Total canopy cover_all shallow sites")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
canopyshallow_plot

attach(canopyshallow)
MannKendall(mean)
detach(canopyshallow)

##########################################################################################
canopydeep <- make_canopy(canopy_deep)

canopydeep_plot <- ggplot(canopydeep, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(canopy$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle("c) Total canopy cover_all deep sites")+
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
canopydeep_plot

attach(canopydeep)
MannKendall(mean)
detach(canopydeep)

############################################################################################

make_eck <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(ecklonia)),
      mean = mean(ecklonia, na.rm = TRUE),
      sd   = sd(ecklonia, na.rm = TRUE),
      se   = sd(ecklonia, na.rm = TRUE) / sqrt(N)
    )
}
############################################################################################
eck <- make_eck(eck_sites)

eck_plot <- ggplot(eck, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(eck$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle("a) All Ecklonia radiata sites")+
  geom_smooth(method=glm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
eck_plot

attach(eck)
MannKendall(mean)
detach(eck)

###############################################################################

eckshallow <- make_eck(eck_shallow)

eckshallow_plot <- ggplot(eckshallow, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(eck$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle("b) Ecklonia radiata_all shallow sites")+
  geom_smooth(method=glm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
eckshallow_plot

attach(eckshallow)
MannKendall(mean)
detach(eckshallow)

###############################################################################

eckdeep <- make_eck(eck_deep)

eckdeep_plot <- ggplot(eckdeep, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(eck$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) cover", sep = ""))) +
  ggtitle("c) Ecklonia radiata_all deep sites")+
  geom_smooth(method=glm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
eckdeep_plot

attach(eckdeep)
MannKendall(mean)
detach(eckdeep)


################################################################################
#Create figures (will be saved to current workdir)
################################################################################

#Canopy cover

png(png_JBMP_canopycover_fn, width=500, height=900)
grid.arrange(canopy_plot,canopyshallow_plot, canopydeep_plot, ncol = 1)
dev.off()

#Ecklonia cover
png(png_JBMP_eckcover_fn, width=500, height=900)
grid.arrange(eck_plot, eckshallow_plot, eckdeep_plot, ncol=1)
dev.off()

