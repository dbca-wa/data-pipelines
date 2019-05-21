setwd("~/projects/data-pipelines/scripts/indicators/seagrass/SIMP")
source("~/projects/data-pipelines/setup/ckan.R")

library(Kendall)

######################################################################################################
#Define all CKAN resource IDs
######################################################################################################

csv_rid <- "d1e0cd1d-9fc0-4069-9781-eb4946d929c8"#CKAN resource ID for data
txt_rid <- "ef76d134-cc13-4c54-8864-0c441a36a127"#CKAN resource ID for r-script

#Shoot density plots
png_shoot_density_rid <- "af4bc41b-6477-4571-9038-1f5784502bdf"#CKAN resource ID for final figure (png)
png_shoot_density_fn = "SIMP shoot density.png"#Name of final figure
png_SIMP_shoot_density_rid <- "186519a9-72aa-4d78-afd6-bce672f476f0"#CKAN resource ID for final figure (png)
png_SIMP_shoot_density_fn = "SIMP overall shoot density.png"#Name of final figure

#Maximum canopy height plots
png_height_rid <- "8f406852-0b15-46cb-b972-c960c4f3bfc8"#CKAN resource ID for final figure (png)
png_height_fn = "SIMP height.png"#Name of final figure
png_SIMP_height_rid <- "a440fa20-596a-474a-a91f-b1108c99f633"#CKAN resource ID for final figure (png)
png_SIMP_height_fn = "SIMP overall height.png"#Name of final figure

################################################################################
#Load data
################################################################################

d <- load_ckan_csv(csv_rid)
d<-in_water_data
################################################################################
#Define graphic properties
################################################################################

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

################################################################################
#Create subsets for each 'sector (south, centre, north) for SIMP
################################################################################

SIMP <- d %>% filter(Park == "SIMP")
SIMP_south <- SIMP %>% filter(Site %in% c("Becher Point", "Becher Point SZ", "Port Kennedy"))
SIMP_warnbro <- SIMP %>% filter(Site %in% c("Warnbro Sound 2.5m" , "Warnbro Sound 3.2m", "Warnbro Sound 5.2m" ,
  "Warnbro Sound 7.0m", "Warnbro Sound 2.0m" , "Mersey Point"))
SIMP_shoalwater <- SIMP %>% filter(Site %in% c("Penguin Island" , "Seal Island", "Bird Island"))
SIMP_north <- SIMP %>% filter(Site %in% c("Causeway"))

################################################################################
#SHOOT DENSITY
################################################################################
#OverallShoot density

make_shootdensity <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(Posidonia_sinuosa)),
      mean = mean(Posidonia_sinuosa, na.rm = TRUE),
      sd   = sd(Posidonia_sinuosa, na.rm = TRUE),
      se   = sd(Posidonia_sinuosa, na.rm = TRUE) / sqrt(N)
    )
}


SIMP_shootdensity <- make_shootdensity(SIMP)

SIMP_shootdensity_plot <- ggplot(SIMP_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003),
                              max(SIMP_shootdensity$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
SIMP_shootdensity_plot

attach(SIMP_shootdensity)
MannKendall(mean)
detach(SIMP_shootdensity)

#SIMP_south Shoot density

SIMP_south_shootdensity <- make_shootdensity(SIMP_south)

SIMP_south_shootdensity_plot <- ggplot(SIMP_south_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") +
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_south_shootdensity$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("d) Becher Point") +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_south_shootdensity_plot

attach(SIMP_south_shootdensity)
MannKendall(mean)
detach(SIMP_south_shootdensity)

#############################################################
#Warnbro Sound shoot density

SIMP_warnbro_shootdensity <- make_shootdensity(SIMP_warnbro)

SIMP_warnbro_shootdensity_plot<-ggplot(SIMP_warnbro_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(2017))) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("c) Warnbro Sound")+
  geom_smooth(method=lm, colour = 1, linetype =1, se=TRUE,fullrange=TRUE)+
  theme_bw() + graphics

SIMP_warnbro_shootdensity_plot

attach(SIMP_warnbro_shootdensity)
MannKendall(mean)
detach(SIMP_warnbro_shootdensity)

#################################################################
#SIMP_shoalwater Bay shoot density

SIMP_shoalwater_shootdensity <- make_shootdensity(SIMP_shoalwater)

SIMP_shoalwater_shootdensity_plot <- ggplot(SIMP_shoalwater_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(2017))) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("b) Shoalwater Bay")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se = FALSE, fullrange=TRUE) +
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_shoalwater_shootdensity_plot

attach(SIMP_shoalwater_shootdensity)
MannKendall(mean)
detach(SIMP_shoalwater_shootdensity)

###########################################################################
#SIMP_north shoot density

SIMP_north_shootdensity <- make_shootdensity(SIMP_north)

SIMP_north_shootdensity_plot<-ggplot(SIMP_north_shootdensity, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_north_shootdensity$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(50)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) density (","0.04m"^-2,")", sep = ""))) +
  ggtitle("a) Point Peron")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se = FALSE, fullrange=TRUE) +
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_north_shootdensity_plot

attach(SIMP_north_shootdensity)
MannKendall(mean)
detach(SIMP_north_shootdensity)

####################################################################################
#MAXIMUM/MEAN CANOPY HEIGHT
####################################################################################

make_maxheight <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(Maximum_height_mm)),
      mean = mean(Maximum_height_mm, na.rm = TRUE),
      sd   = sd(Maximum_height_mm, na.rm = TRUE),
      se   = sd(Maximum_height_mm, na.rm = TRUE) / sqrt(N)
    )
}

#Overall maximum canopy height density

SIMP_height <- make_maxheight(SIMP)

SIMP_height_plot <- ggplot(SIMP_height, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(2009), max(SIMP_height$Year+0.125)), breaks=min(SIMP_height$Year):max(SIMP_height$Year)) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE)",
                   "maximum height (mm)"))) +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=FALSE)+
  ggtitle("a) Maximum canopy height") +
    theme(strip.text = element_text(size=12)) + graphics+
  theme(axis.title.x=element_blank())

SIMP_height_plot

attach(SIMP_height)
MannKendall(mean)
detach(SIMP_height)

#SIMP_south max canopy height

SIMP_south_height <- make_maxheight(SIMP_south)

SIMP_south_height_plot <- ggplot(SIMP_south_height, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_south_height$Year+0.125))) +
  # scale_x_continuous(breaks = seq(2003,2017,2), limits=c(min(2001),(max(2017)))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE)",
                   "maximum height (mm)"))) +
  ggtitle("d) Becher Point") +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_south_height_plot

attach(SIMP_south_height)
MannKendall(mean)
detach(SIMP_south_height)

#############################################################
#Warnbro Sound max canopy height

SIMP_warnbro_height <- make_maxheight(SIMP_warnbro)

SIMP_warnbro_height_plot<-ggplot(SIMP_warnbro_height, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_warnbro_height$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE)",
                   "maximum height (mm)"))) +
  ggtitle("C) Warnbro Sound")+
  # geom_smooth(method = "lm", colour = 1, formula = y ~ splines::bs(x, 2), se = TRUE, fullrange=TRUE)+
  # geom_smooth(method=auto, colour = 1, linetype = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_warnbro_height_plot

attach(SIMP_warnbro_height)
MannKendall(mean)
detach(SIMP_warnbro_height)

#################################################################
#SIMP_shoalwater maxy canopy height

SIMP_shoalwater_height <- make_maxheight(SIMP_shoalwater)

SIMP_shoalwater_height_plot <- ggplot(SIMP_shoalwater_height, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_shoalwater_height$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE)",
                   "maximum height (mm)"))) +
  ggtitle("b) Shoalwater Bay")+
  # geom_smooth(method=lm, colour = 1, linetype = 3,  se=FALSE, fullrange=FALSE)+
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_shoalwater_height_plot

attach(SIMP_shoalwater_height)
MannKendall(mean)
detach(SIMP_shoalwater_height)

###########################################################################
#SIMP_north max canopy height

SIMP_north_height <- make_maxheight(SIMP_north)

SIMP_north_height_plot<-ggplot(SIMP_north_height, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_north_height$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE)",
                   "maximum height (mm)"))) +
  ggtitle("a) Point Peron")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_north_height_plot

attach(SIMP_north_height)
MannKendall(mean)
detach(SIMP_north_height)

###########################################################################################
# MEAN HEIGHT PLOTS
###########################################################################################

make_meanheight <- function(df){
  df %>%
    group_by(Year) %>%
    dplyr::summarise(
      N    = length(!is.na(Mean_height_mm)),
      mean = mean(Mean_height_mm, na.rm = TRUE),
      sd   = sd(Mean_height_mm, na.rm = TRUE),
      se   = sd(Mean_height_mm, na.rm = TRUE) / sqrt(N)
    )
}

#Overall mean canopy height density

SIMP_height1 <- make_meanheight(SIMP)

SIMP_height_plot1 <- ggplot(SIMP_height1, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_height1$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE) 80th",
                   "percentile height (mm)"))) +
  ggtitle("b) 80th percentile canopy height") +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=FALSE)+
  theme(strip.text = element_text(size=12)) + graphics

SIMP_height_plot1

attach(SIMP_height1)
MannKendall(mean)
detach(SIMP_height1)

#SIMP_south max canopy height

SIMP_south_height1 <- make_meanheight(SIMP_south)

SIMP_south_height_plot1 <- ggplot(SIMP_south_height1, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_south_height1$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE) 80th",
                   "percentile height (mm)"))) +
  # ggtitle("d) Becher Point") +
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics

SIMP_south_height_plot1

attach(SIMP_south_height1)
MannKendall(mean)
detach(SIMP_south_height1)

#############################################################
#Warnbro Sound max canopy height

SIMP_warnbro_height1 <- make_meanheight(SIMP_warnbro)

SIMP_warnbro_height_plot1<-ggplot(SIMP_warnbro_height1, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_warnbro_height1$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE) 80th",
                   "percentile height (mm)"))) +
  # ggtitle("C) Warnbro Sound")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=FALSE)+
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_warnbro_height_plot1

attach(SIMP_warnbro_height1)
MannKendall(mean)
detach(SIMP_warnbro_height1)

#################################################################
#SIMP_shoalwater maxy canopy height

SIMP_shoalwater_height1 <- make_meanheight(SIMP_shoalwater)

SIMP_shoalwater_height_plot1 <- ggplot(SIMP_shoalwater_height1, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_shoalwater_height1$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE) 80th",
                   "percentile height (mm)"))) +
  # ggtitle("b) Shoalwater Bay")+
  # geom_smooth(method=lm, colour = 1, linetype = 3,  se=FALSE, fullrange=FALSE)+
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_shoalwater_height_plot1

attach(SIMP_shoalwater_height1)
MannKendall(mean)
detach(SIMP_shoalwater_height1)

###########################################################################
#SIMP_north max canopy height

SIMP_north_height1 <- make_meanheight(SIMP_north)

SIMP_north_height_plot1<-ggplot(SIMP_north_height1, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(2003,2017,2), limits=c(min(2003), max(SIMP_north_height1$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(1000)))+
  xlab("Year") +
  ylab(bquote(atop("Mean (±SE) 80th",
                   "percentile height (mm)"))) +
  # ggtitle("a) Point Peron")+
  # geom_smooth(method=lm, colour = 1, linetype = 3, se=FALSE, fullrange=TRUE)+
  theme_bw() + graphics+
  theme(axis.title.x=element_blank())

SIMP_north_height_plot1

attach(SIMP_north_height1)
MannKendall(mean)
detach(SIMP_north_height1)

#####################################################################################
#Create figures (will be saved to current workdir)
#####################################################################################

#Shoot density
png(png_SIMP_shoot_density_fn, width=500, height=300)
grid.arrange(SIMP_shootdensity_plot)
dev.off()

png(png_shoot_density_fn, width=1000, height=600)
grid.arrange(SIMP_north_shootdensity_plot, SIMP_shoalwater_shootdensity_plot, SIMP_warnbro_shootdensity_plot, SIMP_south_shootdensity_plot, ncol=2)
dev.off()

#Canopy height
png(png_height_fn, width=500, height=600)
grid.arrange(SIMP_height_plot, SIMP_height_plot1, ncol = 1)
dev.off()

png(png_SIMP_height_fn, width=1000, height=900)
grid.arrange(SIMP_north_height_plot, SIMP_north_height_plot1, SIMP_shoalwater_height_plot, SIMP_shoalwater_height_plot1, SIMP_warnbro_height_plot, SIMP_warnbro_height_plot1, SIMP_south_height_plot, SIMP_south_height_plot1,ncol=2)
dev.off()

#####################################################################################
#Upload figures and script back to CKAN
#####################################################################################

ckanr::resource_update(png_SIMP_shoot_density_rid, png_SIMP_shoot_density_fn)
ckanr::resource_update(png_shoot_density_rid, png_shoot_density_fn)
ckanr::resource_update(png_SIMP_height_rid, png_SIMP_height_fn)
ckanr::resource_update(png_height_rid, png_height_fn)

ckanr::resource_update(txt_rid, "SIMP_shoot_density_and_canopy_height_code.R")

