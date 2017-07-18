#Load libraries
library(Hmisc)
library(plyr)
library(ggplot2)
library(mgcv)
library (Kendall)
library(reshape2)
library(plotly)#install.packages("gridExtra")
library(gridExtra)
library(plyr)
library (Kendall)

d <- Book4

d1= subset(d, Habitat %in% c("Sparse", "Dense"))
d1$Habitat <- factor(d1$Habitat, levels = c("Sparse", "Dense"))

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
                 legend.key = element_blank())

cols <- c(Dense="black",Sparse="lightgrey")

haplot <- ggplot(d1, aes(x = Year, y = km, fill = Habitat)) +
  geom_bar(stat = "identity")+
  ylab(expression(paste("Total extent (","km"^-2,")", sep = "")))+
  scale_x_continuous(breaks = seq(2002,2016,2), limits=c(min(2001),(max(2017)))) +
  scale_y_continuous(breaks = seq(0, 3000, 500), expand = c(0, 0), limits=c(min(0), max(3000)))+
  scale_fill_manual(values = cols)+
  theme_bw() + graphics
haplot


perplotp <- ggplot(d1, aes(x = Year, y = percent, fill = Habitat)) +
  geom_bar(stat = "identity")+
  ylab(expression(paste("Seagrass percent (%)")))+
  scale_x_continuous(breaks = seq(2002,2016,2),limits=c(min(2001),(max(2017)))) +
  scale_y_continuous(breaks = seq(0, 100, 10),expand = c(0,0), limits=c(min(0), max(100)))+
  scale_fill_manual(values = cols)+
  theme_bw() + graphics
perplotp

png_SBMP_extent_fn = "SBMP extent.png"#Name of final figure

png(png_SBMP_extent_fn, width=600, height=500)
grid.arrange(haplot,perplotp)
dev.off()





d <- Book4
d$percent=(d$Cover * 100)
names(d)[names(d)=="Habitat"] <- "percent"
names(d)[names(d)=="X3"] <- "Habitat"

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
                 legend.key = element_blank())


#OverallShoot density
habitatcover <- plyr::ddply(d, .(Year, Habitat), summarise,
                                 N    = length(!is.na(percent)),
                                 mean = mean(percent, na.rm=TRUE),
                                 sd   = sd(percent, na.rm=TRUE),
                                 se   = sd(percent, na.rm=TRUE) / sqrt(length(!is.na(percent)) ))
habitatcover

habitat_plot <- ggplot(habitatcover, aes(x=Year, y=mean, group=Habitat)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, colour="black", position=pd) +
  geom_point(aes(shape = Habitat), size = 4) + # 21 is filled circle
  # geom_line()+
  scale_x_continuous(limits=c(min(habitatcover$Year-0.125), max(habitatcover$Year+0.125)), breaks=min(habitatcover$Year):max(habitatcover$Year)) +
  scale_y_continuous(breaks = seq(0, 100, 10),expand = c(0,0), limits=c(min(0), max(75)))+
  xlab("Year") +
  ylab(expression(paste("Mean cover", sep = ""))) +
  stat_smooth(method = "lm", linetype = 3, colour = "black", se=FALSE)+
  theme_bw() + graphics+
  theme(axis.text.x=element_text(angle=75, size = 15, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
        axis.title.x=element_text( size = 20), #removes x axis title
        axis.title.y=element_text(size = 20), #removes y axis title
        axis.text.y=element_text(size = 15))
        # legend.justification=c(3,1), legend.position=c(1,1), # Positions legend (x,y) in this case removes it from the graph
        # legend.title = element_text("Benthic habitat"),
        # legend.key = element_blank())

habitat_plot

attach(habitatcover)
MannKendall(mean)
detach(habitatcover)

png_SBMPdrop_fn = "SBMPdropcamera.png"#Name of final figure

png(png_SBMPdrop_fn, width=800, height=500)
grid.arrange(habitat_plot)
dev.off()
