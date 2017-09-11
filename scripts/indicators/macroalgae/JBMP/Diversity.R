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
# png_JBMP_diversity_rid <- "f98ef2b7-2c97-4d7b-a0d7-10d3001a6db"
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
#####################################################################################

shallow <- make_diverse(shallow)

shallow_plot <- ggplot(shallow, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(shallow$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(120)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) diversity", sep = ""))) +
  ggtitle("a) Shallow sites")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
shallow_plot

attach(shallow)
MannKendall(mean)
detach(shallow)

######################################################################################

deep <- make_diverse(deep)

deep_plot <- ggplot(deep, aes(x=Year, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous (breaks = seq(1998,2017,2), limits=c(min(1998),
                                                          max(deep$Year+0.125))) +
  scale_y_continuous(limits=c(min(0), max(120)))+
  xlab("Year") +
  ylab(expression(paste("Mean (±SE) diversity", sep = ""))) +
  ggtitle("b) Deep sites")+
  # geom_smooth(method=lm, colour = 1, se=TRUE, fullrange=TRUE)+
  theme_bw() + graphics
deep_plot

attach(deep)
MannKendall(mean)
detach(deep)

################################################################################
#Create figures (will be saved to current workdir)
################################################################################

#diversity

png(png_JBMP_diversity_fn, width=500, height=600)
grid.arrange(shallow_plot, deep_plot, ncol = 1)
dev.off()
