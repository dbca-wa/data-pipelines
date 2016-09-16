source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/turtles/sbmpa")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "86eeef07-490a-4223-86d5-5c5b1efc8164"
pdf_rid <- "1aa093d6-771f-4a0d-af6b-3f5c1fe10b70"
txt_rid <- "2efa2c06-8ec7-4703-8987-f6787fc268a5"

pdf_fn <- "sbmpa-loggerhead-tracks.pdf"
txt_fn <- "sbmpa-loggerhead-tracks.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor

# library(ggplot2)
# library(plyr)
library(gridExtra)

d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

################################# by beach

pd <- position_dodge(.25)

byBeach <- ggplot(d, aes(x=Year, y=Tracks, group=Beach, linetype=Beach, shape=Beach)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  xlab("") +
  ylab("Average tracks per night per beach") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        #axis.title.x=element_text(size=10),
        axis.line = element_line(colour="black"), #sets axis lines
        panel.grid.minor = element_blank(), #removes minor grid lines
        panel.grid.major = element_blank(), #removes major grid lines
        panel.border = element_blank(), #removes border
        panel.background = element_blank(), #needed to ensure integrity of axis lines
        legend.justification=c(0,1), legend.position=c(0,1), # Positions legend (x,y)
        legend.title = element_text(),
        legend.key = element_blank())

######################################## all beaches

Y <- ddply(d, .(Year_number), summarise, 
           N    = length(Tracks),
           mean = mean(Tracks, na.rm=TRUE),
           sd   = sd(Tracks, na.rm=TRUE),
           se   = sd(Tracks, na.rm=TRUE) / sqrt(length(!is.na(Tracks))) )

allBeaches <- ggplot(Y, aes(x=Year_number, y=mean)) +
  geom_line() +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, colour="black") +
  xlab("") +
  ylab("Average tracks per night total") +
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8"), 
                   labels=c("2007/08","2008/09","2009/10","2010/11","2011/12","2012/13","2013/14","2014/15")) +
  scale_y_continuous(limits=c(min(0), max(60))) +
  theme_bw() + #grid colour in this case black and white
  theme(
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    axis.line = element_line(colour="black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.justification=c(1,1), legend.position=c(1,1), # Position legend in top right
    legend.title = element_text()
  )
#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir

pdf(pdf_fn, width=7, height=8)
grid.arrange(byBeach, allBeaches, ncol=1)
dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")