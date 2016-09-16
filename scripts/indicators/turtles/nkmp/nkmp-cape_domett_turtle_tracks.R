source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")
library(ggplot2)
library(plyr)


#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/turtles/nkmp")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "9a1d14d1-18e8-40ed-aee3-3bbb53044a6d"


#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- ckan_res(csv_rid)
X <- read.csv(d$url)
# Y <- load_ckan_csv(csv_rid, date_colnames=c("date", "year"))

Y <- ddply(X, .(Year), summarise, 
           N    = length(Counts),
           mean = mean(Counts, na.rm=TRUE),
           sd   = sd(Counts, na.rm=TRUE),
           se   = sd(Counts, na.rm=TRUE) / sqrt(length(!is.na(Counts))) )

pdf("nkmp-flatback_tracks_total.pdf", height=5, width=7)

ggplot(Y, aes(x=Year, y=mean)) +
  geom_line() +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1, colour="black") +
  xlab("") +
  ylab("Mean number of flatback turtle tracks per night") +
  scale_y_continuous(limits=c(min(0), max(60))) +
  theme_bw() + #grid colour in this case black and white
  theme(
    axis.text.x = element_text(),
    axis.title.x = element_blank(),
    axis.line = element_line(colour="black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.justification=c(1,1), legend.position=c(1,1), # Position legend in top right
    legend.title = element_text()
  )

dev.off()

#####################
Z=transform(X, Year = as.character(Year))

pd=position_dodge(0.9)

pdf("nkmp-flatback_tracks_per_night.pdf", height=5, width=7)

ggplot(Z, aes(x=Night, y=Counts, group=Year, fill=Year)) +
  geom_bar(stat="identity", position=pd, colour="black") +
  xlab("Night") +
  ylab("Number of flatback turtle tracks per night") +
  scale_x_continuous(limits=c(min(Z$Night-0.25), max(Z$Night+0.25)), breaks=min(Z$Night):max(Z$Night)) +
  scale_fill_grey()+
  theme_bw() + #grid colour in this case black and white
  theme(
    axis.text.x = element_text(),
    axis.title.x = element_text(),
    axis.line = element_line(colour="black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.justification=c(0,1), legend.position=c(0,1), # Position legend in top right
    legend.title = element_text(),
    legend.key = element_blank()
  )

dev.off()

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
# pdf(pdf_fn, height = 5, width = 7)
# out
# dev.off()

# set to (PDF resource ID, PDF file name)
ckanr::resource_update("48d5c096-ea0c-4c85-b247-57cfbb698f16", "nkmp-flatback_tracks_total.pdf")
ckanr::resource_update("11a7e736-afab-42f5-8c38-58984094b912", "nkmp-flatback_tracks_per_night.pdf")


ckanr::resource_update("8c3c7257-3795-48f7-9b39-4d16106df318", "nkmp-cape_domett_turtle_tracks.R")

setwd("~/projects/data-pipelines")
#------------------------------------------------------------
