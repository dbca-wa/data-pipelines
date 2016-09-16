source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")
library(ggplot2)
library(plyr)


#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/staff_time")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "4b303f18-e293-45ca-9c5d-b90710972739"


#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- ckan_res(csv_rid)
Y <- read.csv(d$url)
# Y <- load_ckan_csv(csv_rid, date_colnames=c("date", "year"))

pd <- position_dodge(0.1)
et10 <- element_text(size=10, angle=0)

Birds<-subset(Y, asset == "Birds")
Cetaceans<-subset(Y, asset == "Cetaceans")
Coral<-subset(Y, asset == "Coral")
Finfish<-subset(Y, asset == "Finfish")
Geomorphology<-subset(Y, asset == "Geomorphology")
Invertebrates<-subset(Y, asset == "Invertebrates")
Turtles<-subset(Y, asset == "Turtles")
Seascapes<-subset(Y, asset == "Seascapes")
Water<-subset(Y, asset == "Water Quality")

pd <- position_dodge(0.25)

graphics=theme(axis.text.x=element_text(size=14),           #rotates the x axis tick labels an angle of 45 degrees
               axis.text.y=element_text(size=14),
               axis.title.x=element_blank(),                #removes x axis title
               axis.title.y=element_text(size=16),          #removes y axis title
               axis.line=element_line(colour="black"),      #sets axis lines
               panel.grid.minor = element_blank(),          #removes minor grid lines
               panel.grid.major = element_blank(),          #removes major grid lines
               panel.border=element_blank(),                #removes border
               panel.background=element_blank(),            #needed to ensure integrity of axis lines
               legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
               legend.title = element_text(),
               legend.key = element_blank())
#############################################################################
pdf("rsmp-person_days_birds.pdf", width=7, height=5)
ggplot(Birds, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(0,4)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-person_days_cetaceans.pdf", width=7, height=5)
ggplot(Cetaceans, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-person_days_coral.pdf", width=7, height=5)
ggplot(Coral, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-person_days_finfish.pdf", width=7, height=5)
ggplot(Finfish, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-person_days_geomorphology.pdf", width=7, height=5)
ggplot(Geomorphology, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-person_days_invertebrates.pdf", width=7, height=5)
ggplot(Invertebrates, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-person_days_turtles.pdf", width=7, height=5)
ggplot(Turtles, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-person_days_seascapes.pdf", width=7, height=5)
ggplot(Seascapes, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-person_days_water.pdf", width=7, height=5)
ggplot(Water, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
# pdf(pdf_fn, height = 5, width = 7)
# out
# dev.off()

# set to (PDF resource ID, PDF file name)
ckanr::resource_update("e1cd6604-adfc-45db-be3f-031a4fe54f0c", "rsmp-person_days_birds.pdf")
ckanr::resource_update("ca0afe6a-fa01-4fca-8264-79fda7cb86b3", "rsmp-person_days_cetaceans.pdf")
ckanr::resource_update("0b93ee30-6716-4457-8c45-af6ee54be649", "rsmp-person_days_coral.pdf")
ckanr::resource_update("b9e4c740-ec2e-42e1-acf8-b204836d9b06", "rsmp-person_days_finfish.pdf")
ckanr::resource_update("313dcc5c-3eb0-47bd-9fcf-6c1a0e6ff9d0", "rsmp-person_days_geomorphology.pdf")
ckanr::resource_update("fefd838c-d21d-489b-b98e-c062b0bc362e", "rsmp-person_days_invertebrates.pdf")
ckanr::resource_update("16a2d5b5-1ecd-4648-b94e-787a02c8809b", "rsmp-person_days_seascapes.pdf")
ckanr::resource_update("06bff1a0-61ba-40d6-8666-146791e34115", "rsmp-person_days_turtles.pdf")
ckanr::resource_update("820e2712-ca07-4e1b-9fc0-8066707d7253", "rsmp-person_days_water.pdf")


ckanr::resource_update("32d54268-ca5d-4001-8153-c895e8b07a99", "code_rsmp.R")

setwd("~/projects/data-pipelines")
#------------------------------------------------------------
