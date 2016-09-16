source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")
library(ggplot2)
library(plyr)


#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/operational_funds")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "cf1f3333-97bb-4619-8b57-24ccf703094d"


#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- ckan_res(csv_rid)
Y <- read.csv(d$url)
# Y <- load_ckan_csv(csv_rid, date_colnames=c("date", "year"))

Birds<-subset(Y, Asset == "Seabirds")
Cetaceans<-subset(Y, Asset == "Cetaceans")
Coral<-subset(Y, Asset == "Coral")
Finfish<-subset(Y, Asset == "Finfish")
Geomorphology<-subset(Y, Asset == "Geomorphology")
Invertebrates<-subset(Y, Asset == "Invertebrates")
Turtles<-subset(Y, Asset == "Turtles")
Seascapes<-subset(Y, Asset == "Seascapes")
Water<-subset(Y, Asset == "Water Quality")

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
pdf("rsmp-operational_funds_birds.pdf", width=7, height=5)
ggplot(Birds, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,2500)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-operational_funds_cetaceans.pdf", width=7, height=5)
ggplot(Cetaceans, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-operational_funds_coral.pdf", width=7, height=5)
ggplot(Coral, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-operational_funds_finfish.pdf", width=7, height=5)
ggplot(Finfish, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,3000)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-operational_funds_geomorphology.pdf", width=7, height=5)
ggplot(Geomorphology, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-operational_funds_invertebrates.pdf", width=7, height=5)
ggplot(Invertebrates, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-operational_funds_turtles.pdf", width=7, height=5)
ggplot(Turtles, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-operational_funds_seascapes.pdf", width=7, height=5)
ggplot(Seascapes, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("rsmp-operational_funds_water.pdf", width=7, height=5)
ggplot(Water, aes(x=Year, y=Funds)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("Year") +
  ylab("District Operational Funds ($)") +
  scale_y_continuous(limits=c(0,1500)) +
  theme_bw() + graphics
dev.off()

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
# pdf(pdf_fn, height = 5, width = 7)
# out
# dev.off()

# set to (PDF resource ID, PDF file name)
ckanr::resource_update("bab0511c-c5c7-49d9-acc8-72e2da685a7d", "rsmp-operational_funds_birds.pdf")
ckanr::resource_update("91181f75-8ea7-4868-a2ec-e01d4ed66835", "rsmp-operational_funds_cetaceans.pdf")
ckanr::resource_update("2601889a-8746-4655-b34b-f4a96c6b37fc", "rsmp-operational_funds_coral.pdf")
ckanr::resource_update("c51a5cd4-25da-4a4f-9c89-7c48a0d69902", "rsmp-operational_funds_finfish.pdf")
ckanr::resource_update("bbfb952e-cd07-4075-9b17-1f6bb99cdb4d", "rsmp-operational_funds_geomorphology.pdf")
ckanr::resource_update("5a09d697-ea12-4a2e-b15d-542cf0173524", "rsmp-operational_funds_invertebrates.pdf")
ckanr::resource_update("c3c27ae3-54de-43fc-a97a-20071a63f237", "rsmp-operational_funds_seascapes.pdf")
ckanr::resource_update("1ad11c77-9744-4de6-a221-bc4f49279c39", "rsmp-operational_funds_turtles.pdf")
ckanr::resource_update("c759b80a-3ad2-4b58-8a02-a2dd07174dda", "rsmp-operational_funds_water.pdf")


ckanr::resource_update("2ba15398-2729-4c65-bd68-c3134ace9c08", "code_rsmp.R")

setwd("~/projects/data-pipelines")
#------------------------------------------------------------
