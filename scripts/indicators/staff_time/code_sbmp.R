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
csv_rid <- "c639a020-3e6a-4ee3-a444-581dda675809"


#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- ckan_res(csv_rid)
Y <- read.csv(d$url)
# Y <- load_ckan_csv(csv_rid, date_colnames=c("date", "year"))

pd <- position_dodge(0.1)
et10 <- element_text(size=10, angle=0)

Cetaceans<-subset(Y, asset == "Cetaceans")
Coral<-subset(Y, asset == "Coral")
Dolphins<-subset(Y, asset == "Dolphins")
Dugongs<-subset(Y, asset == "Dugongs")
Finfish<-subset(Y, asset == "Finfish")
Geomorphology<-subset(Y, asset == "Geomorphology")
Invertebrates<-subset(Y, asset == "Invertebrates")
Mangroves<-subset(Y, asset == "Mangroves")
Microbial<-subset(Y, asset == "Microbial Communities")
Coastalbirds<-subset(Y, asset == "Coastal Birds")
Seagrass<-subset(Y, asset == "Seagrass")
Seascapes<-subset(Y, asset == "Seascapes")
Snakes<-subset(Y, asset == "Sea Snakes")
Sediment<-subset(Y, asset == "Sediment Quality")
Loggerheads<-subset(Y, asset == "Loggerheads")
Greens<-subset(Y, asset == "Greens")
Water<-subset(Y, asset == "Water Quality")
Wilderness<-subset(Y, asset == "Wilderness")

pd <- position_dodge(0.25)

graphics <- theme(axis.text.x=element_text(size=14), #rotates the x axis tick labels an angle of 45 degrees
               axis.text.y=element_text(size=14),
               axis.title.x=element_blank(), #removes x axis title
               axis.title.y=element_text(size=16), #removes y axis title
               axis.line=element_line(colour="black"), #sets axis lines
               panel.grid.minor = element_blank(), #removes minor grid lines
               panel.grid.major = element_blank(), #removes major grid lines
               panel.border=element_blank(), #removes border
               panel.background=element_blank(), #needed to ensure integrity of axis lines
               legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
               legend.title = element_text(),
               legend.key = element_blank())

#############################################################################
pdf("sbmpa-person_days_cetaceans.pdf", width=7, height=5)
ggplot(Cetaceans, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_coral.pdf", width=7, height=5)
ggplot(Coral, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  #scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
# pdf("sbmpa-person_days_dolphins.pdf", width=7, height=5)
# ggplot(Dolphins, aes(x=year, y=fte, group=1)) +
#   geom_line(position=pd) +
#   geom_point(position=pd, size=3) +
#   xlab("") +
#   ylab("District Staff Days (FTE)") +
#   theme_bw() + graphics
# dev.off()
#############################################################################
pdf("sbmpa-person_days_dugongs.pdf", width=7, height=5)
ggplot(Dugongs, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_finfish.pdf", width=7, height=5)
ggplot(Finfish, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_geomorphology.pdf", width=7, height=5)
ggplot(Geomorphology, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_invertebrates.pdf", width=7, height=5)
ggplot(Invertebrates, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_mangroves.pdf", width=7, height=5)
ggplot(Mangroves, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  #scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_microbial.pdf", width=7, height=5)
ggplot(Microbial, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  #scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_coastalbirds.pdf", width=7, height=5)
ggplot(Coastalbirds, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_seagrass.pdf", width=7, height=5)
ggplot(Seagrass, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_seascapes.pdf", width=7, height=5)
ggplot(Seascapes, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_snakes.pdf", width=7, height=5)
ggplot(Snakes, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_sediment.pdf", width=7, height=5)
ggplot(Sediment, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(0,2)) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_loggerheads.pdf", width=7, height=5)
ggplot(Loggerheads, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_greens.pdf", width=7, height=5)
ggplot(Greens, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_water.pdf", width=7, height=5)
ggplot(Water, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("sbmpa-person_days_wilderness.pdf", width=7, height=5)
ggplot(Wilderness, aes(x=year, y=fte, group=1)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
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
ckanr::resource_update("c872726e-7990-45ed-a167-2c1ccc1e4670", "sbmpa-person_days_cetaceans.pdf")
ckanr::resource_update("ba9d4bf7-8a07-40ed-981d-261da84f7aef", "sbmpa-person_days_coral.pdf")
ckanr::resource_update("801b5c27-c58b-4363-906c-0c5b32ae9354", "sbmpa-person_days_dugongs.pdf")
ckanr::resource_update("8eb76ea9-d9c2-40b2-b43f-d203d03ee2a6", "sbmpa-person_days_finfish.pdf")
ckanr::resource_update("d1d09c19-0a47-4115-9377-5293c51c979a", "sbmpa-person_days_geomorphology")
ckanr::resource_update("bc29879a-4d37-44ad-9893-ea4b0f66cc17", "sbmpa-person_days_invertebrates")
ckanr::resource_update("b4f49f28-ad07-4b18-9294-47ca6f4016fa", "sbmpa-person_days_mangroves.pdf")
ckanr::resource_update("b43764cf-1ae9-46b1-8c99-5175fecd7ef4", "sbmpa-person_days_microbial.pdf")
ckanr::resource_update("82d65e14-2b50-47e5-b3eb-9ae044961b33", "sbmpa-person_days_coastalbirds.pdf")
ckanr::resource_update("4aeae241-824f-41c6-8c8f-bd196bd7ca9c", "sbmpa-person_days_seagrass.pdf")
ckanr::resource_update("295e4f4a-c97f-4d3b-a5e8-34cfe365f452", "sbmpa-person_days_seascapes.pdf")
ckanr::resource_update("5301f86c-17be-46fb-b3e2-bdac351238fc", "sbmpa-person_days_sediment.pdf")
ckanr::resource_update("ea2d8ade-024a-4525-ac46-3d0283a34f31", "sbmpa-person_days_loggerheads.pdf")
ckanr::resource_update("1cb05dfe-49a0-4390-96eb-44f414439a90", "sbmpa-person_days_greens.pdf")
ckanr::resource_update("b720b756-be5a-44aa-afa2-8e9975e1c4d1", "sbmpa-person_days_water.pdf")
ckanr::resource_update("d87a9b82-ffad-4167-870f-1e31414f4bfe", "sbmpa-person_days_wilderness.pdf")


ckanr::resource_update("019fe7bb-5ad0-46c0-b4f7-96d0a9dd6748", "code_sbmp.R")

setwd("~/projects/data-pipelines")
#------------------------------------------------------------
