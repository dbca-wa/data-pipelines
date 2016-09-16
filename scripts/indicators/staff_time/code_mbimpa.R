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
csv_rid <- "9bdeb531-8ecf-4b29-8175-1f74a5243838"


#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- ckan_res(csv_rid)
Y <- read.csv(d$url)
# Y <- load_ckan_csv(csv_rid, date_colnames=c("date", "year"))

Coral<-subset(Y, asset == "Coral" | asset == "Patrols")
Finfish<-subset(Y, asset == "Finfish" | asset == "Patrols")
Geomorphology<-subset(Y, asset == "Geomorphology" | asset == "Patrols")
Hydrocarbons<-subset(Y, asset == "Hydrocarbons")
Invertebrates<-subset(Y, asset == "Invertebrates" | asset == "Patrols")
PrimaryProducers<-subset(Y, asset == "Macroalgae and Seagrass" | asset == "Patrols")
Mammals<-subset(Y, asset == "Marine Mammals" | asset == "Patrols")
Mangroves<-subset(Y, asset == "Mangroves" | asset == "Patrols")
Mooring<-subset(Y, asset == "Mooring Management")
Oiled<-subset(Y, asset == "Oiled Wildlife")
Seabirds<-subset(Y, asset == "Seabirds" | asset == "Patrols")
Sediment<-subset(Y, asset == "Sediment Quality" | asset == "Patrols")
Turtles<-subset(Y, asset == "Turtles" | asset == "Patrols")
Water<-subset(Y, asset == "Water Quality" | asset == "Patrols")
Patrols <- subset(Y, asset == "Patrols")

pd <- position_dodge(0.25)

graphics=theme(axis.text.x=element_text(size=14), #rotates the x axis tick labels an angle of 45 degrees
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
pdf("mbimpa-person_days_coral.pdf", width=7, height=5)
ggplot(Coral, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(45))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_finfish.pdf", width=7, height=5)
ggplot(Finfish, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(45))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_hydrocarbons.pdf", width=7, height=5)
ggplot(Hydrocarbons, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(41))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_invertebrates.pdf", width=7, height=5)
ggplot(Invertebrates, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_primary_producers.pdf", width=7, height=5)
ggplot(PrimaryProducers, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_mangroves.pdf", width=7, height=5)
ggplot(Mangroves, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_mammals.pdf", width=7, height=5)
ggplot(Mammals, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(45))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_geomorphology.pdf", width=7, height=5)
ggplot(Geomorphology, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(45))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_seabirds.pdf", width=7, height=5)
ggplot(Seabirds, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(45))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_sediment.pdf", width=7, height=5)
ggplot(Sediment, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(45))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_turtles.pdf", width=7, height=5)
ggplot(Turtles, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(110))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_water.pdf", width=7, height=5)
ggplot(Water, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(45))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_mooring.pdf", width=7, height=5)
ggplot(Mooring, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(20))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_oiled_wildlife.pdf", width=7, height=5)
ggplot(Oiled, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(62))) +
  theme_bw() + graphics
dev.off()
#############################################################################
pdf("mbimpa-person_days_patrols.pdf", width=7, height=5)
ggplot(Patrols, aes(x=year, y=fte, group=asset, linetype=asset, shape=asset)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3) +
  xlab("") +
  ylab("District Staff Days (FTE)") +
  scale_y_continuous(limits=c(min(0), max(45))) +
  theme_bw() + graphics
dev.off()

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
# pdf(pdf_fn, height = 5, width = 7)
# out
# dev.off()

# set to (PDF resource ID, PDF file name)
ckanr::resource_update("1aa56dc7-4cec-4063-afa5-5330159757c8", "mbimpa-person_days_coral.pdf")
ckanr::resource_update("31e443b2-59cd-4b24-9be4-2249ebce30b5", "mbimpa-person_days_finfish.pdf")
ckanr::resource_update("a41f4b1a-e46b-45cc-9a88-31d71de2263b", "mbimpa-person_days_hydrocarbons.pdf")
ckanr::resource_update("5977b267-b64b-4fe2-91be-fa9f60809352", "mbimpa-person_days_invertebrates.pdf")
ckanr::resource_update("2cf6e13a-c371-4d99-aa46-1b4c4ea6bf1b", "mbimpa-person_days_primary_producers.pdf")
ckanr::resource_update("56cbfbd9-537a-475b-a49b-9f210a665029", "mbimpa-person_days_mangroves.pdf")
ckanr::resource_update("e59c2d47-4da7-41fe-a8cf-a99e479a6180", "mbimpa-person_days_mammals.pdf")
ckanr::resource_update("50649365-6b67-40e6-8b89-cad388136ffa", "mbimpa-person_days_geomorphology.pdf")
ckanr::resource_update("162a9999-9029-4678-bc34-b021c33ac3e1", "mbimpa-person_days_seabirds.pdf")
ckanr::resource_update("7263e762-5569-4405-aff6-59792410130d", "mbimpa-person_days_sediment.pdf")
ckanr::resource_update("4473b896-1542-41d2-8e83-e198664f417f", "mbimpa-person_days_turtles.pdf")
ckanr::resource_update("e61ba700-f5f5-470d-ab40-f02c99175e08", "mbimpa-person_days_water.pdf")
ckanr::resource_update("40af0d8a-9ba2-4f7d-998b-35d824dc21b7", "mbimpa-person_days_mooring.pdf")
ckanr::resource_update("0c534cfd-8f84-4c53-80a2-49d6f5747a6f", "mbimpa-person_days_oiled_wildlife.pdf")
ckanr::resource_update("da03d987-f3a3-4329-a1be-222a8f700c78", "mbimpa-person_days_patrols.pdf")

ckanr::resource_update("b66659f5-8dc4-4de3-80a3-7e020c6ecaaa", "code_mbimpa.R")

setwd("~/projects/data-pipelines")
#------------------------------------------------------------
