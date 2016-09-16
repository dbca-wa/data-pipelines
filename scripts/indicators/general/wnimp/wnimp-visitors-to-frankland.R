source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/general/wnimp")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "c4e8c4ea-27c1-440d-a706-bf705dc4a8b2"
pdf_rid <- "c0a72ccf-5fdf-4cd2-9f63-446dcbb3f5ea"
txt_rid <- "f37fc2cb-fef6-47de-a9af-9c0b3ce271db"

pdf_fn <- "wnimp-visitors-to-frankland.pdf"
txt_fn <- "wnimp-visitors-to-frankland.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

################################# by beach

Y=transform(d, Year = as.numeric(Year))

out <- ggplot(Y, aes(x=Year, y=Frankland_visitors)) +
  geom_line() +
  geom_point(size=3) +
  xlab("") +
  ylab("Visitor numbers") +
  scale_x_continuous(limits=c(min(Y$Year-0.15), max(Y$Year+0.15)), breaks=min(Y$Year):max(Y$Year),
                     labels=c("2007-08","2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15")) +
  scale_y_continuous(limits=c(min(0),max(Y$Frankland_visitors))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.text.y=element_text(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(),          #formats y axis title
        axis.line=element_line(colour="black"),      #sets axis lines
        panel.grid.minor = element_blank(),          #removes minor grid lines
        panel.grid.major = element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        legend.justification=c(1,0.8), legend.position=c(1,0.8),        #Positions legend
        legend.title = element_blank(),
        legend.key = element_blank())

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir

pdf(pdf_fn, width=7, height=5)
print(out)
dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")