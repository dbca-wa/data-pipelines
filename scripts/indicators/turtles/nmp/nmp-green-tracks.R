source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/turtles/nmp")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "c4357b0b-a5c2-4eea-b17b-2522a8262e1e"
pdf_rid <- "f4ac1e21-8da7-47d7-beec-bd49069be9df"
txt_rid <- "c2afd790-7272-44fd-9869-8e72ac2d94f2"

pdf_fn <- "nmp-green-tracks.pdf"
txt_fn <- "nmp-green-tracks.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

out <- ggplot(d, aes(x=Year_number, y=Track_counts)) +
  geom_line() +
  geom_point() + # 21 is filled circle
  xlab("") +
  ylab("Average green turtle activity per subsection per day") +
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13"), 
                   labels=c("2002-03", "2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11",
                            "2011-12","2012-13","2013-14","2014-15")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        #axis.title.x=element_text(size=10),
        axis.line = element_line(colour="black"), #sets axis lines
        panel.grid.minor = element_blank(), #removes minor grid lines
        panel.grid.major = element_blank(), #removes major grid lines
        panel.border = element_blank(), #removes border
        panel.background = element_blank(), #needed to ensure integrity of axis lines
        #         legend.justification=c(0,1), legend.position=c(0,1), # Positions legend (x,y)
        #         legend.title = element_text("Beach"),
        legend.key = element_blank())


#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
pdf(pdf_fn, height = 5, width = 7)
print(out)
dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")
