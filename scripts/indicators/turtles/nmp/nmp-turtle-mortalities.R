source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/turtles/nmp")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "f5808537-3bff-49af-8f1d-16dbb120ac63"
pdf_rid <- "7587bcb5-3bfc-47e5-a728-d800a22e6d53"
txt_rid <- "5e40b68b-8664-4c97-8fce-ef3a2de230cc"

pdf_fn <- "nmp-turtle-mortalities.pdf"
txt_fn <- "nmp-turtle-mortalities.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

pd=position_dodge(0.9)

out <- ggplot(d, aes(x=Year, y=Mortalities, group=Species, fill=Species)) +
  geom_bar(stat="identity", position=pd, colour="black") +
  xlab("") +
  ylab("Number of mortalities") +
  scale_fill_grey() +
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
        legend.key = element_blank()
        )

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
pdf(pdf_fn, height = 5, width = 7)
print(out)
dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")
