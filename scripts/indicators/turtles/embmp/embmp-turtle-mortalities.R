source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/turtles/embmp")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "fe7a931f-f932-4b0f-8f4f-ab23c7d91069"
pdf_rid <- "b2d05636-f56e-4f3c-9a46-7a8ab49467f9"
txt_rid <- "2a2eb719-d7e7-45fa-a61c-b697fc9bfbe2"

pdf_fn <- "embmp-turtle-mortalities.pdf"
txt_fn <- "embmp-turtle-mortalities.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

################################# by beach

pd=position_dodge(0.2)

out <- ggplot(d, aes(x=Year, y=Mortalities, group=Species, linetype=Species, shape=Species)) +
  geom_line(position=pd) +
  geom_point(position=pd) + # 21 is filled circle
  xlab("") +
  ylab("Number of mortalities") +
  scale_y_continuous(limits=c(min(0), max=(3))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        #axis.title.x=element_text(size=10),
        axis.line = element_line(colour="black"), #sets axis lines
        panel.grid.minor = element_blank(), #removes minor grid lines
        panel.grid.major = element_blank(), #removes major grid lines
        panel.border = element_blank(), #removes border
        panel.background = element_blank(), #needed to ensure integrity of axis lines
        legend.justification=c(0,1), legend.position=c(0,1), # Positions legend (x,y)
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