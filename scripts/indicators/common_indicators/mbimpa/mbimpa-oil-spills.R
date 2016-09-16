source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/common_indicators/mbimpa")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "6f119af3-e783-40ff-8892-1e59f320b32d"
pdf_rid <- "7b86f7a2-ea5d-40f8-b9c8-bd28dabbffc9"
txt_rid <- "2b8ef461-66cc-471d-828a-93a3951e4569"

pdf_fn <- "mbimpa-oil-spills.pdf"
txt_fn <- "mbimpa-oil-spills.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

pd <- position_dodge(0.1)

out <- ggplot(d, aes(x=Year, y=Number, group=Severity, linetype=Severity, shape=Severity)) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  xlab("") +
  ylab("Number of reports") +
  theme_bw() +
  theme(axis.text.x = element_text(),
        #axis.title.x=element_text(size=10),
        axis.line = element_line(colour="black"), #sets axis lines
        panel.grid.minor = element_blank(), #removes minor grid lines
        panel.grid.major = element_blank(), #removes major grid lines
        panel.border = element_blank(), #removes border
        panel.background = element_blank(), #needed to ensure integrity of axis lines
        legend.justification=c(0,1), legend.position=c(0,1), # Positions legend (x,y)
        legend.title = element_text(),
        legend.key = element_blank())

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
pdf(pdf_fn, width=7, height=5)
print(out)
dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)
setwd("~/projects/data-pipelines")
