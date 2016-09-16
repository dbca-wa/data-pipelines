source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/common_indicators/mbimpa")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "3f6f6bf2-2905-4fa9-8c89-eed025e55b3a"
pdf_rid <- "1a459f06-5c15-40d9-b0ac-99491c48c6a4"
txt_rid <- "f7ae88ea-329b-4ac8-83a0-dac9bbb98971"

pdf_fn <- "mbimpa-licenced-charter-operators.pdf"
txt_fn <- "mbimpa-licenced-charter-operators.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

pd <- position_dodge(0.1)
# et10 <- element_text(size=10, angle=0)

out <- ggplot(d, aes(x=Year_number, y=Licences)) +
  geom_line() +
  geom_point(size=3) +
  xlab("") +
  ylab("Number of charter licences") +
  scale_x_discrete(limits=c("1","2","3","4","5","6","7"),
                   labels=c("2008-09","2009-10","2010-11","2011-12","2012-13","2013-14","2014-15")) +
  scale_y_continuous(limits=c(0,20)) +
  theme_bw() +
  theme(axis.text.x = element_text(),
        axis.line=element_line(colour="black"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border=element_blank(),
        panel.background=element_blank(),
        legend.justification=c(1,1), legend.position=c(1,1), # Position legend in top right
        legend.title = element_blank(),
        legend.key = element_blank())

# out

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
pdf(pdf_fn, width=7, height=5)
print(out)
dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)
setwd("~/projects/data-pipelines")
