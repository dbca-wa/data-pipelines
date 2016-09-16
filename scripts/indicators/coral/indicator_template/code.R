source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
## Settings
## Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/template")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "fd874a78-4d65-46fe-ba07-3b1ef0c54269"
pdf_rid <- ""
txt_rid <- ""

pdf_fn <- "figure.pdf"
txt_fn <- "code.R"

#------------------------------------------------------------------------------#
## Analysis - your code
d <- load_ckan_csv(csv_rid) # your data as data.frame

pd <- position_dodge(0.1)
out <- ggplot(d, aes(x=Year, y=Mean1)) +
  geom_point(aes(x=Year, y=Mean1, group=Site, shape=Site, col=Site), position=pd, size=2) +
  geom_line(aes(x=Year, y=Mean1, group=Site, shape=Site, col=Site), position=pd) +
  labs(title='Y axis metric name', x='Time', y='') +
  scale_x_datetime(
    labels=scales::date_format('%Y-%m'),
    #limits=c(as.POSIXct('2000-11-28 08:00:00'), as.POSIXct('2015-06-18 08:00:00')),
    breaks='1 year', minor_breaks='6 months') +
  theme(
    axis.text.x = element_text(size=10, angle=0),
    axis.text.y = element_text(size=10),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10),
    legend.title = element_text(size=10),
    legend.text = element_text(size=10),
    legend.position = 'right'
  )

out

#------------------------------------------------------------------------------#
## Save outputs and upload to CKAN, restore workdir
pdf(fn, height = 5, width = 7); out; dev.off()

ckanr::resource_update(pdf_resource_id, pdf_fn)
ckanr::resource_update(rscript_resource_id, txt_fn)

setwd("~/projects/data-pipelines")
