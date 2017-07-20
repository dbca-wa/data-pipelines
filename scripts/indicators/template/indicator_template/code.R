source("~/projects/data-pipelines/setup/ckan.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/template")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "fd874a78-4d65-46fe-ba07-3b1ef0c54269"
pdf_rid <- ""
txt_rid <- ""

pdf_fn <- "figure.pdf"
txt_fn <- "code.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid)

pd <- position_dodge(0.1)
et10 <- element_text(size=10, angle=0)

out <- ggplot(d, aes(x=Year, y=Mean1)) +
  geom_point(aes(x=Year, y=Mean1, group=Site, col=Site), position=pd, size=2) +
  geom_line(aes(x=Year, y=Mean1, group=Site, col=Site), position=pd) +
  labs(title='Y axis metric name', x='Time', y='') +
  scale_x_datetime(
    labels=scales::date_format('%Y-%m'),
    #limits=c(as.POSIXct('2000-11-28 08:00:00'), as.POSIXct('2015-06-18 08:00:00')),
    breaks='1 year', minor_breaks='6 months') +
  theme(
    axis.text.x = et10,
    axis.text.y = et10,
    axis.title.x = et10,
    axis.title.y = et10,
    legend.title = et10,
    legend.text = et10,
    legend.position = 'right'
  )

# out

#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
pdf(pdf_fn, height = 7, width = 7); print(out); dev.off()
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)
setwd("~/projects/data-pipelines")
