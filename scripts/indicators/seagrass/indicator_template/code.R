## Please read "scripts/indicators/template/README.md" first!

## Step 0: Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/template")

source("~/projects/data-pipelines/setup/ckan.R")

## Step 1: choose data
## Paste the resource id of your CSV file (the hash after "/resource/")
d <- load_ckan_csv("fd874a78-4d65-46fe-ba07-3b1ef0c54269")

## Step 2: Create output
## Adjust parameters to fit your data
out <- ggplot(d, aes(x=Year, y=Mean1)) +
  geom_point(aes(x=Year, y=Mean1, group=Site, shape=Site, col=Site), position=position_dodge(), size=2) +
  geom_line(aes(x=Year, y=Mean1, group=Site, shape=Site, col=Site), position=position_dodge()) +
  labs(title='Y axis metric name', x='Time', y='') +
  scale_x_datetime(labels=scales::date_format('%Y-%m'),
                   # limits=c(as.POSIXct('2000-11-28 08:00:00'), as.POSIXct('2015-06-18 08:00:00')),
                   breaks='1 year', minor_breaks='3 months') +
  theme(
    axis.text.x = element_text(size=10, angle=0),
    axis.text.y = element_text(size=10),
    axis.title.x = element_text(size=10),
    axis.title.y = element_text(size=10),
    legend.title = element_text(size=10),
    legend.text = element_text(size=10),
    legend.position = 'right'
  )

# Step 3: look at output - adjust Step 2, repeat until good enough
out

# Step 4: Create PDF (will be saved to current workdir)
fn = "figure.pdf"
pdf(fn, height = 5, width = 7); out; dev.off()

## Step 5: Upload to CKAN
## Create a file ckan_secret with this line containing your CKAN API_KEY:
# library(ckanr); ckanr::ckanr_setup(url="http://internal-data.dpaw.wa.gov.au/", key="API_KEY")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## The PDF and R script must already exist on CKAN (if not, create them manually)
pdf_resource_id = ""
rscript_resource_id = ""
## Upload the figure (PDF) and this script (TXT) to CKAN: 
ckanr::resource_update(pdf_resource_id, fn)
ckanr::resource_update(rscript_resource_id, "code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
