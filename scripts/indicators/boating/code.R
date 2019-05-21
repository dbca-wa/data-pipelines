source("~/projects/data-pipelines/setup/ckan.R")
setwd("~/projects/data-pipelines/scripts/indicators/boating")

  # Step 1: choose data
  # http://data.dpaw.wa.gov.au/dataset/industrial-shipping-activity-at-useless-loop-in-the-shark-bay-mpa/
  # Paste resource id (the hash after "/resource/")
  d <- load_ckan_csv("6f278aed-8815-4c4c-83ab-69aa2b4fec3a")

  # Step 2: Create output
  out <- ggplot(d, aes(x=Financial.Year, y=Vessels, group=1)) +
  geom_line() +
  ylim(10, 60) + #set limits on Y
  geom_point(size=2) +  #, position=position_dodge()


    labs(title='Number of industrial vessels', x='Year', y='Vessels') +
    theme(
      axis.text.x = element_text(size=10, angle=45),
      axis.text.y = element_text(size=10),
      axis.title.x = element_text(size=10),
      axis.title.y = element_text(size=10),
      legend.title = element_text(size=10),
      legend.text = element_text(size=10),
      legend.position = 'right'
    )


  # Step 3: look at output - fiddle with Step 2, repeat until good enough
  out


# Step 4: Create PDF (will be saved to current workdir)
fn = "figure.pdf"
pdf(fn, height = 5, width = 7)
out
dev.off()


## Step 5: Upload to CKAN
## Create a file ckan_secret with this line containing your CKAN API_KEY:
# library(ckanr); ckanr::ckanr_setup(url="http://data.dpaw.wa.gov.au/", key="API_KEY")
# source("~/projects/data-pipelines/scripts/ckan_secret.R")
## The PDF and R script must already exist on CKAN (if not, create them manually)
pdf_resource_id = "7a9d07fb-1ccc-4ebb-aa5f-8fd162b0c583"
rscript_resource_id = "75bb72bf-50f5-4b79-92e3-88a63c213d7d"
## Upload the figure (PDF) and this script (TXT) to CKAN:
ckanr::resource_update(pdf_resource_id, fn)
ckanr::resource_update(rscript_resource_id, "code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")

