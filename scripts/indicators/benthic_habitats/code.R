setwd("~/projects/data-pipelines/scripts/indicators/benthic_habitats")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/setup/ckan_setup.R")
library(tidyr) # clean data
library(dplyr) # munge clean data
library(ggplot2) # visualise munged data
library(DT) # DataTables widget
library(vegan) # Multivariate community data analysis

csv_rid <- "216cdeb3-7c94-46bd-be62-531e2b469c57"
pdf_rid = "40ca93b6-09e5-457a-bb8a-3e884b61d75d"
pdf_fn <- "habitat.pdf"
txt_rid = "efbe602f-8800-4198-be63-eed01e03338f"
txt_fn <- "code.R"

# Raw data from CKAN
d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date', 'time', 'Time'))

# Inspect raw data
dplyr::tbl_df(d)
dplyr::glimpse(d)
levels(d$Park)
levels(d$Sector)
levels(d$Zone)
levels(d$ZoneCode)
levels(d$Site)
levels(d$SiteCode)
levels(d$Replicate)
levels(d$ReplicateCode)
levels(d$Survey)

# Survey (replicate x date) class level 3
sp3 <- d %>%
  tbl_df() %>%
  group_by(Survey, Date, Park, Sector, Zone, Site, ReplicateCode, Level3Class) %>%
  tally() %>%
  spread(Level3Class, n, fill=0)

row.names(sp3) <- sp3$Survey
X = select(sp3, -Survey, -Date, -Park, -Sector, -Zone, -Site, -ReplicateCode)
Y = select(sp3, Date, Park, Sector, Zone, Site, ReplicateCode)
datatable(X)
datatable(Y)

X.rel <- decostand(X, "total", 1)
datatable(X.rel)

X.hel <- decostand(X, "hellinger", 1)
datatable(X.hel)
X.rda.hel <- vegan::rda(X.hel, scale=F)
X.rda.hel.plot <- plot(X.rda.hel, main="RDA of Hellinger-transformed surveys (Lvl3)")

# text(X.rda, display="sites")

# Figure in development
# Totals (counts) of Classification Level 1
# all Surveys combined (which is nonsense)
ggplot(sp1, aes(Survey)) +
  geom_bar(position="stack") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5))


# Step 4: Create PDF (will be saved to current workdir)
pdf(pdf_fn, height = 9, width = 7)
X.rda.hel.plot
dev.off()

## Step 5: Upload to CKAN
## Create a file ckan_secret with this line containing your CKAN API_KEY:
# library(ckanr); ckanr::ckanr_setup(url="http://internal-data.dpaw.wa.gov.au/", key="API_KEY")
## The PDF and R script must already exist on CKAN (if not, create them once)

## Upload the figure (PDF) and this script (TXT) to CKAN:
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines/")
