source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/turtles/nmp")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "75ff4cc3-cabf-42e9-af21-59c771e95a10"
pdf_rid <- "15b16cca-05f9-436d-953a-92ffafe0910d"
txt_rid <- "2d1dc32f-27fb-4fed-94b9-3f9d3d3da3e6"

pdf_fn <- "nmp-turtle-tours.pdf"
txt_fn <- "nmp-turtle-tours.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

Y=transform(d, Year = as.numeric(Year))

out <- ggplot(Y, aes(x=Year, y=People)) +
  geom_line() +
  geom_point(size=3) +
  xlab("") +
  ylab("Number of people") +
  scale_x_continuous(limits=c(min(Y$Year-0.15), max(Y$Year+0.15)), breaks=min(Y$Year):max(Y$Year), 
                     labels=c("2010-11","2011-12","2012-13","2013-14","2014-15")) +
  scale_y_continuous(limits=c(min(0), max(600))) +
  theme_bw() +
  theme(axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=16),          #formats y axis title
        axis.line=element_line(colour="black"),      #sets axis lines
        panel.grid.minor = element_blank(),          #removes minor grid lines
        panel.grid.major = element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        legend.justification=c(0,1), legend.position=c(0,1),        #Positions legend
        legend.title = element_blank(),
        legend.key = element_blank())


#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
pdf(pdf_fn, height = 5, width = 7)
print(out)
dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")
