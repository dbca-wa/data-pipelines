source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/general/wnimp")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "f45eaf1d-1bd3-4ef8-be1a-86ab629f2f6e"
pdf_rid <- "3264c258-8f2c-4e93-9a3f-3f4a271cdb0a"
txt_rid <- "cfd5d18e-aae3-4df0-a0c7-485a23bb1eee"

pdf_fn <- "wnimp-visitors-per-access-site.pdf"
txt_fn <- "wnimp-visitors-per-access-site.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

################################# by beach

Y=transform(d, Year = as.numeric(Year))

out <- ggplot(Y, aes(x=Year, y=Visitors, group=Location, linetype=Location, shape=Location)) +
  geom_line() +
  geom_point(size=3) +
  xlab("") +
  ylab("Visitor numbers") +
  scale_x_continuous(limits=c(min(Y$Year-0.15), max(Y$Year+0.15)), breaks=min(Y$Year):max(Y$Year),
                     labels=c("2009-10","2010-11","2011-12","2012-13","2013-14","2014-15")) +
  scale_y_continuous(limits=c(min(0),max(Y$Visitors))) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, hjust=1),
        axis.text.y=element_text(),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(),          #formats y axis title
        axis.line=element_line(colour="black"),      #sets axis lines
        panel.grid.minor = element_blank(),          #removes minor grid lines
        panel.grid.major = element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        #legend.justification=c(1,0.8), legend.position=c(1,0.8),        #Positions legend
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