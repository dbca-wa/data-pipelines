source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/turtles/nmp")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "bff5a388-42b7-442b-a421-5e5a4fb23224"
pdf_rid <- "1e104dde-db86-42ee-b8ed-9715bf156524"
txt_rid <- "cfb3bc69-4966-4237-a68f-8bbf42911519"

pdf_fn <- "nmp-loggerhead-nest-success.pdf"
txt_fn <- "nmp-loggerhead-nest-success.R"

#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor
d <- load_ckan_csv(csv_rid, date_colnames=c("date"))

out <- ggplot(d, aes(x=Year_number, y=Loggerhead_success)) +
  geom_line() +
  geom_point() + # 21 is filled circle
  xlab("") +
  ylab("Loggerhead turtle nesting success %") +
  scale_x_discrete(limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13"), 
                   labels=c("2002-03", "2003-04","2004-05","2005-06","2006-07","2007-08","2008-09","2009-10","2010-11",
                            "2011-12","2012-13","2013-14","2014-15")) +
  scale_y_continuous(limits=c(min(0), max=(100))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        #axis.title.x=element_text(size=10),
        axis.line = element_line(colour="black"), #sets axis lines
        panel.grid.minor = element_blank(), #removes minor grid lines
        panel.grid.major = element_blank(), #removes major grid lines
        panel.border = element_blank(), #removes border
        panel.background = element_blank(), #needed to ensure integrity of axis lines
        #         legend.justification=c(0,1), legend.position=c(0,1), # Positions legend (x,y)
        #         legend.title = element_text("Beach"),
        legend.key = element_blank())


#------------------------------------------------------------------------------#
# Save outputs and upload to CKAN, restore workdir
pdf(pdf_fn, height = 5, width = 7)
print(out)
dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")
