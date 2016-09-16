source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")
library(reshape)
library(reshape2)
library(ggplot2)
library(plyr)

#------------------------------------------------------------------------------#
## Settings
## Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/coral/heat_stress")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "ee03b97f-65d4-4c2e-a28c-d62ba327c6d2"
pdf_rid <- "089436fe-2b0e-4b6e-8fb4-11de2eaf791b"
txt_rid <- "0b63a75b-a118-4831-907f-87c0542f05a0"

pdf_fn <- "figure.pdf"
txt_fn <- "code.R"

#------------------------------------------------------------------------------#
## Analysis - your code
d <- load_ckan_csv(csv_rid,  date_colnames=c("date")) # your data as data.frame

# DHWparks1=read.table("C:/temp/DHW_Summarised.csv",sep=",",header=TRUE)
# DHWparks2 = melt(DHWparks1, id=(c("marine.park", "year"))) 
# write.csv(DHWparks2, "C:/temp/DHW_Summarised_melt.csv")
#
# # to obtain mean
# aims3=ddply(aims2, .(Yr, variable), summarise, 
# mean = mean(value), sd = sd(value), se = sd(value) / sqrt(length(value)))

pd <- position_dodge(0.9)
xlim <- c(min(d$Year-1),max(d$Year+1))
xbr <- min(d$Year):max(d$Year)

out <- ggplot(d, aes(x=Year,  y=Value, group=Variable, fill=Variable)) +
  #geom_line(position=pd, colour="black") +
  geom_bar(stat="identity", position="dodge") +
  geom_hline(yintercept=0)+
  ylab(expression(paste("Total number of DHW occurances", sep = "")))  +
  scale_fill_grey(start=0, end=0.8) +
  scale_x_continuous(limits=xlim, breaks=xbr) +
  facet_wrap( ~ Marine.park, ncol=2) +  
  theme_bw() +
  theme(
    axis.text.x = element_text(angle=45, vjust=-0.1),
    axis.title.x = element_blank(),
    strip.background = element_blank(),   
    axis.line = element_line(colour="black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.position="top",
    legend.title = element_blank()
  )


#------------------------------------------------------------------------------#
## Save outputs and upload to CKAN, restore workdir

out; dev.print(pdf, file=pdf_fn, width=7, height=7, pointsize=12); dev.off()

# this will work as well
# pdf(pdf_fn, height = 7, width = 7); print(out); dev.off()

ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")
