source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

#------------------------------------------------------------------------------#
# Settings
# Adjust the following path to the location of this file:
setwd("~/projects/data-pipelines/scripts/indicators/shorebirds")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "c42ad53b-7a46-4c68-97b2-cdc2132d9d0b"
txt_rid <- "d1f17458-0f51-4668-9dfa-399d6743b72d"
pdf_rid_calcan <- "a2f3ed27-5f7f-420a-9d75-c3f3e2f6a91c"
pdf_rid_calten <- "ef87e31b-b0f1-4bbe-b89c-aba03733adc1"
pdf_rid_limlap <- "ad5d5141-00c2-4135-89cf-0a2a2aa679f4"
pdf_rid_sprich <- "02f97f5e-db70-466c-a805-25c1be1fb36d"
pdf_rid_even <- "1dd38b70-6f1a-40f2-82ac-3e3c4d2e1247"

txt_fn <- "code.R"
pdf_fn <- "figure.pdf"
pdf_fn_calcan <- "Calidris_canutus_abundance.pdf"
pdf_fn_calten <- "Calidris_tenuirostris_abundance.pdf"
pdf_fn_limlap <- "Limosa_lapponica_abundance.pdf"
pdf_fn_sprich <- "species_richness.pdf"
pdf_fn_even <- "species_diversity.pdf"

d <- load_ckan_csv(csv_rid, date_colnames=c("date"))
#------------------------------------------------------------------------------#
# Analysis - your code
# add date_colnames you wish to read as POSIXct date, remove to keep as factor

library(qcc)
univ.indices <- d
labels <- univ.indices$Year

#------------------------------------------------------------------------------#
# Create and Save outputs and upload to CKAN, restore workdir

obj.univ.indices <- qcc(univ.indices$Cal_can[1:8],type="xbar.one",newdata=univ.indices$Cal_can[9:11])
pdf(pdf_fn_calcan, height = 5, width = 7, pointsize = 12, useDingbats = FALSE)
plot(univ.indices$Cal_can,cex=1.5,xaxt='n',
     col=ifelse((univ.indices$Cal_can > obj.univ.indices$limits[,2] | univ.indices$Cal_can < obj.univ.indices$limits[,1]),"red","black"), 
     pch=19,ylim=c(0,40000),ylab='Abundance',xlab='Year')
axis(1,at=seq(1,11,by=1),labels=labels)
abline(h=(obj.univ.indices$center),lty=1)
abline(h=(obj.univ.indices$limits [,2]),lty=2) # this calls up the upper limit
abline(h=(obj.univ.indices$limits [,1]),lty=2) # this calls up the lower limit which is not necessary if it is a number less than zero
abline(v=8.5,lty=2)
lines(univ.indices$Cal_can,type='c')
dev.off()


obj.univ.indices <- qcc(univ.indices$Cal_ten[1:8],type="xbar.one",newdata=univ.indices$Cal_ten[9:11])
pdf(pdf_fn_calten, height = 5, width = 7, pointsize = 12, useDingbats = FALSE)
plot(univ.indices$Cal_ten,cex=1.5,xaxt='n',col=ifelse((univ.indices$Cal_ten > obj.univ.indices$limits[,2] | univ.indices$Cal_ten < obj.univ.indices$limits[,1]),"red","black"), pch=19,ylim=c(0,150000),ylab='Abundance',xlab='Year')
axis(1,at=seq(1,11,by=1),labels=labels)
abline(h=(obj.univ.indices$center),lty=1)
abline(h=(obj.univ.indices$limits [,2]),lty=2) # this calls up the upper limit
abline(h=(obj.univ.indices$limits [,1]),lty=2) # this calls up the lower limit which is not always necessary if it is a number less than zero
abline(v=8.5,lty=2)
lines(univ.indices$Cal_ten,type='c')
dev.off()


obj.univ.indices <- qcc(univ.indices$Lim_lap[1:8],type="xbar.one",newdata=univ.indices$Lim_lap[9:11])
pdf(pdf_fn_limlap, height = 5, width = 7, pointsize = 12, useDingbats = FALSE)
plot(univ.indices$Lim_lap,cex=1.5,xaxt='n',col=ifelse((univ.indices$Lim_lap > obj.univ.indices$limits[,2] | univ.indices$Lim_lap < obj.univ.indices$limits[,1]),"red","black"), pch=19,ylim=c(0,60000),ylab='Abundance',xlab='Year')
axis(1,at=seq(1,11,by=1),labels=labels)
abline(h=(obj.univ.indices$center),lty=1)
abline(h=(obj.univ.indices$limits [,2]),lty=2) # this calls up the upper limit
abline(h=(obj.univ.indices$limits [,1]),lty=2) # this calls up the lower limit which is not always necessary if it is a number less than zero
abline(v=8.5,lty=2)
lines(univ.indices$Lim_lap,type='c')
dev.off()

obj.univ.indices <- qcc(univ.indices$Sprich[1:8],type="xbar.one",newdata=univ.indices$Sprich[9:11])
pdf(pdf_fn_sprich, height = 5, width = 7, pointsize = 12, useDingbats = FALSE)
plot(univ.indices$Sprich,cex=1.5,xaxt='n',col=ifelse((univ.indices$Sprich > obj.univ.indices$limits[,2] | univ.indices$Sprich < obj.univ.indices$limits[,1]),"red","black"), pch=19,ylim=c(22,34),ylab='Species Richness',xlab='Year')
axis(1,at=seq(1,11,by=1),labels=labels)
abline(h=(obj.univ.indices$center),lty=1)
abline(h=(obj.univ.indices$limits [,2]),lty=2) # this calls up the upper limit
abline(h=(obj.univ.indices$limits [,1]),lty=2) # this calls up the lower limit which is not always necessary if it is a number less than zero
abline(v=8.5,lty=2)
lines(univ.indices$Sprich,type='c')
dev.off()

obj.univ.indices <- qcc(univ.indices$Evenness[1:8],type="xbar.one",newdata=univ.indices$Evenness[9:11])
pdf(pdf_fn_even, height = 5, width = 7, pointsize = 12, useDingbats = FALSE)
plot(univ.indices$Evenness,cex=1.5,xaxt='n',col=ifelse((univ.indices$Evenness > obj.univ.indices$limits[,2] | univ.indices$Evenness < obj.univ.indices$limits[,1]),"red","black"), pch=19,ylim=c(0.4,0.8),ylab='Abundance',xlab='Year')
axis(1,at=seq(1,11,by=1),labels=labels)
abline(h=(obj.univ.indices$center),lty=1)
abline(h=(obj.univ.indices$limits [,2]),lty=2) # this calls up the upper limit
abline(h=(obj.univ.indices$limits [,1]),lty=2) # this calls up the lower limit which is not always necessary if it is a number less than zero
abline(v=8.5,lty=2)
lines(univ.indices$Evenness,type='c')
dev.off()

ckanr::resource_update(pdf_rid_calcan, pdf_fn_calcan)
ckanr::resource_update(pdf_rid_calten, pdf_fn_calten)
ckanr::resource_update(pdf_rid_limlap, pdf_fn_limlap)
ckanr::resource_update(pdf_rid_sprich, pdf_fn_sprich)
ckanr::resource_update(pdf_rid_even, pdf_fn_even)

ckanr::resource_update(txt_rid, txt_fn)

setwd("~/projects/data-pipelines")
