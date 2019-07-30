#This script is designed to created an automated annual summary report for fish from DOV data
#Created by Molly Moustaka July 2019 adapted from Tim Langlois and Brooke Gibbons (UWA MEG lab)
#Dept. Biodiversity, Conservation and Attractions, Marine Science Program
#Email: molly.moustaka@dbbca.wa.gov.au

#Code is based on TL & BG scripts 2 &3

###-----1.1 Get set up----####
#Clear environment
rm(list=ls())

#Install & Load packages
#install.packages(c("ggplot2", "dplyr","plotrix","gridExtra","RColorBrewer","gplots", "ggpubr","vegan"),dependencies = T)
library(tidyr)
library(dplyr)
library(readr)
library(data.table)
library(magrittr)
library(googlesheets)
library(ggplot2)
library(httpuv)
library(RCurl) # needed to download data from GitHub
library(stringr)

filter = dplyr::filter #make sure R uses the DPLYR version of filter

#I'm also playing with the new tools in tidyverse for reshaping data which aren't formally in the package yet
#devtools::install_github("tidyverse/tidyr") #select 3 in first option menu
library(tidyr)

#Set the working directory (must use forward slash / )
work.dir=("T:/529-CALMscience/Shared Data/Marine Science Program/MONITORING/Pluto Offset 4 Monitoring/Assets/Fish")
pdf.out=paste(work.dir,"Monitoring Summaries",sep="/")
data=paste(work.dir,"Data",sep="/")
text=paste(work.dir,"Data/Raw Text Files",sep="/")
temp=paste(work.dir,"Data/Temporary Files",sep="/")
plots=paste(work.dir,"Data/Plots",sep="/")
tidy=paste(work.dir,"Data/Concatenated, Cleaned and Checked Data",sep="/")

#View files in the directory
dir()

#Set up study name
st=format(Sys.time(), "%Y-%m") #make an object with todays date
study<-paste("DMP_Monitoring",st, sep="_")  ##change this to inc. date and change subsequent outputs


####----1.2 Merge EventMeasure outputs----####
setwd(text)

files<-list.files(text, pattern = ".txt") #make a list of all file names in the data directory ending in CSV

dat<- NA # make a blank data frame

for (f in files) {
  x<-read.table(f,header=TRUE,sep="\t", skip=4)
  dat<-rbind(dat,x)

}

#remove the bank row at the top thats made by the importing process
dat<-dat[-1,]

#Write CSV to data folder
setwd(data)

write.csv(dat,file=paste(study,"Fish_DOVS_ToDate.csv",sep="_"))

####1.3 OPTIONAL: Push updated fish mastersheet onto CKAN (DBCA data catalogue)####
#You may want to do this at the end post-data checking and formatting depending on what you want,

#source("~/projects/data-pipelines/setup/ckan.R")
dataset_rid <- "44a67383-db52-4c45-b1f7-b62ae046a3f5"
csv_rid <- "3f407973-3fe6-41d1-acbf-b4702c5a7d29"
upload_to_ckan(data=dat,resource_title="Dampier Fish Diversity and Abundance Mastersheet",dataset_id=dataset_rid, resource_id =csv_rid)

####1.4 OPTIONAL: Pull fish mastersheet off CKAN (DBCA data catalogue)####
#source("~/projects/data-pipelines/setup/ckan.R")
#csv_rid <- "3f407973-3fe6-41d1-acbf-b4702c5a7d29"
#d <- load_ckan_csv(csv_rid)
#dplyr::glimpse(d)


####---- 1.3 Rename a few columns to make TL script run ----####
names(dat)

dat<-dat%>%
  dplyr::rename(
    length=Length..mm.,
    periodtime=Period.time..mins.,
    range=Range..mm.
  )
names(dat)

####---- 1.4 Create an Opcode per transect ----####
dat<-dat%>%
  mutate(tranopcode=paste0(OpCode,Period,sep="_"))

####----1.5 Calculate SumCount----#### #changed this section from BRUV script
sumcount<-dat%>%
  setNames(tolower(names(.)))%>%
  mutate(number=as.numeric(number))%>%
  group_by(opcode,tranopcode,family,genus,species)%>% #changed this row to tranopcode from frame
  dplyr::summarise(sumcount=sum(number))%>%
  ungroup()%>%
  filter(!sumcount%in%c(NA,""))%>%
  tidyr::replace_na(list(family = "Unknown",genus = "Unknown", species = "spp"))%>% # Removes all NA's in family, genus and species
  dplyr::mutate(family = ifelse(family%in%c("NA",""),"Unknown", as.character(family)))%>%
  dplyr::mutate(genus = ifelse(genus%in%c("genus","NA","NANA","sp","spp","unknown","","Genus"),"Unknown", as.character(genus)))%>%
  dplyr::mutate(species = ifelse(species%in%c("NA","sp","","unknown"),"spp", as.character(species)))%>%
  dplyr::rename(sample=tranopcode)%>%
  mutate(scientific=paste(family,genus,species,sep = " "))%>%
  filter(!scientific=="Unknown Unknown spp")%>% # removes where completely unidentifiable
  glimpse()

####----1.6 Format length data----####
length3dpoints<-dat%>%
  setNames(tolower(names(.)))%>%
  tidyr::replace_na(list(family = "Unknown",genus = "Unknown", species = "spp"))%>% # Removes all NA's
  dplyr::mutate(family = ifelse(family%in%c("NA",""),"Unknown", as.character(family)))%>%
  dplyr::mutate(genus = ifelse(genus%in%c("genus","NA","NANA","sp","spp","unknown","","Genus"),"Unknown", as.character(genus)))%>%
  dplyr::mutate(species = ifelse(species%in%c("NA","sp","","unknown"),"spp", as.character(species)))%>%
  dplyr::mutate(length=as.numeric(length))%>%
  dplyr::mutate(number=as.numeric(number))%>%
  select(opcode,tranopcode,family,genus,species,number,length,stage,activity,periodtime,range)%>%
  mutate(scientific=paste(family,genus,species,sep = " "))%>%
  dplyr::rename(sample=tranopcode)%>% #changed this from opcode to tranopcode
  glimpse()

####----1.7 Write files----####
setwd(temp)
dir()

write_csv(sumcount, path=paste(study,"sumcount.csv",sep = "_"))
write_csv(length3dpoints, path=paste(study,"Len3DPoints.csv",sep = "_"))








##############                                   ##############
############## STARTING PART TWO - DATA CHECKING ##############
##############                                   ##############

####----2.1 Load functions from GitHub----####
gsr <- getURL("https://raw.githubusercontent.com/TimLanglois/Fuctions/master/gsr.r", ssl.verifypeer = FALSE)
eval(parse(text = gsr))
detach("package:RCurl", unload=TRUE) # will error - don't panic - need to make sure it is not loaded as will interfer with dplyr() # TJL


####----2.2 A function we should use throughout----####
clean_names <- function(dat){
  # Takes a data.frame, returns the same data frame with cleaned names
  old_names <- names(dat)
  new_names <- old_names %>%
    gsub("%", "percent", .) %>%
    make.names(.) %>%
    gsub("[.]+", ".", .) %>%
    tolower(.) %>%
    gsub("_$", "", .)
  setNames(dat, new_names)
}

####---- 2.3 Import sumcount and length/3d files----####
setwd(temp)

# Import sumcount file
sumcount<-read_csv(paste(study,"sumcount.csv",sep = "_"))%>%
  mutate(sumcount=as.numeric(sumcount))%>%
  data.frame()%>%
  glimpse()

# Import length/3d filee
length<-read_csv(paste(study,"Len3DPoints.csv",sep = "_"))%>%
  mutate(number=as.numeric(number))%>%
  mutate(range=as.numeric(range))%>%
  mutate(length=as.numeric(length))%>%
  filter(!is.na(number)) %>% # find and remove sync points that are not fish
  data.frame()%>%
  glimpse()

####---- 2.4 Begin basic checks ----####
setwd(data)

# Check if we have 3d points (Number) in addition to length
three.d.points<-length%>%
  filter(is.na(length))%>%
  filter(!is.na(number))%>%
  glimpse() # Do we have 3d points? In this example - YES, if no 3D points will be empty

# Check if we have schools associated with single length measures
schools<-length%>%
  filter(number>1)%>%
  glimpse() # Do we have schools? YES, if no schools will be empty

####----  2.5 Standardise for RANGE and Error for Length ----####
# To standardise for RANGE and Error we can remove any length observations outside Range and Error rules
# i.e. the length data, and any abundance calculated from it, will be restricted by range

summary(length$range) # shows min, mean and max range
out.of.range<-filter(length,range>10000)%>%glimpse() ## In this example there are no fish out of range (empty dataframe)


####----  2.6  Check on the BIG fish length data----####
fish.greater.than.1.meter<-filter(length,length>1000)%>%
  glimpse() # Shows fish that have a length measurement greater than 1 m (usually sharks, but should be checked, but will come out again if larger than the max length in the life history sheet)

setwd(temp)
write.csv(fish.greater.than.1.meter,file=paste(study,"check","length.greater.than.1.meter.csv",sep = "_"), row.names=FALSE)


####----  2.7 Plot to visualise length data ----####
setwd(plots)

check.length<-ggplot(data=length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="blue",
                 alpha = .2)
check.length # most fish less than 300mm in this example

ggsave(check.length,file=paste(study,"check.length.png",sep = "_"))


####----  2.8  Plot to visualise range data----####
check.range<-ggplot(data=length, aes(as.numeric(range))) +
  geom_histogram(aes(y =..density..),
                 col="red",
                 fill="green",
                 alpha = .2)
check.range # most fish recorded under 2 m away

ggsave(check.range,file=paste(study,"check.range.png",sep = "_"))

####----  2.9  Plot to visualise length/range data----####
check.range.vs.length<-ggplot(data=length, aes(range,length)) +
  geom_point()+
  geom_smooth()
check.range.vs.length

ggsave(check.range.vs.length,file=paste(study,"check.range.vs.length.png",sep = "_"))

##############                                           ##############
############## STARTING PART THREE - SERIOUS data checking ##############
##############                                           ##############

####----  3.1 Read in species list to compare against----####
# PLEASE EMAIL brooke.gibbons@uwa.edu.au if you would like this googlesheet to be shared with you
# or substitute your own life history sheet in here
master<-gs_title("Australia.life.history")%>%
  gs_read_csv(ws = "australia.life.history")%>%clean_names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  filter(grepl('NW', marine.region))%>% # Select marine region (this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>%
  distinct()%>%
  glimpse()

####----  3.2 Update names of species that may have changed, using synonyms list----####
# PLEASE EMAIL brooke.gibbons@uwa.edu.au if you would like this googlesheet to be shared with you
synonyms <- gs_title("Synonyms_Australia")%>%
  gs_read_csv(ws = "Synonyms_Australia")%>%
  distinct()%>%
  clean_names()%>%
  select(-comment)

# Change synonyms
sumcount<-left_join(sumcount,synonyms,by=c("family","genus","species"))%>%
  mutate(genus=ifelse(!is.na(genus_correct),genus_correct,genus))%>%
  mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
  mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
  select(-c(family_correct,genus_correct,species_correct))

length<-left_join(length,synonyms,by=c("family","genus","species"))%>%
  mutate(genus=ifelse(!is.na(genus_correct),genus_correct,genus))%>%
  mutate(species=ifelse(!is.na(species_correct),species_correct,species))%>%
  mutate(family=ifelse(!is.na(family_correct),family_correct,family))%>%
  select(-c(family_correct,genus_correct,species_correct))


####----  3.3 Check for taxa.not.match----####
setwd(temp)

# sumcount
sumcount.taxa.not.match.life.history<-master%>%
  anti_join(sumcount,.,by=c("family","genus","species"))%>%
  distinct(sample,family,genus,species)%>%
  filter(!species%in%c("spp","sp10","sp1"))

write.csv(sumcount.taxa.not.match.life.history,file=paste(study,"check.sumcount.taxa.not.match.life.history.csv",sep = "_"), row.names=FALSE)

# Length
length.taxa.not.match<-master%>%
  anti_join(length,.,by=c("family","genus","species"))%>%
  dplyr::distinct(sample,family,genus,species)%>%
  filter(!species%in%c("spp","sp10","sp1"))

write.csv(length.taxa.not.match,file=paste(study,"check.length.taxa.not.match.life.history.csv",sep = "_"), row.names=FALSE)

####----  3.4 SERIOUS Check for Min Max Length compared to Master list----####
library(plyr)
names(master)

# Mean max length for each family in the master list
family.max.length<-master%>%
  replace_na(list(fb.length_max=0))%>%
  dplyr::group_by(family)%>%
  dplyr::summarise(famlength_max=mean(fb.length_max),na.rm = T)%>%
  dplyr::filter(!famlength_max==0)%>%
  select(-na.rm)

####----  3.5 Create a new master list with family mean max where missing maximum length----####
# (you can also replace all "family" with "genus" to create a genus average)
master.with.fam.max<-left_join(master,family.max.length,by=c("family"))%>%
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),famlength_max,fb.length_max))%>%
  dplyr::select(-c(famlength_max))

wrong.length.taxa<-left_join(length,master.with.fam.max,by=c("family","genus","species"))%>%
  dplyr::filter(length >= fb.length_max)%>%
  dplyr::select(sample,family,genus,species,length,fb.length_max,fb.ltypemaxm)%>%
  dplyr::mutate(percent.error=(length-fb.length_max)/fb.length_max*100)%>%
  dplyr::arrange(desc(percent.error))

setwd(temp)
write.csv(wrong.length.taxa,file=paste(study,"check.wrong.length.taxa.vs.life.history.csv",sep = "_"), row.names=FALSE)


####----  3.6 Drop wrong lengths----####
drop.length<-wrong.length.taxa%>% # TO REMOVE LENGTHS OUTSIDE THE MIN/MAX OF MASTER LIST
  distinct(family,genus,species,length)%>%
  dplyr::select(family,genus,species,length)%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))

# Do not run this if you would like to keep those in wrong.length.taxa
length<-length%>%
  dplyr::mutate(key = paste(family,genus,species,length, sep = '_'))%>%
  anti_join(drop.length,by="key")%>% # for dropping wrong.lengths
  dplyr::select(-c(key))%>%
  glimpse()

####----  3.7 Check how many sumcount per species are missing from Stereosumcount, e.g. how many lengths are missing from the possible sumcount----####
# can only look at samples where lengths were possible
length.sample <- length%>%distinct(sample)

# summairse length and then compare to sumcount
taxa.sumcount.vs.stereo.summary<-length%>%
  group_by(sample,family,genus,species)%>%
  dplyr::summarise(stereo.sumcount=sum(number))%>%
  left_join(sumcount)%>%
  mutate(percent.diff = (sumcount-stereo.sumcount)/sumcount)%>%
  semi_join(length.sample)%>% # only keep ones where length was possible
  replace_na(list(percent.diff=1))%>%
  filter(!percent.diff%in%c(0))%>%
  glimpse()

write.csv(taxa.sumcount.vs.stereo.summary,file=paste(study,"taxa.sumcount.vs.stereo.summary.csv",sep = "_"), row.names=FALSE)

####----  3.8 WRITE FINAL checked data----####
setwd(tidy)
dir()

write.csv(sumcount, file=paste(study,"checked.sumcount.notformatted.csv",sep = "_"), row.names=FALSE)
write.csv(length, file=paste(study,"checked.length.notformatted.csv",sep = "_"), row.names=FALSE)









##############                                      ##############
############## STARTING PART FOUR - DATA FORMATING  ##############
##############                                      ##############

####----  4.1  Read in the data----####
setwd(tidy)
dir()

####----  4.2   Make species families to merge back in after data is complete----####
sumcount.families<-read_csv(file=paste(study,"checked.sumcount.notformatted.csv",sep = "_"),na = c("", " "))%>%
  filter(!(family=="Unknown"))%>%
  select(c(family,genus,species,scientific))%>%
  distinct() #to join back in after complete


####----  4.3  Make complete.sumcount from sumcount and complete.length.number.mass from length3D----####

# Make complete.sumcount: fill in 0, make Total and Species Richness and join in factors
dat<-read_csv(file=paste(study,"checked.sumcount.notformatted.csv",sep = "_"),na = c("", " "))%>%
  select(c(sample,family,genus,species,sumcount))%>%
  complete(sample,nesting(family,genus,species)) %>%
  replace_na(list(sumcount = 0))%>%
  group_by(sample,family,genus,species)%>%
  dplyr::summarise(sumcount=sum(sumcount))%>%
  ungroup()%>% #always a good idea to ungroup() after you have finished using the group_by()!
  mutate(scientific=paste(family,genus,species,sep=" "))%>%
  dplyr::select(sample,scientific,sumcount)%>%
  spread(scientific,sumcount, fill = 0)%>%
  glimpse()

complete.sumcount<-dat%>%
  gather(key=scientific, value = sumcount,-sample)%>%
  data.frame()%>%
  inner_join(sumcount.families,by=c("scientific"))%>%
  glimpse()

####----  4.4 Make complete.length.number.mass: fill in 0 and join in factors----####
# This data is useful for calculating abundance based on length rules--
length.families<-read_csv(file=paste(study,"checked.length.notformatted.csv",sep = "_"),na = c("", " "))%>%
  filter(!(family=="Unknown"))%>%
  select(family,genus,species)%>%
  distinct()%>% #to join back in after complete
  glimpse()

complete.length.number<-read_csv(file=paste(study,"checked.length.notformatted.csv",sep = "_"))%>% #na = c("", " "))
  filter(!(family=="Unknown"))%>%
  dplyr::select(sample,family,genus,species,length,number,range,activity)%>%
  complete(sample,nesting(family,genus,species)) %>%
  replace_na(list(number = 0))%>% #we add in zeros - in case we want to calulate abundance of species based on a length rule (e.g. greater than legal size)
  ungroup()%>%
  filter(!is.na(number))%>% #this should not do anything
  mutate(length=as.numeric(length))%>%
  data.frame()%>%
  glimpse()

####----  4.5 MAKE mass data from number.length.complete----####
# Import master from Life-history-
master<-gs_title("Australia.life.history")%>%
  gs_read_csv(ws = "australia.life.history")%>%clean_names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('NW', marine.region))%>%
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm,rls.trophic.group)%>%
  distinct()%>%
  glimpse()

####----  4.6 Biomass----####
# Check for species missing length weight relationship
taxa.missing.lw <- complete.length.number%>%
  distinct(family,genus,species)%>%
  anti_join(filter(master,!is.na(a)), by=c("family","genus","species"))%>%
  glimpse() # 8 missing - all spp's

# Missing Genus length weight
genus.missing.lw <- complete.length.number%>%
  distinct(genus)%>%
  anti_join(filter(master,!is.na(a)), by="genus") # 1 (Unknown)

# Missing Family length weight
family.missing.lw <- complete.length.number%>%
  distinct(family)%>%
  anti_join(filter(master,!is.na(a)), by="family") # None

####----  4.7 Fill length data with relevant a and b and if blank use family----####
length.species.ab<-master%>% #done this way around to avoid duplicating Family coloum
  select(-family)%>%
  inner_join(complete.length.number,., by=c("genus","species")) # only keeps row if has a and b

family.lw <- master%>%
  dplyr::group_by(family,length.measure)%>%
  dplyr::mutate(log.a = log10(a))%>%
  dplyr::summarise(a = 10^(mean(log.a, na.rm = T)),
                   b = mean(b, na.rm = T),
                   all = mean(all, na.rm = T),
                   bll = mean(bll, na.rm = T))%>%
  filter(!is.na(a))%>%
  dplyr::mutate(all=str_replace_all(all,"NaN","0"))%>%
  dplyr::mutate(bll=str_replace_all(bll,"NaN","1"))%>%
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(rank = ifelse(length.measure=="FL",1,ifelse(length.measure=="TL", 2, 3)))%>%
  dplyr::mutate(min.rank = rank - min(rank, na.rm = TRUE))%>%
  dplyr::filter(min.rank == 0)

length.family.ab<-complete.length.number%>%
  anti_join(master, by=c("genus","species"))%>%
  left_join(family.lw, by="family")

####----  4.8 Fill length data with relevant a and b and if blank use family?----####
complete.length.number.mass<-length.species.ab%>%
  bind_rows(length.family.ab)%>%
  dplyr::filter(!is.na(a))%>% #this gets rid of species with no lw
  mutate(length.cm = length/10)%>%
  mutate(all = ifelse(is.na(all)&length.measure%in%c("TL", "FL","SL"), 0, all))%>% # Temporary fix, remove later
  mutate(bll = ifelse(is.na(bll)&length.measure%in%c("TL", "FL","SL"), 1, bll))%>% # Temporary fix, remove later
  mutate(adjLength = ((length.cm*bll)+all)) %>%
  mutate(mass.g = (adjLength^b)*a*number)%>%
  dplyr::select(c(sample,family,genus,species,length,range,number,mass.g,length.cm))%>%
  glimpse()

####----  4.9 Check the mass estimates across species - in kg's----####
top.mass<- complete.length.number.mass %>%
  dplyr::group_by(family,genus,species)%>%
  filter(mass.g>0)%>%
  dplyr::mutate(mass.kg.individual = (mass.g/number)/1000)%>% # Work out the mass per individual fish
  dplyr::mutate(length=length/10)%>%
  mutate(length=round(length,digits=2))%>%
  dplyr::summarise(mean.kg = mean(mass.kg.individual, na.rm = TRUE),max.kg = max(mass.kg.individual, na.rm = TRUE),min.kg = min(mass.kg.individual, na.rm = TRUE),min.length = min(length, na.rm = TRUE),mean.length = mean(length, na.rm = TRUE),max.length = max(length, na.rm = TRUE))%>%
  arrange(-mean.kg)%>%
  glimpse()%>%
  mutate(mean.kg=round(mean.kg,digits=3))%>%
  mutate(max.kg=round(max.kg,digits=3))%>%
  mutate(min.kg=round(min.kg,digits=3))%>%
  mutate(mean.length=round(mean.length,digits=2))

####----  4.10 WRITE  complete and expanded data----####
setwd(tidy)
dir()

write.csv(complete.sumcount, file=paste(study,"complete.sumcount.csv",sep = "_"), row.names=FALSE)

write.csv(complete.length.number.mass, file=paste(study,"complete.length.number.mass.csv",sep = "_"), row.names=FALSE)

write.csv(complete.length.number, file=paste(study,"complete.length.number.csv",sep = "_"), row.names=FALSE)



##############                                                                  ##############
############## STARTING PART Five - Adding feeding guild/target status/metadata ##############
##############                                                                  ##############
####UPDATE THIS SECTION WITH NEW DBCA MSP MASTER SHEET FOR GUILD/TARGET STATUS####
####----  5.1 Pull out feeding guild data and make Scientific name col----####
feeding<-master%>%
  select(c(family,genus,species,rls.trophic.group))%>%
  mutate(scientific=paste(family,genus,species, sep=" "))%>%
  glimpse()

####----  5.2 Make sure all datasets contain a 'scientific' column----####
complete.sumcount<-complete.sumcount%>%
  mutate(scientific=paste(family,genus,species, sep=" "))

complete.length.number.mass<-complete.length.number.mass%>%
  mutate(scientific=paste(family,genus,species, sep=" "))

complete.length.number<-complete.length.number%>%
  mutate(scientific=paste(family,genus,species, sep=" "))

####----  5.2 Make sure all datasets contain a 'opcode column and add transect back in'----####
head(complete.sumcount) #no- correct here
complete.sumcount$opcode<-str_sub(complete.sumcount$sample,1,str_length(complete.sumcount$sample)-2) #removes the transect # to make op code column
complete.sumcount$transect<-str_sub(complete.sumcount$sample,str_length(complete.sumcount$sample)-1,str_length(complete.sumcount$sample)-1) #makes transect column

head(complete.length.number.mass) #no - correct here
complete.length.number.mass$opcode<-str_sub(complete.length.number.mass$sample,1,str_length(complete.length.number.mass$sample)-2) #removes the transect #
complete.length.number.mass$transect<-str_sub(complete.length.number.mass$sample,str_length(complete.length.number.mass$sample)-1,str_length(complete.sumcount$sample)-1) #makes transect column

head(complete.length.number) #no - correct here
complete.length.number$opcode<-str_sub(complete.length.number$sample,1,str_length(complete.length.number$sample)-2) #removes the transect #
complete.length.number$transect<-str_sub(complete.length.number$sample,str_length(complete.length.number$sample)-1,str_length(complete.sumcount$sample)-1) #makes transect column


####----  5.3 Add feeding guild data----####
feeding.complete.sumcount<- feeding%>%
  select(-c(family,genus, species))%>%
    right_join(complete.sumcount, by="scientific")

feeding.complete.length.number.mass<- feeding%>%
  select(-c(family,genus, species))%>%
  right_join(complete.length.number.mass, by="scientific")

feeding.complete.length.number<-feeding%>%
  select(-c(family,genus, species))%>%
  right_join(complete.length.number, by="scientific")

####----  5.4 Pull out species with no feeding guild data----####
sumcount.feeding.not.in.life.history<-feeding.complete.sumcount%>%
  dplyr::filter(is.na(rls.trophic.group))%>%
  group_by(family,genus,species)%>%
dplyr::summarise(count=sum(sumcount))%>%
  glimpse()

#Write the data - you need to check this and make sure you are happy with it
setwd(temp)
write.csv(sumcount.feeding.not.in.life.history, file=paste(study,"feeding.guilds.missing.csv",sep = "_"), row.names=FALSE )

####----  5.5 Add in targeted species data----####
#Read in the file
setwd(data)
target<-read.csv("DBCA_Fish_TargetStatus_20190729.csv", header=T, fileEncoding="UTF-8-BOM")

#Pull out the data for your region of interest
unique(target$Region.Code)
target<-target%>%
  filter(Region.Code=="MBIMCR") #This is for the montebellos

#Make scientific column
target<-target%>%
  setNames(tolower(names(.)))%>%
  mutate(scientific=paste(family,genus,species, sep=" "))%>%
  select(-c(unique.id,region.code,code,family,genus,species,genus.species))

#Attach target species information to datasheets
target.feeding.complete.sumcount<-left_join(feeding.complete.sumcount,target,by="scientific")

target.feeding.complete.length.number.mass<-left_join(feeding.complete.length.number.mass,target,by="scientific")

target.feeding.complete.length.number<-left_join(feeding.complete.length.number,target,by="scientific")

#review the list of target species to ensure you are happy and it matches the most recent DPIRD reports
unique(target$Target.Code) #T =target, HT= highly targeted, P=protected, OR=ocassionally retained, NT= not targeted
targetspecies<- target.feeding.complete.sumcount%>%
dplyr:: filter(target.code==c("HT", "T"))%>%
  distinct(scientific)%>%
  glimpse()

#Write targeted species as csv for checking
dir(temp)
write.csv(targetspecies,file=paste(study,"targeted.species.csv",sep = "_"), row.names=FALSE )

#Check species missing target status
missingtarget<- target.feeding.complete.sumcount%>%
  dplyr:: filter(is.na(target.code))%>%
  distinct(scientific)%>%
  glimpse() #we have missing target status - whether you take the time to fill this depends what is missing and what your question is (i.e. do you care about NT species)

#Write missing targeted status species as csv for checking
dir(temp)
write.csv(missingtarget,file=paste(study,"targeted.status.missing.csv",sep = "_"), row.names=FALSE )

####----  5.6 Pull in Metadata ----####
setwd(work.dir)
metadata<- read.csv("DMP_DOV_Labsheet_20190722.csv", header=T,fileEncoding="UTF-8-BOM")

####----  5.7 Create year column ----####
#make the date column into date format
metadata$Date<-as.Date(metadata$Date, format="%d/%m/%Y")

#make a year column
metadata$Year<- as.factor(format(metadata$Date, "%Y"))

####----  5.8 Select columns out for merging ----####
names(metadata)
met<-metadata%>%
  setNames(tolower(names(.)))%>%
  select(c(location,opcode,latitude,longitude,year, site,depth))

####----  5.9 Merge in metadata ----####
complete.sumcount.meta<-target.feeding.complete.sumcount%>%
  left_join(met, by="opcode")
head(complete.sumcount)

complete.length.number.mass.meta<-target.feeding.complete.length.number.mass%>%
  left_join(met, by="opcode")

complete.length.number.meta<-target.feeding.complete.length.number%>%
  left_join(met, by="opcode")

####----  5.10 ADDING IN HABITAT DATA WILL GO HERE----####

####----  5.11 WRITE  complete and expanded data----####
#note, this section overwrites the data created in section 4 as it is the same but with added cols
setwd(tidy)
dir()

write.csv(complete.sumcount.meta, file=paste(study,"complete.sumcount.csv",sep = "_"), row.names=FALSE)

write.csv(complete.length.number.mass.meta, file=paste(study,"complete.length.number.mass.csv",sep = "_"), row.names=FALSE)

write.csv(complete.length.number.meta, file=paste(study,"complete.length.number.csv",sep = "_"), row.names=FALSE)
