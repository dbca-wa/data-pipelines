###Code##
#---------------
barrowbird1=read.table("C:/Users/kimfr/Desktop/TattlerCreekBirdData.csv",sep=",",header=TRUE)
#library(plyr) #needed for count

barrowbird2 = melt(barrowbird1, id=(c("Shorebird_Area", "Yr", "Count_Area"))) #- only if imported variable in two columns from pivot table in excel


# # to obtain mean
# aims3=ddply(aims2, .(Yr, variable), summarise, 
#             mean = mean(value),
#             sd = sd(value),
#             se   = sd(value) / sqrt(length(value)))

write.csv(barrowbird2, "T:/529-CALMscience/Shared Data/Marine Science Program/STAFF/KimF/TattlerCreekBirdData_melt.csv")
#----------------
setwd("C:/temp")
