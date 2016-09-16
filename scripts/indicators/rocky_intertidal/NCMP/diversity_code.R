setwd("~/projects/data-pipelines/scripts/indicators/Rocky_intertidal/NCMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "941357d0-b76c-4967-a8b8-b78cedd36804"
pdf_rid <- "1972556c-9c73-44a7-86f9-ca9ed684f34b"
txt_rid <- "e9b8bdd3-3597-4c8a-b879-c97b0f538e88"
pdf_fn = "final.pdf"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))

d["Year"] <- 2015

d_small<-d[,c("Year","Site","Geology", "Diversity", "Abundance")]
d_small = d[-206,]


pd <- position_dodge(0.1)
graphics = theme(axis.text.x=element_text(angle=45, hjust=0.9), #rotates the x axis tick labels an angle of 45 degrees
                 axis.title.x=element_text(), #removes x axis title
                 axis.title.y=element_text(), #removes y axis title
                 axis.line=element_line(colour="black"), #sets axis lines 
                 plot.title =element_text(hjust = 0.05),
                 panel.grid.minor = element_blank(), #removes minor grid lines
                 panel.grid.major = element_blank(), #removes major grid lines
                 panel.border=element_blank(), #removes border
                 panel.background=element_blank(), #needed to ensure integrity of axis lines
                 legend.justification=c(10,10), legend.position=c(10,10), # Positions legend (x,y) in this case removes it from the graph
                 legend.title = element_text(),
                 legend.key = element_blank()
)
##################################################################################
#Outer zone diversity
outer = subset(d_small, Zone %in% c("Outer")) 

d_sum <- plyr::ddply(outer, .(Year,Geology), summarise,
                     N    = length(!is.na(Diversity)),
                     mean = mean(Diversity, na.rm=TRUE),
                     sd   = sd(Diversity, na.rm=TRUE),
                     se   = sd(Diversity, na.rm=TRUE) / sqrt(length(!is.na(Diversity)) ))

NCMP_diversity_out <- ggplot(d_sum, aes(x=Geology, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.3, binwidth=0.5) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(10)))+
  xlab("Habitat") +
  ylab(expression(paste("Mean No. species (","m"^-2,")", sep = ""))) +
  ggtitle("a) Diversity_outer zone") +
  theme_bw() + graphics

NCMP_diversity_out

############################################################################
#Outer zone Abundance

d_sum <- plyr::ddply(outer, .(Year,Geology), summarise,
                     N    = length(!is.na(Abundance)),
                     mean = mean(Abundance, na.rm=TRUE),
                     sd   = sd(Abundance, na.rm=TRUE),
                     se   = sd(Abundance, na.rm=TRUE) / sqrt(length(!is.na(Abundance)) ))

NCMP_abundance_out <- ggplot(d_sum, aes(x=Geology, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.3, binwidth=0.5) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(500)))+
  xlab("Habitat") +
  ylab(expression(paste("Mean abundance (","m"^-2,")", sep = ""))) +
  ggtitle("a) Abundance_outer zone") +
  theme_bw() + graphics

NCMP_abundance_out

#############################################################
#NCMP Inner Zone Diversity

d_in=subset(d_small, Geology %in%c("Limestone"))

d_sum <- plyr::ddply(d_in, .(Year, Zone), summarise,
                     N    = length(!is.na(Diversity)),
                     mean = mean(Diversity, na.rm=TRUE),
                     sd   = sd(Diversity, na.rm=TRUE),
                     se   = sd(Diversity, na.rm=TRUE) / sqrt(length(!is.na(Diversity)) ))

NCMP_diversity_in <- ggplot(d_sum, aes(x=Zone, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.3, binwidth=0.5) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(10)))+
  xlab("Zone") +
  ylab(expression(paste("Mean No. species (","m"^-2,")", sep = ""))) +
  ggtitle("b) Diversity_limestone") +
  theme_bw() + graphics

NCMP_diversity_in

#############################################################
#NCMP Inner Zone Diversity

d_sum <- plyr::ddply(d_in, .(Year, Zone), summarise,
                     N    = length(!is.na(Abundance)),
                     mean = mean(Abundance, na.rm=TRUE),
                     sd   = sd(Abundance, na.rm=TRUE),
                     se   = sd(Abundance, na.rm=TRUE) / sqrt(length(!is.na(Abundance)) ))

NCMP_abundance_in <- ggplot(d_sum, aes(x=Zone, y=mean)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_bar(position=pd, colour="black",stat="identity", width=0.3, binwidth=0.5) +
  scale_y_continuous(expand = c(0,0), limits=c(min(0), max(100)))+
  xlab("Zone") +
  ylab(expression(paste("Mean abundance (","m"^-2,")", sep = ""))) +
  ggtitle("b) Abundance_limestone") +
  theme_bw() + graphics

NCMP_abundance_in

#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(NCMP_diversity_out, NCMP_abundance_out, NCMP_diversity_in, NCMP_abundance_in,nrow=2)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "diversity_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
