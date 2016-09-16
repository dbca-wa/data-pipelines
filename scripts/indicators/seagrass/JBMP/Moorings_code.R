setwd("~/projects/data-pipelines/scripts/indicators/seagrass/JBMP")
source("~/projects/data-pipelines/setup/ckan.R")
source("~/projects/data-pipelines/scripts/ckan_secret.R")

library(ggplot2)
#install.packages("gridExtra")
library(gridExtra)
library(plyr)

csv_rid <- "f3d79b83-6b9f-41bc-826f-6dccef3eeee2"
pdf_rid <- "3092ad5c-9423-4bb6-b109-ab1b1a6c9e31"
txt_rid <- "ea76f3a2-348c-42da-bd8d-e28c9481c984"
pdf_fn = "final.pdf"

d <- load_ckan_csv(csv_rid, date_colnames = c('date', 'Date'))

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
#JBMP_south Shoot density


d_sum <- plyr::ddply(JBMP_s, .(Year, Zone), summarise,
               N    = length(!is.na(Pos_total)),
               mean = mean(Pos_total, na.rm=TRUE),
               sd   = sd(Pos_total, na.rm=TRUE),
               se   = sd(Pos_total, na.rm=TRUE) / sqrt(length(!is.na(Pos_total)) ))

JBMP_mooring <- ggplot(d, aes(x=Year, y=Seagrass)) +
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.02, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d$Year-0.125), max(d$Year+0.125)), breaks=min(d$Year):max(d$Year)) +
  scale_y_continuous(limits=c(min(0), max(100)))+
  xlab("Year") +
  ylab(expression(paste("Total number", sep = ""))) +
  ggtitle("Moorings in seagrass")+
  theme_bw() + graphics

JBMP_mooring

###############################################################################
# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(JBMP_mooring, ncol=1)
dev.off()


## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "shoot_density_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
