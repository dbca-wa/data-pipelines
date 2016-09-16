setwd("~/projects/mpa-reporting/scripts/indicators/wave_height/JBMP")
source("~/projects/mpa-reporting/scripts/ckan.R")
source("~/projects/mpa-reporting/scripts/ckan_secret.R")
## Please read "scripts/indicators/README.md" first! (Click "Preview as HTML")

# Paste data (CSV), figure (PDF) and code (TXT) resource IDs:
csv_rid <- "/d579407e-6c95-4d38-8ddd-ca0a9ec8bb18"
pdf_rid <- "4250855c-a13b-438d-b31e-6374217679d8"
txt_rid <- "30a2e8ae-e146-4b0b-b445-72ec29410f8b"
pdf_fn <- "figure.pdf"


###################################
library(ggplot2)
library(plyr)

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
                 legend.key = element_blank())

jbmp <- ggplot(d, aes(x=Year, y=Days, group=Type, linetype=Type, shape=Type)) +
  geom_line(position=pd) +
  geom_hline(aes(yintercept=Mean_mean), linetype="dashed") + #includes horizontal line
  geom_hline(aes(yintercept=Max_mean)) + #includes horizontal line
  geom_point(position=pd, size=3, fill="black") + # 21 is filled circle
  scale_x_continuous(limits=c(min(d$Year-0.125), max(d$Year+0.125)), breaks=min(d$Year):max(d$Year)) +
  scale_y_continuous(limits=c(min(0), max(60)))+
  xlab("Year") +
  ylab(expression(paste("No of days >4m", sep = ""))) +
  ggtitle("Wave height")+
  theme_bw() + graphics

jbmp

#####################################################################################

# Step 4: Create PDF (will be saved to current workdir)

pdf(pdf_fn, width=8, height=7)
grid.arrange(jbmp, ncol=1)
dev.off()

## Step 5: Upload to CKAN
ckanr::resource_update(pdf_rid, pdf_fn)
ckanr::resource_update(txt_rid, "wave height_code.R")

# Step 6: set workdir to main report location
setwd("~/projects/mpa-reporting/reports")
