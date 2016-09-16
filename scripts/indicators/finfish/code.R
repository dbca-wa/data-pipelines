setwd("~/projects/data-pipelines/scripts/indicators/finfish")
source("~/projects/data-pipelines/setup/ckan.R")
library(ggplot2)
library(plyr)
library(gridExtra)

# Step 1: choose data
# Find your CSV on http://internal-data.dpaw.wa.gov.au/
# E.g.: http://internal-data.dpaw.wa.gov.au/dataset/abundance-of-target-finfish-species-at-the-montebello-and-barrow-islands-marine-protected-areas/resource/fd874a78-4d65-46fe-ba07-3b1ef0c54269
# Paste resource id (the hash after "/resource/")
a <- load_ckan_csv("1eac24f5-aada-4532-b990-54557542ea3a", date_colnames = c('date', 'Date', 'date.start', 'date.end', 'start.date','end.date'))

# Step 2: Create output
Lethse <- ddply(a, .(Year, Zone), summarise,
                N    = length(Diversity),
                mean = mean(Diversity),
                sd   = sd(Diversity),
                se   = sd(Diversity) / sqrt(length(Diversity)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

Leth <- ggplot(Lethse, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Species per 90 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Macroalgae (RZ n = 27, SZ n = 63)") +
  scale_x_continuous(limits=c(min(Lethse$Year-0.25),max(Lethse$Year+0.25)), breaks=min(Lethse$Year):max(Lethse$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_blank(),                #removes x axis title
        axis.title.y=element_text(size=14,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        legend.position="none",
        legend.justification=c(1,1), legend.position=c("right"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.text=element_blank())

Leth
#################################
b <- load_ckan_csv("028e765b-8c07-41ba-81f0-e2330ca4c1e6", date_colnames = c('date', 'Date', 'date.start', 'date.end', 'start.date','end.date'))

# Step 2: Create output
Backse <- ddply(b, .(Year, Zone), summarise,
                N    = length(Diversity),
                mean = mean(Diversity),
                sd   = sd(Diversity),
                se   = sd(Diversity) / sqrt(length(Diversity)) )
pd <- position_dodge(0.25)
limits <- aes(ymax = mean + se, ymin = mean - se)

Back <- ggplot(Backse, aes(x=Year, y=mean, group=Zone, linetype=Zone, shape=Zone)) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.25, position=pd) + # error bars
  geom_line(position=pd) +                      # line
  geom_point(position=pd, size=3) +             # points
  xlab("Survey Year") +
  ylab(expression(paste("Species per 90 ", m^2, "", " +/- SE", sep = ""))) +
  ggtitle("Back Reef (RZ n = 27, SZ n = 108)") +
  scale_x_continuous(limits=c(min(Backse$Year-0.25),max(Backse$Year+0.25)), breaks=min(Backse$Year):max(Backse$Year)) +
  theme_bw() +
  theme(axis.text=element_text(size=10),                  #rotates the x axis tick labels an angle of 45 degrees
        axis.text.x=element_text(angle=45, vjust=0.4),
        axis.title.x=element_text(size=14,face="bold"),                #removes x axis title
        axis.title.y=element_text(size=14,face="bold"),               #removes y axis title
        axis.line=element_line(colour="black"),   #sets axis lines
        panel.grid.minor=element_blank(),          #removes minor grid lines
        panel.grid.major=element_blank(),          #removes major grid lines
        panel.border=element_blank(),                #removes border
        panel.background=element_blank(),            #needed to ensure integrity of axis lines
        plot.title=element_text(hjust=1,size=14,face="bold"),
        #legend.position="none",
        legend.justification=c(1,1), legend.position=c("bottom"), # Positions legend (x,y) in this case removes it from the graph
        legend.title=element_text(size=14,face="bold"),
        legend.key=element_blank(),
        legend.text=element_text(size=12))

Back

# Step 4: Create PDF (will be saved to current workdir)
pdf_fn <- "NMP_Fish_JuvDivers_Lagoon.pdf"
pdf(pdf_fn, height = 9, width = 7)
grid.arrange(Leth,Back)
dev.off()

## Step 5: Upload to CKAN
## Create a file ckan_secret with this line containing your CKAN API_KEY:
# library(ckanr); ckanr::ckanr_setup(url="http://internal-data.dpaw.wa.gov.au/", key="API_KEY")

## The PDF and R script must already exist on CKAN (if not, create them manually)
pdf_resource_id = "baae321a-64ca-4400-ae65-3c979199f8df"
rscript_resource_id = "8f3b3ca5-5260-4caf-8e1c-ff0460c43005"
## Upload the figure (PDF) and this script (TXT) to CKAN:
ckanr::resource_update(pdf_resource_id, pdf_fn)
ckanr::resource_update(rscript_resource_id, "code.R")

# Step 6: set workdir to main report location
setwd("~/projects/data-pipelines")
