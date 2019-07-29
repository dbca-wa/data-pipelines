#' Install or update all required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(bitops,
               caTools,
               # ckanr,
               colorspace,
               dplyr,
               # tidyr,
               ggplot2,
               Hmisc,
               knitr,
               lubridate,
               markdown,
               readr,
               rjson,
               RColorBrewer,
               RCurl,
               scales,
               shiny,
               stringr,
               update=T)
remotes::install_github("tidyverse/tidyr")
remotes::install_github("ropensci/ckanr")
