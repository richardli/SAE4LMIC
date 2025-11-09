#
#
# This file sets up the dependency and common objects for all models in this folder
#
library(sf)
library(ggplot2)
library(tidyr)
library(patchwork)
library(robustbase)
library(raster)
library(devtools)
library(forcats)
library(qs)
library(countrycode)
library(purrr)
library(ggrepel)
library(devtools)
library(geodata)
library(ggpattern)
library(rdhs)
library(kableExtra)
library(png)
library(grid) 
library(viridis)
library(gridExtra)
library(dplyr)

# install_github("richardli/surveyPrev",force = T)
# install_github("richardli/SUMMER")

library(surveyPrev)
library(SUMMER)
library(INLA)
library(here)
# source_path is the folder where this github repository lives in 
source_path <- dirname(here::here())
# source_path is the path for this github repository 
git_path <- here::here()

# Read in the list of indicators and surveys from the spreadsheet
infolist <- read.csv(here(git_path, "info", "infolist.csv"))
surveys <- read.csv(here(git_path,  "info", "surveyslist.csv"))
indicatorlist <- infolist$ID







