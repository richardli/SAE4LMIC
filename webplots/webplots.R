library(qs)
library(surveyPrev)
library(dplyr)
library(htmlwidgets)
library(htmltools)
library(leafsync)
library(leaflet)
library(leaflegend)
library(RColorBrewer)
library(ggplot2)
library(patchwork)
library(sf)
# devtools::install_github("statnmap/HatchedPolygons")
library(HatchedPolygons)
library(ggplotify)



library(here)
# source_path is the folder where this github repository lives in 
source_path <- dirname(here::here())
# source_path is the path for this github repository 
git_path <- here::here()

# All static plots (ridge plots and static prevMaps in overview page) are stored in the gates app github page

static_path <- file.path(source_path, "Gates-results/StaticWebPlots/")
# The folder to save all html detailed prevmaps.
# stored and uploded into two seperate github repos
# countries starts with A-M stored in folder 1, N-Z in folder 2
html_path1 <- file.path(source_path, "Gates-results/ShinyPlots1")
html_path2 <-file.path(source_path, "Gates-results/ShinyPlots2")

# Source plotting functions
source(file.path(git_path, "webplots", "fct_plotting.R"))
source(file.path(git_path, "webplots", "fct_reading_result.R"))
source(file.path(git_path, "webplots", "fct_gen_all_map.R"))
load(file.path(source_path, "Gates-data/WebShapeFile", "poly_shp.RData"))

# Get the list of countries to plot
surveys  <- read.csv(file.path(git_path, "info", "surveyslist.csv"))
country_list <- unique(surveys$country)
country_list <- country_list[country_list != "South Africa"]


# The following generates all plots
# subset country and plot type to generate for only a subset

for(country in country_list){
  if(substr(country, 1, 1) <= "M"){
    html_path <- html_path1
  } else {
    html_path <- html_path2
  }
  load_and_gen_plots(country, static_path, html_path,
                     static = TRUE,
                     html = TRUE,
                     ridge = TRUE,
                     ridge_diff = TRUE)
}
