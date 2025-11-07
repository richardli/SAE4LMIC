#
# This script prepares survey data for each indicator.  This file reads basic.Rdata and loops over all indicators to produce data
#
#

library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)
library(robustbase)
library(raster)
library(devtools)
library(forcats)
library(qs)
library(countrycode)
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
source(here(git_path, "prep/prepare_functions.R"))

# Read in the list of indicators and surveys from the spreadsheet
infolist <- read.csv(here(git_path, "info", "infolist.csv"))
surveys <- read.csv(here(git_path,  "info", "surveyslist.csv"))
indicatorlist=infolist$ID


countryList=c("Burkina Faso" ,
              "Congo Democratic Republic",
              "Ethiopia",
              "Kenya",
              "Mozambique",
              "Nigeria",
              "Rwanda",
              "Senegal" ,
              "Sierra Leone",
              "Tanzania",
              # 11/2025 new countries
              "Mali",
              # "Zambia",
              "South Africa")


# to loop over all countries 1:seq_len(nrow(surveys))
# to loop over one country which(surveys$country %in% country)
# to loop over all indicator indicatorlist=infolist$ID
# to loop over some indicators indicatorlist<-c("RH_DELP_C_HOT") #"RH_DELP_C_DHT", "RH_DELP_C_PUT","RH_DELP_C_PRT",

indicatorlist="ML_NETC_C_ITN"

for (i in which(surveys$country %in% countryList) ) {






  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL

  results_path <- file.path(source_path, "Gates-results/Results", country, year)


  fn<-get_dhs_filenames(country,year)
  irname<-fn$irname
  krname<-fn$krname
  prname<-fn$prname
  brname<-fn$brname

  message(sprintf("\n--- work on %s %s (%s) ---", country, year, survey_name))


# ---- run for all indicators ----
for (indicator in indicatorlist) {
  tryCatch(
    savedata_one_indicator(indicator, country, year,irname,prname,krname,brname,
    infolist, source_path,results_path),
    error = function(e) {
      message("Failed indicator ", indicator, ": ", conditionMessage(e))
    }
  )
  gc()
}
}























