#
# This script sets the directories needed for the workflow.
# When a new survey/indicator is added, rerun this script SCOPED to that survey
# (see the countryList/yearList selector below) so it does not create empty
# folders for countries whose results you do not have locally.
#

# The following set wd to be e.g.
# setwd("/Users/qianyu/Dropbox/binary_code/pcg/GATES/SAE4LMIC/prep")

library(here)
# source_path is the folder where this github repository lives in 
source_path <- dirname(here::here())
# source_path is the path for this github repository 
git_path <- here::here()
# Read in the list of indicators and surveys from the spreadsheet
infolist <- read.csv(here(git_path, "info", "infolist.csv"))
surveys <- read.csv(here(git_path,  "info", "surveyslist.csv"))


# --------------------------------------------------------------------------
# Subset: which surveys to create folders for.
#   full list  : countryList <- unique(surveys$country); yearList <- NULL
#   new survey : countryList <- "Ethiopia";              yearList <- 2024
# yearList = NULL means all years for the listed countries.
# --------------------------------------------------------------------------
# Pick ONE of the two options below:

# Option A — ALL countries/surveys (default)
countryList <- unique(surveys$country)
yearList    <- NULL

# Option B — ONE country (optionally one year). To use, comment out Option A
# above and uncomment the two lines below. Set yearList <- NULL for all years.
countryList <- "Ethiopia"
yearList    <- 2024

sel <- which(surveys$country %in% countryList &
             (if (is.null(yearList)) TRUE else surveys$year %in% yearList))


#make result folders
for (i in sel) {
  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  
  # per-survey results path
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  dir.create(results_path, recursive = TRUE, showWarnings = TRUE)
  
}


#make estimates folders
for (i in sel) {
  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  
  # per-survey results path
  results_path <- file.path(source_path, "Gates-results/estimates", country, year)
  dir.create(results_path, recursive = TRUE, showWarnings = TRUE)
  
}


#make Report Plots folders
for (i in sel) {
  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  
  # per-survey results path
  results_path <- file.path(source_path, "Gates-results/ReportPlots", country, year)
  dir.create(results_path, recursive = TRUE, showWarnings = TRUE)
  
}



#make check folders
for (i in sel) {
  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  
  # per-survey results path
  results_path <- file.path(source_path, "Gates-results/check", country, year)
  dir.create(results_path, recursive = TRUE, showWarnings = TRUE)
  
}


#make WEB folders
for (i in sel) {
  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  
  # per-survey results path
  results_path <- file.path(source_path, "Gates-results/ShinyPlots", country, year)
  dir.create(results_path, recursive = TRUE, showWarnings = TRUE)
  
}

