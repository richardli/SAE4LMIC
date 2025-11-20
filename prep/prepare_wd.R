#
# This script sets the directories needed for the workflow for all countries & surveys
# When a new survey/indicator is added, this script needs to be rerun
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


project_root <- rprojroot::find_root(rprojroot::is_here)


#make result folders
for (i in seq_len(nrow(surveys))) {
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
for (i in seq_len(nrow(surveys))) {
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
for (i in seq_len(nrow(surveys))) {
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
for (i in seq_len(nrow(surveys))) {
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
for (i in seq_len(nrow(surveys))) {
  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  
  # per-survey results path
  results_path <- file.path(source_path, "Gates-results/ShinyPlots", country, year)
  dir.create(results_path, recursive = TRUE, showWarnings = TRUE)
  
}

