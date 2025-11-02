


library(here)



setwd("/Users/qianyu/Dropbox/binary_code/pcg/GATES/Gates-data")
source_path<- here::here()
infolist <- read.csv(here(source_path, "infolist.csv"))
surveys <- read.csv(here(source_path, "surveyslist.csv"))




source_path<- "/Users/qianyu/Dropbox/binary_code/pcg/GATES/"
infolist <- read.csv(file.path(source_path, "infolist.csv"))
surveys <- read.csv(file.path(source_path, "surveyslist.csv"))



#make result folders
for (i in seq_len(nrow(surveys))) {
  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  
  # per-survey results path
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  dir.create(results_path, recursive = TRUE, showWarnings = FALSE)
  
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
  dir.create(results_path, recursive = TRUE, showWarnings = FALSE)
  
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
  dir.create(results_path, recursive = TRUE, showWarnings = FALSE)
  
}

