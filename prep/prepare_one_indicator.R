#
# This script builds <indicator>.qs for one or a few indicators across all
# surveys. Use it after appending a new row to info/infolist.csv, instead of
# editing the survey loop in prepare_data.R.
#
# Surveys are skipped (with a message) when the raw DHS files, the filename
# map in get_dhs_filenames(), or the Gates-results/Results/<country>/<year>
# folder are not present on this machine. Existing .qs files are kept unless
# overwrite <- TRUE.
#
#   source("prep/prepare_one_indicator.R")
#
library(dplyr)
library(qs)
library(surveyPrev)
library(here)

source_path <- dirname(here::here())
git_path    <- here::here()
source(here(git_path, "prep/prepare_functions.R"))

infolist <- read.csv(here(git_path, "info", "infolist.csv"))
surveys  <- read.csv(here(git_path, "info", "surveyslist.csv"))

indicatorlist <- c("CM_ECMR_C_U5F")
countryList   <- unique(surveys$country)
#  countryList <- c("Malawi",
#                   "Ethiopia")
overwrite <- FALSE

stopifnot(all(indicatorlist %in% infolist$ID))   # add the row to infolist.csv first

for (i in which(surveys$country %in% countryList)) {
  country      <- as.character(surveys$country[i])
  year         <- as.character(surveys$year[i])
  survey_name  <- as.character(surveys$survey_name[i])
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  raw_path     <- file.path(source_path, "Gates-data/rawDHS", country, year)
  message(sprintf("\n--- %s %s (%s) ---", country, year, survey_name))

  if (!dir.exists(raw_path))     { message("No raw DHS data; skipped"); next }
  if (!dir.exists(results_path)) { message("No results folder (run prepare_wd.R); skipped"); next }
  fn <- tryCatch(get_dhs_filenames(country, year),
                 error = function(e) { message("Not in get_dhs_filenames(); skipped"); NULL })
  if (is.null(fn)) next

  for (indicator in indicatorlist) {
    out <- file.path(results_path, paste0(indicator, ".qs"))
    if (file.exists(out) && !overwrite) { message(indicator, ".qs exists; skipped"); next }
    tryCatch(
      savedata_one_indicator(indicator, country, year, fn$irname, fn$prname, fn$krname, fn$brname,
                             infolist, source_path, results_path),
      error = function(e) message("Failed ", indicator, ": ", conditionMessage(e))
    )
    gc()
  }
}
