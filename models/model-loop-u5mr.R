#
#
# This script fits a child-mortality indicator for all countries
# using model-core-u5mr.R (the country scripts source model-core-newvarfix.R,
# which cannot handle mortality data, so they are not used here)
#
#
indicator_to_run <- "CM_ECMR_C_U5F"
countryList <- unique(surveys$country)
#  countryList <- c("Malawi",
#                   "Ethiopia")
for(country_to_run in countryList){
  source(here(git_path, "models", "model-core-u5mr.R"))
}
