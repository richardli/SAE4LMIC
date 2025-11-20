#
#
# This script fits the models used in the web app for all countries
#
#
countryList=c("Mali",
              "Mozambique"
            )
#  "Senegal" ,  
# "Tanzania", 
# "Sierra Leone", 
# "Mali",
# "Zambia",
# "South Africa"
core_script <- here(git_path,  "models", "model-core-newvarfix.R") 
for(country_to_run in countryList){
  source(here(git_path,  "models", paste0("model-", country_to_run, ".R")))
}








