#
#
# This script fits the models used in the web app for all countries
#
#
countryList=c("Burkina Faso" ,
              "Congo Democratic Republic", 
              "Ethiopia",
              "Kenya", 
              "Mozambique",
              "Nigeria", 
              "Rwanda",
              "Senegal" ,  
              "Tanzania", 
              # 11/2025 new countries
              "Sierra Leone", 
              "Mali",
              "Zambia",
              "South Africa")
 
core_script <- here(git_path,  "models", "model-core.R") 
for(country_to_run in countryList){
  source(here(git_path,  "models", paste0("model-", country, ".R")))
}








