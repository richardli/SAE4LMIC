

library(here)
library(qs)
library(dplyr)


library(here)
# source_path is the folder where this github repository lives in 
source_path <- dirname(here::here())
# source_path is the path for this github repository 
git_path <- here::here()
source(here(git_path, "prep/prepare_functions.R"))

read_two_model_result<-function(ad1_name, ad2_name, country){
  

  infolist <- read.csv(file.path(git_path, "info", "infolist.csv"))
  surveys  <- read.csv(file.path(git_path, "info", "surveyslist.csv"))
  
  for (i in which(surveys$country %in% country) ){
    


  country       <- surveys$country[i]
  year          <- surveys$year[i]
  year_key      <- as.character(year)           
  version       <- surveys$version[i]
  survey_name   <- surveys$survey_name[i]
  iso3          <- surveys$iso3[i]
  results_path  <- file.path(source_path, "Gates-results/Results", country, year)

  
  
  for (indicator in   infolist$ID ) {
    qfile_adm1 <- file.path(results_path, paste0(ad1_name, indicator, ".qs"))
    qfile_adm2 <- file.path(results_path, paste0(ad2_name, indicator, ".qs"))
    
    if (!file.exists(qfile_adm1)) { message("Missing ADM1 ", indicator, " for ", ctry, " ", year); next }
    if (!file.exists(qfile_adm2)) { message("Missing ADM2 ", indicator, " for ", ctry, " ", year); next }
    
    loaded_adm1 <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("Failed to read ", qfile_adm1, ": ", e$message); return(NULL) })
    loaded_adm2 <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("Failed to read ", qfile_adm2, ": ", e$message); return(NULL) })
    
    if (is.null(loaded_adm1) || is.null(loaded_adm2)) next
    
    # store both under the indicator
    if (is.null(res[[iso3]]))          res[[iso3]] <- list()
    if (is.null(res[[iso3]][[year_key]])) res[[iso3]][[year_key]] <- list()
    
    res[[iso3]][[year_key]][[indicator]] <- list(
      adm1 = loaded_adm1,
      adm2 = loaded_adm2
    )
    
  
  }
  }
  
  return(res)
}



tesr<-read_two_model_result(
  ad1_name="new_res_adm1-",
  ad2_name="new_FH_adm2_fix_nest-",
  country="Nigeira"
)





