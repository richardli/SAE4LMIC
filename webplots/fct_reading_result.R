library(here)
library(qs)
library(dplyr)

source(here(git_path, "prep/prepare_functions.R"))

# load pre-generated result from dropbox folder

load_data <- function(country_name, country, year1, year2){
  res_data <- read_two_model_result(
    ad1_name="new_res_adm1-",
    ad2_name="new_FH_adm2_fix_nest-",
    country=country_name
  )
  
  CI <- 0.9
  
  
  for (ind in res_data[[country]][[year1]]){
    new <- ind$adm2
    
    if(!is.null(new)){
      qs12 <- apply(new$admin2_post, 2, quantile,
                    probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
      new$res.admin2$lower <- qs12[1, ]
      new$res.admin2$upper <- qs12[2, ]
      
      ind$adm2 <- new
    }
    
    
    
    new <- ind$adm1
    
    if(!is.null(new)){
      qs12 <- apply(new$admin1_post, 2, function(col) {
        if (all(is.nan(col) | is.na(col))) {
          return(rep(NaN, 2))  #set both quantile to NaN if
        } else {
          return(quantile(col, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2), na.rm = TRUE))
        }
      })
      
      new$res.admin1$lower <- qs12[1, ]
      new$res.admin1$upper <- qs12[2, ]
      
      ind$adm1 <- new
    }
  }
  
  for (ind in res_data[[country]][[year2]]){
    new <- ind$adm2
    
    if(!is.null(new)){
      qs12 <- apply(new$admin2_post, 2, quantile,
                    probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
      new$res.admin2$lower <- qs12[1, ]
      new$res.admin2$upper <- qs12[2, ]
      
      ind$adm2 <- new
    }
    
    
    
    new <- ind$adm1
    
    if(!is.null(new)){
      qs12 <- apply(new$admin1_post, 2, function(col) {
        if (all(is.nan(col) | is.na(col))) {
          return(rep(NaN, 2))  #set both quantile to NaN if
        } else {
          return(quantile(col, probs = c((1 - CI) / 2, 1 - (1 - CI) / 2), na.rm = TRUE))
        }
      })
      
      new$res.admin1$lower <- qs12[1, ]
      new$res.admin1$upper <- qs12[2, ]
      
      ind$adm1 <- new
    }
  }
  
  return(res_data)
}

read_two_model_result<-function(ad1_name, ad2_name, country){
  

  infolist <- read.csv(file.path(git_path, "info", "infolist.csv"))
  surveys  <- read.csv(file.path(git_path, "info", "surveyslist.csv"))
  if (!exists("res"))          res <- list()
  
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
    
    if (!file.exists(qfile_adm1)) { message("Missing ADM1 ", indicator, " for ", country, " ", year); next }
    if (!file.exists(qfile_adm2)) { message("Missing ADM2 ", indicator, " for ", country, " ", year); next }
    
    loaded_adm1 <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("Failed to read ", qfile_adm1, ": ", e$message); return(NULL) })
    loaded_adm2 <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("Failed to read ", qfile_adm2, ": ", e$message); return(NULL) })
    
    if (is.null(loaded_adm1) || is.null(loaded_adm2)) next
    
    # store both under the indicator
    if (is.null(res[[iso3]]))          res[[iso3]] <- list()
    if (is.null(res[[iso3]][[year_key]])) res[[iso3]][[year_key]] <- list()
    
    n_c1 <- loaded_adm1$data.info$n_clusters
    n_s1 <- loaded_adm1$data.info$n_samples
    loaded_adm1$data.info$n_clusters <- ifelse(n_c1 == 1 & n_s1 == 0, 0, n_c1)
    
    n_c2 <- loaded_adm2$data.info$n_clusters
    n_s2 <- loaded_adm2$data.info$n_samples
    loaded_adm2$data.info$n_clusters <- ifelse(n_c2 == 1 & n_s2 == 0, 0, n_c2)
    
    res[[iso3]][[year_key]][[indicator]] <- list(
      adm1 = loaded_adm1,
      adm2 = loaded_adm2
    )
    
  
  }
  }
  
  return(res)
}