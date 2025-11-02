

readresult<-function(resultname, country, indicatorlistloop){
  
  idx <- which(surveys$country == country)
  res_data_ad=list()
  # ad_store=list()
  for (i in seq(min(idx), max(idx))) {
    country       <- surveys$country[i]
    year          <- surveys$year[i]
    year_key      <- as.character(year)
    version       <- surveys$version[i]
    survey_name   <- surveys$survey_name[i]
    iso3          <- surveys$iso3[i]         
    
    results_path <- file.path(source_path, "Gates-results/Results", country, year)
    load(file.path(results_path, "basic.Rdata"))
    
    for (indicator in indicatorlistloop) {
      qfile <- file.path(results_path, paste0(resultname, indicator, ".qs"))
      if (!file.exists(qfile)) {
        message("Missing ", indicator, " for ", country, " ", year)
        next
      }
      
      loaded_data <- qs::qread(qfile)
      message("Loaded ", indicator, " (", survey_name, ")")
      
      
      res_data_ad[[iso3]][[year_key]][[indicator]] <- loaded_data
      
      # # Get existing map for this country/indicator, append this year
      # ci_map <- ad_store[[country]][[indicator]]
      # if (is.null(ci_map)) ci_map <- list()
      # ci_map[[year_key]] <- loaded_data
      # 
      # # Write back to ad_store (if you still want to keep that structure)
      # if (is.null(ad_store[[country]])) ad_store[[country]] <- list()
      # ad_store[[country]][[indicator]] <- ci_map
      # 
      # # --- New write path (ISO3 -> year -> indicator) ---
      # if (is.null(res_data_ad[[iso3]]))            res_data_ad[[iso3]] <- list()
      # if (is.null(res_data_ad[[iso3]][[year_key]])) res_data_ad[[iso3]][[year_key]] <- list()
      # res_data_ad[[iso3]][[year_key]][[indicator]] <- ci_map
    }
    
    
    
  }
  return(res_data_ad)
}



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



country="Nigeria"




source_path<- "/Users/qianyu/Dropbox/binary_code/pcg/GATES/"
infolist <- read.csv(file.path(source_path, "infolist.csv"))
surveys <- read.csv(file.path(source_path, "surveyslist.csv"))
indicatorlist=infolist$ID




res_data_ad1<-readresult(resultname="res_adm1-",
                         country=country, 
                         indicatorlistloop=indicatorlist)





for (i in which(surveys$country %in% country)  ) {
  
  
  
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  year_key      <- as.character(year)
  version       <- surveys$version[i]
  survey_name   <- surveys$survey_name[i]
  iso3          <- surveys$iso3[i]    

  
  res_adm11 <-  res_data_ad1[[iso3]][[year_key]][[indicator]]

  if(!is.null(res_adm11)) table=res_adm11$res.admin1[c("admin1.name", "direct.est","direct.se","direct.lower",  
                                                    "direct.upper",  "cv2"),]
  
  table$
  table <- table %>%
    rename(
      Region_Name = admin1.name,
      Direct_Est = direct.est,
      Direct_SE = direct.se,
      CV = cv2
    )

  
}



