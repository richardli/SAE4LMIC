#
# This script prepares admin.info1, admin.info2, cluster.info for each survey and they are saved as basic.Rdata in Gates-results/Results/country/year. 
# 
# A map (cluster.info$map) used to check boundary alignment and cluster locations is saved under Gates-results/check/country/year. The wrong.points are printed in the title of each plot. These points should be the clusters without GPS information. The current version wrong.points are the ones which cannot be fixed in clusterinfo(). The fixed points are saved in cluster.info$fixed.points. 
#
# The second loop is to calculate numbers of Admin 1 and Admin 2 for each survey and will be saved as shapefileList.csv in folder Gates. 


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
# source_path is the folder where this github repository lives in 
source_path <- dirname(here::here())
# source_path is the path for this github repository 
git_path <- here::here()
source(here(git_path, "prep/prepare_functions.R"))

# Read in the list of indicators and surveys from the spreadsheet
infolist <- read.csv(here(git_path, "info", "infolist.csv"))
surveys <- read.csv(here(git_path,  "info", "surveyslist.csv"))


countryList=c("Burkina Faso" ,
              "Congo Democratic Republic", 
              "Ethiopia",
              "Kenya", 
              "Mozambique",
              "Nigeria", 
              "Rwanda",
              "Senegal" ,  
              "Sierra Leone", 
              "Tanzania", 
              # 11/2025 new countries
              "Mali",
              "Zambia",
              "South Africa")

#------------------------------------------------------------------#
# Step 1.1
# Prepare and save cluster.info, admin.info2, admin.info1,
#  poly.adm1, poly.adm2, geo for each survey (2 each country)
#------------------------------------------------------------------#

## Admin note: replace with only a subset of countries when updating
country <- countryList[1:length(countryList)]


for (i in which(surveys$country %in% country) ) {
  row <- surveys[i, ]
  country <- as.character(row$country)
  year <- as.character(row$year)
  survey_name <- if (!is.null(row$survey_name)) as.character(row$survey_name) else NULL
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  country_short<-row$country_short

  
  
  
  # if (country=="Rwanda"&year==2014){year=2015}
  # API0_PATH= paste0(source_path,"Gates-data/API/admin0/",country_short,"_",year,"_DHS_est.rda")
  # load(API0_PATH)
  
 
  
  

  # per-survey results path
  data_path <- file.path(source_path, "Gates-data")
  results_path <- file.path(source_path, "Gates-results/Results", country, year)

  # detect geoname folder inside rawDHS
  geoname <- detect_geoname(data_path, country, year, survey_name)
  if (is.null(geoname)) {
    warning(sprintf("Could not detect geoname folder for %s %s (checked rawDHS). Skipping." , country, year))
    next
  }


  # now run the rest of the script inside a tryCatch so a single failure doesn't stop the loop
  tryCatch({



    if (country %in% c("Tanzania")) {
      iso3 <- countrycode(country, origin = "country.name", destination = "iso3c")
      
      poly.adm1 <- geodata::gadm(country = iso3, level = 1, path = tempdir())
      poly.adm1<- sf::st_as_sf(poly.adm1)
      poly.adm2 <- geodata::gadm(country = iso3, level = 2, path = tempdir())
      poly.adm2<- sf::st_as_sf(poly.adm2)
      
      
    }else if(country %in% c("Sierra Leone")){
      
      path <- file.path(data_path,"rawDHS","Sierra Leone","SLE_shp.rds")
      SLE_shp <- readRDS(path)
      
      poly.adm1=SLE_shp$`Admin-1`
      poly.adm1<- sf::st_as_sf(poly.adm1)
      
      poly.adm2=SLE_shp$`Admin-2`
      poly.adm2<- sf::st_as_sf(poly.adm2)
      
      
      
      
    } else{
      
      iso3 <- countrycode(country, origin = "country.name", destination = "iso3c")
      p_adm1 <- surveyPrev:::get_geoBoundaries(iso3, adm = "ADM1")
      p_adm2 <- surveyPrev:::get_geoBoundaries(iso3, adm = "ADM2")
      
      # if (country == "Rwanda") {
      #  p_adm1 <- surveyPrev:::get_geoBoundaries(iso3, adm = "ADM2")
      #  p_adm2 <- surveyPrev:::get_geoBoundaries(iso3, adm = "ADM3")
      # }
      
      
      # --- boundaries ---
      poly.adm1 <- sf::st_as_sf(p_adm1)
      poly.adm2 <- sf::st_as_sf(p_adm2)
      
      poly.adm2 <- surveyPrev:::addUpper(
        poly.adm.upper = poly.adm1,
        poly.adm       = poly.adm2,
        by.adm         = "shapeName",
        by.adm.upper   = "shapeName",
        out_lower      = "NAME_2",
        out_upper      = "NAME_1",
        sort           = TRUE
      )
      poly.adm1 <- poly.adm1 %>% rename(NAME_1 = shapeName)
      
 
      
      
    }
    
    
    # --- geo ---

    file_path_geo <- file.path(data_path, "rawDHS", country, year, geoname, paste0(geoname, ".shp"))
    geo <- sf::st_read(file_path_geo, quiet = TRUE)

    cluster.info <- surveyPrev::clusterInfo(geo = geo,
                                poly.adm1 = poly.adm1,
                                poly.adm2 = poly.adm2,
                                by.adm1 = "NAME_1",
                                by.adm2 = "NAME_2",
                                map=TRUE)

    
    
    admin.info2 <- surveyPrev::adminInfo(poly.adm = poly.adm2,
                                         admin = 2,
                                         by.adm = "NAME_2",
                                         by.adm.upper = "NAME_1")
    
    admin.info1 <- surveyPrev::adminInfo(poly.adm = poly.adm1,
                                         admin = 1,
                                         by.adm = "NAME_1")
    


    
    ##save the map check for bountry.
    ggsave(    
      cluster.info$map+
        labs( title = cluster.info$wrong.points ) ,
           filename = file.path(source_path, "Gates-results/check",country, year, "check-bountry.png"),
           width = 10, height = 10,
           dpi = 300)
    

    
    
    
    
    
    
    
    save(cluster.info, admin.info2, admin.info1, poly.adm1, poly.adm2, geo,
         file = file.path(results_path, "basic.Rdata"))

    message(sprintf("Finished preparing %s %s -> results saved to %s", country, year, results_path))
  }, error = function(e) {
    warning(sprintf("Error preparing %s %s: %s", country, year, e$message))
  })
}





#table for number of admin 1 and admin 2 and shapefile. 


rows <- vector("list", length = length(which(surveys$country %in% countryList)))
k <- 1
for (i in  seq_len(nrow(surveys))) {
  rowi         <- surveys[i, ]
  country      <- as.character(rowi$country)
  year         <- as.character(rowi$year)
  survey_name  <- if (!is.null(rowi$survey_name)) as.character(rowi$survey_name) else NA_character_
  
  message(sprintf("\n--- Preparing %s %s (%s) ---", country, year, survey_name))
  
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  basic_path   <- file.path(results_path, "basic.Rdata")
  
  n_admin1 <- NA_integer_
  n_admin2 <- NA_integer_
  
  if (file.exists(basic_path)) {
    # load into a temporary env so we don't pollute the global env
    e <- new.env(parent = emptyenv())
    load(basic_path, envir = e)
    
    # admin.info1/admin.info2 might not exist in some files—be defensive
    n_admin1 <- tryCatch(
      length(get("admin.info1", envir = e)$data$admin1.name),
      error = function(...) NA_integer_
    )
    n_admin2 <- tryCatch(
      length(get("admin.info2", envir = e)$data$admin2.name.full),
      error = function(...) NA_integer_
    )
  } else {
    warning("Missing basic.Rdata for: ", country, " ", year)
  }
  
  rows[[k]] <- data.frame(
    survey_name = survey_name,
    country     = country,
    year        = as.integer(year),
    n_admin1    = n_admin1,
    n_admin2    = n_admin2,
    stringsAsFactors = FALSE
  )
  k <- k + 1
}

summary_tbl <- do.call(rbind, rows)
summary_tbl[order(summary_tbl$country, summary_tbl$year), ]
summary_tbl$shapefile="geoboundaries"
summary_tbl$shapefile[which(summary_tbl$country=="Tanzania")]<-"GADM"
summary_tbl$shapefile[which(summary_tbl$country=="Sierra Leone")]<-"WHO"


write.csv(summary_tbl, file.path(git_path, "info/shapefileList.csv"), row.names = FALSE)



