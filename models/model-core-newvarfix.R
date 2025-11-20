#
# This script fits the two levels of models for the SAE4LMIC web app using variance fix code 10/30/2025 version
#

## -------------------------------------------------------------------------##
## ------------------- Direct estimation -----------------------------------##
## -------------------------------------------------------------------------##

source(here(git_path, "misc", "directEST_1030.R"))
source(here(git_path, "misc", "fhModel_1030.R"))

library(dplyr)
library(purrr)
library(ggrepel)
library(kableExtra)
library(devtools)
library(surveyPrev)
library(INLA)
library(geodata)
library(sf)
library(ggpattern)
library(SUMMER)
library(rdhs)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(kableExtra)
library(png)
library(grid) 
library(sf)
library(viridis)
library(gridExtra)

for (i in which(surveys$country %in% country_to_run)){ 
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  country_short <- surveys$country_short[i]
  survey_name   <- surveys$survey_name[i]
  version<-surveys$version[i]
  message("Processing survey: ", survey_name, " (", country, " ", year, ")")
  
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  
  # Load basic Rdata once per survey
  load(file.path(results_path, "basic.Rdata"))
  
  for (indicator in indicator_to_run) {
    
    qfile=  paste0(file.path(results_path, indicator),".qs")
    
    
    
    if (file.exists(qfile)) {
      
      loaded_data1 <- qread(qfile)
      data=loaded_data1
      
      ##
      ##  Countries with alternative strata
      ##
      if (country == "Rwanda" |
          (country == "Tanzania" && year == 2015) |
          (country == "Mali" && year == 2023) |
          country == "Sierra Leone") {
        
        data.info<-surveyPrev::datainfo(
          data = data,
          cluster.info = cluster.info,
          admin.info1 = admin.info1,
          admin.info2 = admin.info2
        )
        
        
        
        
        
        res_adm2_fix<-directEST_1030(
          data = data, cluster.info = cluster.info, admin = 2,
          admin.info = admin.info2, aggregation = FALSE, var.fix = TRUE,
          alt.strata = "v022",
          threshold = 1e-30,
          CI=0.9
          
        )
        res_adm2_fix$data.info<-data.info$summary.ad2
        
        
        res_adm2<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 2,
          admin.info = admin.info2, aggregation = FALSE, var.fix = FALSE,
          alt.strata = "v022",
          CI=0.9
        )
        res_adm2$data.info<-data.info$summary.ad2
        
        
        
        
        
        res_adm1<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 1,
          admin.info = admin.info1, aggregation = FALSE, var.fix = FALSE,
          alt.strata = "v022",
          CI=0.9
        )
        res_adm1$data.info<-data.info$summary.ad1
        
        res_adm0<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 0,
          admin.info = admin.info1, aggregation = FALSE, var.fix = FALSE,
          alt.strata = "v022",
          CI=0.9
        )
        
        res_adm0$data.info<-data.info$summary.ad0
        
        ##
        ##  Countries with default strata
        ##    
      }else{
        
        data.info<-surveyPrev::datainfo(
          data = data,
          cluster.info = cluster.info,
          admin.info1 = admin.info1,
          admin.info2 = admin.info2
        )
        
        
        # res_adm2_fix <- tryCatch(
        #   directEST_1030(
        #     data = data, cluster.info = cluster.info, admin = 2,
        #     admin.info = admin.info2, aggregation = FALSE, var.fix=TRUE, all.fix = FALSE,
        #     CI=0.9
        #     
        #   ),
        #   error = function(e) {
        #     message( "Dir_varfix Admin-2 fix failed for ", indicator, ": ", conditionMessage(e))
        #     "failed"
        #   }  )
        # 
        # res_adm2_fix<- directEST_1030(
        #     data = data, cluster.info = cluster.info, admin = 2,
        #     admin.info = admin.info2, aggregation = FALSE, var.fix=TRUE, all.fix = FALSE,
        #     CI=0.9
        # 
        #   )
   
        
        
        
        
        res_adm2_fix <- tryCatch(
          directEST_1030(
            data = data, cluster.info = cluster.info, admin = 2,
            admin.info = admin.info2, aggregation = FALSE, var.fix=TRUE, all.fix = FALSE,
            CI=0.9

          ),
          error = function(e) {
            message( "Dir_varfix Admin-2 fix failed for ", indicator, ": ", conditionMessage(e))
            "failed"
          }  )

        res_adm2_fix$data.info<-data.info$summary.ad2
        
        
        res_adm2<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 2,
          admin.info = admin.info2, aggregation = FALSE, var.fix = FALSE,
          CI=0.9
        )
        res_adm2$data.info<-data.info$summary.ad2
        
        
        
        res_adm1<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 1,
          admin.info = admin.info1, aggregation = FALSE,
          CI=0.9
        )
        res_adm1$data.info<-data.info$summary.ad1
        
        res_adm0<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 0,
          admin.info = admin.info1, aggregation = FALSE,
          CI=0.9
        )
        res_adm0$data.info<-data.info$summary.ad0
        
        
      }
      
      out0 <- file.path(results_path, paste0( "new_res_adm0-",indicator, ".qs"))
      qs::qsave(res_adm0, file = out0)
      message("Saved: ", out0)
      
      
      
      out1 <- file.path(results_path, paste0( "new_res_adm1-",indicator, ".qs"))
      qs::qsave(res_adm1, file = out1)
      message("Saved: ", out1)
      
      
      
      out2 <- file.path(results_path, paste0( "new_res_adm2_fix-",indicator, ".qs"))
      qs::qsave(res_adm2_fix, file = out2)
      message("Saved: ", out2)
      
      
      
      out3 <- file.path(results_path, paste0( "new_res_adm2-",indicator, ".qs"))
      qs::qsave(res_adm2, file = out3)
      message("Saved: ", out3)
      
      
      
      # res_data_ad0[[surveys$iso3[i]]][[as.character(year)]][[indicator]]<-res_adm0
      # 
      # res_data_ad1[[surveys$iso3[i]]][[as.character(year)]][[indicator]]<-res_adm1
      # 
      # res_data_ad2[[surveys$iso3[i]]][[as.character(year)]][[indicator]]<-res_adm2_fix
      # 
      
      message("done: ", indicator, year)
    } else {
      message("Missing ", indicator)
    }    
  }
}



## -------------------------------------------------------------------------##
## ---------------------------- FH model -----------------------------------##
## -------------------------------------------------------------------------##


for (i in which(surveys$country %in% country_to_run)) { #seq_len(nrow(surveys))
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  country_short <- surveys$country_short[i]
  survey_name   <- surveys$survey_name[i]
  version<-surveys$version[i]
  message("Processing survey: ", survey_name, " (", country, " ", year, ")")
  
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  
  # Load basic Rdata once per survey
  load(file.path(results_path, "basic.Rdata"))
  
  
  
  
  
  for (indicator in indicator_to_run) {
    
    qfile=  paste0(file.path(results_path, indicator),".qs")
    
    
    
    if (file.exists(qfile)) {
      
      loaded_data1 <- qread(qfile)
      data=loaded_data1
      
      
      
      
      if (country == "Rwanda" |
          (country == "Tanzania" && year == 2015) |
          (country == "Mali" && year == 2023) |
          country == "Sierra Leone"){
        
        data.info<-surveyPrev::datainfo(
          data = data,
          cluster.info = cluster.info,
          admin.info1 = admin.info1,
          admin.info2 = admin.info2
        )
        
        FH_adm2_fix_nest <- tryCatch(
          fhModel_1030(
            data,
            CI=0.9,
            cluster.info = cluster.info,
            admin.info = admin.info2,
            admin = 2,
            model = "bym2",
            aggregation = FALSE,
            var.fix = TRUE,
            nested = TRUE,
            alt.strata = "v022"
          ),
          error = function(e) {
            message("FH Admin-2 fix failed for ", indicator, ": ", conditionMessage(e))
            "failed"
          }  )
        
        FH_adm2_fix_nest$data.info=data.info$summary.ad2
        
        
        
      } else{
        data.info<-surveyPrev::datainfo(
          data = data,
          cluster.info = cluster.info,
          admin.info1 = admin.info1,
          admin.info2 = admin.info2
        )
        
        FH_adm2_fix_nest <- tryCatch(
          fhModel_1030(
            data=data,
            CI=0.9,
            cluster.info = cluster.info,
            admin.info = admin.info2,
            admin = 2,
            model = "bym2",
            aggregation = FALSE,
            var.fix = TRUE,
            nested = TRUE
          ),
          error = function(e) {
            message("FH Admin-2 fix failed for ", indicator, ": ", conditionMessage(e))
            "failed"
          }
        )
        
        FH_adm2_fix_nest$data.info=data.info$summary.ad2
        
      }
      
      
      
      
      
      out <- file.path(results_path, paste0("new_FH_adm2_fix_nest-",indicator, ".qs"))
      qs::qsave(FH_adm2_fix_nest, file = out)
      message("Saved: ", out)
      
      
      out1 <- file.path(results_path, paste0( "new_summary-FH_adm2_fix_nest-",indicator, ".qs"))
      qs::qsave(summary(FH_adm2_fix_nest$model$fit), file = out1)
      message("Saved: ", out1)
      
      
      
      message("done: ", indicator, year)
      
      
    }else {
      message("Missing ", indicator)
    }   
  }
} 




