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





library(here)
setwd("/Users/qianyu/Dropbox/binary_code/pcg/GATES/Gates-data")
source_path<- here::here()
infolist <- read.csv(here(source_path, "infolist.csv"))
surveys <- read.csv(here(source_path, "surveyslist.csv"))




source_path<- "/Users/qianyu/Dropbox/binary_code/pcg/GATES/"
infolist <- read.csv(file.path(source_path, "infolist.csv"))
surveys <- read.csv(file.path(source_path, "surveyslist.csv"))
indicatorlist=infolist$ID

# indicatorlist= c("ED_LITR_W_LIT","CO_MOBB_W_MOB",
#                  "ED_EDUC_W_SEH","HC_WIXQ_P_12Q",
#                  "FP_CUSM_W_MOD","RH_ANCN_W_N4P",
#                  "RH_DELP_C_DHT","RH_DELA_C_SKP",
#                  "CH_VACC_C_DP3","CH_VACC_C_MSL",
#                  "CH_VACC_C_NON","CN_NUTS_C_WH2",
#                  "CN_NUTS_C_HA2","ML_NETC_C_ITN",
#                  "CM_ECMR_C_NNF","RH_DELP_C_HOM")





countryList="Tanzania"

countryList="Nigeria"





#step 2.1  direct estimates

for (i in which(surveys$country %in% countryList)) { #seq_len(nrow(surveys))
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  country_short <- surveys$country_short[i]
  survey_name   <- surveys$survey_name[i]
  version<-surveys$version[i]
  message("Processing survey: ", survey_name, " (", country, " ", year, ")")

  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  
  # Load basic Rdata once per survey
  load(file.path(results_path, "basic.Rdata"))


  

  
  for (indicator in indicatorlist) {

    qfile=  paste0(file.path(results_path, indicator),".qs")



    if (file.exists(qfile)) {

      loaded_data1 <- qread(qfile)
      data=loaded_data1




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
        
        
        res_adm2_fix<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 2,
          admin.info = admin.info2, aggregation = FALSE, var.fix = TRUE,
          alt.strata = "v022",
          threshold = 1e-30,
          CI=0.9
          
        )
        res_adm2_fix$data.info<-data.info$summary.ad2
        
        res_adm1<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 1,
          admin.info = admin.info1, aggregation = FALSE, var.fix = TRUE,
          alt.strata = "v022",
          CI=0.9
        )
        res_adm1$data.info<-data.info$summary.ad1
        
        res_adm0<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 0,
          admin.info = admin.info1, aggregation = FALSE, var.fix = TRUE,
          alt.strata = "v022",
          CI=0.9
        )
        
        res_adm0$data.info<-data.info$summary.ad0
        
    
      }else{
        
        data.info<-surveyPrev::datainfo(
          data = data,
          cluster.info = cluster.info,
          admin.info1 = admin.info1,
          admin.info2 = admin.info2
        )
        
        res_adm2_fix<-surveyPrev::directEST(
          data =data, cluster.info = cluster.info, admin = 2,
          admin.info = admin.info2, aggregation = FALSE, var.fix = TRUE,
          threshold = 1e-30,
          CI=0.9
        )
        res_adm2_fix$data.info<-data.info$summary.ad2
        
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


      
      out0 <- file.path(results_path, paste0( "res_adm0-",indicator, ".qs"))
      qs::qsave(res_adm0, file = out0)
      message("Saved: ", out0)
      
      
      
      out1 <- file.path(results_path, paste0( "res_adm1-",indicator, ".qs"))
      qs::qsave(res_adm1, file = out1)
      message("Saved: ", out1)
      
      
      
      out2 <- file.path(results_path, paste0( "res_adm2_fix-",indicator, ".qs"))
      qs::qsave(res_adm2_fix, file = out2)
      message("Saved: ", out2)
      
      

     
      # res_data_ad0[[surveys$iso3[i]]][[as.character(year)]][[indicator]]<-res_adm0
      # 
      # res_data_ad1[[surveys$iso3[i]]][[as.character(year)]][[indicator]]<-res_adm1
      # 
      # res_data_ad2[[surveys$iso3[i]]][[as.character(year)]][[indicator]]<-res_adm2_fix
      # 

      message("done: ", indicator, year)


    }else {
      message("Missing ", indicator)
    }
    
    
      
      
    
  }


  

  }




#step 2.2  fh model 



for (i in which(surveys$country %in% countryList)) { #seq_len(nrow(surveys))
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  country_short <- surveys$country_short[i]
  survey_name   <- surveys$survey_name[i]
  version<-surveys$version[i]
  message("Processing survey: ", survey_name, " (", country, " ", year, ")")
  
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  
  # Load basic Rdata once per survey
  load(file.path(results_path, "basic.Rdata"))
  

  
  
  
  for (indicator in indicatorlist) {
    
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
          surveyPrev::fhModel(
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
                  fhModel(
                    data=data,
                    CI=0.9,
                    cluster.info = cluster.info,
                    admin.info = admin.info2,
                    admin = 2,
                    model = "bym2",
                    aggregation = FALSE,
                    var.fix = TRUE,
                    nested = TRUE,
                    threshold = 1e-30
                  ),
                  error = function(e) {
                    message("FH Admin-2 fix failed for ", indicator, ": ", conditionMessage(e))
                    "failed"
                  }
                )

    FH_adm2_fix_nest$data.info=data.info$summary.ad2
                
      }
      
      
      
      
      
      out <- file.path(results_path, paste0( "FH_adm2_fix_nest-",indicator, ".qs"))
      qs::qsave(FH_adm2_fix_nest, file = out)
      message("Saved: ", out)
      
      
    
      
      message("done: ", indicator, year)
      
      
    }else {
      message("Missing ", indicator)
    }
    
    
    
    
    
  }
  
  
  
  
}







