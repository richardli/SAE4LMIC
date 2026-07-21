




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
library(purrr)
# install_github("richardli/surveyPrev",force = T)
# install_github("richardli/SUMMER")

library(surveyPrev)
library(SUMMER)
library(INLA)



# source_path is the folder where this github repository lives in
source_path <- dirname(here::here())
# source_path is the path for this github repository
git_path <- here::here()

# Read in the list of indicators and surveys from the spreadsheet
infolist <- read.csv(here(git_path, "info", "infolist.csv"))
surveys <- read.csv(here(git_path,  "info", "surveyslist.csv"))
indicatorlist <- infolist$ID

countryList=c(
  # "Burkina Faso" ,
              # "Congo Democratic Republic",
              # "Ethiopia",
              "Kenya",
              "Mozambique",
              "Nigeria",
              # "Rwanda",
              "Senegal" ,
              "South Africa",
              "Tanzania",
              "Sierra Leone",
              "Mali"
)
# countryList="Senegal"


#
# countryList<- unique(surveys$country)
# countryList="Nigeria"

# Ethiopia, Rwanda need to look separately

countryList="Nigeria"
countryList="Burkina Faso"
countryList="Ethiopia"
countryList=c("Zambia","Sierra Leone")



countryList= unique(surveys$country)
indicatorlist=infolist$ID


for (i in 7 ) { #seq_len(nrow(surveys)) which(surveys$country %in% countryList)

  summary_df<-c()
  res_data_ad0<-list()
  all_results<-list()
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  country_short <- surveys$country_short[i]
  survey_name   <- surveys$survey_name[i]
  version<-surveys$version[i]
  message("Processing survey: ", survey_name, " (", country, " ", year, ")")

  results_path <- file.path(source_path, "/Gates-results/Results", country, year)

  # Load basic Rdata once per survey
  load(file.path(results_path, "basic.Rdata"))



  if (country=="Rwanda"&year==2014){
  API0_PATH= paste0(source_path,"/Gates-data/API/admin0/",country_short,"_","2015","_DHS_est.rda")
  load(API0_PATH)
  }else{
    API0_PATH= paste0(source_path,"/Gates-data/API/admin0/",country_short,"_",year,"_DHS_est.rda")
    load(API0_PATH)
  }

  for (indicator in indicatorlist) {



    API=infolist$API[infolist$ID==indicator]



    if (indicator == "HC_WIXQ_P_12Q") {
      api0 <- NA_real_
    } else {
      vals <- tmp_res$Value[tmp_res$IndicatorId == API]


      if (API=="RH_DELP_C_PRV"& survey_name%in%c("NG2024DHS",
                                                 "TZ2022DHS",
                                                 "CD2023DHS",
                                                 "MZ2022DHS",
                                                 "SN2023DHS",
                                                 "ML2023DHS",
                                                 "ZM2024DHS",
                                                 "MW2024DHS") ){

        nonngo <-tmp_res$Value[
          which(
            tmp_res$IndicatorId == "RH_DELP_C_PNN" &
              tmp_res$ByVariableLabel %in% c(
                "Two years preceding the survey",
                "Two years preceding the survey (DHS)"
              )
          )
        ]
        ngo <-tmp_res$Value[
          which(
            tmp_res$IndicatorId == "RH_DELP_C_PNG" &
              tmp_res$ByVariableLabel %in% c(
                "Two years preceding the survey",
                "Two years preceding the survey (DHS)"
              )
          )
        ]


        vals=nonngo+ngo

      }


      if (API=="CH_DIAT_C_ORT" ){
        vals <-tmp_res$Value[
          which(
            tmp_res$IndicatorId == "CH_DIAT_C_ORT" &
              tmp_res$ByVariableLabel %in% c(
                "Five years preceding the survey"
              )
          )
        ]
        # & survey_name%in%c( "BF2021DHS",    
        #                     "ET2019DHS",
        #                     "ML2018DHS",    
        #                     "MZ2011DHS",
        #                     "NG2018DHS",  
        #                     "RW2015DHS",
        #                     "ZA2016DHS",      
        #                     "TZ2015DHS",
        #                     "TZ2022DHS" 

      }


      if (length(vals) == 0) {
        api0 <- NA_real_
      } else if (length(vals) > 1) {
        # Try to pick the “two years preceding” row; fall back to first available
        pick <- tmp_res$Value[
          tmp_res$IndicatorId == API &
            tmp_res$ByVariableLabel %in% c("Two years preceding the survey",
                                           "Two years preceding the survey (DHS)")
        ]
        if (length(pick) == 0) pick <- vals
        api0 <- pick[1]
      } else {
        api0 <- vals[1]
      }


      # Normalize if it looks like a percentage (guard against NA/length-0)
      api0 <- suppressWarnings(as.numeric(api0))
      if (!is.na(api0) && api0 > 1) api0 <- api0 / 100
      if (API=="CM_ECMR_C_NNR") api0 <- api0 / 10
    }





    qfile=  paste0(file.path(results_path, indicator),".qs")



    if (file.exists(qfile)) {

      loaded_data1 <- qread(qfile)
      data=loaded_data1



      if(all(is.na(data$value))){

        all_results[[length(all_results) + 1]] <- data.frame(
          survey_name   = survey_name,
          country       = country,
          year          = year,
          indicator=indicator,
          surveyPrev=   NA,
          hasFile=1,
          hasData=0,
          API       =api0,
          stringsAsFactors = FALSE
        )



      }else{

        res_adm0<-surveyPrev::directEST(
          data = data, cluster.info = cluster.info, admin = 0,
          admin.info = admin.info1, aggregation = FALSE, var.fix = TRUE,
          alt.strata = "v022",
          CI=0.9
        )
        res_data_ad0[[surveys$iso3[i]]][[as.character(year)]][[indicator]]<-res_adm0


        all_results[[length(all_results) + 1]] <- data.frame(
          survey_name   = survey_name,
          country       = country,
          year          = year,
          indicator=indicator,
          surveyPrev=    as.numeric(res_adm0$res.natl$direct.est),
          hasFile=1,
          hasData=1,
          API       =api0,
          stringsAsFactors = FALSE
        )
        message("done: ", indicator, year)



      }









    }else {
      message("Missing ", indicator)



      all_results[[length(all_results) + 1]] <- data.frame(
        survey_name   = survey_name,
        country       = country,
        year          = year,
        indicator=indicator,
        surveyPrev=  NA,
        hasFile=0,
        hasData=0,
        API       =api0,
        stringsAsFactors = FALSE
      )

    }













  }





  # combine into one data.frame
  summary_df <- dplyr::bind_rows(all_results)


  summary_df$diff=summary_df$surveyPrev-summary_df$API
  summary_df$match=as.integer(abs(summary_df$diff) < 0.01)

  PLOTSET=summary_df[summary_df$indicator != "HC_WIXQ_P_12Q", ]

  tol <- 0.01

  checkplot<-ggplot(PLOTSET, aes(x = API, y = surveyPrev)) +
    geom_point(aes(color = abs(diff) < 0.01), size = 1,alpha=.8) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray40") +
    geom_ribbon(
      aes(x = API, ymin = API - tol, ymax = API + tol),
      fill = "lightblue", alpha = 0.2, inherit.aes = FALSE
    ) +
    scale_color_manual(
      values = c("FALSE" = "red", "TRUE" = "blue"),
      labels = c("FALSE" = "Outside", "TRUE" = "Within 0.01")
    ) +
    labs(
      x = "API",
      y = "surveyPrev",
      color = "check"
    ) +
    coord_equal() +
    theme_bw()




  out0 <- file.path(source_path,  "Gates-results/Results" ,country,year,paste0( "dir-ad0", ".qs"))
  qs::qsave(res_data_ad0, file = out0)





  write.csv(summary_df, file.path(source_path, "Gates-results/check", country,year,paste0(country,"Checkapi0.csv")), row.names = FALSE)



  ggsave(checkplot ,
         filename = file.path(source_path, "Gates-results/check",country,year, paste0(country,"check-national.png")),

         width = 10, height = 10,
         dpi = 300)



}







# root of all those folders
check_root <- file.path(source_path, "Gates-results", "check")

# 1. find all "*Checkapi0.csv" under country/year folders
csv_files <- list.files(
  check_root,
  pattern = "Checkapi0\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# 2. read and combine
all_checkapi0 <- map_dfr(csv_files, function(f) {
  df <- read.csv(f, stringsAsFactors = FALSE)

  ## If summary_df does *not* already have country/year columns,
  ## extract them from the path and add:
  rel_path <- sub(paste0("^", check_root, .Platform$file.sep), "", f)
  parts    <- strsplit(rel_path, .Platform$file.sep)[[1]]
  country  <- parts[1]
  year     <- parts[2]

  df %>%
    mutate(country = country,
           year    = year,
           .before = 1)
})



#1. no data 
all_checkapi0_nodata <- all_checkapi0 %>%
  filter(is.na(surveyPrev) & is.na(API))


keys <- tibble::tibble(
  country   = c("Mozambique", "Kenya", "Rwanda",   "Rwanda",   "Rwanda"),
  year      = c(2022,         2022,    2014,       2014,       2019),
  indicator = c("RH_DELP_C_PRT",
                "RH_DELP_C_PRT",
                "RH_DELP_C_PRT",
                "CH_VACC_C_NON",
                "CH_VACC_C_NON")
)

#1. pass eyeball check, no api, or need /100
all_checkapi0_eyeballcheck <- all_checkapi0 %>%
  semi_join(keys, by = c("country", "year", "indicator"))



all_checkapi0_HC_WIXQ_P_12Q=all_checkapi0[ all_checkapi0$indicator=="HC_WIXQ_P_12Q",]
#all_checkapi0_HC_WIXQ_P_12Q$surveyPrev



all_checkapi0_issue=all_checkapi0[(is.na(all_checkapi0$match) | all_checkapi0$match == 0)& all_checkapi0$indicator!="HC_WIXQ_P_12Q",]


all_checkapi0_issue_smallset <- all_checkapi0_issue %>%
  anti_join(all_checkapi0_eyeballcheck, by = c("country", "year", "indicator")) %>%
  anti_join(all_checkapi0_nodata,        by = c("country", "year", "indicator"))




write.csv(
  all_checkapi0,
  file.path(check_root, "Checkapi_ALL.csv"),
  row.names = FALSE
)


write.csv(
  all_checkapi0_nodata,
  file.path(check_root, "Checkapi_NODATA.csv"),
  row.names = FALSE
)


write.csv(
  all_checkapi0_eyeballcheck,
  file.path(check_root, "Checkapi_eyeballchecked.csv"),
  row.names = FALSE
)

write.csv(
  all_checkapi0_issue_smallset,
  file.path(check_root, "Checkapi_ALL_ISSUE_FINAL.csv"),
  row.names = FALSE
)





