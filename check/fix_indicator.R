
get_dhs_filenames <- function(country, year) {
  if (country == "Nigeria" & year == 2024) {
    irname <- "NGIR8ADT/NGIR8AFL.DTA"
    krname <- "NGKR8ADT/NGKR8AFL.DTA"
    prname <- "NGPR8ADT/NGPR8AFL.DTA"
    brname <- "NGBR8ADT/NGBR8AFL.DTA"

  } else if (country == "Nigeria" & year == 2018) {
    irname <- "NGIR7BDT/NGIR7BFL.DTA"
    krname <- "NGKR7BDT/NGKR7BFL.DTA"
    prname <- "NGPR7BDT/NGPR7BFL.DTA"
    brname <- "NGBR7BDT/NGBR7BFL.DTA"

  } else if (country == "Burkina Faso" & year == 2021) {
    irname <- "BFIR81DT/BFIR81FL.DTA"
    krname <- "BFKR81DT/BFKR81FL.DTA"
    prname <- "BFPR81DT/BFPR81FL.DTA"
    brname <- "BFBR81DT/BFBR81FL.DTA"

  } else if (country == "Burkina Faso" & year == 2010) {
    irname <- "BFIR62DT/BFIR62FL.DTA"
    krname <- "BFKR62DT/BFKR62FL.DTA"
    prname <- "BFPR62DT/BFPR62FL.DTA"
    brname <- "BFBR62DT/BFBR62FL.DTA"

  } else if (country == "Congo Democratic Republic" & year == 2023) {
    irname <- "CDIR81DT/CDIR81FL.DTA"
    krname <- "CDKR81DT/CDKR81FL.DTA"
    prname <- "CDPR81DT/CDPR81FL.DTA"
    brname <- "CDBR81DT/CDBR81FL.DTA"

  } else if (country == "Congo Democratic Republic" & year == 2013) {
    irname <- "CDIR61DT/CDIR61FL.DTA"
    krname <- "CDKR61DT/CDKR61FL.DTA"
    prname <- "CDPR61DT/CDPR61FL.DTA"
    brname <- "CDBR61DT/CDBR61FL.DTA"

  } else if (country == "Ethiopia" & year == 2019) {
    irname <- "ETIR81DT/ETIR81FL.DTA"
    krname <- "ETKR81DT/ETKR81FL.DTA"
    prname <- "ETPR81DT/ETPR81FL.DTA"
    brname <- "ETBR81DT/ETBR81FL.DTA"

  } else if (country == "Ethiopia" & year == 2016) {
    irname <- "ETIR71DT/ETIR71FL.DTA"
    krname <- "ETKR71DT/ETKR71FL.DTA"
    prname <- "ETPR71DT/ETPR71FL.DTA"
    brname <- "ETBR71DT/ETBR71FL.DTA"

  } else if (country == "Kenya" & year == 2022) {
    irname <- "KEIR8CDT/KEIR8CFL.DTA"
    krname <- "KEKR8CDT/KEKR8CFL.DTA"
    prname <- "KEPR8CDT/KEPR8CFL.DTA"
    brname <- "KEBR8CDT/KEBR8CFL.DTA"

  } else if (country == "Kenya" & year == 2014) {
    irname <- "KEIR72DT/KEIR72FL.DTA"
    krname <- "KEKR72DT/KEKR72FL.DTA"
    prname <- "KEPR72DT/KEPR72FL.DTA"
    brname <- "KEBR72DT/KEBR72FL.DTA"

  } else if (country == "Mozambique" & year == 2022) {
    irname <- "MZIR81DT/MZIR81FL.DTA"
    krname <- "MZKR81DT/MZKR81FL.DTA"
    prname <- "MZPR81DT/MZPR81FL.DTA"
    brname <- "MZBR81DT/MZBR81FL.DTA"

  } else if (country == "Mozambique" & year == 2011) {
    irname <- "MZIR62DT/MZIR62FL.DTA"
    krname <- "MZKR62DT/MZKR62FL.DTA"
    prname <- "MZPR62DT/MZPR62FL.DTA"
    brname <- "MZBR62DT/MZBR62FL.DTA"

  } else if (country == "Rwanda" & year == 2019) {
    irname <- "RWIR81DT/RWIR81FL.DTA"
    krname <- "RWKR81DT/RWKR81FL.DTA"
    prname <- "RWPR81DT/RWPR81FL.DTA"
    brname <- "RWBR81DT/RWBR81FL.DTA"

  } else if (country == "Rwanda" & year == 2014) {
    irname <- "RWIR70DT/RWIR70FL.DTA"
    krname <- "RWKR70DT/RWKR70FL.DTA"
    prname <- "RWPR70DT/RWPR70FL.DTA"
    brname <- "RWBR70DT/RWBR70FL.DTA"

  } else if (country == "Senegal" & year == 2019) {
    irname <- "SNIR8BDT/SNIR8BFL.DTA"
    krname <- "SNKR8BDT/SNKR8BFL.DTA"
    prname <- "SNPR8BDT/SNPR8BFL.DTA"
    brname <- "SNBR8BDT/SNBR8BFL.DTA"

  } else if (country == "Senegal" & year == 2023) {
    irname <- "SNIR8RDT/SNIR8RFL.DTA"
    krname <- "SNKR8RDT/SNKR8RFL.DTA"
    prname <- "SNPR8RDT/SNPR8RFL.DTA"
    brname <- "SNBR8RDT/SNBR8RFL.DTA"

  } else if (country == "South Africa" & year == 2016) {
    irname <- "ZAIR71DT/ZAIR71FL.DTA"
    krname <- "ZAKR71DT/ZAKR71FL.DTA"
    prname <- "ZAPR71DT/ZAPR71FL.DTA"
    brname <- "ZABR71DT/ZABR71FL.DTA"

  } else if (country == "Tanzania" & year == 2022) {
    irname <- "TZIR82DT/TZIR82FL.DTA"
    krname <- "TZKR82DT/TZKR82FL.DTA"
    prname <- "TZPR82DT/TZPR82FL.DTA"
    brname <- "TZBR82DT/TZBR82FL.DTA"

  } else if (country == "Tanzania" & year == 2015) {
    irname <- "TZIR7BDT/TZIR7BFL.DTA"
    krname <- "TZKR7BDT/TZKR7BFL.DTA"
    prname <- "TZPR7BDT/TZPR7BFL.DTA"
    brname <- "TZBR7BDT/TZBR7BFL.DTA"
  }  else if (country == "Sierra Leone" & year == 2013) {
    irname <- "SLIR61DT/SLIR61FL.DTA"
    krname <- "SLKR61DT/SLKR61FL.DTA"
    prname <- "SLPR61DT/SLPR61FL.DTA"
    brname <- "SLBR61DT/SLBR61FL.DTA"
  }  else if (country == "Sierra Leone" & year == 2019) {
    irname <- "SLIR7ADT/SLIR7AFL.DTA"
    krname <- "SLKR7ADT/SLKR7AFL.DTA"
    prname <- "SLPR7ADT/SLPR7AFL.DTA"
    brname <- "SLBR7ADT/SLBR7AFL.DTA"
  }  else if (country == "Mali" & year == 2018) {
    irname <- "MLIR7ADT/MLIR7AFL.DTA"
    krname <- "MLKR7ADT/MLKR7AFL.DTA"
    prname <- "MLPR7ADT/MLPR7AFL.DTA"
    brname <- "MLBR7ADT/MLBR7AFL.DTA"
  }  else if (country == "Mali" & year == 2023) {
    irname <- "MLIR8ADT/MLIR8AFL.DTA"
    krname <- "MLKR8ADT/MLKR8AFL.DTA"
    prname <- "MLPR8ADT/MLPR8AFL.DTA"
    brname <- "MLBR8ADT/MLBR8AFL.DTA"
  }  else if (country == "Zambia" & year == 2018) {
    irname <- "ZMIR71DT/ZMIR71FL.DTA"
    krname <- "ZMKR71DT/ZMKR71FL.DTA"
    prname <- "ZMPR71DT/ZMPR71FL.DTA"
    brname <- "ZMBR71DT/ZMBR71FL.DTA"
  }    else if (country == "Zambia" & year == 2024) {
    irname <- "ZMIR81DT/ZMIR81FL.DTA"
    krname <- "ZMKR81DT/ZMKR81FL.DTA"
    prname <- "ZMPR81DT/ZMPR81FL.DTA"
    brname <- "ZMBR81DT/ZMBR81FL.DTA"
  }  else {
    stop("No match found for this country and year.")
  }

  return(list(irname = irname, krname = krname, prname = prname, brname = brname))
}
get_dhs_for_indicator <- function(ind,country,year,irname,prname,krname,brname,
                                  infolist, source_path) {
  .dhs_cache <- new.env(parent = emptyenv())

  recode <- infolist$recode[
    infolist$ID == ind
  ]

  if (length(recode) == 0 || is.na(recode)) stop("No recode for indicator: ", ind)

  if (!exists(recode, envir = .dhs_cache)) {
    fp <- switch(
      recode,
      IR = paste0(source_path,"/Gates-data/rawDHS/",country,"/",year,"/",irname),
      KR = paste0(source_path,"/Gates-data/rawDHS/",country,"/",year,"/",krname),
      PR = paste0(source_path,"/Gates-data/rawDHS/",country,"/",year,"/",prname),
      BR = paste0(source_path,"/Gates-data/rawDHS/",country,"/",year,"/",brname),

      stop("Unknown recode: ", recode)
    )
    message("Reading DHS (", recode, "): ", fp)
    assign(recode, as.data.frame(haven::read_dta(fp)), envir = .dhs_cache)
  }
  get(recode, envir = .dhs_cache)
}


library(devtools)
# install_github("richardli/surveyPrev",force = T)

# library(surveyPrev)

library(dplyr)
library(kableExtra)
library(labelled)
library(naniar)
library(sjlabelled)

library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(robustbase)
library(raster)
library(devtools)
library(forcats)
library(qs)
library(countrycode)
# install_github("richardli/surveyPrev",force = T)
# install_github("richardli/SUMMER")

# library(surveyPrev)
library(SUMMER)
library(INLA)



# source_path is the folder where this github repository lives in
source_path <- "/Users/qianyu/Dropbox/binary_code/pcg/GATES"
# source_path is the path for this github repository
git_path <- "/Users/qianyu/Dropbox/binary_code/pcg/GATES/SAE4LMIC"

# Read in the list of indicators and surveys from the spreadsheet
infolist <- read.csv(file.path(git_path, "info", "infolist.csv"))
surveys <- read.csv(file.path(git_path,  "info", "surveyslist.csv"))
indicatorlist <- infolist$ID





ind="RH_PCCT_C_DY2"
ind="RH_DELP_C_PRT"
ind="RH_DELA_C_SKP"




country="Kenya"
year=2014


country="Sierra Leone"
year=2013
year=2019


country="Mozambique"
year=2011

ind="ED_EDUC_W_SEH"


country="Rwanda"
year=2019

country="Mali"
year=2018


ind="ML_NETC_C_ITN"
year=2018
country="Zambia"

ind="CH_VACC_C_MSL"
country="Rwanda"
year=2014


CHECK_API=function(country="Burkina Faso",
          year=2021,
          ind="RH_PCCT_C_DY2"
          )
{
  country_short=surveys$country_short[surveys$country==country &surveys$year==year ]


dhsData=NULL
fn<-get_dhs_filenames(country,year)
irname<-fn$irname
krname<-fn$krname
prname<-fn$prname
brname<-fn$brname
dhsData <- get_dhs_for_indicator(ind= ind,
                                 country=country,
                                 year=year,
                                 irname=irname,
                                 prname=prname,
                                 krname=krname,
                                 brname=brname,
                                 infolist=infolist,
                                 source_path=source_path)

data    <- surveyPrev::getDHSindicator(dhsData, indicator = ind)


results_path <- file.path(source_path, "/Gates-results/Results", country, year)

load(file.path(results_path, "basic.Rdata"))

# load( paste0(results_path,indicator,".Rdata"))






if (country=="Rwanda"&year==2014){
  API0_PATH= paste0(source_path,"/Gates-data/API/admin0/",country_short,"_","2015","_DHS_est.rda")
  load(API0_PATH)
}else{
  API0_PATH= paste0(source_path,"/Gates-data/API/admin0/",country_short,"_",year,"_DHS_est.rda")
  load(API0_PATH)
}



vals <- tmp_res$Value[tmp_res$IndicatorId == ind]
vals
res_adm0<-surveyPrev::directEST(
  data = data, cluster.info = cluster.info, admin = 0,
  admin.info = admin.info1, aggregation = FALSE, var.fix = TRUE,
  alt.strata = "v022",
  CI=0.9
)

res_adm0$res.natl$direct.est

c(res_adm0$res.natl$direct.est
,vals)


}


CHECK_API(country="Congo Democratic Republic",
          year=2013,
          ind="RH_PCCT_C_DY2"
)


CHECK_API(country="Rwanda",
          year=2014,
          ind="RH_PCCT_C_DY2"
)




CHECK_API(country="Sierra Leone",
          year=2013,
          ind="RH_PCCT_C_DY2"
)




CHECK_API(country="Sierra Leone",
          year=2013,
          ind="RH_PCCT_C_DY2"
)


CHECK_API(country="Senegal",
          year=2019,
          ind="RH_DELA_C_SKP"
)


CHECK_API(country="Rwanda",
          year=2019,
          ind="RH_DELA_C_SKP"
)

CHECK_API(country="Mali",
          year=2018,
          ind="RH_DELA_C_SKP"
)

CHECK_API(country="Mozambique",
          year=2011,

          ind="RH_DELA_C_SKP"
)



CHECK_API(country="Mali",
          year=2023,
          ind="ED_EDUC_W_SEH"
)




CHECK_API(country="Rwanda",
          year=2014,
          ind="CH_VACC_C_DP3"
)


CHECK_API(country="Rwanda",
          year=2014,
          ind="CH_VACC_C_MSL"
)


CHECK_API(country="Rwanda",
          year=2014,
          ind="CH_DIAT_C_ORT"
)

CHECK_API(country="Nigeria",
          year=2018,
          ind="CH_DIAT_C_ORT"
)
CHECK_API(country="Burkina Faso",
          year=2021,
          ind="CH_DIAT_C_ORT"
)



CHECK_API(country="Mali",
          year=2018,
          ind="CH_DIAT_C_ORT"
)



CHECK_API(country="Mozambique",
          year=2011,
          ind="CH_DIAT_C_ORT"
)



CHECK_API(country="South Africa",
          year=2016,
          ind="CH_DIAT_C_ORT"
)



CHECK_API(country="Senegal",
          year=2023,
          ind="RH_DELA_C_SKP"
)




CHECK_API(country="Zambia",
          year=2024,
          ind="RH_DELA_C_SKP"
)

