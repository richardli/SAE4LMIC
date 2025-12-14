
library(rmapshaper)
library(qs)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(dplyr)
library(purrr)
library(tibble)
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



infolist <- read.csv(file.path(git_path, "info", "infolist.csv"))
surveys <- read.csv(file.path(git_path,  "info", "surveyslist.csv"))
shapeused<- read.csv(file.path(git_path,  "info", "shapefileList.csv"))


source(here(git_path,  "plots/report_helper.R"))


indicatorlist=ids=infolist$ID

middle_path="Gates-results/Results"
out_middle="Gates-results/estimates"
#model names 
adm_name<-"new_res_adm0-"
ad2_name="new_FH_adm2_fix_nest-"
ad1_name="new_res_adm1-"
ad2_name_dir=  "new_res_adm2-" 
ad2_name_fix=  "new_res_adm2_fix-"


countrylist=unique(surveys$country)




for (country in countrylist[-2]){ 
  plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country)
  # 
  save_tab1(
    country = country,
    adm_name=adm_name,
    ids=infolist$ID,
    middle_path=middle_path,
    out_middle=out_middle
  )

  # save_tab3(
  #   country    = country,
  #   ad2_name_dir = ad2_name_dir,
  #   ad2_name_fix=ad2_name_fix,
  #   ids=infolist$ID,
  #   middle_path=middle_path,
  #   out_middle=out_middle
  # )
  # 
  # save_tab456(
  #   country   = country,
  #   adm_name=adm_name,
  #   ids=infolist$ID,
  #   middle_path=middle_path,
  #   out_middle=out_middle
  # )
  # print( "table done ")

  
  
  
}





