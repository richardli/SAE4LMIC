

#
#
# This file sets up the dependency and common objects for all models in this folder
#
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

# Read in the list of indicators and surveys from the spreadsheet
infolist <- read.csv(here(git_path, "info", "infolist.csv"))
surveys <- read.csv(here(git_path,  "info", "surveyslist.csv"))
ids=indicatorlist <- infolist$ID



country="Nigeira"


#direc
for (i in 1:2 ) {
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  year_key      <- as.character(year)           # use a name, not positional index
  version       <- surveys$version[i]
  survey_name   <- surveys$survey_name[i]
  
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  # dir.create(file.path(source_path, "plots", country, version, year),
  #            recursive = TRUE, showWarnings = FALSE)
  #
  # If needed:
  # if (file.exists(file.path(results_path, "basic.Rdata")))
  # load(file.path(results_path, "basic.Rdata"))
  
  for (indicator in ids) {
    new1 <- file.path(results_path, paste0("new_res_adm2_fix-",indicator, ".qs"))
    old1 <- file.path(results_path, paste0("res_adm2_fix-",indicator, ".qs"))
    no1 <- file.path(results_path, paste0("new_res_adm2-",indicator, ".qs"))
    
    
    if (!file.exists(qfile)) {
      message("Missing ", indicator, " for ", country, " ", year)
      next
    }
    
    new <- qread(new1)
    old <- qread(old1)
    no <- qread(no1)
    

    s5 <- scatterPlot(
      res1=new$res.admin2,
      res2=old$res.admin2,
      value1="direct.est",
      value2="direct.est",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," Prevalence"),
      label1="new",
      label2="old")
    s6 <- scatterPlot(
      res1=no$res.admin2,
      res2=old$res.admin2,
      value1="direct.est",
      value2="direct.est",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," Prevalence"),
      label1="no",
      label2="old")
    
    s7 <- scatterPlot(
      res1=no$res.admin2,
      res2=new$res.admin2,
      value1="direct.est",
      value2="direct.est",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," Prevalence"),
      label1="no",
      label2="new")
    
    
    
    ss5 <- scatterPlot(
      res1=new$res.admin2,
      res2=old$res.admin2,
      value1="direct.se",
      value2="direct.se",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," direct.se"),
      label1="new",
      label2="old")
    ss6 <- scatterPlot(
      res1=no$res.admin2,
      res2=old$res.admin2,
      value1="direct.se",
      value2="direct.se",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," direct.se"),
      label1="no",
      label2="old")
    
    ss7 <- scatterPlot(
      res1=no$res.admin2,
      res2=new$res.admin2,
      value1="direct.se",
      value2="direct.se",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," direct.se"),
      label1="no",
      label2="new")
    
    
    # ss5 <- scatterPlot(
    #   res1=new$res.admin2,
    #   res2=old$res.admin2,
    #   value1="cv2",
    #   value2="cv2",
    #   by.res1="admin2.name.full",
    #   by.res2="admin2.name.full",
    #   title= paste0(indicator,year," cv2"),
    #   label1="new",
    #   label2="old")
    # 
    # ss6 <- scatterPlot(
    #   res1=no$res.admin2,
    #   res2=old$res.admin2,
    #   value1="cv2",
    #   value2="cv2",
    #   by.res1="admin2.name.full",
    #   by.res2="admin2.name.full",
    #   title= paste0(indicator,year," cv2"),
    #   label1="no",
    #   label2="old")
    # 
    # ss7 <- scatterPlot(
    #   res1=no$res.admin2,
    #   res2=new$res.admin2,
    #   value1="cv2",
    #   value2="cv2",
    #   by.res1="admin2.name.full",
    #   by.res2="admin2.name.full",
    #   title= paste0(indicator,year,"cv2"),
    #   label1="no",
    #   label2="new")
    
    checkplot<-(s5+s6+s7)/(ss5+ss6+ss7)
  
    ggsave(checkplot ,
           filename = file.path(source_path, "Gates-results/check",country,year, paste0(indicator,"-check-var.fix.png")),
           
           width = 15, height = 10,
           dpi = 200)
    
    
    
    
  }
}




# fh
for (i in 1:2 ) {
  country       <- surveys$country[i]
  year          <- surveys$year[i]
  year_key      <- as.character(year)           # use a name, not positional index
  version       <- surveys$version[i]
  survey_name   <- surveys$survey_name[i]
  
  results_path <- file.path(source_path, "Gates-results/Results", country, year)
  # dir.create(file.path(source_path, "plots", country, version, year),
  #            recursive = TRUE, showWarnings = FALSE)
  #
  # If needed:
  # if (file.exists(file.path(results_path, "basic.Rdata")))
  # load(file.path(results_path, "basic.Rdata"))
  
  for (indicator in ids) {
    new1 <- file.path(results_path, paste0("new_FH_adm2_fix_nest-",indicator, ".qs"))
    old1 <- file.path(results_path, paste0("FH_adm2_fix_nest-",indicator, ".qs"))
    no1 <- file.path(results_path, paste0("new_res_adm2-",indicator, ".qs"))
    
    
    if (!file.exists(qfile)) {
      message("Missing ", indicator, " for ", country, " ", year)
      next
    }
    
    new <- qread(new1)
    old <- qread(old1)
    no <- qread(no1)
    
    
    s5 <- scatterPlot(
      res1=new$res.admin2,
      res2=old$res.admin2,
      value1="mean",
      value2="mean",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," Prevalence"),
      label1="new",
      label2="old")
    s6 <- scatterPlot(
      res1=no$res.admin2,
      res2=old$res.admin2,
      value1="direct.est",
      value2="mean",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," Prevalence"),
      label1="no",
      label2="old")
    
    s7 <- scatterPlot(
      res1=no$res.admin2,
      res2=new$res.admin2,
      value1="direct.est",
      value2="mean",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," Prevalence"),
      label1="no",
      label2="new")
    
    
    
    ss5 <- scatterPlot(
      res1=new$res.admin2,
      res2=old$res.admin2,
      value1="cv2",
      value2="cv2",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," cv2"),
      label1="new",
      label2="old")
    ss6 <- scatterPlot(
      res1=no$res.admin2,
      res2=old$res.admin2,
      value1="cv2",
      value2="cv2",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," cv2"),
      label1="no",
      label2="old")
    
    ss7 <- scatterPlot(
      res1=no$res.admin2,
      res2=new$res.admin2,
      value1="cv2",
      value2="cv2",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(indicator,year," cv2"),
      label1="no",
      label2="new")
    
    
    
    checkplot<-(s5+s6+s7)/(ss5+ss6+ss7)
    
    ggsave(checkplot ,
           filename = file.path(source_path, "Gates-results/check",country,year, paste0(indicator,"-check-var-fh.fix.png")),
           
           width = 15, height = 10,
           dpi = 200)
    
    
    
    
  }
}

