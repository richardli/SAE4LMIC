
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
ad2_name_dir=  "new_res_adm2_fix-"

# plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country)

countrylist= c(
               "Kenya",        
               "Mozambique" ,
                "Rwanda"  ,     
                "Sierra Leone"  ,    
               "Burkina Faso",
               "Ethiopia"  ,
               "Mali",
               "Congo Democratic Republic"        
               )

# indicatorlist=c("CO_MOBB_W_MOB","ML_NETC_C_ITN","CH_DIAT_C_ORT","RH_PCCT_C_DY2","CH_VACC_C_MSL","ED_EDUC_W_SEH")


countrylist=c( "Burkina Faso")
countrylist=unique(surveys$country)

start <- proc.time()


for (country in countrylist[10:13]){ 
print(paste0(country, " now"))
{

savemaps_ridge(country=country,
               ad2_name=ad2_name,
               ad1_name=ad1_name,
               indicatorlist =indicatorlist,
               adm_name = adm_name,
               middle_path=middle_path,
               plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))


print( "maps_ridge done ")

# 
# saveclustermap(
#   country= country,
#   middle_path=middle_path,
#   plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country),
#   indicatorlist =indicatorlist
# )
# 
# 
# 
# print( "clustermap done ")


saveinterval_overlay(country=country,
                     ad2_name=ad2_name,
                     ad1_name=ad1_name,
                     indicatorlist =indicatorlist,
                     middle_path=middle_path,
                     plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))


print( "interval_overlay done ")

savescatter( country=country,
             ad2_name= ad2_name,
             ad2_name_dir= ad2_name_dir,
             middle_path=middle_path,
             plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))

print( "scatter done ")



}


}

end <- proc.time() - start
end





start <- proc.time()

countrylist=unique(surveys$country)
for (country in countrylist){ 
  print(paste0(country, " now"))
  {
  
    
    saveclustermap(
      country= country,
      middle_path=middle_path,
      plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country),
      indicatorlist =indicatorlist
    )
    
    print( "clustermap done ")
  }
  
  
}

end <- proc.time() - start
end

