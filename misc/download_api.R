


library(rdhs)
library(dplyr)

set_rdhs_config(email = "qdong14@ucsc.edu",
                project = "Small Area Estimaiton Using DHS Data")#update_rdhs_config



library(jsonlite)
library(dplyr)
library(qs)


# # API call to get all DHS surveys
# url <- "https://api.dhsprogram.com/rest/dhs/v8/surveys?surveyYearStart=2000&surveyType=DHS&f=json"
# survey_data <- fromJSON(url)$Data
# 
# # Convert to a dataframe
# survey_df <- as.data.frame(survey_data)
# 
# SurveyId_list <- unique(survey_df$SurveyId)
# 
# 
# Directory to store .qs files

main_dir="/Users/qianyu/Dropbox/DHS-indicators"

output_dir=file.path(main_dir,"Step_3_Data","all_survey_est_sub")
# dir.create(output_dir, showWarnings = FALSE)


#SurveyId_list[147]
# Loop through each SurveyId
#ML2023DHS


for (survey_id in "ZM2024DHS") {
  print(paste("Fetching data for Survey:", survey_id))
  
  # Initialize variables for pagination
  page_num <- 1
  has_more_data <- TRUE
  all_pages_data <- list()
  
  while (has_more_data) {
    print(paste("Fetching page", page_num, "for", survey_id))
    
    # Construct API URL with pagination
    api_url <- paste0("https://api.dhsprogram.com/rest/dhs/v8/data?breakdown=subnational&surveyIds=",
                      survey_id, "&lang=en&perpage=5000&page=", page_num, "&f=json")
    
    # Fetch the JSON response
    survey_data <- tryCatch({
      fromJSON(api_url)$Data  # Extract the "Data" part of JSON
    }, error = function(e) {
      print(paste("Error fetching page", page_num, "for", survey_id))
      return(NULL)
    })
    
    # Check if data is returned
    if (!is.null(survey_data) && length(survey_data) > 0) {
      
      df <- as.data.frame(survey_data)
      
      
      # Convert all columns to character to prevent type mismatches
      # df <- df %>% mutate(across(everything(), as.character))
      
      # Convert `LevelRank` to character to ensure consistency
      if ("LevelRank" %in% names(df)) {
        df$LevelRank <- as.integer(df$LevelRank)
      }
      
      all_pages_data[[page_num]] <- df  # Store the page data
      page_num <- page_num + 1  # Move to the next page
    } else {
      has_more_data <- FALSE  # Stop loop if no more data
    }
  }
  
  # Combine all pages into a single dataframe
  if (length(all_pages_data) > 0) {
    final_survey_data <- bind_rows(all_pages_data)
    
    # Save as a .qs file (compressed and fast to read/write)
    file_path <- file.path(output_dir, paste0(survey_id, ".qs"))
    qsave(final_survey_data, file_path)
    
    print(paste("Saved:", file_path))
  } else {
    print(paste("No data found for", survey_id))
  }
}















###############################################################
### download all possible
###############################################################


#update TZ2022DHS
# "CD2023DHS",
# "MZ2022DHS",
# "SN2023DHS",
# "ML2023DHS",
## for RH_DELP_C_PNN(non-NGO) ,RH_DELP_C_PNG(NGO

dhs_survey_list <- rdhs::dhs_surveys()
dhs_survey_list <- dhs_survey_list[dhs_survey_list$SurveyYear>2000 & dhs_survey_list$SurveyType== 'DHS',]

# dhs_survey_list[dhs_survey_list$SurveyId==SurveyId_list[147],]

main_dir="/Users/qianyu/Dropbox/DHS-indicators"

output_dir=file.path(main_dir,"Step_3_Data","all_survey_est")


for (i in which(dhs_survey_list$SurveyId == "ZM2024DHS")){
  print(i)
  tmp_cty_code <- dhs_survey_list$DHS_CountryCode[i]
  tmp_svy_year <- dhs_survey_list$SurveyYear[i]
  print(paste0(dhs_survey_list$DHS_CountryCode[i],dhs_survey_list$SurveyYear[i]))
  
  
  # cty_svy_res_file <- paste0('E:/Dropbox/YunhanJon/DHS-indicators/Step_3_Data/all_survey_est/',
  #                       tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda')
  
  cty_svy_res_file <- paste0(output_dir,
                             tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda')
  
  if(!file.exists(cty_svy_res_file)){
    
    
    tmp_call <- paste0("https://api.dhsprogram.com/rest/dhs/data?countryIds=",tmp_cty_code,"&surveyYear=",tmp_svy_year,"&perpage=10000&f=csv")
    tmp_res <- read.csv(tmp_call)
    
    
    print('done')
    # save(tmp_res,file=paste0('E:/Dropbox/YunhanJon/DHS-indicators/Step_3_Data/all_survey_est/',
    #                          tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda'))
    
    save(tmp_res,file=paste0(output_dir,"/",
                             tmp_cty_code,'_',tmp_svy_year,'_DHS_est.rda'))
  }
  
}



