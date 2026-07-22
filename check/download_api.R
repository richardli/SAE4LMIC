


library(rdhs)
library(dplyr)
library(here)

# git_path = this repo (SAE4LMIC); source_path = its parent (GATES root)
git_path    <- here::here()
source_path <- dirname(git_path)

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

# main_dir="/Users/qianyu/Dropbox/DHS-indicators"
# 
# output_dir=file.path(main_dir,"Step_3_Data","all_survey_est_sub")
# # dir.create(output_dir, showWarnings = FALSE)
# 
# #SurveyId_list[147]
# # Loop through each SurveyId
# #ML2023DHS
# #EH
# for (survey_id in "ET2024DHS") {
#   print(paste("Fetching data for Survey:", survey_id))
#   
#   # Initialize variables for pagination
#   page_num <- 1
#   has_more_data <- TRUE
#   all_pages_data <- list()
#   
#   while (has_more_data) {
#     print(paste("Fetching page", page_num, "for", survey_id))
#     
#     # Construct API URL with pagination
#     api_url <- paste0("https://api.dhsprogram.com/rest/dhs/v8/data?breakdown=subnational&surveyIds=",
#                       survey_id, "&lang=en&perpage=5000&page=", page_num, "&f=json")
#     
#     # Fetch the JSON response
#     survey_data <- tryCatch({
#       fromJSON(api_url)$Data  # Extract the "Data" part of JSON
#     }, error = function(e) {
#       print(paste("Error fetching page", page_num, "for", survey_id))
#       return(NULL)
#     })
#     
#     # Check if data is returned
#     if (!is.null(survey_data) && length(survey_data) > 0) {
#       
#       df <- as.data.frame(survey_data)
#       
#       
#       # Convert all columns to character to prevent type mismatches
#       # df <- df %>% mutate(across(everything(), as.character))
# 
#       # Convert `LevelRank` to character to ensure consistency
#       if ("LevelRank" %in% names(df)) {
#         df$LevelRank <- as.integer(df$LevelRank)
#       }
#       
#       all_pages_data[[page_num]] <- df  # Store the page data
#       page_num <- page_num + 1  # Move to the next page
#     } else {
#       has_more_data <- FALSE  # Stop loop if no more data
#     }
#   }
#   
#   # Combine all pages into a single dataframe
#   if (length(all_pages_data) > 0) {
#     final_survey_data <- bind_rows(all_pages_data)
#     
#     # Save as a .qs file (compressed and fast to read/write)
#     file_path <- file.path(output_dir, paste0(survey_id, ".qs"))
#     qsave(final_survey_data, file_path)
#     
#     print(paste("Saved:", file_path))
#   } else {
#     print(paste("No data found for", survey_id))
#   }
# }
# 
# 



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

main_dir <- path.expand("~/Dropbox/DHS-indicators")   # external DHS archive (not under source_path)
output_dir=file.path(main_dir,"Step_3_Data","all_survey_est")
output_dir <-  file.path(source_path, "Gates-data/API/admin0")


for (i in which(dhs_survey_list$SurveyId == "ET2024DHS")){
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




# ============================================================================
#  alternative of admin 1 loop
# Fixes two problems:
#  (1) "Error fetching page 1" — fromJSON(url) fails in R even though the URL
#      works in the browser/shell. This fetches with curl (libcurl, the same
#      path that works from the shell) and retries, instead of fromJSON(url).
#  (2) multi-page type mismatch — ET2024DHS is ~40k rows / 8 pages, so bind_rows
#      hits character-vs-numeric column clashes. Each page is coerced to
#      character before binding.
# ============================================================================

library(jsonlite)
library(dplyr)
library(qs)

output_dir <- file.path("/Users/qianyu/Dropbox/DHS-indicators",
                         "Step_3_Data", "all_survey_est_sub")

output_dir <-  file.path(source_path, "Gates-data/API/admin1")


# Robust fetch: curl (libcurl) first, then base url() fallback; retry a few times.
fetch_dhs_json <- function(u, tries = 3, pause = 2) {
  for (k in seq_len(tries)) {
    out <- tryCatch({
      if (requireNamespace("curl", quietly = TRUE)) {
        r <- curl::curl_fetch_memory(u)
        if (r$status_code != 200L) stop("HTTP ", r$status_code)
        jsonlite::fromJSON(rawToChar(r$content))
      } else {
        jsonlite::fromJSON(paste(readLines(u, warn = FALSE), collapse = ""))
      }
    }, error = function(e) {
      message("  fetch attempt ", k, " failed: ", conditionMessage(e)); NULL
    })
    if (!is.null(out)) return(out)
    Sys.sleep(pause)
  }
  NULL
}

download_sub <- function(survey_id, perpage = 5000,output_dir) {
  message("Fetching subnational data for ", survey_id)

  build_url <- function(page) paste0(
    "https://api.dhsprogram.com/rest/dhs/v8/data?breakdown=subnational&surveyIds=",
    survey_id, "&lang=en&perpage=", perpage, "&page=", page, "&f=json")

  to_char <- function(x) {
    df <- as.data.frame(x)
    df <- dplyr::mutate(df, dplyr::across(dplyr::everything(), as.character))
    if ("LevelRank" %in% names(df)) df$LevelRank <- as.integer(df$LevelRank)
    df
  }

  first <- fetch_dhs_json(build_url(1))
  if (is.null(first) || length(first$Data) == 0) {
    message("  no data / failed for ", survey_id); return(invisible(NULL))
  }
  total_pages <- suppressWarnings(as.integer(first$TotalPages))
  if (is.na(total_pages) || total_pages < 1L) total_pages <- 1L
  message("  ", first$RecordCount, " records over ", total_pages, " page(s)")

  pages <- vector("list", total_pages)
  pages[[1]] <- to_char(first$Data)
  for (p in seq_len(total_pages)[-1]) {
    message("  page ", p, "/", total_pages)
    resp <- fetch_dhs_json(build_url(p))
    if (is.null(resp) || length(resp$Data) == 0) break
    pages[[p]] <- to_char(resp$Data)
  }

  final_survey_data <- dplyr::bind_rows(pages)
  # if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  file_path <- file.path(output_dir, paste0(survey_id, ".qs"))
  qs::qsave(final_survey_data, file_path)
  message("  saved ", nrow(final_survey_data), " rows -> ", file_path)
  invisible(final_survey_data)
}

# Run it:
# 

download_sub("ET2024DHS",output_dir= output_dir)



