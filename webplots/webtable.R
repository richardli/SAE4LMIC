


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
# source_path is the folder where this github repository lives in 
source_path <- dirname(here::here())
# source_path is the path for this github repository 
git_path <- here::here()




infolist <- read.csv(file.path(git_path, "info", "infolist.csv"))
surveys <- read.csv(file.path(git_path,  "info", "surveyslist.csv"))

indicatorlist=infolist$ID


save_two_levels_estiamtes_csv <- function(ad1_name, ad2_name, country ) {

  idx <- which(surveys$country %in% country)
  if (length(idx) == 0) stop("No surveys for country = ", country)
  res_df <- tibble()   # accumulator
  
  for (i in idx) {
    country      <- surveys$country[i]
    year         <- surveys$year[i]
    year_key     <- as.character(year)
    survey_name  <- surveys$survey_name[i]
    iso3         <- surveys$iso3[i]
    results_path <- file.path(source_path, "Gates-results", "Results", country, year)
    
    for (indicator in infolist$ID) {
      qfile_adm1 <- file.path(results_path, paste0(ad1_name, indicator, ".qs"))
      qfile_adm2 <- file.path(results_path, paste0(ad2_name, indicator, ".qs"))
      
      if (!file.exists(qfile_adm1)) { message("Missing ADM1 ", indicator, " for ", country, " ", year); next }
      if (!file.exists(qfile_adm2)) { message("Missing ADM2 ", indicator, " for ", country, " ", year); next }
      
      loaded_adm1 <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("read fail: ", qfile_adm1, " :: ", e$message); return(NULL) })
      loaded_adm2 <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("read fail: ", qfile_adm2, " :: ", e$message); return(NULL) })
      if (is.null(loaded_adm1) || is.null(loaded_adm2)) next
      
      ad1 <- loaded_adm1$res.admin1 %>%
        mutate(
          Region_Name = admin1.name,
          Admin = 1,
          Mean   = signif(direct.est,   4),
          Median = signif(direct.est,   4),
          Lower_CI = signif(direct.lower, 4),
          Upper_CI = signif(direct.upper, 4),
          Width_90_CI = signif(direct.upper - direct.lower, 4),
          Coefficient_of_Variation = signif(cv2, 4)
        ) %>%
        transmute(
          ISO3 = iso3, Country = country, Year = year, Survey = survey_name,
          Indicator = indicator, Admin, Region_Name,
          Mean, Median, Lower_CI, Upper_CI, Width_90_CI, Coefficient_of_Variation
        )
      
      ad2 <- loaded_adm2$res.admin2 %>%
        mutate(
          Region_Name = admin2.name.full,
          Admin = 2,
          Mean   = signif(mean,   4),
          Median = signif(median, 4),
          Lower_CI = signif(lower, 4),
          Upper_CI = signif(upper, 4),
          Width_90_CI = signif(upper - lower, 4),
          Coefficient_of_Variation = signif(cv2, 4)
        ) %>%
        transmute(
          ISO3 = iso3, Country = country, Year = year, Survey = survey_name,
          Indicator = indicator, Admin, Region_Name,
          Mean, Median, Lower_CI, Upper_CI, Width_90_CI, Coefficient_of_Variation
        )
      
      res_df <- bind_rows(res_df, ad1, ad2)
    }
  }
  res_df
}




countrList=unique(surveys$country)

res_tbl <- save_two_levels_estiamtes_csv(
  ad1_name    = "new_res_adm1-",
  ad2_name    = "new_FH_adm2_fix_nest-",
  country     = "Nigeria"
)

write.csv(res_tbl,
          file = file.path(source_path, "Gates-results/estimates",country,"combined_results.csv"),
          row.names = FALSE)

countrList="Zambia" 

for (ctry in countrList) {
  message("Processing: ", ctry)
  
  res_tbl <- save_two_levels_estiamtes_csv(
    ad1_name    = "new_res_adm1-",
    ad2_name    = "new_FH_adm2_fix_nest-",
    country     = ctry
  )
  
  # If function returns empty (no data), skip writing file
  if (nrow(res_tbl) == 0) {
    message("No results for ", ctry, " — skipping CSV.")
    next
  }
  
  # Create an output directory per country
  out_dir <- file.path(source_path, "Gates-results", "estimates", ctry)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Write CSV
  out_file <- file.path(out_dir, "combined_results.csv")
  write.csv(res_tbl, out_file, row.names = FALSE)
  
  message("Saved: ", out_file)
}



# ===========================================================================
# NEW (added at end; old code above untouched)
# save_two_levels_estiamtes_csv_yearpair()
#   Combined Admin-1 + Admin-2 estimates for a CHOSEN old/new survey-year pair,
#   written to the per-year-pair folder:
#       Gates-results/estimates/<country>/<yr1>-<yr2>/combined_results.csv
#   Prefixes are resolved per survey folder (plain vs new_) via misc/resolve_qs.R,
#   so a mixed old/new pair reads correctly. yr1/yr2 default to min/max.
# ===========================================================================
save_two_levels_estiamtes_csv_yearpair <- function(country,
                                                   yr1 = NULL, yr2 = NULL,
                                                   ad1_name = "res_adm1-",
                                                   ad2_name = "FH_adm2_fix_nest-",
                                                   write    = TRUE) {

  if (!exists("resolve_qs")) source(file.path(git_path, "misc", "resolve_qs.R"))

  yrs_all <- sort(unique(surveys$year[surveys$country == country]))
  if (length(yrs_all) == 0) stop("No surveys for country = ", country)
  if (is.null(yr1)) yr1 <- min(yrs_all)
  if (is.null(yr2)) yr2 <- max(yrs_all)
  if (!all(c(yr1, yr2) %in% yrs_all))
    stop("yr1/yr2 must be survey years for ", country,
         " (have: ", paste(yrs_all, collapse = ", "), ")")
  if (yr1 == yr2) stop("yr1 and yr2 must differ")
  if (yr1 > yr2) { tmp <- yr1; yr1 <- yr2; yr2 <- tmp }   # old < new

  idx    <- which(surveys$country == country & surveys$year %in% c(yr1, yr2))
  res_df <- tibble()

  for (i in idx) {
    year         <- surveys$year[i]
    survey_name  <- surveys$survey_name[i]
    iso3         <- surveys$iso3[i]
    results_path <- file.path(source_path, "Gates-results", "Results", country, year)

    for (indicator in infolist$ID) {
      qfile_adm1 <- resolve_qs(results_path, ad1_name, indicator)   # plain or new_
      qfile_adm2 <- resolve_qs(results_path, ad2_name, indicator)

      if (!file.exists(qfile_adm1)) { message("Missing ADM1 ", indicator, " for ", country, " ", year); next }
      if (!file.exists(qfile_adm2)) { message("Missing ADM2 ", indicator, " for ", country, " ", year); next }

      loaded_adm1 <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("read fail: ", qfile_adm1, " :: ", e$message); return(NULL) })
      loaded_adm2 <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("read fail: ", qfile_adm2, " :: ", e$message); return(NULL) })
      if (is.null(loaded_adm1) || is.null(loaded_adm2)) next

      ad1 <- loaded_adm1$res.admin1 %>%
        mutate(
          Region_Name = admin1.name,
          Admin = 1,
          Mean   = signif(direct.est,   4),
          Median = signif(direct.est,   4),
          Lower_CI = signif(direct.lower, 4),
          Upper_CI = signif(direct.upper, 4),
          Width_90_CI = signif(direct.upper - direct.lower, 4),
          Coefficient_of_Variation = signif(cv2, 4)
        ) %>%
        transmute(
          ISO3 = iso3, Country = country, Year = year, Survey = survey_name,
          Indicator = indicator, Admin, Region_Name,
          Mean, Median, Lower_CI, Upper_CI, Width_90_CI, Coefficient_of_Variation
        )

      ad2 <- loaded_adm2$res.admin2 %>%
        mutate(
          Region_Name = admin2.name.full,
          Admin = 2,
          Mean   = signif(mean,   4),
          Median = signif(median, 4),
          Lower_CI = signif(lower, 4),
          Upper_CI = signif(upper, 4),
          Width_90_CI = signif(upper - lower, 4),
          Coefficient_of_Variation = signif(cv2, 4)
        ) %>%
        transmute(
          ISO3 = iso3, Country = country, Year = year, Survey = survey_name,
          Indicator = indicator, Admin, Region_Name,
          Mean, Median, Lower_CI, Upper_CI, Width_90_CI, Coefficient_of_Variation
        )

      res_df <- bind_rows(res_df, ad1, ad2)
    }
  }

  if (isTRUE(write) && nrow(res_df) > 0) {
    out_dir  <- file.path(source_path, "Gates-results", "estimates",
                          country, paste0(yr1, "-", yr2))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    out_file <- file.path(out_dir, "combined_results.csv")
    write.csv(res_df, out_file, row.names = FALSE)
    message("Saved: ", out_file)
  } else if (nrow(res_df) == 0) {
    message("No results for ", country, " (", yr1, "-", yr2, ") — nothing written.")
  }

  invisible(res_df)
}

# Example:
# save_two_levels_estiamtes_csv_yearpair("Ethiopia", 2019, 2024)


