



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




source_path<- "/Users/qianyu/Dropbox/binary_code/pcg/GATES/"
infolist <- read.csv(file.path(git_path, "info", "infolist.csv"))
surveys <- read.csv(file.path(git_path,  "info", "surveyslist.csv"))
shapeused<- read.csv(file.path(git_path,  "info", "shapefileList.csv"))


indicatorlist=infolist$ID


save_tab1 <- function(country, 
                      adm_name,
                      ids=infolist$ID,
                      middle_path="Gates-results/Results",
                      out_middle="Gates-results/estimates"
                      ) {
  

  # surveys for this country (sorted by year)
  df_surv <- dplyr::filter(surveys, country == !!country) |> dplyr::arrange(year)
  if (nrow(df_surv) == 0) stop("No surveys for country = ", country)
  years <- df_surv$year
  
  # all (indicator, year) pairs
  grid <- tidyr::crossing(indicator = ids, year = years)
  
  # helper to read a single value
  get_one <- function(ind, yr) {
    results_path <- file.path(source_path, middle_path, country, yr)
    qfile <- file.path(results_path, paste0(adm_name, ind, ".qs"))
    if (!file.exists(qfile)) return(NA_real_)
    val <- tryCatch(qs::qread(qfile)$res.natl$direct.est, error = function(e) NA_real_)
    as.numeric(val)
  }
  
  # read everything once
  grid$value <- purrr::map2_dbl(grid$indicator, grid$year, get_one)
  
  # join description (fallback to ID if missing)
  label_df <- dplyr::select(infolist, ID, Description)
  tab <- grid |>
    dplyr::left_join(label_df, by = c("indicator" = "ID")) |>
    dplyr::mutate(
      Indicator = dplyr::if_else(is.na(Description) | Description == "", indicator, Description),
      value = ifelse(is.na(value), NA, round(100 * value, 1))  # percent with 1 decimal
    ) |>
    dplyr::select(Indicator, year, value) |>
    tidyr::pivot_wider(
      names_from = year, values_from = value,
      names_prefix = "DHS "
    ) |>
    dplyr::arrange(Indicator)
  print(tab)
  # write CSV
  out_dir <- file.path(source_path, out_middle, country)
  out_file <- file.path(out_dir, "National.csv")
  readr::write_csv(tab, out_file)
  message("Saved: ", out_file)
  
  invisible(tab)
}





save_tab1(
  country     = "Nigeria",
  adm_name<-"new_res_adm0-",
  ids=infolist$ID,
  middle_path="Gates-results/Results",
  out_middle="Gates-results/estimates"
)




for (ctry in countryList) {
  message("Processing: ", ctry)
  
  save_tab1 <- save_two_levels_estiamtes_csv(
    country     = ctry,
    adm_name<-"new_res_adm0-"
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



save_tab3 <- function(country,
                      ad2_name = "new_res_adm2_fix-",
                      ids=infolist$ID,
                      middle_path="Gates-results/Results",
                      out_middle="Gates-results/estimates"
                      ) {
  
  indicators=infolist$ID
  # years to use
  yrs_all <- sort(surveys$year[surveys$country == country])
  if (length(yrs_all) == 0) {
    message("No surveys for ", country)
    return(invisible(NULL))
  }
  
  if (identical(country, "South Africa")) {
    years <- 2016L
    labels <- c("current")
  } else {
    # choose "current" = latest year; "previous" = latest-1 (if exists)
    if (length(yrs_all) >= 2) {
      years <- tail(yrs_all, 2)
      labels <- c("previous", "current")
    } else {
      years <- tail(yrs_all, 1)
      labels <- c("current")
    }
  }
  
  # helper: read qfile and compute (no_ad2, fixed)
  get_triplet <- function(ind, yr) {
    results_path  <- file.path(source_path, middle_path, country, yr)
    qfile_adm2   <- file.path(results_path, paste0(ad2_name, ind, ".qs"))
    if (!file.exists(qfile_adm2)) return(c(NA_real_, NA_real_))
    x <- tryCatch(qs::qread(qfile_adm2), error = function(e) NULL)
    if (is.null(x)) return(c(NA_real_, NA_real_))

    no_ad2 <- tryCatch(
      nrow(x$data.info) - nrow(x$res.admin2),
      error = function(e) NA_real_
    )
    fixed <- tryCatch(length(x$fixed_areas), error = function(e) NA_real_)
    c(no_ad2, fixed)
  }
  
  # build rows for each indicator
  rows <- lapply(indicators, function(ind) {
    vals <- unlist(lapply(years, function(yr) get_triplet(ind, yr)))
    data.frame(ID = ind, t(vals), check.names = FALSE)
  })
  tab3 <- do.call(rbind, rows)
  
  # name the columns
  year_cols <- unlist(lapply(labels, function(lbl) paste0(lbl, c("_no_ad2", "_fixed"))))
  names(tab3) <- c("ID", year_cols)
  
  # write CSV
  out_dir <- file.path(source_path,out_middle, country)
  out_file <- file.path(out_dir, "National3.csv")
  write.csv(tab3, out_file, row.names = FALSE)
  message("Saved: ", out_file)
  
  invisible(tab3)
}


save_tab3(
  country    = "Nigeria",
  ad2_name = "new_res_adm2_fix-",
  ids=infolist$ID,
  middle_path="Gates-results/Results",
  out_middle="Gates-results/estimates"
)



countryList <- unique(surveys$country)
for (ctry in countryList) {
  message("Processing: ", ctry)
  try(save_tab3(ctry), silent = TRUE)
}





save_tab456 <- function(country, 
                        adm_name,
                        ids=infolist$ID,
                        middle_path="Gates-results/Results",
                        out_middle="Gates-results/estimates"
) {
  
  
  # surveys for this country (sorted by year)
  df_surv <- dplyr::filter(surveys, country == !!country) |> dplyr::arrange(year)
  if (nrow(df_surv) == 0) stop("No surveys for country = ", country)
  years <- df_surv$year
  
  # all (indicator, year) pairs
  grid <- tidyr::crossing(indicator = ids, year = years)
  
  # helper to read a single value
  get_one <- function(ind, yr) {
    results_path <- file.path(source_path, middle_path, country, yr)
    qfile <- file.path(results_path, paste0(adm_name, ind, ".qs"))
    if (!file.exists(qfile)) return(NA_real_)
    dt <- tryCatch(qs::qread(qfile)$data.info, error = function(e) NA_real_)
    dt
  }
  
  # read everything once
  # grid$value <- purrr::map2(grid$indicator, grid$year, get_one)
  
  
  out_list <- purrr::map2(grid$indicator, grid$year, get_one)
  
  # Find the column names from the first non-NA tibble
  template <- out_list[[which(purrr::map_lgl(out_list, ~ is.data.frame(.x)))[1]]]
  col_names <- names(template)
  
  # Convert everything into 1-row tibbles (NA rows when needed)
  out_list_fixed <- purrr::map(out_list, function(x) {
    if (is.data.frame(x)) {
      x
    } else {
      # return a 1-row tibble of NAs with the correct columns
      tibble(!!!setNames(rep(list(NA_real_), length(col_names)), col_names))
    }
  })
  # Combine results row-wise with original grid
  grid_expanded <- dplyr::bind_cols(
    grid,
    dplyr::bind_rows(out_list_fixed)
  )
  label_df <- dplyr::select(infolist, ID, Description)
  
  base_tab <- grid_expanded |>
    dplyr::left_join(label_df, by = c("indicator" = "ID")) |>
    dplyr::mutate(
      Indicator = dplyr::if_else(is.na(Description) | Description == "", indicator, Description)
    )
  
  
  tab_samples <- base_tab |>
    dplyr::select(Indicator, year, n_samples) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = n_samples,
      names_prefix = "DHS "
    ) |>
    dplyr::arrange(Indicator)
  
  tab_events <- base_tab |>
    dplyr::select(Indicator, year, n_events) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = n_events,
      names_prefix = "DHS "
    ) |>
    dplyr::arrange(Indicator)
  
  tab_clusters <- base_tab |>
    dplyr::select(Indicator, year, n_clusters) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = n_clusters,
      names_prefix = "DHS "
    ) |>
    dplyr::arrange(Indicator)
  
  
  
  out_dir <- file.path(source_path, out_middle, country)
  out_file <- file.path(out_dir, "National_clusters.csv")
  readr::write_csv(tab_clusters, out_file)
  
  
  out_dir <- file.path(source_path, out_middle, country)
  out_file <- file.path(out_dir, "National_events.csv")
  readr::write_csv(tab_events, out_file)
  
  out_dir <- file.path(source_path, out_middle, country)
  out_file <- file.path(out_dir, "National_samples.csv")
  readr::write_csv(tab_samples, out_file)
  
  
  
  message("Saved: ", out_file)
  
  invisible(tab_clusters)
  invisible(tab_events)
  invisible(tab_samples)
}

save_tab456(
  country = "Burkina Faso",
  adm_name<-"new_res_adm0-",
  ids=infolist$ID,
  middle_path="Gates-results/Results",
  out_middle="Gates-results/estimates"
)
