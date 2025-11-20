





check_model_results <- function(
    country_to_run,
    indicatorlist,
    source_path) {
  
  print("Checking results")
  get_class_or_status <- function(path, extractor = NULL) {
    if (!file.exists(path)) return("no file")
    
    obj <- tryCatch(qs::qread(path),
                    error = function(e) e)
    if (inherits(obj, "error")) return("no file")
    
    # detect your sentinel value from tryCatch
    if (is.character(obj) && length(obj) == 1L && identical(obj, "failed")) {
      return("failed")
    }
    
    if (!is.null(extractor)) {
      obj <- tryCatch(extractor(obj), error = function(e) e)
      if (inherits(obj, "error")) return("extract_failed")
    }
    
    paste(class(obj), collapse = ",")
  }
  
  
  res_list <- list()
  
  for (i in which(surveys$country %in% country_to_run)) {
    country <- surveys$country[i]
    year    <- surveys$year[i]
    
    results_path <- file.path(source_path, "Gates-results", "Results", country, year)
    
    for (indicator in indicatorlist) {
      row <- list(
        country   = country,
        year      = year,
        indicator = indicator
      )
      
      ## ---------- direct model outputs ----------
      row$res_adm0_class      <- get_class_or_status(
        file.path(results_path, paste0("new_res_adm0-", indicator, ".qs"))
      )
      row$res_adm1_class      <- get_class_or_status(
        file.path(results_path, paste0("new_res_adm1-", indicator, ".qs"))
      )
      row$res_adm2_fix_class  <- get_class_or_status(
        file.path(results_path, paste0("new_res_adm2_fix-", indicator, ".qs"))
      )
      row$res_adm2_class      <- get_class_or_status(
        file.path(results_path, paste0("new_res_adm2-", indicator, ".qs"))
      )
      
      ## ---------- FH model object ----------
      # top-level FH object class
      row$FH_adm2_fix_nest_class <- get_class_or_status(
        file.path(results_path, paste0("new_FH_adm2_fix_nest-", indicator, ".qs"))
      )
      
      
      res_list[[length(res_list) + 1L]] <- row
    }
  }
  
  dplyr::bind_rows(res_list)
}




check_FH_summary_hyperpar <- function(
    country_to_run,
    indicatorlist,
    source_path) {
  idx_surv <- which(surveys$country %in% country_to_run)
  n_steps  <- length(idx_surv) * length(indicatorlist)
  step     <- 0L
  
  # pb <- utils::txtProgressBar(min = 0, max = n_steps, style = 3)
  # res_list <- list()
  
  for (i in idx_surv) {
    country <- surveys$country[i]
    year    <- surveys$year[i]
    results_path <- file.path(source_path, "Gates-results", "Results", country, year)
    
    for (indicator in indicatorlist) {
      # step <- step + 1L
      # setTxtProgressBar(pb, step)
      # 
      qfile_summary <- file.path(
        results_path,
        paste0("new_summary-FH_adm2_fix_nest-", indicator, ".qs")
      )
      
      hp <- get_hyperpars(qfile_summary)  # c(Precision=..., phi=...)
      
      row <- list(
        country   = country,
        year      = year,
        indicator = indicator,
        Precision = hp["Precision"],
        phi       = hp["phi"]
      )
      
      res_list[[length(res_list) + 1L]] <- row
    }
  }
  
  # close(pb)
  dplyr::bind_rows(res_list)
}



