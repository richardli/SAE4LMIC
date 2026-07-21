





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


get_hyperpars <- function(path) {
  # Always return a named numeric vector: c(Precision = ..., phi = ...)
  
  # file missing
  if (!file.exists(path)) {
    return(c(Precision = NA_real_, phi = NA_real_))
  }
  
  # try to read
  obj <- tryCatch(qs::qread(path),
                  error = function(e) NULL)
  if (is.null(obj)) {
    return(c(Precision = NA_real_, phi = NA_real_))
  }
  
  # check structure
  if (is.null(obj$hyperpar) ||
      is.null(obj$hyperpar$mean) ||
      length(obj$hyperpar$mean) < 2) {
    return(c(Precision = NA_real_, phi = NA_real_))
  }
  
  # OK: pull the first two means
  c(
    Precision = as.numeric(obj$hyperpar$mean[1]),
    phi       = as.numeric(obj$hyperpar$mean[2])
  )
}






check_FH_summary_hyperpar <- function(
    country_to_run,
    indicatorlist,
    source_path) {
  idx_surv <- which(surveys$country %in% country_to_run)
  n_steps  <- length(idx_surv) * length(indicatorlist)
  step     <- 0L
  
  # pb <- utils::txtProgressBar(min = 0, max = n_steps, style = 3)
  res_list <- list()
  
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



#for unsaved summary, read model results 
get_hyperpars_ontimeuse<- function(path) {
  # Always return a named numeric vector: c(Precision = ..., phi = ...)
  
  # file missing
  if (!file.exists(path)) {
    return(c(Precision = NA_real_, phi = NA_real_))
  }
  
  # try to read
  obj <- tryCatch(qs::qread(path),
                  error = function(e) NULL)
  if (is.null(obj)) {
    return(c(Precision = NA_real_, phi = NA_real_))
  }
  
  obj<-summary(obj$model$fit)
  
  # check structure
  if (is.null(obj$hyperpar) ||
      is.null(obj$hyperpar$mean) ||
      length(obj$hyperpar$mean) < 2) {
    return(c(Precision = NA_real_, phi = NA_real_))
  }
  
  # OK: pull the first two means
  c(
    Precision = as.numeric(obj$hyperpar$mean[1]),
    phi       = as.numeric(obj$hyperpar$mean[2])
  )
}

check_FH_summary_hyperpar_ontimeuse <- function(
    country_to_run,
    indicatorlist,
    source_path) {
  idx_surv <- which(surveys$country %in% country_to_run)
  n_steps  <- length(idx_surv) * length(indicatorlist)
  step     <- 0L
  
  # pb <- utils::txtProgressBar(min = 0, max = n_steps, style = 3)
  res_list <- list()
  
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
        paste0("new_FH_adm2_fix_nest-", indicator, ".qs")
      )
      
      
      hp <- get_hyperpars_ontimeuse(qfile_summary)  # c(Precision=..., phi=...)
      
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








country="Nigeria"
temp=check_FH_summary_hyperpar(country_to_run=country,
                               indicatorlist=indicatorlist,
                               source_path)


countrylist=unique(surveys$country)
temp=check_FH_summary_hyperpar_ontimeuse(country_to_run=countrylist,
                                         indicatorlist=indicatorlist,
                                         source_path)


out_middle="Gates-results/estimates"
out_dir <- file.path(source_path, out_middle)
out_file <- file.path(out_dir, "Hyper.csv")
readr::write_csv(temp, out_file)



ggplot(temp, aes(x = phi)) +
  geom_histogram(binwidth = 0.05, color = "white") +
  facet_wrap(~ indicator, nrow = 4) +
  labs(x = "phi", y = "Count",
       title = "phi by indicator")


ggplot(temp, aes(x = phi)) +
  geom_histogram(binwidth = 0.05, color = "white") +
  facet_wrap(~ country, nrow = 4) +
  labs(x = "phi", y = "Count",
       title = "phi by country")





ggplot(temp, aes(x = Precision)) +
  geom_histogram(binwidth = 0.05, color = "grey20") +
  facet_wrap(~ indicator, nrow = 4) +
  coord_cartesian(xlim = c(0, 50)) + 
  labs(x = "phi", y = "Count",
       title = "Precision by indicator")


ggplot(temp, aes(x = Precision)) +
  geom_histogram(binwidth = 0.05, color = "grey20") +
  facet_wrap(~ country, nrow = 4) +
  coord_cartesian(xlim = c(0, 50)) + 
  labs(x = "phi", y = "Count",
       title = "Precision by country")

