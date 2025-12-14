
########## ########## ########## 
########## TABLES.    ##########
########## ########## ########## 
# Table 1: National estimates (two years) :save_tab1()  National.csv
# Table 3:  number of no admin 2 and unstable variances (two years) :save as National3.csv
# Table 456: number of clusters sample and events for each indicator (two years).  National_samples.csv,National_clusters.csv,National_envents.csv



# Table 1: National estimates (two years)

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
      ID = indicator, #dplyr::if_else(is.na(Description) | Description == "", indicator, Description)
      value = ifelse(is.na(value), NA, round(100 * value, 1))  # percent with 1 decimal
    ) |>
    dplyr::select(ID, year, value) |>
    tidyr::pivot_wider(
      names_from = year, values_from = value,
      names_prefix = "DHS "
    ) 
  
  print(tab)
  # write CSV
  out_dir <- file.path(source_path, out_middle, country)
  out_file <- file.path(out_dir, "National.csv")
  readr::write_csv(tab, out_file)
  message("Saved: ", out_file)
  
  invisible(tab)
}

# Table 3: number of no admin 2 and unstable variance 
# 
# save_tab3 <- function(country,
#                       ad2_name = "new_res_adm2_fix-",
#                       ids=infolist$ID,
#                       middle_path="Gates-results/Results",
#                       out_middle="Gates-results/estimates"
# ) {
#   
#   indicators=infolist$ID
#   # years to use
#   yrs_all <- sort(surveys$year[surveys$country == country])
#   if (length(yrs_all) == 0) {
#     message("No surveys for ", country)
#     return(invisible(NULL))
#   }
#   
#   if (identical(country, "South Africa")) {
#     years <- 2016L
#     labels <- c("current")
#   } else {
#     # choose "current" = latest year; "previous" = latest-1 (if exists)
#     if (length(yrs_all) >= 2) {
#       years <- tail(yrs_all, 2)
#       labels <- c("previous", "current")
#     } else {
#       years <- tail(yrs_all, 1)
#       labels <- c("current")
#     }
#   }
#   
#   # helper: read qfile and compute (no_ad2, fixed)
#   get_triplet <- function(ind, yr) {
#     results_path  <- file.path(source_path, middle_path, country, yr)
#     qfile_adm2   <- file.path(results_path, paste0(ad2_name, ind, ".qs"))
#     if (!file.exists(qfile_adm2)) return(c(NA_real_, NA_real_))
#     x <- tryCatch(qs::qread(qfile_adm2), error = function(e) NULL)
#     if (is.null(x)) return(c(NA_real_, NA_real_))
#     
#     no_ad2 <- tryCatch(
#       nrow(x$data.info) - nrow(x$res.admin2),
#       error = function(e) NA_real_
#     )
#     fixed <- tryCatch(length(x$fixed_areas), error = function(e) NA_real_)
#     c(no_ad2, fixed)
#   }
#   
#   # build rows for each indicator
#   rows <- lapply(indicators, function(ind) {
#     vals <- unlist(lapply(years, function(yr) get_triplet(ind, yr)))
#     data.frame(ID = ind, t(vals), check.names = FALSE)
#   })
#   tab3 <- do.call(rbind, rows)
#   
#   # name the columns
#   year_cols <- unlist(lapply(labels, function(lbl) paste0(lbl, c("_no_ad2", "_fixed"))))
#   names(tab3) <- c("ID", year_cols)
#   
#   # write CSV
#   out_dir <- file.path(source_path,out_middle, country)
#   out_file <- file.path(out_dir, "National3.csv")
#   write.csv(tab3, out_file, row.names = FALSE)
#   message("Saved: ", out_file)
#   
#   invisible(tab3)
# }


save_tab3 <- function(country,
                      ad2_name_dir=  "new_res_adm2-" ,
                      ad2_name_fix=  "new_res_adm2_fix-",
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
    qfile_adm2   <- file.path(results_path, paste0(ad2_name_dir, ind, ".qs"))
    qfile_adm2fix   <- file.path(results_path, paste0(ad2_name_fix, ind, ".qs"))
    
    if (!file.exists(qfile_adm2)) return(c(NA_real_, NA_real_))
    x <- tryCatch(qs::qread(qfile_adm2), error = function(e) NULL)
    xf <- tryCatch(qs::qread(qfile_adm2fix), error = function(e) NULL)
    
    if (is.null(x)) return(c(NA_real_, NA_real_))
    no_ad2 <- tryCatch(
      nrow(x$data.info) - nrow(x$res.admin2),
      error = function(e) NA_real_
    )
    fixed <- tryCatch(length(xf$fixed_areas), error = function(e) NA_real_)
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







# Table 456: number of no admin 2 and unstable variance 



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



########## ########## ########## 
########## PLOTS    ##########
########## ########## ########## 


# savemaps_ridge:   "ridge_",  "ad2_map"*2, "ad1_map"*2 
# saveclustermap:   "Basic-"*2
# saveinterval_overlay: "overlay-" "interval-ad2-" "interval-ad1-"
# savescatter: "scatter-"


####################
# savemaps_ridge
# report_map.R
# admin 1 and admin 2 MAPs for 1)prevalence, 2)length of credible interval, and 3) exceedance. "ad2_map_", "ad1_map_"
# admin 1 ridge plot :  "ridge_"
# hepler: save_tab2_from_files(), has_data(), make_placeholder(),ridgePlot1(),exceedPlot1(), exceedplot2()
####################


save_tab2_from_files <- function(country, adm_name = "res_adm0-", ids = indicatorlist) {
  # surveys for this country (sorted by year)
  df_surv <- dplyr::filter(surveys, country == !!country) |> dplyr::arrange(year)
  if (nrow(df_surv) == 0) stop("No surveys for country = ", country)
  years <- df_surv$year
  
  # helper: read (est, lower, upper) triplet from one file
  read_triplet <- function(ind, yr) {
    results_path <- file.path(source_path, "Gates-results", "Results", country, yr)
    qfile <- file.path(results_path, paste0(adm_name, ind, ".qs"))
    if (!file.exists(qfile)) return(c(NA_real_, NA_real_, NA_real_))
    tryCatch({
      rn <- qs::qread(qfile)$res.natl
      c(as.numeric(rn$direct.est), as.numeric(rn$direct.lower), as.numeric(rn$direct.upper))
    }, error = function(e) c(NA_real_, NA_real_, NA_real_))
  }
  
  # format helper (whole %)
  fmt_pct0 <- function(x) ifelse(is.na(x), NA, round(x, 2))
  
  # one row per indicator
  one_row <- function(ind) {
    mat <- do.call(rbind, lapply(years, function(yr) read_triplet(ind, yr)))
    colnames(mat) <- c("est", "lower", "upper")
    
    # default NA triplets
    prev <- rep(NA_real_, 3)
    curr <- rep(NA_real_, 3)
    
    # first row = previous year (yr1)
    if (nrow(mat) >= 1 && !all(is.na(mat[1, ]))) {
      prev <- mat[1, ]
    }
    
    # second row = current year (yr2)
    if (nrow(mat) >= 2 && !all(is.na(mat[2, ]))) {
      curr <- mat[2, ]
    }
    
    prev <- fmt_pct0(prev); curr <- fmt_pct0(curr)
    prev <- unname(prev);   curr <- unname(curr)
    
    if (identical(country, "South Africa")) {
      data.frame(
        ID = ind,
        current_est   = curr[1],
        current_lower = curr[2],
        current_upper = curr[3],
        check.names   = FALSE
      )
    } else {
      data.frame(
        ID = ind,
        previous_est   = prev[1],
        previous_lower = prev[2],
        previous_upper = prev[3],
        current_est    = curr[1],
        current_lower  = curr[2],
        current_upper  = curr[3],
        check.names    = FALSE
      )
    }
  }
  
  tab2 <- dplyr::bind_rows(lapply(ids, one_row))
  
  # # write CSV
  # out_dir  <- file.path(source_path, "Gates-results", "estimates", country)
  # if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  # out_file <- file.path(out_dir, "National.csv")
  # readr::write_csv(tab2, out_file)
  # message("Saved: ", out_file)
  # 
  return(tab2)
}
has_data <- function(x) {
  !is.null(x) &&
    !identical(x, "failed") &&
    is.list(x) &&
    (!is.null(x$res.admin1)|| !is.null(x$res.admin2) )  &&
    (   NROW(x$res.admin1) ||    NROW(x$res.admin2)  )> 0
}
make_placeholder <- function(title_left, legend_title) {
  ggplot() +
    theme_void() +
    xlim(0, 1) + ylim(0, 1) +
    annotate("text", x = 0.5, y = 0.6,
             label = paste("No data for", title_left),
             size = 5) +
    annotate("text", x = 0.5, y = 0.4,
             label = paste0("(", legend_title, ")"),
             size = 3.5) +
    theme(plot.background = element_rect(fill = "grey95", color = NA))
}
 
ridgePlot1<-function (x = NULL, nsim = 1000, draws = NULL, year.plot = NULL,
                      year_plot =NULL,
                      strata.plot = NULL, strata_plot = NULL,
                      by.year = TRUE, ncol = 4, scale = 2, per1000 = FALSE, order = 0,
                      direction = 1, linewidth = 0.5, results = NULL, save.density = FALSE,custom.order = NULL ,
                      ...){
  
  if (lifecycle::is_present(year_plot)) {
    lifecycle::deprecate_soft("2.0.0", "ridgePlot(year_plot)",
                              "ridgePlot(year.plot)")
    year.plot <- year_plot
  }
  if (lifecycle::is_present(strata_plot)) {
    lifecycle::deprecate_soft("2.0.0", "ridgePlot(strata_plot)",
                              "ridgePlot(strata.plot)")
    strata.plot <- strata_plot
  }
  
  
  years <- y <- ..x.. <- region <- value <- region.name <- admin2.name.short <- NA
  if (class(x) %in% c("fhModel", "clusterModel", "directEST")) {
    x_att <- attributes(x)
    domain.names <- sort(x_att$domain.names)
    if (x_att$class %in% c("fhModel", "clusterModel", "directEST")) {
      if ("admin2_post" %in% x_att$names) {
        samples = x$admin2_post
      }
      else {
        samples = x$admin1_post
      }
    }
    else {
      if ("res.admin1" %in% names(x)) {
        domain.names <- x$res.admin1$admin1.name
        samples <- matrix(NA, nsim, length(domain.names))
        for (i in 1:dim(x$res.admin1)[1]) {
          samples[, i] <- expit(rnorm(nsim, mean = x$res.admin1$direct.logit.est[i],
                                      sd = (x$res.admin1$direct.logit.prec[i])^(-1/2)))
        }
      }
      else {
        domain.names <- x$res.admin2$admin2.name.full
        samples <- matrix(NA, nsim, length(domain.names))
        for (i in 1:dim(x$res.admin2)[1]) {
          samples[, i] <- expit(rnorm(nsim, mean = x$res.admin2$direct.logit.est[i],
                                      sd = (x$res.admin2$direct.logit.prec[i])^(-1/2)))
        }
      }
    }
    samples.long <- data.frame(region.name = rep(domain.names,
                                                 each = nrow(samples)), value = as.numeric(samples))
    
    
    
    decreasing <- order >= 0  # TRUE => largest on top
    
    
    
    
    
    if (!"group.name" %in% names(samples.long)) {
      # Admin-1 case: reorder by median(value) across the region
      ord <- aggregate(value ~ region.name, samples.long, median, na.rm = TRUE)
      ord <- ord[order(ord$value, decreasing = decreasing), "region.name"]
      samples.long$region.name <- factor(samples.long$region.name, levels = ord)
    } else {
      # Admin-2 faceted case: reorder within each group (admin1)
      samples.long <- dplyr::group_by(samples.long, group.name)
      samples.long <- dplyr::mutate(
        samples.long,
        admin2.name.short = forcats::fct_reorder(
          admin2.name.short, value, .fun = median, .desc = decreasing
        )
      )
      samples.long <- dplyr::ungroup(samples.long)
    }
    
    
    if (!is.null(custom.order)) {
      samples.long$region.name <- factor(
        samples.long$region.name,
        levels = custom.order
      )
    } else if (order != 0) {
      # your existing median/mean ordering logic
    } else {
      # keep original order
    }
    
    
    if ("res.admin2" %in% names(x)) {
      upper <- x$res.admin2[, c("admin2.name.full", "admin1.name")]
      upper$admin2.name.short <- NA
      for (i in 1:dim(upper)[1]) {
        k <- nchar(as.character(upper$admin1.name[i]))
        upper$admin2.name.short[i] <- substr(upper$admin2.name.full[i],
                                             start = k + 2, stop = nchar(upper$admin2.name.full[i]))
      }
      colnames(upper) <- c("region.name", "group.name",
                           "admin2.name.short")
      samples.long <- dplyr::left_join(samples.long, upper)
    }
    n.levels <- dim(samples)[2]
    ridge.max <- max(samples.long$value) * 1.03
    if (ridge.max > 0.95) {
      ridge.max = 1
    }
    ridge.min <- min(samples.long$value) * 0.97
    if (ridge.min < 0.05) {
      ridge.min = 0
    }
    g <- ggplot2::ggplot(samples.long) + aes(x = value,
                                             y = region.name) + ggridges::geom_density_ridges_gradient(aes(fill = ggplot2::after_stat(x))) +
      ggplot2::scale_fill_viridis_c(lim = c(ridge.min,
                                            ridge.max), direction = direction) + ggplot2::theme(legend.position = "none") +
      xlab("") + ylab("")
    if ("group.name" %in% colnames(samples.long)) {
      g <- g + aes(y = admin2.name.short) + ggplot2::facet_wrap(~group.name,
                                                                scale = "free_y")
    }
    return(g)
  }
  if (!isTRUE(requireNamespace("INLA", quietly = TRUE))) {
    stop("You need to install the packages 'INLA'. Please run in your R terminal:\n  install.packages('INLA', repos=c(getOption('repos'), INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)")
  }
  if (isTRUE(requireNamespace("INLA", quietly = TRUE))) {
    if (!is.element("INLA", (.packages()))) {
      attachNamespace("INLA")
    }
    is.density <- FALSE
    if (!is.null(results)) {
      results <- results$data
      region_names <- unique(results$region)
      timelabel.yearly <- levels(results$years)
      if (order != 0)
        warning("Plotting pre-calculated densities, order argument is ignored.")
      is.density <- "y" %in% colnames(results)
    }
    else {
      Amat <- x$Amat
      if (is.null(Amat) && is.null(draws)) {
        region_names <- "All"
        region_nums <- 0
      }
      else if (!is.null(draws)) {
        region_names <- unique(draws$overall$region)
        region_nums <- 1:length(region_names)
      }
      else {
        region_names <- colnames(Amat)
        region_nums <- 1:length(region_names)
      }
      if (!is.null(draws)) {
        if ("draws.est" %in% draws == FALSE && is(x,
                                                  "list"))
          stop("draws argument is not correctly specified. It should be the whole output of getSmoothed() function.")
        message("Use posterior draws from input.")
        x <- draws
      }
      else if (is.null(x)) {
        if (!is.null(x$family)) {
          message("Draws not specified. Use getSmoothed() to calculate posterior draws. Please be aware this does not take into account strata weighting.")
          draws <- getSmoothed(x, Amat = Amat, nsim = nsim,
                               save.draws = TRUE)
          x <- draws
        }
      }
      else if (is.null(x$fit)) {
        stop("Neither fitted object nor posterior draws are provided.")
      }
      if ((is(x, "list") || is(x, "SUMMERprojlist")) &&
          is.null(x$fit)) {
        is.density <- FALSE
        if (is.null(x$draws.est.overall)) {
          stop("Posterior draws not found. Please rerun getSmoothed() with save.draws = TRUE.")
        }
        tmp <- NULL
        if (is.null(strata.plot)) {
          draws.plot <- x$draws.est.overall
        }
        else {
          draws.plot <- NULL
          counter <- 1
          for (i in 1:length(x$draws.est)) {
            if (x$draws.est[[i]]$strata == draws.plot) {
              draws.plot[[counter]] <- x$draws.est[[i]]
              counter <- counter + 1
            }
          }
        }
        for (i in 1:length(draws.plot)) {
          if (draws.plot[[i]]$years %in% tmp)
            next
          tmp <- c(tmp, draws.plot[[i]]$years)
        }
        year.label <- tmp
        timelabel.yearly <- year.label
        results <- NULL
        for (i in 1:length(timelabel.yearly)) {
          for (j in 1:length(region_names)) {
            draw_est_j <- draws.plot[lapply(draws.plot,
                                            "[[", "region") == region_names[j]]
            draw_est_ij <- draw_est_j[lapply(draw_est_j,
                                             "[[", "years") == timelabel.yearly[i]]
            tmp <- data.frame(draws = draw_est_ij[[1]]$draws)
            tmp$region <- region_nums[j]
            tmp$years <- timelabel.yearly[i]
            results <- rbind(results, tmp)
          }
        }
        results$years.num <- suppressWarnings(as.numeric(as.character(results$years)))
        results$x <- results$draws
        if (region_names[1] != "All") {
          results$region <- region_names[results$region]
        }
        else {
          results$region <- "All"
        }
        results$years <- factor(results$years, levels = timelabel.yearly)
        if (order != 0 && by.year && length(region_names) >
            1) {
          tmp <- data.frame(region = region_names, median = NA)
          for (j in 1:length(region_names)) {
            tmp$median[j] <- median(results[results$region ==
                                              region_names[j] & results$years == timelabel.yearly[length(timelabel.yearly)],
            ]$draws, na.rm = T)
          }
          tmp <- tmp[order(tmp$median, decreasing = (order >
                                                       0)), ]
          results$region <- factor(results$region, levels = tmp$region)
        }
        else {
          results$region <- factor(results$region, rev(sort(as.character(unique(results$region)))))
        }
      }
      else {
        is.density <- TRUE
        is.yearly = x$is.yearly
        year.label <- x$year.label
        if (is.yearly) {
          timelabel.yearly <- c(x$year.range[1]:x$year.range[2],
                                year.label)
        }
        else {
          timelabel.yearly <- year.label
        }
        names <- expand.grid(area = region_nums, time = timelabel.yearly)
        mod <- x$fit
        lincombs.info <- x$lincombs.info
        results <- NULL
        for (i in 1:length(timelabel.yearly)) {
          for (j in 1:length(region_names)) {
            index <- lincombs.info$Index[lincombs.info$District ==
                                           region_nums[j] & lincombs.info$Year ==
                                           i]
            tmp <- data.frame(INLA::inla.tmarginal(expit,
                                                   mod$marginals.lincomb.derived[[index]]))
            tmp$region <- region_nums[j]
            tmp$years <- timelabel.yearly[i]
            results <- rbind(results, tmp)
          }
        }
        results$is.yearly <- !(results$years %in% year.label)
        results$years.num <- suppressWarnings(as.numeric(as.character(results$years)))
        if (region_names[1] != "All") {
          results$region <- region_names[results$region]
        }
        else {
          results$region <- "All"
        }
        results$years <- factor(results$years, levels = timelabel.yearly)
        if (order != 0 && by.year && length(region_names) >
            1) {
          tmp <- data.frame(region = region_names, median = NA)
          for (j in 1:length(region_names)) {
            index <- lincombs.info$Index[lincombs.info$District ==
                                           region_nums[j] & lincombs.info$Year ==
                                           length(timelabel.yearly)]
            tmp$median[j] <- INLA::inla.qmarginal(0.5,
                                                  mod$marginals.lincomb.derived[[index]])
          }
          tmp <- tmp[order(tmp$median, decreasing = (order >
                                                       0)), ]
          results$region <- factor(results$region, levels = tmp$region)
        }
        else {
          results$region <- factor(results$region, rev(sort(as.character(unique(results$region)))))
        }
      }
    }
    
    
    
    results.plot <- results
    if (per1000)
      results.plot$x <- 1000 * results.plot$x
    if (is.null(year.plot)) {
      year.plot <- year.label
    }
    if (by.year && is.density) {
      g <- ggplot2::ggplot(subset(results.plot, years %in%
                                    year.plot), ggplot2::aes(x = x, y = region,
                                                             height = y, fill = ..x..))
    }
    else if (is.density) {
      results.plot$years <- factor(results.plot$years,
                                   levels = rev(timelabel.yearly))
      g <- ggplot2::ggplot(subset(results.plot, years %in%
                                    year.plot), ggplot2::aes(x = x, y = years, height = y,
                                                             fill = ..x..))
    }
    if (is.density)
      g <- g + ggridges::geom_density_ridges_gradient(stat = "identity",
                                                      alpha = 0.5, linewidth = linewidth)
    if (by.year && !is.density) {
      g <- ggplot2::ggplot(subset(results.plot, years %in%
                                    year.plot), ggplot2::aes(x = x, y = region))
    }
    else if (!is.density) {
      results.plot$years <- factor(results.plot$years,
                                   levels = rev(timelabel.yearly))
      g <- ggplot2::ggplot(subset(results.plot, years %in%
                                    year.plot), ggplot2::aes(x = x, y = years))
    }
    if (!is.density)
      g <- g + ggridges::geom_density_ridges_gradient(ggplot2::aes(fill = ..x..),
                                                      scale = scale, alpha = 0.5, linewidth = linewidth)
    g <- g + ggplot2::scale_fill_viridis_c(option = "D",
                                           direction = direction) + ggplot2::theme_bw() + ggplot2::ylab("") +
      ggplot2::theme(legend.position = "none") + ggplot2::xlab("")
    if (by.year) {
      if (length(year.plot) > 1)
        g <- g + ggplot2::facet_wrap(~years, ncol = ncol)
    }
    else {
      if (length(region_names) > 1)
        g <- g + ggplot2::facet_wrap(~region, ncol = ncol)
    }
    if (save.density) {
      return(list(data = results, g = g))
    }
    else {
      return(g)
    }
  }
}



#for two outcomes
exceedPlot1 <- function(x, exceed = TRUE, direction = 1, threshold,
                        geo, by.geo = NULL, ylim = NULL,
                        facet_var = "year", facet_labels = NULL, ...) {
  
  # helper: from one model -> data.frame(region.name, value)
  one_exceed <- function(mod, thr) {
    xa <- attributes(mod)
    samples <- if (!is.null(mod$admin2_post)) mod$admin2_post else mod$admin1_post
    if (!is.matrix(samples)) stop("Posterior 'samples' must be a matrix: nsamp x nregion.")
    
    dat <- data.frame(region.name = sort(xa$domain.names))
    p <- if (isTRUE(exceed)) colMeans(samples > thr) else colMeans(samples < thr)
    dat$value <- p
    dat
  }
  
  if (is.list(x) && !inherits(x, "fhModel")) {
    # --- facetted path: x is a (named) list of models
    n <- length(x)
    if (length(threshold) == 1) threshold <- rep(threshold, n)
    if (length(threshold) != n) stop("If x is a list, 'threshold' must be length 1 or length(x).")
    
    if (is.null(facet_labels)) {
      facet_labels <- names(x)
      if (is.null(facet_labels)) facet_labels <- as.character(seq_len(n))
    }
    
    pieces <- Map(function(mod, thr, lab) {
      d <- one_exceed(mod, thr)
      d[[facet_var]] <- lab
      d
    }, x, threshold, facet_labels)
    
    dat_long <- do.call(rbind, pieces)
    
    g <- SUMMER::mapPlot(
      data = dat_long, geo = geo,
      by.data = "region.name", by.geo = by.geo,
      is.long = TRUE, variable = facet_var, value = "value",
      legend.label = "Exceedance\nProbability",
      direction = direction, ylim = ylim, removetab = TRUE, ...
    )
    
  } else {
    # --- single model (no facets)
    dat <- one_exceed(x, threshold)
    g <- SUMMER::mapPlot(
      data = dat, geo = geo,
      by.data = "region.name", by.geo = by.geo,
      variable = "value",
      legend.label = "Exceedance\nProbability",
      direction = direction, ylim = ylim, removetab = TRUE, ...
    )
  }
  
  return(g)
}

#for single map
exceedplot3 <- function(
    x,
    exceed = TRUE,
    direction = 1,
    threshold = NA,
    geo = geo,
    by.geo = NULL,
    ylim = NULL,
    yr = "2022",
    ...
) {
  if (is.na(threshold))
    stop("A numerical threshold must be specified.")
  
  x_att <- attributes(x)
  
  
  # if (is.na(threshold)) 
  #   stop("A numerical threshold need to be specified.")
  # x_att <- attributes(x)
  # if (x_att$class %in% c( "directEST","fhModel", "clusterModel")) {
  #   if ("admin2_post" %in% x_att$names) {
  #     samples = x$admin2_post
  #   }
  #   else {
  #     samples = x$admin1_post
  #   }
  # }
  
  # determine samples
  samples <- if ("admin2_post" %in% x_att$names) x$admin2_post else x$admin1_post
  
  # compute probability
  dat <- data.frame(region.name = x_att$domain.names, value = NA_real_)
  for (i in seq_len(ncol(samples))) {
    dat$value[i] <- mean(samples[, i] > threshold)
    if (!exceed)
      dat$value[i] <- mean(samples[, i] < threshold)
  }
  
  # ---- KEY FIX: rename 'value' to the year label ----
  names(dat)[names(dat) == "value"] <- yr
  
  # ---- IMPORTANT FIX: use `variables` not `value` ----
  g <- SUMMER::mapPlot(
    data = dat,
    geo = geo,
    by.data = "region.name",
    by.geo = by.geo,
    variables = yr,       # <-- this triggers SUMMER header
    is.long = FALSE,      # wide format
    legend.label = "Probability",
    direction = direction,
    ylim = ylim,
    ...
  )
  
  return(g)
}

savemaps_ridge<-function(country="Nigeria",
                         ad2_name="new_FH_adm2_fix_nest-",
                         ad1_name="new_res_adm1-",
                         indicatorlist =infolist$ID,
                         adm_name = "new_res_adm0-",
                         middle_path="Gates-results/Results",
                         plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country),
                         dpi = 150
){
  
  
  yr1=min(surveys[surveys$country==country,]$year)
  yr2=max(surveys[surveys$country==country,]$year)
  
  
  
  
  tab2<-save_tab2_from_files(country,adm_name =adm_name,infolist$ID)
  # ad2_name<-"new_FH_adm2_fix_nest-"
  # country="Nigeria"X
  results_path<-file.path(source_path, middle_path, country, yr1)
  load(file.path(results_path, "basic.Rdata"))
  # poly.adm2 <- ms_simplify(poly.adm2, keep = 0.1, keep_shapes = TRUE)  # keep ~10% of vertices
  # plot_path_c<- file.path(source_path,"Gates-results/ReportPlots",country)
  for (indicator in indicatorlist ) {
    
    if (infolist[infolist$ID == indicator, ]$direction == -1) {
      color.paletteHERE <- rev(brewer.pal(5, "RdYlGn"))
    } else {
      color.paletteHERE <- brewer.pal(5, "RdYlGn")
    }
    
    
    if(indicator %in% c("CM_ECMR_C_NNF")){
      
      thre1 = tab2[tab2$ID==indicator, ]$previous_est 
      thre2 = tab2[tab2$ID==indicator, ]$current_est  
      
      LABELS = function(x) x * 1000
      scale_fill_gradientn_prevalence<-scale_fill_gradientn(colors = color.paletteHERE, name = "Rate",labels = LABELS)
      
      facet_labels = c(
        paste0(yr1, " National: ", thre1* 1000, " per 1,000"),
        paste0(yr2, " National: ", thre2* 1000, " per 1,000")
      )
      
      exceedlegend_singleyear1 <- paste0("Probability\nExceeding ", thre1*1000 )
      exceedlegend_singleyear2 <- paste0("Probability\nExceeding ", thre2*1000 )
      
      
    } else {
      
      thre1 = tab2[tab2$ID==indicator, ]$previous_est 
      thre2 = tab2[tab2$ID==indicator, ]$current_est  
      
      LABELS = scales::percent_format(accuracy = 1)
      scale_fill_gradientn_prevalence<-scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence",labels = LABELS)
      facet_labels = c(
        paste0(yr1, " National: ", thre1* 100, "%"),
        paste0(yr2, " National: ", thre2* 100, "%")
      )
      exceedlegend_singleyear1 <- paste0("Probability\nExceeding ", thre1*100, "%\n")
      exceedlegend_singleyear2 <- paste0("Probability\nExceeding ", thre2*100, "%\n")
      
    }
    
    
    
    
    # yr1=min(surveys[surveys$country==country,]$year)
    results_path_yr1 <- file.path(source_path, middle_path, country, yr1)
    
    # yr2=max(surveys[surveys$country==country,]$year)
    results_path_yr2 <- file.path(source_path, middle_path, country, yr2)
    
    qfile_adm1 <- file.path(results_path_yr1, paste0(ad2_name, indicator, ".qs"))
    qfile_adm2 <- file.path(results_path_yr2, paste0(ad2_name, indicator, ".qs"))
    
    
    old <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("tryCatched: Failed to read ", qfile_adm1, ": ", e$message); return("failed") })
    new <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("tryCatched: Failed to read ", qfile_adm2, ": ", e$message); return("failed") })
    
    
    # old <- ad2_store[[iso3]][[as.character(yr1)]][[indicator]]
    # new <- ad2_store[[iso3]][[as.character(yr2)]][[indicator]]
    
    ok11 <- has_data(old)
    ok12 <- has_data(new)
    
    
    # --------------------
    # 
    # ----- map ----------
    # 
    # --------------------
    plot_one_year <- function(df, year_label, value_col, legend_title,scale_layer) {
      if (missing(df) || is.null(df)) {
        return(make_placeholder(year_label, legend_title))
      }
      
      # out1, yr1, "Prevalence", "Prevalence",
      
      names(df)[names(df) == value_col] <- year_label
      mp <- mapPlot(
        data   = df,
        geo    = poly.adm2,
        by.data= "admin2.name.full",
        by.geo = "admin2.name.full",
        is.long= FALSE,                # single map, not faceted
        # value  = value_col,
        variables= year_label,
        # legend.label = year_label,
        size = .05,
        border = "gray50"
      ) +
        scale_layer
      # + ggtitle(as.character(year_label))
      mp
    }
    
    out1=out11=c()
    if (ok11) {
      
      
      out1 <- old$res.admin2[, c("admin2.name.full", "median", "sd",
                                 "lower", "upper", "cv2")]
      colnames(out1)[c(2)] <- c("Prevalence")
      out1$version <- yr1
      
      out1$width <- out1$upper - out1$lower
      out1$oddratio<- out1$Prevalence/(1-out1$Prevalence)
      out1$oddratio<- log( (out1$Prevalence/(1-out1$Prevalence)) / ( thre1/ (1-thre1)))
      
      
    }
    
    if (ok12) {
      
      
      out11 <- new$res.admin2[, c("admin2.name.full", "median", "sd",
                                  "lower", "upper", "cv2")]
      colnames(out11)[c(2)] <- c("Prevalence")
      out11$version <- yr2
      
      out11$width <- out11$upper - out11$lower
      out11$oddratio<- log( (out11$Prevalence/(1-out11$Prevalence)) / ( thre2/ (1-thre2)))
      
      
      exceedlegend_singleyear <- paste0("Probability\nExceeding ", thre1, "%\n")
      
    }
    
    
    if (ok11 && ok12) {
      # ----- both years available -----
      outadm2 <- rbind(out1, out11)
      outadm2$version <- factor(outadm2$version, levels = unique(outadm2$version))
      outadm2$width <- outadm2$upper - outadm2$lower
      
      
      poly.adm2$admin2.name.full=paste0(poly.adm2$NAME_1,"_",poly.adm2$NAME_2)
      g1 <- mapPlot(data = outadm2, geo = poly.adm2,
                    by.data = "admin2.name.full",  by.geo = "admin2.name.full", is.long = TRUE,
                    variable = "version", value = "Prevalence", legend.label = "Prevalence",
                    direction = -1, ncol = 2, size = .05, border = "gray50") +
        scale_fill_gradientn_prevalence
      
      g4 <- mapPlot(data = outadm2, geo = poly.adm2,
                    by.data = "admin2.name.full",  by.geo = "admin2.name.full", is.long = TRUE,
                    variable = "version", value = "width", legend.label = "90% CI\nwidth",
                    ncol = 2, size = .05, border = "gray50") +
        scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"), 
                               name = "90% CI\nwidth",
                               labels = LABELS)
      
      rng <- range(outadm2$oddratio, na.rm = TRUE)
      g2 <- mapPlot(data = outadm2, geo = poly.adm2,
                    by.data = "admin2.name.full",  by.geo = "admin2.name.full", is.long = TRUE,
                    variable = "version", value = "oddratio",
                    ncol = 2, size = .05, border = "gray50")  +
        scale_fill_distiller(
          palette = "Spectral",
          values = scales::rescale(c(rng[1], 0, rng[2])),
          name    = "Log odds ratio"
        )
      
      
      g5 <- exceedPlot1(
        x = list(old,  new),
        facet_labels = facet_labels,
        threshold = c(thre1, thre2),
        exceed = TRUE, direction = infolist[infolist$ID == indicator, ]$direction ,
        geo = poly.adm2, by.geo = "admin2.name.full",
        facet_var = "year",                # column name to facet on
        ncol = 2, size = 0.05, border = "gray50"
      ) +
        scale_fill_distiller(name = "Exceedance\nProbability",
                             palette = "Spectral", direction = infolist[infolist$ID == indicator, ]$direction,
                             limits = c(0,1), breaks = c(0.25,0.5,0.75),
                             labels = scales::percent_format(accuracy = 1))
      
      
      
      
      
    } else {
      poly.adm2$admin2.name.full=paste0(poly.adm2$NAME_1,"_",poly.adm2$NAME_2)
      
      # ----- at least one year missing -----
      # ----- compute metrics only when present -----
      # Prevalence
      g1_left <- if (ok11) {
        plot_one_year(out1, yr1, "Prevalence", "Prevalence",
                      scale_fill_gradientn_prevalence)
        # scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence",labels = LABELS))
      } else {
        make_placeholder(yr1, "Prevalence")
      }
      g1_right <- if (ok12) {
        plot_one_year(out11, yr2, "Prevalence", "Prevalence",
                      scale_fill_gradientn_prevalence)
        # scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence",labels = LABELS))
      } else {
        make_placeholder(yr2, "Prevalence")
      }
      g1 <- g1_left | g1_right
      
      
      # 90% CI width
      g4_left <- if (ok11) {
        plot_one_year(out1, yr1, "width", "90% CI\nwidth",
                      scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth",labels = LABELS))
      } else {
        make_placeholder(yr1, "90% CI width")
      }
      g4_right <- if (ok12) {
        plot_one_year(out11, yr2, "width", "90% CI\nwidth",
                      scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth",labels = LABELS))
      } else {
        make_placeholder(yr2, "90% CI width")
      }
      g4 <- g4_left | g4_right
      
      # Exceedance
      g5_left <- if (ok11) {
        
        
        # thre1=tab2[tab2$ID==indicator,]$previous_est
        # thre2=tab2[tab2$ID==indicator,]$current_est
        # old$admin2_post=old$admin2_post*100
        # exceedlegend_singleyear <- paste0("Probability\nExceeding ", thre1, "%\n")
        exceedplot3(
          old,
          threshold = thre1,
          exceed = TRUE,
          yr = as.character(yr1),   # "2022"
          direction = infolist[infolist$ID == indicator, ]$direction,
          geo = poly.adm2,
          by.geo = "admin2.name.full",
          border = "gray80",
          size = 0
        )+
          scale_fill_distiller(exceedlegend_singleyear1, palette = "Spectral",
                               direction =  infolist[infolist$ID == indicator, ]$direction ,
                               , labels = scales::percent_format(accuracy = 1))
        
        
      } else {
        make_placeholder(yr1, "Exceedance")
      }
      g5_right <- if (ok12) {
        
        # # thre1=tab2[tab2$ID==indicator,]$previous_est
        # thre2=tab2[tab2$ID==indicator,]$current_est
        # # res_adm11$admin1_post=res_adm11$admin1_post*100
        # new$admin2_post=new$admin2_post*100
        # # names(new$res.admin2)[names(new$res.admin2) == value_col] <- yr2
        # 
        legend <- paste0("Probability\nExceeding ", thre2, "%\n")
        
        exceedplot3(
          new,
          threshold = thre2,
          exceed = TRUE,
          yr = as.character(yr2),   # "2022"
          direction = infolist[infolist$ID == indicator, ]$direction,
          geo = poly.adm2,
          by.geo = "admin2.name.full",
          border = "gray80",
          size = 0
        )+
          scale_fill_distiller(exceedlegend_singleyear2, palette = "Spectral",
                               direction =  infolist[infolist$ID == indicator, ]$direction,
                               , labels = scales::percent_format(accuracy = 1))
        
      } else {
        make_placeholder(yr2, "Exceedance")
      }
      g5 <-   g5_left |   g5_right
      
      
      
      
    }
    
    final_plot <- (g1 / g4/ g5) + plot_layout(heights = c(1,1,1))
    
    # stack the four rows
    
    ggsave(final_plot,
           filename = file.path(plot_path_c, paste0("ad2_map_", indicator, ".png")),
           width = 6, height = 9, dpi = dpi)
    
    
  }
  
  
  # ad1_name<-"new_res_adm1-"
  # results_path<-file.path(source_path, "Gates-results/Results", country, yr1)
  # load(file.path(results_path, "basic.Rdata"))
  poly.adm1 <- ms_simplify(poly.adm1, keep = 0.1, keep_shapes = TRUE)  # keep ~10% of vertices
  
  tab2<-save_tab2_from_files(country,adm_name =adm_name,infolist$ID)
  for (indicator in indicatorlist ) {
    
    if (infolist[infolist$ID == indicator, ]$direction == -1) {
      color.paletteHERE <- rev(brewer.pal(5, "RdYlGn"))
    } else {
      color.paletteHERE <- brewer.pal(5, "RdYlGn")
    }
    
    
    if(indicator %in% c("CM_ECMR_C_NNF")){
      
      thre1 = tab2[tab2$ID==indicator, ]$previous_est 
      thre2 = tab2[tab2$ID==indicator, ]$current_est  
      
      LABELS = function(x) x * 1000
      scale_fill_gradientn_prevalence<-scale_fill_gradientn(colors = color.paletteHERE, name = "Rate",labels = LABELS)
      
      facet_labels = c(
        paste0(yr1, " National: ", thre1* 1000, " per 1,000"),
        paste0(yr2, " National: ", thre2* 1000, " per 1,000")
      )
      exceedlegend_singleyear1 <- paste0("Probability\nExceeding ", thre1*1000)
      exceedlegend_singleyear2 <- paste0("Probability\nExceeding ", thre2*1000)
      
      title_prev<-"Rate (per 1,000)"
    } else {
      
      thre1 = tab2[tab2$ID==indicator, ]$previous_est 
      thre2 = tab2[tab2$ID==indicator, ]$current_est  
      
      LABELS = scales::percent_format(accuracy = 1)
      scale_fill_gradientn_prevalence<-scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence",labels = LABELS)
      
      facet_labels = c(
        paste0(yr1, " National: ", thre1* 100, "%"),
        paste0(yr2, " National: ", thre2* 100, "%")
      )
      
      exceedlegend_singleyear1 <- paste0("Probability\nExceeding ", thre1*100, "%\n")
      exceedlegend_singleyear2 <- paste0("Probability\nExceeding ", thre2*100, "%\n")
      
      title_prev<-"Prevalence (%)"
    }
    
    
    
    
    res_adm11=res_adm12=c()
    
    yr1=min(surveys[surveys$country==country,]$year)
    results_path_yr1 <- file.path(source_path, middle_path, country, yr1)
    
    yr2=max(surveys[surveys$country==country,]$year)
    results_path_yr2 <- file.path(source_path, middle_path, country, yr2)
    
    qfile_adm1 <- file.path(results_path_yr1, paste0(ad1_name, indicator, ".qs"))
    qfile_adm2 <- file.path(results_path_yr2, paste0(ad1_name, indicator, ".qs"))
    
    
    res_adm11 <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("tryCatched: Failed to read ", qfile_adm1, ": ", e$message); return("failed") })
    res_adm12 <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("tryCatched:Failed to read ", qfile_adm2, ": ", e$message); return("failed") })
    
    
    
    
    ok11 <- has_data(res_adm11)
    ok12 <- has_data(res_adm12)
    
    # CI <- 0.9
    
    # ----- helper to make one-year map for a given metric -----
    # value_col is one of: "Prevalence", "sd", "cv", "width"
    plot_one_year <- function(df, year_label, value_col, legend_title,scale_layer) {
      if (missing(df) || is.null(df)) {
        return(make_placeholder(year_label, legend_title))
      }
      
      # out1, yr1, "Prevalence", "Prevalence",
      
      names(df)[names(df) == value_col] <- year_label
      mp <- mapPlot(
        data   = df,
        geo    = poly.adm1,
        by.data= "admin1.name",
        by.geo = "NAME_1",
        is.long= FALSE,                # single map, not faceted
        # value  = value_col,
        variables= year_label,
        # legend.label = year_label,
        size = .05,
        border = "gray50"
      ) + scale_layer
      # + ggtitle(as.character(year_label))
      mp
    }
    
    
    out1=out11=c()
    if (ok11) {
      
      out1 <- res_adm11$res.admin1[, c("admin1.name", "direct.est", "direct.se",
                                       "direct.lower", "direct.upper")]
      colnames(out1)[c(2, 3, 4, 5)] <- c("Prevalence", "sd", "lower", "upper")
      out1$version <- yr1
      out1$width <- out1$upper - out1$lower
      out1$oddratio<-log( (out1$Prevalence/(1-out1$Prevalence)) / ( thre1/ (1-thre1)))
      
    }
    
    if (ok12) {
      
      out11 <- res_adm12$res.admin1[, c("admin1.name", "direct.est", "direct.se",
                                        "direct.lower", "direct.upper")]
      colnames(out11)[c(2, 3, 4, 5)] <- c("Prevalence", "sd", "lower", "upper")
      out11$version <- yr2
      
      out11$width <- out11$upper - out11$lower
      out11$oddratio<- log( (out11$Prevalence/(1-out11$Prevalence)) / ( thre2/ (1-thre2)))
      
    }
    
    
    # --------------------
    # 
    # ----- map ad1  -----
    # 
    # --------------------
    if (ok11 && ok12) {
      # ----- both years available -----
      outadm2 <- rbind(out1, out11)
      outadm2$version <- factor(outadm2$version, levels = unique(outadm2$version))
      outadm2$width <- outadm2$upper - outadm2$lower
      
      # poly.adm1 <- poly.adm1 %>%
      #   dplyr::left_join(outadm2, by = c("NAME_1" = "admin1.name"))
      # hatching.gadm <- poly.adm1 %>%
      #   subset(is.na(width) )
      
      
      g1 <- mapPlot(data = outadm2, geo = poly.adm1,
                    by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
                    variable = "version", value = "Prevalence", legend.label = "",
                    direction = -1, ncol = 2, size =0.05, border = "gray50") + #, size = .05, border = "gray50"
        scale_fill_gradientn_prevalence
      
      
      g4 <- mapPlot(data = outadm2, geo = poly.adm1,
                    by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
                    variable = "version", value = "width", legend.label = "90% CI\nwidth",
                    ncol = 2, size = .05, border = "gray50") +
        scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth",labels = LABELS)
      
      
      
      rng <- range(outadm2$oddratio, na.rm = TRUE)
      g2 <- mapPlot(data = outadm2, geo = poly.adm1,
                    by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
                    variable = "version", value = "oddratio",
                    ncol = 2, size = .05, border = "gray50"
      )+
        ggplot2::theme (legend.text=ggplot2::element_text(size=12),
                        legend.title = ggplot2::element_text(size=14),
                        strip.text.x = ggplot2::element_text(size = 12),
                        legend.key.height = ggplot2::unit(1,'cm') )  +
        scale_fill_distiller(
          palette = "Spectral",
          values = scales::rescale(c(rng[1], 0, rng[2])),
          name    = "Log odds ratio"
        )
      
      
      g5 <- exceedPlot1(
        x = list(res_adm11,  res_adm12),
        facet_labels = facet_labels,
        threshold = c(thre1, thre2),
        exceed = TRUE, direction = infolist[infolist$ID == indicator, ]$direction ,
        geo = poly.adm1, by.geo = "NAME_1",
        facet_var = "year",                # column name to facet on
        ncol = 2, size = 0.05, border = "gray50"
      ) +
        scale_fill_distiller(name = "Exceedance\nProbability",
                             palette = "Spectral", direction = infolist[infolist$ID == indicator, ]$direction ,
                             limits = c(0,1), breaks = c(0.25,0.5,0.75),
                             labels = scales::percent_format(accuracy = 1) 
        )
      
    } else {
      # ----- at least one year missing -----
      # ----- compute metrics only when present -----
      
      
      
      # Prevalence
      g1_left <- if (ok11) {
        plot_one_year(out1, yr1, "Prevalence", "Prevalence",
                      scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence", labels=LABELS))
      } else {
        make_placeholder(yr1, "Prevalence")
      }
      g1_right <- if (ok12) {
        plot_one_year(out11, yr2, "Prevalence", "Prevalence",
                      scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence", labels=LABELS))
      } else {
        make_placeholder(yr2, "Prevalence")
      }
      g1 <- g1_left | g1_right
      
      
      
      # 90% CI width
      g4_left <- if (ok11) {
        plot_one_year(out1, yr1, "width", "90% CI\nwidth",
                      scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth", labels=LABELS))
        
      } else {
        make_placeholder(yr1, "90% CI width")
      }
      g4_right <- if (ok12) {
        plot_one_year(out11, yr2, "width", "90% CI\nwidth",
                      scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth", labels=LABELS))
      } else {
        make_placeholder(yr2, "90% CI width")
      }
      g4 <- g4_left | g4_right
      
      
      # Exceedance
      g5_left <- if (ok11) {
        
        
        
        exceedplot3(
          res_adm11,
          threshold = thre1,
          exceed = TRUE,
          yr = as.character(yr1),   # "2022"
          direction = infolist[infolist$ID == indicator, ]$direction,
          geo = poly.adm1,
          by.geo = "NAME_1",
          border = "gray80",
          size = 0
        )+
          scale_fill_distiller(exceedlegend_singleyear1, palette = "Spectral",
                               direction =  infolist[infolist$ID == indicator, ]$direction,
                               labels = scales::percent_format(accuracy = 1) )
        
        
        
        
        
      } else {
        make_placeholder(yr1, "Exceedance")
      }
      g5_right <- if (ok12) {
        
        
        
        exceedplot3(
          res_adm12,
          threshold = thre2,
          exceed = TRUE,
          yr = as.character(yr2),   # "2022"
          direction = infolist[infolist$ID == indicator, ]$direction,
          geo = poly.adm1,
          by.geo = "NAME_1",
          border = "gray80",
          size = 0
        )+scale_fill_distiller(exceedlegend_singleyear2, palette = "Spectral",
                               direction =  infolist[infolist$ID == indicator, ]$direction,
                               labels = scales::percent_format(accuracy = 1) )
        
        
      } else {
        make_placeholder(yr2, "Exceedance")
      }
      g5 <-   g5_left |   g5_right
      
      
      
      
    }
    
    
    
    # final_plot_t <- (g1/ g4/ g5/ g2) + plot_layout(heights = c(1,1,1,1))
    
    
    final_plot <- (g1/ g4/ g5) + plot_layout(heights = c(1,1,1))
    
    #
    #   final_plot <- (g1 / g4 / g5) +
    #     plot_layout(heights = c(1, 1, 1), guides = "collect") &
    #     theme(
    #       # legend.position = "right"
    #       # legend.justification = "center"  # centers the legends across rows
    #     )
    
    # final_plot
    
    
    # stack the four rows
    
    ggsave(final_plot,
           filename = file.path(plot_path_c, paste0("ad1_map_", indicator, ".png")),
           width = 6, height = 9, dpi = dpi)
    
    
    # ggsave(final_plot_t,
    #        filename = file.path(plot_path_c, paste0("rate_test_ad1_map_", indicator, ".png")),
    #        width = 6, height = 9, dpi = dpi)
    # 
    # 
    # --------------------
    # 
    # ----- ridge.   -----
    # 
    # --------------------
    
    if (ok11 && ok12) {
      
      
      
      if (infolist[infolist$ID == indicator, ]$direction == -1) {
        color.paletteHERE <- rev(brewer.pal(5, "RdYlGn"))
        colours =  RColorBrewer::brewer.pal(9, "Blues")
      } else {
        color.paletteHERE <- (brewer.pal(5, "RdYlGn"))
        colours =  RColorBrewer::brewer.pal(9, "Blues")
        
      }
      
      
      # low_col  <- color.paletteHERE[1]
      # high_col <- color.paletteHERE[5]
      low_col  <- colours[5]
      high_col <- colours[1]
      
      res_adm11$admin1_post= res_adm12$admin1_post-res_adm11$admin1_post
      
      
      v1<- ridgePlot1(res_adm12,year.plot = yr2,  by.year = TRUE, order = -1
      )+
        theme_bw() +
        theme(legend.position = "top",
              # legend.title = element_blank(),
              axis.text.y  = element_text(size = 14)
        )+ 
        scale_fill_gradientn( 
          name    = title_prev,
          colours = color.paletteHERE,
          labels  = LABELS) +
        scale_x_continuous(
          labels = LABELS
        )
      
      
      
      my_order <- levels(v1$data$region.name)
      
      
      v2<- ridgePlot1(res_adm11,  by.year = TRUE,
                      palette = color.paletteHERE,
                      custom.order = my_order
                      
      )+
        geom_vline(xintercept = 0,
                   linetype = "dashed",
                   color = "red",
                   linewidth = 0.6)+
        theme_bw() +
        theme(legend.position = "top",
              # legend.title = element_blank(),
              axis.text.y  = element_text(size = 14)
        )+
        scale_fill_gradientn(
          name    = "Change",
          colours = colours,
          labels  = LABELS
        )+ 
        scale_x_continuous(
          labels = LABELS
        )
      
      
      
      g1= v1+v2
      
    } else {
      # ----- at least one year missing -----
      # ----- compute metrics only when present -----
      
      
      if (infolist[infolist$ID == indicator, ]$direction == -1) {
        color.paletteHERE <- (brewer.pal(5, "RdYlGn"))
        colours =  rev(RColorBrewer::brewer.pal(9, "Blues"))
      } else {
        color.paletteHERE <- brewer.pal(5, "RdYlGn")
        colours =  rev(RColorBrewer::brewer.pal(9, "Blues"))
        
      }
      
      
      
      
      # Prevalence
      g1_left <- if (ok11) {
        ridgePlot1(res_adm11,year.plot = yr1,  by.year = TRUE, order = -1
        )+
          theme_bw() +
          theme(legend.position = "top",
                # legend.title = element_blank(),
                axis.text.y  = element_text(size = 14)
          )+ 
          scale_fill_gradientn( 
            name    = title_prev,
            colours = color.paletteHERE,
            labels  = LABELS) +
          scale_x_continuous(
            labels = LABELS
          )
      } else {
        make_placeholder(yr1, "Prevalence")
      }
      g1_right <- if (ok12) {
        ridgePlot1(res_adm12,year.plot = yr2,  by.year = TRUE, order = -1
        )+
          theme_bw() +
          theme(legend.position = "top",
                # legend.title = element_blank(),
                axis.text.y  = element_text(size = 14)
          )+ 
          scale_fill_gradientn( 
            name    = title_prev,
            colours = color.paletteHERE,
            labels  = LABELS) +
          scale_x_continuous(
            labels = LABELS
          )
      } else {
        make_placeholder(yr2, "Prevalence")
      }
      g1 <- g1_left | g1_right
      
      
    }
    
    # stack the four rows
    
    ggsave(g1,
           filename = file.path(plot_path_c, paste0("ridge_", indicator, ".png")),
           width = 15, height = 15, dpi = dpi)
    
    
    
    
  }
  
  
  
  
}



####################
# saveclustermap 
# report_clustermap.R
# admin 1 and admin 2 MAPs number clusters ,samples and events,  "Basic-yr1", "Basic-yr2"
# hepler: sample_info_map_static()
####################


sample_info_map_static <-function(model.gadm.level,
                                  strat.gadm.level=1,
                                  analysis.dat,
                                  gadm.list.visual,
                                  cluster.info){
  
  if (!requireNamespace("ggthemes", quietly = TRUE)) {
    stop("Package 'ggthemes' is required for this function. Please install it with install.packages('ggthemes').")
  }
  
  ### if no non-missing values, return NA
  if(sum(analysis.dat$value,na.rm=T)==0){
    return(NULL)
  }
  
  ### remove NAs and merge cluster info
  analysis.dat <- analysis.dat[!is.na(analysis.dat$value),]
  analysis.dat <- dplyr::left_join(analysis.dat,cluster.info$data,by="cluster")
  
  ### determine whether the gadm level is finer than stratification level
  if(model.gadm.level > strat.gadm.level){pseudo_level=2}else{pseudo_level=1}
  
  ### make plot for pseudo admin-1
  if(pseudo_level==1){
    
    sample.info.df <- analysis.dat %>%
      dplyr::group_by(admin1.name) %>%
      dplyr::summarise(n.samples=dplyr::n(),
                       n.clusters= dplyr::n_distinct(cluster),
                       n.events= sum(value,na.rm=T))
    
    adm.sf <- gadm.list.visual[[paste0('Admin-',model.gadm.level)]]
    adm.sf$admin1.name <- adm.sf[[paste0("NAME_",model.gadm.level)]]
    
    adm.sf <- adm.sf %>%
      dplyr::left_join(sample.info.df, by='admin1.name')
    
    
  }
  
  ### make plot for admin-2 or finer spatial scale
  
  if(pseudo_level>1){
    
    sample.info.df <- analysis.dat %>%
      dplyr::group_by(admin2.name.full) %>%
      dplyr::summarise(n.samples=dplyr::n(),
                       n.clusters= dplyr::n_distinct(cluster),
                       n.events= sum(value,na.rm=T))
    
    
    adm.sf <- gadm.list.visual[[paste0('Admin-',model.gadm.level)]]
    
    adm.sf$region.name <- adm.sf[[paste0("NAME_",model.gadm.level)]]
    adm.sf$upper.adm.name <- adm.sf[[paste0("NAME_",model.gadm.level-1)]]
    
    adm.sf <- adm.sf %>%
      dplyr::mutate(admin2.name.full = paste0(upper.adm.name, "_", region.name))
    
    
    adm.sf <- adm.sf %>%
      dplyr::left_join(sample.info.df, by='admin2.name.full')
    
    
  }
  
  n_cluster_map <- adm.sf %>%
    ggplot2::ggplot() +
    #ggspatial::annotation_map_tile(type = "osm",zoomin=0) +
    ggplot2::geom_sf(ggplot2::aes(geometry=geometry, fill=n.clusters), colour=NA) +
    ggplot2::geom_sf(data=adm.sf, ggplot2::aes(geometry=geometry), lwd=0.5, fill=NA) +
    ggplot2::scale_fill_distiller(palette="Blues", direction=1,name='Number of \n Clusters') +
    ggthemes::theme_map() +
    ggplot2::theme(legend.position="right")+
    ggplot2::theme(
      legend.position = "right",  # Position of the legend
      legend.text = ggplot2::element_text(size = 12),  # Larger text for the legend
      legend.title = ggplot2::element_text(size = 14),  # Larger title for the legend
      legend.key.size = ggplot2::unit(1, "cm")  # Larger key size
    )
  
  
  n_sample_map <- adm.sf %>%
    ggplot2::ggplot() +
    #ggspatial::annotation_map_tile(type = "osm",zoomin=0) +
    ggplot2::geom_sf(ggplot2::aes(geometry=geometry, fill=n.samples), colour=NA) +
    ggplot2::geom_sf(data=adm.sf, ggplot2::aes(geometry=geometry), lwd=0.5, fill=NA) +
    ggplot2::scale_fill_distiller(palette="Greens", direction=1,name='Number of \n samples') +
    ggthemes::theme_map() +
    ggplot2::theme(legend.position="right")+
    ggplot2::theme(
      legend.position = "right",  # Position of the legend
      legend.text = ggplot2::element_text(size = 12),  # Larger text for the legend
      legend.title = ggplot2::element_text(size = 14),  # Larger title for the legend
      legend.key.size = ggplot2::unit(1, "cm")  # Larger key size
    )
  
  n_event_map <- adm.sf %>%
    ggplot2::ggplot() +
    #ggspatial::annotation_map_tile(type = "osm",zoomin=0) +
    ggplot2::geom_sf(ggplot2::aes(geometry=geometry, fill=n.events), colour=NA) +
    ggplot2::geom_sf(data=adm.sf, ggplot2::aes(geometry=geometry), lwd=0.5, fill=NA) +
    ggplot2::scale_fill_distiller(palette="Oranges", direction=1,name='Number of \n events') +
    ggthemes::theme_map() +
    ggplot2::theme(legend.position="right")+
    ggplot2::theme(
      legend.position = "right",  # Position of the legend
      legend.text = ggplot2::element_text(size = 12),  # Larger text for the legend
      legend.title = ggplot2::element_text(size = 14),  # Larger title for the legend
      legend.key.size = ggplot2::unit(1, "cm")  # Larger key size
    )
  
  return(list(n_event_map=n_event_map,
              n_cluster_map=n_cluster_map,
              n_sample_map=n_sample_map,
              adm.sample.info =adm.sf))
  
}



saveclustermap<-function(
    country= "Nigeria",
    middle_path="Gates-results/Results",
    plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country),
    indicatorlist =infolist$ID  
){
  
  
  
  for (indicator in indicatorlist) {
    
    yr1=min(surveys[surveys$country==country,]$year)
    yr2=max(surveys[surveys$country==country,]$year)
    
    
    
    for (yr in c(yr1, yr2)) {
      
      results_path <- file.path(source_path, middle_path, country, yr)
      
      # load basic info for this survey/year
      load(file.path(results_path, "basic.Rdata"))
      
      # target file for this indicator & year
      qfile <- file.path(results_path, paste0(indicator, ".qs"))
      
      
      if (!file.exists(qfile)) {
        message("Skipping: ", indicator, " (file not found)")
        next  # move to next indicator
      }
      loaded_data <- qread(qfile)
      
      data<-loaded_data
      data <- data[rowSums(is.na(data)) == 0, ]
      
      
      
      gadm.list.visualt=list(
        `Admin-1`=poly.adm1,
        `Admin-2`=poly.adm2
      )
      
      dd=sample_info_map_static(
        model.gadm.level=1,
        strat.gadm.level=1,
        analysis.dat=data,
        gadm.list.visual=gadm.list.visualt,
        cluster.info=cluster.info
      )
      
      dd2=sample_info_map_static(
        model.gadm.level=2,
        strat.gadm.level=1,
        analysis.dat=data,
        gadm.list.visual=gadm.list.visualt,
        cluster.info=cluster.info
      )
      
      
      
      shrink_legend <- theme(
        # legend.key.size = unit(0.3, "cm"),   # box size
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 12)
      )
      
      # apply to each map
      p1 <- dd$n_cluster_map  + shrink_legend
      p2 <- dd2$n_cluster_map + shrink_legend
      p3 <- dd$n_event_map    + shrink_legend
      p4 <- dd2$n_event_map   + shrink_legend
      p5 <- dd$n_sample_map   + shrink_legend
      p6 <- dd2$n_sample_map  + shrink_legend
      
      # final_plot <- (p1 | p2) / (p3 | p4) / (p5 | p6)
      # final_plot
      
      # stack the four rows
      
      
      final_plot <- (p1 | p2) / (p3 | p4) / (p5 | p6) +
        plot_layout(heights = c(1, 1, 1), widths = c(1, 1))
      
      ggsave(
        final_plot,
        filename = file.path(plot_path_c, paste0("Basic-", indicator,"-", yr, ".png")),
        width = 10, height = 12, dpi = 300
      )
      
      
    }
    
    
  }
  
  
}





####################
# saveinterval_overlay 
# report_interval.R
# admin 1 and admin 2 interval and overlay : "overlay-" "interval-ad2-" "interval-ad1-"
# hepler: intervalplot1()
####################
intevalplot1=intervalplot1<-function (admin = 0, compare = FALSE, model = NULL, group = FALSE,
                                      sort_by = NULL, decreasing = FALSE,highlight_by_model = NULL,
                                      highlight_label = "Unstable variance" )
{
  if (compare) {
    if (admin == 0) {
      dt = data.frame(mean = NA, lower = NA, upper = NA,
                      model = NA, group = NA)
      for (i in 1:length(model)) {
        if (!is.null(model[[i]]$agg.natl) && colnames(model[[i]]$agg.natl[1]) ==
            "direct.est") {
          colnames(model[[i]]$agg.natl)[colnames(model[[i]]$agg.natl) ==
                                          "direct.est"] <- "mean"
          colnames(model[[i]]$agg.natl)[colnames(model[[i]]$agg.natl) ==
                                          "direct.lower"] <- "lower"
          colnames(model[[i]]$agg.natl)[colnames(model[[i]]$agg.natl) ==
                                          "direct.upper"] <- "upper"
          model[[i]]$agg.natl$model = names(model[i])
          dt[i, ] = model[[i]]$agg.natl[, c("mean",
                                            "lower", "upper", "model")]
        }
        else if (!is.null(model[[i]]$res.admin0)) {
          colnames(model[[i]]$res.admin0)[colnames(model[[i]]$res.admin0) ==
                                            "direct.est"] <- "mean"
          colnames(model[[i]]$res.admin0)[colnames(model[[i]]$res.admin0) ==
                                            "direct.lower"] <- "lower"
          colnames(model[[i]]$res.admin0)[colnames(model[[i]]$res.admin0) ==
                                            "direct.upper"] <- "upper"
          model[[i]]$res.admin0$model = names(model[i])
          dt[i, ] = model[[i]]$res.admin0[, c("mean",
                                              "lower", "upper", "model")]
        }
        else {
          model[[i]]$agg.natl$model = names(model[i])
          dt[i, ] = model[[i]]$agg.natl[, c("mean",
                                            "lower", "upper", "model")]
        }
      }
      if (group) {
        allgroup <- NULL
        for (i in 1:length(model)) {
          dt[i, ]$group = model[[i]]$group
          allgroup <- c(allgroup, model[[i]]$group)
        }
        dt$group <- factor(dt$group, levels = unique(allgroup))
      }
      rownames(dt) <- dt$model
      dt$model <- factor(dt$model, levels = names(model))
      if (group) {
        ggplot(dt, aes(y = model, x = mean, group = group)) +
          geom_point(aes(shape = group)) + geom_errorbarh(aes(xmin = lower,
                                                              xmax = upper), alpha = 1, height = 0.1) +
          theme_bw()
      }
      else {
        ggplot(dt, aes(y = model, x = mean)) + geom_point(aes()) +
          geom_errorbarh(aes(xmin = lower, xmax = upper),
                         alpha = 1, height = 0.1) + theme_bw()
      }
    }
    else if (admin == 1) {
      n.region.all <- 0
      for (i in 1:length(model)) {
        n.region <- dim(model[[i]]$agg.admin1)[1]
        if (is.null(n.region))
          n.region <- dim(model[[i]]$res.admin1)[1]
        if (is.null(n.region))
          stop(paste0("The following object in the list `model` cannot be parsed: ",
                      i))
        n.region.all <- n.region.all + n.region
      }
      dt <- data.frame(admin1.name = rep(NA, n.region.all),
                       mean = rep(NA, n.region.all), lower = rep(NA,
                                                                 n.region.all), upper = rep(NA, n.region.all),
                       model = rep(NA, n.region.all), group = rep(NA,
                                                                  n.region.all))
      allgroup <- NULL
      counter <- 1
      for (i in 1:length(model)) {
        if (is.null(model[[i]]$agg.admin1)) {
          if (colnames(model[[i]]$res.admin1)[2] ==
              "direct.est") {
            colnames(model[[i]]$res.admin1)[colnames(model[[i]]$res.admin1) ==
                                              "direct.est"] <- "mean"
            colnames(model[[i]]$res.admin1)[colnames(model[[i]]$res.admin1) ==
                                              "direct.lower"] <- "lower"
            colnames(model[[i]]$res.admin1)[colnames(model[[i]]$res.admin1) ==
                                              "direct.upper"] <- "upper"
          }
          model[[i]]$res.admin1$model = names(model[i])
          if (!is.null(model[[i]]$res.admin1$type)) {
            model[[i]]$res.admin1 = model[[i]]$res.admin1[model[[i]]$res.admin1$type ==
                                                            "full", ]
          }
          dd = dim(model[[i]]$res.admin1)[1]
          dt[counter:(counter + dd - 1), ] = model[[i]]$res.admin1[,
                                                                   c("admin1.name", "mean", "lower", "upper",
                                                                     "model")]
          counter <- counter + dd
        }
        else {
          if (colnames(model[[i]]$agg.admin1)[2] ==
              "direct.est") {
            colnames(model[[i]]$agg.admin1)[colnames(model[[i]]$agg.admin1) ==
                                              "direct.est"] <- "mean"
            colnames(model[[i]]$agg.admin1)[colnames(model[[i]]$agg.admin1) ==
                                              "direct.lower"] <- "lower"
            colnames(model[[i]]$agg.admin1)[colnames(model[[i]]$agg.admin1) ==
                                              "direct.upper"] <- "upper"
          }
          model[[i]]$agg.admin1$model = names(model[i])
          if (!is.null(model[[i]]$agg.admin1$type)) {
            model[[i]]$agg.admin1 = model[[i]]$agg.admin1[model[[i]]$agg.admin1$type ==
                                                            "full", ]
          }
          dd = dim(model[[i]]$agg.admin1)[1]
          dt[counter:(counter + dd - 1), 1:5] = model[[i]]$agg.admin1[,
                                                                      c("admin1.name", "mean", "lower", "upper",
                                                                        "model")]
          counter <- counter + dd
        }
        if (group) {
          if (is.null(model[[i]]$group))
            stop(paste0("Input model ", i, " does not have the group information"))
          dt[(1 + (i - 1) * dd):(i * dd), ]$group = model[[i]]$group
          allgroup <- c(allgroup, model[[i]]$group)
        }
      }
      dt <- dt[!is.na(dt$model), ]
      dt$model <- factor(dt$model, levels = names(model))
      if (!is.null(group))
        dt$group <- factor(dt$group, levels = unique(allgroup))
      if (!is.null(sort_by) && sort_by %in% dt$model) {
        tmp <- subset(dt, model == sort_by)
        tmp$mean_to_order <- tmp$mean
        tmp$mean_to_order[is.na(tmp$mean_order)] <- min(tmp$mean,
                                                        na.rm = TRUE)
        dt <- left_join(dt, tmp[, c("admin1.name", "mean_to_order")])
      }
      else {
        dt$mean_to_order <- NA
      }
      if (group) {
        ggplot(dt, aes(x = reorder(admin1.name, mean_to_order,
                                   decreasing = decreasing), y = mean, color = group,
                       shape = model)) + geom_point(position = position_dodge(width = 0.8)) +
          scale_shape_manual(values = c(0:5, 15:25)) +
          geom_errorbar(aes(ymin = lower, ymax = upper),
                        alpha = 0.8, position = position_dodge(width = 0.8),
                        width = 0.1) + scale_color_brewer(palette = "Set1") +
          theme_bw() + theme(axis.text.x = element_text(angle = 45,
                                                        hjust = 1)) + labs(title = "", x = "Region",
                                                                           y = "value") + theme(legend.title = element_text(size = 10),
                                                                                                legend.text = element_text(size = 10), legend.key.size = unit(1.5,
                                                                                                                                                              "lines"), axis.text.x = element_text(size = 10),
                                                                                                axis.text.y = element_text(size = 10))
      }
      else {
        ggplot(dt, aes(x = reorder(admin1.name, mean_to_order,
                                   decreasing = decreasing), y = mean, group = model,
                       color = model)) + geom_point(position = position_dodge(width = 0.8)) +
          scale_shape_manual(values = c(0:5, 15:25)) +
          geom_errorbar(aes(ymin = lower, ymax = upper,
                            group = model), alpha = 0.8, position = position_dodge(width = 0.8),
                        width = 0.1) + scale_color_brewer(palette = "Set1") +
          theme_bw() + theme(axis.text.x = element_text(angle = 45,
                                                        hjust = 1)) + labs(title = "", x = "Region",
                                                                           y = "value") + theme(legend.title = element_text(size = 10),
                                                                                                legend.text = element_text(size = 10), legend.key.size = unit(1.5,
                                                                                                                                                              "lines"), axis.text.x = element_text(size = 10),
                                                                                                axis.text.y = element_text(size = 10))
      }
    }
    else if (admin == 2) {
      
      
      
      n.region.all <- 0
      for (i in 1:length(model)) {
        n.region <- dim(model[[i]]$res.admin2)[1]
        n.region.all <- n.region.all + n.region
      }
      
      dt <- data.frame(admin2.name.full = rep(NA, n.region.all),
                       mean = rep(NA, n.region.all), lower = rep(NA,
                                                                 n.region.all), upper = rep(NA, n.region.all),
                       model = rep(NA, n.region.all), group = rep(NA,
                                                                  n.region.all), admin1.name = rep(NA, n.region.all),
                       admin2.name = rep(NA, n.region.all))
      allgroup <- NULL
      counter <- 1
      for (i in 1:length(model)) {
        if (colnames(model[[i]]$res.admin2)[2] == "direct.est") {
          colnames(model[[i]]$res.admin2)[colnames(model[[i]]$res.admin2) ==
                                            "direct.est"] <- "mean"
          colnames(model[[i]]$res.admin2)[colnames(model[[i]]$res.admin2) ==
                                            "direct.lower"] <- "lower"
          colnames(model[[i]]$res.admin2)[colnames(model[[i]]$res.admin2) ==
                                            "direct.upper"] <- "upper"
        }
        model[[i]]$res.admin2$model = names(model[i])
        if (!is.null(model[[i]]$res.admin2$type)) {
          model[[i]]$res.admin2 = model[[i]]$res.admin2[model[[i]]$res.admin2$type ==
                                                          "full", ]
        }
        dd = dim(model[[i]]$res.admin2)[1]
        dt[counter:(counter + dd - 1), c(1:5, 7, 8)] = model[[i]]$res.admin2[,
                                                                             c("admin2.name.full", "mean", "lower", "upper",
                                                                               "model", "admin1.name", "admin2.name")]
        counter <- counter + dd
        if (group) {
          if (is.null(model[[i]]$group))
            stop(paste0("Input model ", i, " does not have the group information"))
          dt[(1 + (i - 1) * dd):(i * dd), ]$group = model[[i]]$group
          allgroup <- c(allgroup, model[[i]]$group)
        }
      }
      dt <- dt[!is.na(dt$model), ]
      dt$model <- factor(dt$model, levels = names(model))
      if (!is.null(group))
        dt$group <- factor(dt$group, levels = unique(allgroup))
      if (!is.null(sort_by) && sort_by %in% dt$model) {
        tmp <- subset(dt, model == sort_by)
        tmp$mean_to_order <- tmp$mean
        tmp$mean_to_order[is.na(tmp$mean_order)] <- min(tmp$mean,
                                                        na.rm = TRUE)
        dt <- left_join(dt, tmp[, c("admin2.name.full",
                                    "mean_to_order")])
      }
      else {
        dt$mean_to_order <- NA
      }
      
      
      if (is.null(highlight_by_model) && !is.null(highlight_adm2)) {
        mm <- unique(as.character(dt$model))
        highlight_by_model <- setNames(rep(list(highlight_adm2), length(mm)), mm)
      }
      
      # Build per-row highlight flag using the row's model
      model_chr <- as.character(dt$model)
      hi_vec <- vapply(
        seq_len(nrow(dt)),
        function(i) {
          areas <- highlight_by_model[[ model_chr[i] ]]
          if (is.null(areas)) return(FALSE)
          dt$admin2.name.full[i] %in% areas
        },
        logical(1)
      )
      
      dt$mark <- ifelse(hi_vec, highlight_label, "Stable Variance")
      dt$mark[is.na(dt$mark)] <- "Stable Variance"
      dt$mark <- factor(dt$mark, levels = c("Stable Variance", highlight_label))
      
      pd <- position_dodge(width = 0.8)
      
      # Helpful sanity checks while debugging:
      # print(table(dt$mark, useNA = "ifany"))  # should show only "Other" and highlight_label
      
      ggplot(
        dt,
        aes(x = reorder(admin2.name, mean_to_order, decreasing = decreasing),
            y = mean, color = model, group = model, shape = mark)
      ) +
        # draw bars first so points sit on top
        geom_errorbar(aes(ymin = lower, ymax = upper), position = pd, width = 0.1, alpha = 0.8) +
        geom_point(position = pd, size = 2.2) +          # <- same size as the other plot
        
        geom_point(position = pd, size = .8, stroke = .8) +
        scale_color_brewer(palette = "Set1") +
        # use solid shapes; names MUST match levels(dt$mark)
        scale_shape_manual(values = setNames(c(16, 17), levels(dt$mark))) +
        labs(title = "", x = "Region", y = "value", shape = NULL) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        facet_wrap(~admin1.name, scales = "free_x")
    }
  }
  
}





saveinterval_overlay<-function( 
    country="Nigeria",
    ad1_name="new_res_adm1-",
    ad2_name="new_FH_adm2_fix_nest-",
    indicatorlist =infolist$ID,
    middle_path="Gates-results/Results",
    plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country),
    dpi=150
){
  
  
  
  for(country in country){
    
    
    yr1=min(surveys[surveys$country==country,]$year)
    yr2=max(surveys[surveys$country==country,]$year)
    
    
    
    for (indicator in indicatorlist) {
      
      if (infolist[infolist$ID == indicator, ]$direction == -1) {
        color.paletteHERE <- rev(brewer.pal(5, "RdYlGn"))
      } else {
        color.paletteHERE <- brewer.pal(5, "RdYlGn")
      }
      # CI <- 0.9
      
      
      
      type="percentage"
      if( indicator %in% c("CM_ECMR_C_NNF")){type="per1000"}
      
      
      # --- Titles depend on scale type ---
      if (type == "probability") {
        factorr=1
        title_prev  <- "Prevalence"
      } else if (type == "percentage") {
        
        title_prev  <- "Prevalence (%)"
        LABELS=scales::label_percent(scale = 100)
        factorr=100
        labels_overley= scales::label_number(suffix = "%")
        
      } else { # per1000
        
        title_prev  <- "Rate (per 1,000)"
        LABELS=scales::label_number(scale = 1000, accuracy = 1)
        factorr=1000
        labels_overley= scales::label_number(scale = 1)
        
      }
      
      
      yr1=min(surveys[surveys$country==country,]$year)
      results_path_yr1 <- file.path(source_path, middle_path, country, yr1)
      
      yr2=max(surveys[surveys$country==country,]$year)
      results_path_yr2 <- file.path(source_path, middle_path, country, yr2)
      
      qfile_adm1 <- file.path(results_path_yr1, paste0(ad2_name, indicator, ".qs"))
      qfile_adm2 <- file.path(results_path_yr2, paste0(ad2_name, indicator, ".qs"))
      
      
      old <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("Failed to read ", qfile_adm1, ": ", e$message); return("failed") })
      new <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("Failed to read ", qfile_adm2, ": ", e$message); return("failed") })
      
      qfile_adm11 <- file.path(results_path_yr1, paste0(ad1_name, indicator, ".qs"))
      qfile_adm22 <- file.path(results_path_yr2, paste0(ad1_name, indicator, ".qs"))
      
      
      res_adm11 <- tryCatch(qs::qread(qfile_adm11), error = function(e) { message("Failed to read ", qfile_adm1, ": ", e$message); return("failed") })
      res_adm12 <- tryCatch(qs::qread(qfile_adm22), error = function(e) { message("Failed to read ", qfile_adm2, ": ", e$message); return("failed") })
      
      
      
      # 
      # old <- ad2_store[[country]][[indicator]][[as.character(yr1)]]
      # new <- ad2_store[[country]][[indicator]][[as.character(yr2)]]
      ok21 <- has_data(old)
      ok22 <- has_data(new)
      # res_adm11 <- res_adm1_store[[country]][[indicator]][[as.character(yr1)]]
      # res_adm12 <- res_adm1_store[[country]][[indicator]][[as.character(yr2)]]
      ok11 <- has_data(res_adm11)
      ok12 <- has_data(res_adm12)
      
      
      # --------------------
      # 
      # ----- interval -----
      # 
      # --------------------
      
      if (ok11 && ok12  && ok21 && ok22 ) {
        # ----- both years available -----
        
        
        if( is.null(old$fixed_areas) &&   is.null(new$fixed_areas)){
          
          p <- intervalPlot(admin = 2,
                            compare = TRUE,
                            model = list("Baseline" = old,
                                         "Latest" = new )
                            
          )+
            theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))+
            guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
            theme(
              legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
            )+ scale_y_continuous(labels = LABELS) +
            labs(color = title_prev)
          
        }else{
          p <- intervalplot1(admin = 2,
                             compare = TRUE,
                             model = list("Baseline" = old,
                                          "Latest" = new ),
                             highlight_by_model = list(
                               "Baseline" = old$fixed_areas,
                               "Latest"   = new$fixed_areas
                             ),
                             highlight_label = "Unstable variance" )+
            theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))+
            guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
            theme(
              legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
            ) +  scale_y_continuous(labels = LABELS) +
            labs(color = title_prev)
          
        }
        
        
        p1<-
          intervalplot1(admin = 1, compare = TRUE, model = list(
            "Baseline"= res_adm11,
            "Latest"= res_adm12
            
          ), sort_by = "Latest")+
          theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1))+
          guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
          theme(
            legend.position = "bottom",
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
          )+  scale_y_continuous(labels = LABELS )+
          labs(color = title_prev)
        
        
        
        
        
      } else {
        # ----- at least one year missing -----
        # ----- compute metrics only when present -----
        
        
        
        # Prevalence
        p <- if (ok21) {
          
          if( is.null(old$fixed_areas) ){
            
            intervalPlot(admin = 2,
                         compare = TRUE,
                         model = list("Baseline" = old)
                         
            )+
              theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))+
              guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
              theme(
                legend.position = "bottom",
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
              )+  scale_y_continuous(labels = LABELS)+
              labs(color = title_prev)
          }else{
            intevalplot1(admin = 2,
                         compare = TRUE,
                         model = list("Baseline" = old ),
                         highlight_by_model = list(
                           "Baseline" = old$fixed_areas
                         ),
                         highlight_label = "Unstable variance" )+
              theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))+
              guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
              theme(
                legend.position = "bottom",
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
              )+  scale_y_continuous(labels = LABELS)+
              labs(color = title_prev)
          }
          
          
          
        } else if (ok22) {
          if(  is.null(new$fixed_areas)){
            
            intervalPlot(admin = 2,
                         compare = TRUE,
                         model = list(
                           "Latest" = new )
                         
            )+
              theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))+
              guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
              theme(
                legend.position = "bottom",
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
              )+  scale_y_continuous(labels =LABELS)+
              labs(color = title_prev)
          }else{
            intevalplot1(admin = 2,
                         compare = TRUE,
                         model = list(
                           "Latest" = new ),
                         highlight_by_model = list(
                           "Latest"   = new$fixed_areas
                         ),
                         highlight_label = "Unstable variance" )+
              theme(axis.text.x = element_text(size = 6, angle = 45, hjust = 1))+
              guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
              theme(
                legend.position = "bottom",
                axis.title.x = element_blank(),
                axis.title.y = element_blank()
              )+  scale_y_continuous(labels =LABELS)+
              labs(color = title_prev)
          }
          
        }else{
          make_placeholder("interval", "interval")
          
        }
        
        
        
        
        
        # Prevalence admin 1 
        p1<- if (ok11) {
          
          intervalPlot(admin = 1, compare = TRUE, model = list(
            "Baseline"= res_adm11
            
          ), sort_by = "Baseline")+
            theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1))+
            guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
            theme(
              legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
            )    +  scale_y_continuous(labels = LABELS)+
            labs(color = title_prev)
          
        } else if(ok12) {
          intervalPlot(admin = 1, compare = TRUE, model = list(
            "Latest"= res_adm12
          ), sort_by = "Latest")+
            theme(axis.text.x = element_text(size = 14, angle = 45, hjust = 1))+
            guides(shape = guide_legend(override.aes = list(size = 2))) +   # legend size
            theme(
              legend.position = "bottom",
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
            ) +  scale_y_continuous(labels = LABELS)+
            labs(color = title_prev) 
        }else{
          make_placeholder("interval", "interval")
          
        }
      }
      ggsave(p ,
             filename = file.path(plot_path_c, paste0("interval-ad2-", indicator, ".png")),
             width = 15, height = 15,
             dpi = dpi)
      
      ggsave(p1 ,
             filename = file.path(plot_path_c, paste0("interval-ad1-", indicator, ".png")),
             width = 15, height = 10,
             dpi = dpi)
      
      
      
      
      
      
      # --------------------
      # 
      # ----- overlay ------
      # 
      # --------------------
      
      
      out1=out11=c()
      if (ok11) {
        out1 <- res_adm11$res.admin1[, c("admin1.name", "direct.est", "direct.se",
                                         "direct.lower", "direct.upper")]
        colnames(out1)[c(2, 3, 4,5)] <- c("Prevalence", "sd","lower", "upper")
        out1$version <- yr1
        out1$Prevalence= out1$Prevalence*factorr
        out1$upper= out1$upper*factorr
        out1$lower= out1$lower*factorr
        out1$width <- out1$upper - out1$lower
        
      }
      if (ok12) {
        out11 <- res_adm12$res.admin1[, c("admin1.name", "direct.est", "direct.se",
                                          "direct.lower", "direct.upper")]
        colnames(out11)[c(2, 3, 4, 5)] <- c("Prevalence", "sd", "lower", "upper")
        out11$version <- yr2
        
        out11$Prevalence= out11$Prevalence*factorr
        out11$upper= out11$upper*factorr
        out11$lower= out11$lower*factorr
        out11$width <- out11$upper - out11$lower
        
      }
      
      out2=out22=c()
      if (ok21) {
        
        out2 <- old$res.admin2[, c("admin2.name.full", "median", "sd",
                                   "lower", "upper")]
        colnames(out2)[c(2, 3, 4,5)] <- c("Prevalence", "sd","lower", "upper")
        out2$version <- yr1
        out2$Prevalence= out2$Prevalence*factorr
        out2$upper= out2$upper*factorr
        out2$lower= out2$lower*factorr
        out2$width <- out2$upper - out2$lower
        
      }
      if (ok22) {
        out22 <- new$res.admin2[, c("admin2.name.full", "median", "sd",
                                    "lower", "upper")]
        colnames(out22)[c(2)] <- c("Prevalence")
        out22$version <- yr2
        out22$Prevalence= out22$Prevalence*factorr
        out22$upper= out22$upper*factorr
        out22$lower= out22$lower*factorr
        out22$width <- out22$upper - out22$lower
        
      }   
      
      if (ok11 && ok12  && ok21 && ok22 ) {
        # ----- both years available -----
        outadm2 <- rbind(out2, out22)
        outadm2$version <- factor(outadm2$version, levels = unique(outadm2$version))
        outadm2$width <- outadm2$upper - outadm2$lower
        
        
        outadm2ad1 <- rbind(out1, out11)
        outadm2ad1$version <- factor(outadm2ad1$version, levels = unique(outadm2ad1$version))
        outadm2ad1$width <- outadm2ad1$upper - outadm2ad1$lower
        
        
        outadm2ad1$admin=1
        outadm2ad1$model="Direct Estimates"
        outadm2ad1$admin2.name.full <- "All"
        outadm2$admin=2
        outadm2$model="FH"
        outadm2$admin1.name= sub("_.*", "", outadm2$admin2.name.full)
        
        out <- bind_rows(outadm2ad1, outadm2)
        out$admin <- factor(out$admin)
        tmp <- subset(out, model == "Direct Estimates" & admin == 1 & version == yr2 )
        tmp$mean_to_order <- tmp$Prevalence
        tmp$mean_to_order[is.na(tmp$mean_order)] <- min(tmp$Prevalence, na.rm = TRUE)
        out <- left_join(out, tmp[, c("admin1.name", "mean_to_order")])
        
        
        
        Overlay=ggplot(out) +
          aes(x = Prevalence, y = reorder(admin1.name, mean_to_order, decreasing = FALSE), shape = admin, color = admin, alpha = admin, size = admin) +
          geom_point(alpha=0.7) +
          scale_size_manual(title_prev, values = c(6, 1.4), labels = c("Admin 1", "Admin 2")) +
          scale_alpha_manual(title_prev, values = c(1, .6), labels = c("Admin 1", "Admin 2")) +
          scale_shape_manual(title_prev, values = c(108, 16), labels = c("Admin 1", "Admin 2")) +
          scale_color_manual(title_prev, values = c("violet", "royalblue"), labels = c("Admin 1", "Admin 2")) +
          # scale_color_manual("version", values = c("firebrick1", "royalblue"), labels = c(yr1, yr2)) +
          facet_wrap(~version, ncol = 2) +
          theme_bw() + xlab("") + ylab("") +
          theme(lengend.position = "bottom")+
          scale_x_continuous(labels = labels_overley)
        
        # +
        # ggtitle(paste0(infolist[infolist$ID==indicator,]$Description))
        
      } else if(!ok11 && !ok12 && !ok21 && !ok22
      ){
        Overlay=make_placeholder("overlay", "overlay")
        
      }else {
        # ----- at least one year missing -----
        # ----- compute metrics only when present -----
        
        
        
        # Prevalence
        g1_left <- if  (ok11&ok21) {
          
          outadm2 <- rbind(out2)
          outadm2$version <- factor(outadm2$version, levels = unique(outadm2$version))
          outadm2$width <- outadm2$upper - outadm2$lower
          
          
          outadm2ad1 <- rbind(out1)
          outadm2ad1$version <- factor(outadm2ad1$version, levels = unique(outadm2ad1$version))
          outadm2ad1$width <- outadm2ad1$upper - outadm2ad1$lower
          
          
          outadm2ad1$admin=1
          outadm2ad1$model="Direct Estimates"
          outadm2ad1$admin2.name.full <- "All"
          outadm2$admin=2
          outadm2$model="FH"
          outadm2$admin1.name= sub("_.*", "", outadm2$admin2.name.full)
          
          out <- bind_rows(outadm2ad1, outadm2)
          out$admin <- factor(out$admin)
          tmp <- subset(out, model == "Direct Estimates" & admin == 1 & version == yr1)
          tmp$mean_to_order <- tmp$Prevalence
          tmp$mean_to_order[is.na(tmp$mean_order)] <- min(tmp$Prevalence, na.rm = TRUE)
          out <- left_join(out, tmp[, c("admin1.name", "mean_to_order")])
          ggplot(out) +
            aes(x = Prevalence, y = reorder(admin1.name, mean_to_order, decreasing = FALSE), shape = admin, color = admin, alpha = admin, size = admin) +
            geom_point(alpha=0.7) +
            scale_size_manual(title_prev, values = c(6, 1.4), labels = c("Admin 1", "Admin 2")) +
            scale_alpha_manual(title_prev, values = c(1, .6), labels = c("Admin 1", "Admin 2")) +
            scale_shape_manual(title_prev, values = c(108, 16), labels = c("Admin 1", "Admin 2")) +
            scale_color_manual(title_prev, values = c("violet", "royalblue"), labels = c("Admin 1", "Admin 2")) +
            theme_bw() + xlab("") + ylab("")+
            scale_x_continuous(labels = labels_overley)
          
          
        } else {
          make_placeholder(yr1, "overlay")
        }
        g1_right <- if (ok12&ok22) {
          outadm2 <- rbind(out22)
          outadm2$version <- factor(outadm2$version, levels = unique(outadm2$version))
          outadm2$width <- outadm2$upper - outadm2$lower
          
          
          outadm2ad1 <- rbind(out11)
          outadm2ad1$version <- factor(outadm2ad1$version, levels = unique(outadm2ad1$version))
          outadm2ad1$width <- outadm2ad1$upper - outadm2ad1$lower
          
          
          outadm2ad1$admin=1
          outadm2ad1$model="Direct Estimates"
          outadm2ad1$admin2.name.full <- "All"
          outadm2$admin=2
          outadm2$model="FH"
          outadm2$admin1.name= sub("_.*", "", outadm2$admin2.name.full)
          
          out <- bind_rows(outadm2ad1, outadm2)
          out$admin <- factor(out$admin)
          tmp <- subset(out, model == "Direct Estimates" & admin == 1 & version == yr1)
          tmp$mean_to_order <- tmp$Prevalence
          tmp$mean_to_order[is.na(tmp$mean_order)] <- min(tmp$Prevalence, na.rm = TRUE)
          out <- left_join(out, tmp[, c("admin1.name", "mean_to_order")])
          ggplot(out) +
            aes(x = Prevalence, y = reorder(admin1.name, mean_to_order, decreasing = FALSE), shape = admin, color = admin, alpha = admin, size = admin) +
            geom_point(alpha=0.7) +
            scale_size_manual( title_prev, values = c(6, 1.4), labels = c("Admin 1", "Admin 2")) +
            scale_alpha_manual(title_prev, values = c(1, .6), labels = c("Admin 1", "Admin 2")) +
            scale_shape_manual(title_prev, values = c(108, 16), labels = c("Admin 1", "Admin 2")) +
            scale_color_manual(title_prev, values = c("violet", "royalblue"), labels = c("Admin 1", "Admin 2")) +
            theme_bw() + xlab("") + ylab("")+
            scale_x_continuous(labels = labels_overley)
          
          
        } else {
          make_placeholder(yr2, "overlay")
        }
        Overlay <- g1_left | g1_right
        
        
        
      }
      
      
      
      ggsave(Overlay,
             filename = file.path(plot_path_c, paste0("overlay-", indicator, ".png")),
             width = 10, height = 6, dpi = dpi)
      
      
      
      
      
      
      
      
    }
  }
  
}



####################
# savescatter 
# report_scatter.R
# admin 2 scatter plot: "scatter-"
# hepler: scatterPlot1()
####################


scatterPlot1 <- function(res1, value1, res2, value2, label1, label2,
                         by.res1, by.res2, title, highlight_ids = NULL) {
  res1$value_x <- res1[, value1]
  res2$value_y <- res2[, value2]
  df <- merge(res1, res2, by.x = by.res1, by.y = by.res2)
  
  # helper for highlighting rows by the join key
  hi_df <- if (!is.null(highlight_ids)) {
    subset(df, df[[by.res1]] %in% highlight_ids)
  } else NULL
  
  if (length(res1$value_x) == length(res2$value_y)) {
    lim <- range(c(df$value_x, df$value_y), na.rm = TRUE)
    p <- ggplot(df, aes(x = value_x, y = value_y)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_point(alpha = 0.5, color = "royalblue") +
      labs(title = title) + xlab(label1) + ylab(label2) +
      xlim(lim) + ylim(lim) + theme_bw()
  } else {
    # handle missings as you had
    missing <- if (length(res1$value_x) < length(res2$value_y)) {
      tmp <- subset(res2, !res2[[by.res2]] %in% res1[[by.res1]])
      tmp$value_x <- rep(min(c(df$value_x, df$value_y)), nrow(tmp))
      tmp
    } else {
      tmp <- subset(res1, !res1[[by.res1]] %in% res2[[by.res2]])
      tmp$value_y <- rep(min(c(df$value_x, df$value_y)), nrow(tmp))
      tmp
    }
    lim <- range(c(df$value_x, df$value_y), na.rm = TRUE)
    p <- ggplot(df, aes(x = value_x, y = value_y)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      geom_point(alpha = 0.5, color = "royalblue") +
      geom_point(data = missing, aes(x = value_x, y = value_y),
                 color = "red", shape = 17) +
      labs(title = title) + xlab(label1) + ylab(label2) +
      xlim(lim) + ylim(lim) + theme_bw()
  }
  
  # add red overlay for highlighted areas (both branches)
  if (!is.null(hi_df) && nrow(hi_df)) {
    p <- p + geom_point(
      data = hi_df,
      aes(x = value_x, y = value_y),
      inherit.aes = FALSE,
      color = "red",
      size = 1.5,
      stroke = 1.2,
      shape = 15
      # for hollow red circles instead: shape = 21, fill = NA, stroke = 1.2
    )
  }
  
  p
}




savescatter <- function(
    country       = "Nigeria",
    ad2_name      = "new_FH_adm2_fix_nest-",
    ad2_name_dir  = "new_res_adm2_fix-",
    middle_path   = "Gates-results/Results",
    plot_path_c   = file.path(source_path, "Gates-results/ReportPlots", country),
    indicatorlist = infolist$ID,
    dpi           = 150
) {
  
  # Ensure output directory exists
  if (!dir.exists(plot_path_c)) {
    dir.create(plot_path_c, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Years and paths (don’t recompute inside loop)
  surv_cty <- surveys[surveys$country == country, ]
  yr1 <- min(surv_cty$year)
  yr2 <- max(surv_cty$year)
  
  results_path_yr1 <- file.path(source_path, middle_path, country, yr1)
  results_path_yr2 <- file.path(source_path, middle_path, country, yr2)
  
  read_qs_safe <- function(path) {
    tryCatch(
      qs::qread(path),
      error = function(e) {
        message("Failed to read ", path, ": ", e$message)
        NULL
      }
    )
  }
  
  
  
  
  
  for (indicator in indicatorlist) {
    
    type="percentage"
    if( indicator %in% c("CM_ECMR_C_NNF")){type="per1000"}
    
    
    # --- Titles depend on scale type ---
    if (type == "probability") {
      factorr=1
      title_prev  <- "Prevalence"
      title_width <- "90% CI width"
    } else if (type == "percentage") {
      factorr=100
      title_prev  <- "Prevalence (%)"
      title_width <- "90% CI width (%)"
      labels_scatter<- scales::label_number(suffix = "%")
      
    } else { # per1000
      factorr=1000
      title_prev  <- "Rate (per 1,000)"
      title_width <- "90% CI width (per 1,000)"
      labels_scatter<- scales::label_number(scale = 1)
    }
    # axis labels are always just Smoothed / Unsmoothed
    val_xlab   <- "Unsmoothed"
    val_ylab   <- "Smoothed"
    width_xlab <- "Unsmoothed"
    width_ylab <- "Smoothed"
    
    # ---- Load results ----
    old       <- read_qs_safe(file.path(results_path_yr1, paste0(ad2_name,     indicator, ".qs")))
    new       <- read_qs_safe(file.path(results_path_yr2, paste0(ad2_name,     indicator, ".qs")))
    res_adm11 <- read_qs_safe(file.path(results_path_yr1, paste0(ad2_name_dir, indicator, ".qs")))
    res_adm12 <- read_qs_safe(file.path(results_path_yr2, paste0(ad2_name_dir, indicator, ".qs")))
    
    ok11 <- has_data(res_adm11)  # direct yr1
    ok12 <- has_data(res_adm12)  # direct yr2
    ok21 <- has_data(old)        # smoothed yr1
    ok22 <- has_data(new)        # smoothed yr2
    
    # ---- Put everything on the same scale ----
    # direct: direct.est in %, width = 90% CI width in percentage points
    if (ok11) {
      res_adm11$res.admin2$direct.est <-
        res_adm11$res.admin2$direct.est * factorr
      res_adm11$res.admin2$width <-
        (res_adm11$res.admin2$direct.upper -
           res_adm11$res.admin2$direct.lower) * factorr
    }
    
    if (ok12) {
      res_adm12$res.admin2$direct.est <-
        res_adm12$res.admin2$direct.est * factorr
      res_adm12$res.admin2$width <-
        (res_adm12$res.admin2$direct.upper -
           res_adm12$res.admin2$direct.lower) * factorr
    }
    
    # smoothed: mean in %, width = 90% CI width in percentage points
    if (ok21) {
      old$res.admin2$median  <- old$res.admin2$median * factorr
      old$res.admin2$width <- (old$res.admin2$upper -
                                 old$res.admin2$lower) * factorr
    }
    
    if (ok22) {
      new$res.admin2$median  <- new$res.admin2$median * factorr
      new$res.admin2$width <- (new$res.admin2$upper -
                                 new$res.admin2$lower) * factorr
    }
    
    # ---- Build plots ----
    if (ok11 && ok12 && ok21 && ok22) {
      # Both years available, all models
      
      s5 <- scatterPlot1(
        res1    = res_adm11$res.admin2,
        value1  = "direct.est",
        res2    = old$res.admin2,
        value2  = "median",
        by.res1 = "admin2.name.full",
        by.res2 = "admin2.name.full",
        title   = paste0(yr1, " ", title_prev),
        label1  = val_xlab,
        label2  = val_ylab
      )
      # grab the common range currently used by the plot
      lim <- range(ggplot_build(s5)$data[[2]][, c("x","y")], na.rm = TRUE)
      pad <- 0.03 * diff(lim)
      lim <- lim + c(-pad, pad)
      s5<-s5 +
        coord_equal(xlim = lim, ylim = lim, expand = FALSE) +
        scale_x_continuous(limits = lim, labels = labels_scatter) +
        scale_y_continuous(limits = lim, labels = labels_scatter)
      
      s6 <- scatterPlot1(
        res1    = res_adm12$res.admin2,
        value1  = "direct.est",
        res2    = new$res.admin2,
        value2  = "median",
        by.res1 = "admin2.name.full",
        by.res2 = "admin2.name.full",
        title   = paste0(yr2, " ", title_prev),
        label1  = val_xlab,
        label2  = val_ylab
      )
      # grab the common range currently used by the plot
      lim <- range(ggplot_build(s6)$data[[2]][, c("x","y")], na.rm = TRUE)
      pad <- 0.03 * diff(lim)
      lim <- lim + c(-pad, pad)
      s6<-s6 +
        coord_equal(xlim = lim, ylim = lim, expand = FALSE) +
        scale_x_continuous(limits = lim, labels = labels_scatter) +
        scale_y_continuous(limits = lim, labels = labels_scatter)
      
      
      s55 <- scatterPlot1(
        res1    = res_adm11$res.admin2,
        value1  = "width",
        res2    = old$res.admin2,
        value2  = "width",
        by.res1 = "admin2.name.full",
        by.res2 = "admin2.name.full",
        title   = paste0(yr1, " ", title_width),
        label1  = width_xlab,
        label2  = width_ylab
      )
      # grab the common range currently used by the plot
      lim <- range(ggplot_build(s55)$data[[2]][, c("x","y")], na.rm = TRUE)
      pad <- 0.03 * diff(lim)
      lim <- lim + c(-pad, pad)
      s55<-s55 +
        coord_equal(xlim = lim, ylim = lim, expand = FALSE) +
        scale_x_continuous(limits = lim, labels = labels_scatter) +
        scale_y_continuous(limits = lim, labels = labels_scatter)
      
      s66 <- scatterPlot1(
        res1    = res_adm12$res.admin2,
        value1  = "width",
        res2    = new$res.admin2,
        value2  = "width",
        by.res1 = "admin2.name.full",
        by.res2 = "admin2.name.full",
        title   = paste0(yr2, " ", title_width),
        label1  = width_xlab,
        label2  = width_ylab
      )
      # grab the common range currently used by the plot
      lim <- range(ggplot_build(s66)$data[[2]][, c("x","y")], na.rm = TRUE)
      pad <- 0.03 * diff(lim)
      lim <- lim + c(-pad, pad)
      s66<-s66 +
        coord_equal(xlim = lim, ylim = lim, expand = FALSE) +
        scale_x_continuous(limits = lim, labels = labels_scatter) +
        scale_y_continuous(limits = lim, labels = labels_scatter)
      
      
      
      scatter <- (s5 + s6) / (s55 + s66)
      
    } else {
      # At least one year missing – use placeholders where needed
      
      g1_left <- if (ok11 && ok21) {
        s5 <- scatterPlot1(
          res1    = res_adm11$res.admin2,
          value1  = "direct.est",
          res2    = old$res.admin2,
          value2  = "median",
          by.res1 = "admin2.name.full",
          by.res2 = "admin2.name.full",
          title   = paste0(yr1, " ", title_prev),
          label1  = val_xlab,
          label2  = val_ylab
        )
        # grab the common range currently used by the plot
        lim <- range(ggplot_build(s5)$data[[2]][, c("x","y")], na.rm = TRUE)
        pad <- 0.03 * diff(lim)
        lim <- lim + c(-pad, pad)
        s5<-s5 +
          coord_equal(xlim = lim, ylim = lim, expand = FALSE) +
          scale_x_continuous(limits = lim, labels = labels_scatter) +
          scale_y_continuous(limits = lim, labels = labels_scatter)
        
        
        s55 <- scatterPlot1(
          res1    = res_adm11$res.admin2,
          value1  = "width",
          res2    = old$res.admin2,
          value2  = "width",
          by.res1 = "admin2.name.full",
          by.res2 = "admin2.name.full",
          title   = paste0(yr1, " ", title_width),
          label1  = width_xlab,
          label2  = width_ylab
        )
        lim <- range(ggplot_build(s55)$data[[2]][, c("x","y")], na.rm = TRUE)
        pad <- 0.03 * diff(lim)
        lim <- lim + c(-pad, pad)
        s55<-s55 +
          coord_equal(xlim = lim, ylim = lim, expand = FALSE) +
          scale_x_continuous(limits = lim, labels = labels_scatter) +
          scale_y_continuous(limits = lim, labels = labels_scatter)
        
        
        s5 / s55
      } else {
        make_placeholder(yr1, "Scatter")
      }
      
      g1_right <- if (ok12 && ok22) {
        s6 <- scatterPlot1(
          res1    = res_adm12$res.admin2,
          value1  = "direct.est",
          res2    = new$res.admin2,
          value2  = "median",
          by.res1 = "admin2.name.full",
          by.res2 = "admin2.name.full",
          title   = paste0(yr2, " ", title_prev),
          label1  = val_xlab,
          label2  = val_ylab
        )
        lim <- range(ggplot_build(s6)$data[[2]][, c("x","y")], na.rm = TRUE)
        pad <- 0.03 * diff(lim)
        lim <- lim + c(-pad, pad)
        s6<-s6 +
          coord_equal(xlim = lim, ylim = lim, expand = FALSE) +
          scale_x_continuous(limits = lim, labels = labels_scatter) +
          scale_y_continuous(limits = lim, labels = labels_scatter)
        
        s66 <- scatterPlot1(
          res1    = res_adm12$res.admin2,
          value1  = "width",
          res2    = new$res.admin2,
          value2  = "width",
          by.res1 = "admin2.name.full",
          by.res2 = "admin2.name.full",
          title   = paste0(yr2, " ", title_width),
          label1  = width_xlab,
          label2  = width_ylab
        )
        lim <- range(ggplot_build(s66)$data[[2]][, c("x","y")], na.rm = TRUE)
        pad <- 0.03 * diff(lim)
        lim <- lim + c(-pad, pad)
        s66<-s66 +
          coord_equal(xlim = lim, ylim = lim, expand = FALSE) +
          scale_x_continuous(limits = lim, labels = labels_scatter) +
          scale_y_continuous(limits = lim, labels = labels_scatter)
        
        s6 / s66
      } else {
        make_placeholder(yr2, "Scatter")
      }
      
      scatter <- g1_left | g1_right
    }
    
    # ---- Save plot ----
    ggsave(
      filename = file.path(plot_path_c, paste0("scatter-", indicator, ".png")),
      plot     = scatter,
      width    = 10,
      height   = 10,
      dpi      = dpi
    )
  }
}

