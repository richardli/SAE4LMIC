
save_tab2_from_files <- function(country, adm_name = "res_adm0-", ids = indicatorlist) {
  # surveys for this country (sorted by year)
  df_surv <- dplyr::filter(surveys, country == !!country) |> dplyr::arrange(year)
  if (nrow(df_surv) == 0) stop("No surveys for country = ", country)
  years <- df_surv$year
  
  # helper: read (est, lower, upper) triplet from one file
  read_triplet <- function(ind, yr) {
    results_path <- file.path(source_path, "Gates-results", "Results", country, yr)
    qfile <- resolve_qs(results_path, adm_name, ind)
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




exceedplot2<- function (x, exceed = TRUE, direction = 1, threshold = NA, geo = geo, 
                        by.geo = NULL, ylim = NULL, ...) {
  if (is.na(threshold)) 
    stop("A numerical threshold need to be specified.")
  x_att <- attributes(x)
  if (x_att$class %in% c( "directEST","fhModel", "clusterModel")) {
    if ("admin2_post" %in% x_att$names) {
      samples = x$admin2_post
    }
    else {
      samples = x$admin1_post
    }
  }
  dat <- data.frame(region.name = x_att$domain.names, value = NA)
  for (i in 1:dim(samples)[2]) {
    dat$value[i] <- sum(samples[, i] > threshold)/dim(samples)[1]
    if (!exceed) 
      dat$value[i] <- sum(samples[, i] < threshold)/dim(samples)[1]
  }
  g <- SUMMER::mapPlot(data = dat, geo = geo, by.data = "region.name", 
                       by.geo = by.geo, variable = "value", removetab = TRUE, 
                       legend.label = "Probability", direction = direction, 
                       ylim = ylim, ...)
  return(g)
}



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

library(tibble)

library(rmapshaper)
library(SUMMER)
library(surveyPrev)
library(qs)
library(ggplot2)
library(RColorBrewer)
library(patchwork)

library(here)
# source_path<- "/Users/qianyu/Dropbox/binary_code/pcg/GATES/"
source_path <- dirname(here::here())
# source_path is the path for this github repository 
git_path <- here::here()




infolist <- read.csv(file.path(git_path, "info", "infolist.csv"))
surveys <- read.csv(file.path(git_path,  "info", "surveyslist.csv"))



country="Nigeria"
ad1_name="new_res_adm1-"
ad2_name="new_FH_adm2_fix_nest-"
indicatorlist =infolist$ID




country=""
ad1_name="new_res_adm1-"
ad2_name="new_FH_adm2_fix_nest-"
indicator="RH_PCCT_C_DY2"
adm_name = "new_res_adm0-"

adm_name = "new_res_adm0-"
country="Burkina Faso"
ad1_name="new_res_adm1-"
ad2_name="new_FH_adm2_fix_nest-"
# indicatorlist= "CO_MOBB_W_MOB"

middle_path="Gates-results/Results"
plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country)


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
    exceedlegend_singleyear1 <- paste0("Probability\nExceeding ", thre1, "%\n")
    exceedlegend_singleyear2 <- paste0("Probability\nExceeding ", thre2, "%\n")
    
  }
  
  
  
  
  # yr1=min(surveys[surveys$country==country,]$year)
  results_path_yr1 <- file.path(source_path, middle_path, country, yr1)
  
  # yr2=max(surveys[surveys$country==country,]$year)
  results_path_yr2 <- file.path(source_path, middle_path, country, yr2)
  
  qfile_adm1 <- resolve_qs(results_path_yr1, ad2_name, indicator)
  qfile_adm2 <- resolve_qs(results_path_yr2, ad2_name, indicator)
  
  
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
                                "lower", "upper", "cv")]
    colnames(out1)[c(2)] <- c("Prevalence")
    out1$version <- yr1

    out1$width <- out1$upper - out1$lower
    # out1$oddratio<- log( (out1$Prevalence/(1-out1$Prevalence)) / ( thre1/ (1-thre1)))
    out1$oddratio<- ( (out1$Prevalence/(1-out1$Prevalence)) / ( thre1/ (1-thre1)))
    
    
  }
  
  if (ok12) {
  
    
    out11 <- new$res.admin2[, c("admin2.name.full", "median", "sd",
                                 "lower", "upper", "cv")]
    colnames(out11)[c(2)] <- c("Prevalence")
    out11$version <- yr2

    out11$width <- out11$upper - out11$lower
    # out11$oddratio<- log( (out11$Prevalence/(1-out11$Prevalence)) / ( thre2/ (1-thre2)))
    out11$oddratio<- ( (out11$Prevalence/(1-out11$Prevalence)) / ( thre2/ (1-thre2)))
    
    
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
        # values = scales::rescale(c(rng[1], 0, rng[2])),
        values = scales::rescale(c(rng[1], 1, rng[2])),
        
        name    = "Odds ratio"
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
  
  if (country == "Malawi") {
    g5 <- g5 + theme(strip.text = element_text(size = 7))
  }
  final_plot <- (g1 / g4/ g5) + plot_layout(heights = c(1,1,1))
  
  # stack the four rows
  
  ggsave(final_plot,
         filename = file.path(plot_path_c, paste0("ad2_map_", indicator, ".png")),
         width = 6, height = 9, dpi = dpi)

  # 
  # final_plot_t <- (g1 / g4/ g5 /g2) + plot_layout(heights = c(1,1,1,1))
  # # 
  # ggsave(final_plot_t,
  #        filename = file.path(plot_path_c, paste0("rate_test_ad2_map_", indicator, ".png")),
  #        width = 6, height = 9, dpi = dpi)
}


# ad1_name<-"new_res_adm1-"
# results_path<-file.path(source_path, "Gates-results/Results", country, yr1)
load(file.path(results_path, "basic.Rdata"))
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
  
  qfile_adm1 <- resolve_qs(results_path_yr1, ad1_name, indicator)
  qfile_adm2 <- resolve_qs(results_path_yr2, ad1_name, indicator)
  
  
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
    out1$oddratio<-( (out1$Prevalence/(1-out1$Prevalence)) / ( thre1/ (1-thre1)))
    
  }
  
  if (ok12) {

    out11 <- res_adm12$res.admin1[, c("admin1.name", "direct.est", "direct.se",
                                     "direct.lower", "direct.upper")]
    colnames(out11)[c(2, 3, 4, 5)] <- c("Prevalence", "sd", "lower", "upper")
    out11$version <- yr2
  
    out11$width <- out11$upper - out11$lower
    out11$oddratio<- ( (out11$Prevalence/(1-out11$Prevalence)) / ( thre2/ (1-thre2)))
    
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
    g23 <- mapPlot(data = outadm2, geo = poly.adm1,
                  by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
                  variable = "version", value = "oddratio",
                  ncol = 2, size = .05, border = "gray50"
                  )+     scale_fill_distiller(
        palette = "Spectral",
        values = scales::rescale(c(rng[1], 1, rng[2])),
        name    = "Odds ratio"
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
    
    # right before this existing line:
    # final_plot <- (g1 / g4/ g5) + plot_layout(heights = c(1,1,1))
    
   
    
    
  }
  
  
  
  # final_plot_t <- (g1/ g4/ g5/ g23) + plot_layout(heights = c(1,1,1,1))
  # 
  if (country == "Malawi") {
    g5 <- g5 + theme(strip.text = element_text(size = 7))
  }
  
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
  
  # 
  # ggsave(final_plot_t,
  #        filename = file.path(plot_path_c, paste0("rate_test_ad1_map_", indicator, ".png")),
  #        width = 6, height = 9, dpi = dpi)
  # ggsave(  g2,
  #        filename = file.path(plot_path_c, paste0("odd_r_map_2", indicator, ".png")),
  #        width = 6, height = 3, dpi = 300)
  # ggsave(  g23,
  #          filename = file.path(plot_path_c, paste0("odd_r_map_1", indicator, ".png")),
  #          width = 6, height = 3, dpi = 300)
  # # 
  

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


savemaps_ridge(country="Nigeria",
         ad2_name="new_FH_adm2_fix_nest-",
         ad1_name="new_res_adm1-",
         indicatorlist =infolist$ID,
         adm_name = "res_adm0-",
         middle_path="Gates-results/Results",
         plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))




savemaps_ridge(country="Burkina Faso",
               ad2_name="new_FH_adm2_fix_nest-",
               ad1_name="new_res_adm1-",
               indicatorlist =indicator,
               adm_name = "new_res_adm0-",
               middle_path="Gates-results/Results",
               plot_path_c=file.path(source_path,"Gates-results/ReportPlots","Burkina Faso"))


savemaps_ridge(country="Malawi",
               ad2_name="FH_adm2_fix_nest-",
               ad1_name="res_adm1-",
               indicatorlist =indicatorlist,
               adm_name = "res_adm0-",
               middle_path="Gates-results/Results",
               plot_path_c=file.path(source_path,"Gates-results/ReportPlots","Malawi"))



countryList <- "Malawi"
for (ctry in countryList) {
  message("Processing: ", ctry)
  try(savemaps(ctry), silent = TRUE)
}





