


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






savescatter( country= "Nigeria",
             ad2_name= "new_FH_adm2_fix_nest-",
             ad2_name_dir=  "new_res_adm2_fix-",
             middle_path="Gates-results/Results",
             plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))



savescatter( country= country,
             ad2_name= "new_FH_adm2_fix_nest-",
             ad2_name_dir=  "new_res_adm2_fix-",
             middle_path="Gates-results/Results",
             indicatorlist = infolist$ID[18],
             plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))




countryList <- unique(surveys$country)
for (ctry in countryList) {
  message("Processing: ", ctry)
  try(savescatter(ctry), silent = TRUE)
}



