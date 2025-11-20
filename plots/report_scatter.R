


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


savescatter<-function(
      country= "Nigeria",
      ad2_name= "new_FH_adm2_fix_nest-",
      ad2_name_dir=  "new_res_adm2_fix-",
      middle_path="Gates-results/Results",
      plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country),
      indicatorlist =infolist$ID
  ){
  
  
  
for (indicator in indicatorlist) {
  
  

  
  yr1=min(surveys[surveys$country==country,]$year)
  yr2=max(surveys[surveys$country==country,]$year)
  
  
  res_adm11=res_adm12=old=old=new=c()
  
  yr1=min(surveys[surveys$country==country,]$year)
  results_path_yr1 <- file.path(source_path, middle_path ,country, yr1)
  
  yr2=max(surveys[surveys$country==country,]$year)
  results_path_yr2 <- file.path(source_path, middle_path, country, yr2)
  

  qfile_adm10 <- file.path(results_path_yr1, paste0(ad2_name, indicator, ".qs"))
  qfile_adm20 <- file.path(results_path_yr2, paste0(ad2_name, indicator, ".qs"))
  old <- tryCatch(qs::qread(qfile_adm10), error = function(e) { message("Failed to read ", qfile_adm10, ": ", e$message); return(NULL) })
  new <- tryCatch(qs::qread(qfile_adm20), error = function(e) { message("Failed to read ", qfile_adm20, ": ", e$message); return(NULL) })
  

  qfile_adm1 <- file.path(results_path_yr1, paste0(ad2_name_dir, indicator, ".qs"))
  qfile_adm2 <- file.path(results_path_yr2, paste0(ad2_name_dir, indicator, ".qs"))
  res_adm11 <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("Failed to read ", qfile_adm1, ": ", e$message); return(NULL) })
  res_adm12 <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("Failed to read ", qfile_adm2, ": ", e$message); return(NULL) })
  
  
  
  
  
  # 
  # old <- ad2_store[[country]][[indicator]][[as.character(yr1)]]
  # new <- ad2_store[[country]][[indicator]][[as.character(yr2)]]
  ok21 <- has_data(old)
  ok22 <- has_data(new)
  # res_adm11 <- ad2_store_direct[[country]][[indicator]][[as.character(yr1)]]
  # res_adm12 <- ad2_store_direct[[country]][[indicator]][[as.character(yr2)]]
  ok11 <- has_data(res_adm11)
  ok12 <- has_data(res_adm12)
  out1=out11=c()
  if (ok11) {
    
    
    # res_adm11$res.admin2$cv1 <-  with(res_adm11$res.admin2,
    #                             sd / (mean * (1 - mean)))
    
    res_adm11$res.admin2$direct.est=res_adm11$res.admin2$direct.est*100
    res_adm11$admin2_post=res_adm11$admin2_post*100
    # qs11 <- apply(res_adm11$admin2_post, 2, quantile,
    #               probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
    # res_adm11$res.admin2$lower <- qs11[1, ]
    # res_adm11$res.admin2$upper <- qs11[2, ]
    res_adm11$res.admin2$width <-  res_adm11$res.admin2$direct.upper -  res_adm11$res.admin2$direct.lower
    
    
    
    
    # out2 <- res_adm11$res.admin2[, c("admin2.name.full", "direct.est", "direct.se",
    #                            "cv", "direct.lower", "direct.upper")]
    # colnames(out2)[c(2, 3, 5, 6)] <- c("Prevalence", "sd", "lower", "upper")
    # out2$version <- yr1
    # out2$Prevalence= out2$Prevalence*100
    # out2$upper= out2$upper*100
    # out2$lower= out2$lower*100
    # out2$width <- out2$upper - out2$lower
    
  }
  if (ok12) {
    # new$res.admin2$cv1 <- with(new$res.admin2,
    #                                  pmax(sd / mean, sd / (1 - mean))
    # )
    # res_adm12$res.admin2$cv1 <-  with(res_adm12$res.admin2,
    #                             sd / (mean * (1 - mean)))
    
    res_adm12$res.admin2$direct.est=res_adm12$res.admin2$direct.est*100
    # res_adm12$admin2_post=res_adm12$admin2_post*100
    # qs12 <- apply(res_adm12$admin2_post, 2, quantile,
    #               probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
    # res_adm12$res.admin2$lower <- qs12[1, ]
    # res_adm12$res.admin2$upper <- qs12[2, ]
    res_adm12$res.admin2$width <-  res_adm12$res.admin2$direct.upper -  res_adm12$res.admin2$direct.lower
    
    # out22 <- res_adm12$res.admin2[, c("admin2.name.full", "direct.est", "direct.se",
    #                                   "cv", "direct.lower", "direct.upper")]
    # colnames(out22)[c(2, 3, 5, 6)] <- c("Prevalence", "sd", "lower", "upper")
    # out22$version <- yr2
    # out22$Prevalence= out22$Prevalence*100
    # out22$upper= out22$upper*100
    # out22$lower= out22$lower*100
    # out22$width <- out22$upper - out22$lower
    
  }
  ok21 <- has_data(old)
  ok22 <- has_data(new)
  out2=out22=c()
  if (ok21) {
    #
    #
    # old$res.admin2$cv1 <-  with(old$res.admin2,
    #                             sd / (mean * (1 - mean)))
    #
    
    old$res.admin2$mean=old$res.admin2$mean*100
    # old$admin2_post=old$admin2_post*100
    # qs11 <- apply(old$admin2_post, 2, quantile,
    #               probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
    # old$res.admin2$lower <- qs11[1, ]
    # old$res.admin2$upper <- qs11[2, ]
    old$res.admin2$width <-  old$res.admin2$upper -  old$res.admin2$lower

    #
    # out2 <- old$res.admin2[, c("admin2.name.full", "mean", "sd",
    #                            "cv", "lower", "upper")]
    # colnames(out2)[c(2, 3, 5, 6)] <- c("Prevalence", "sd", "lower", "upper")
    # out2$version <- yr1
    # out2$Prevalence= out2$Prevalence*100
    # out2$upper= out2$upper*100
    # out2$lower= out2$lower*100
    # out2$width <- out2$upper - out2$lower
    
  }
  if (ok22) {
    # new$res.admin2$cv1 <- with(new$res.admin2,
    #                                  pmax(sd / mean, sd / (1 - mean))
    # # )
    # new$res.admin2$cv1 <-  with(new$res.admin2,
    #                             sd / (mean * (1 - mean)))
    #
    
    new$res.admin2$mean=new$res.admin2$mean*100
    # new$admin2_post=new$admin2_post*100
    # qs12 <- apply(new$admin2_post, 2, quantile,
    #               probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
    # new$res.admin2$lower <- qs12[1, ]
    # new$res.admin2$upper <- qs12[2, ]
    
    new$res.admin2$width <-  new$res.admin2$upper -  new$res.admin2$lower
    # out22 <- new$res.admin2[, c("admin2.name.full", "mean", "sd",
    #                             "cv", "lower", "upper")]
    # colnames(out22)[c(2, 3, 5, 6)] <- c("Prevalence", "sd", "lower", "upper")
    # out22$version <- yr2
    # out22$Prevalence= out22$Prevalence*100
    # out22$upper= out22$upper*100
    # out22$lower= out22$lower*100
    # out22$width <- out22$upper - out22$lower
    
  }
  if (ok11 && ok12  && ok21 && ok22 ) {
    # ----- both years available -----
    
    s5 <- scatterPlot(
      res1=res_adm11$res.admin2,
      res2=old$res.admin2,
      value1="direct.est",
      value2="mean",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(yr1," Prevalence"),
      label1="Unsmoothed",
      label2="Smoothed")
    
    s6 <- scatterPlot(
      res1=res_adm12$res.admin2,
      res2=new$res.admin2,
      value1="direct.est",
      value2="mean",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(yr2," Prevalence"),
      label1="Unsmoothed",
      label2="Smoothed")
    
    s55 <- scatterPlot(
      res1=res_adm11$res.admin2,
      res2=old$res.admin2,
      value1="width",
      value2="width",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(yr1," 90% CI width"),
      label1="Unsmoothed",
      label2="Smoothed")
    
    s66 <- scatterPlot(
      res1=res_adm12$res.admin2,
      res2=new$res.admin2,
      value1="width",
      value2="width",
      by.res1="admin2.name.full",
      by.res2="admin2.name.full",
      title= paste0(yr2," 90% CI width"),
      label1="Unsmoothed",
      label2="Smoothed")
    
    
    
    scatter= (s5+ s6 ) /(s55+s66)
    
    
    
    
    
    
  } else {
    # ----- at least one year missing -----
    # ----- compute metrics only when present -----
    
    
    
    # Prevalence
    g1_left <- if (ok11&ok21) {
      
      
      s5 <- scatterPlot(
        res1=res_adm11$res.admin2,
        res2=old$res.admin2,
        value1="direct.est",
        value2="mean",
        by.res1="admin2.name.full",
        by.res2="admin2.name.full",
        title= paste0(yr1," Prevalence"),
        label1="Unsmoothed",
        label2="Smoothed")
      
      
      
      s55 <- scatterPlot(
        res1=res_adm11$res.admin2,
        res2=old$res.admin2,
        value1="width",
        value2="width",
        by.res1="admin2.name.full",
        by.res2="admin2.name.full",
        title= paste0(yr1," 90% CI width"),
        label1="Unsmoothed",
        label2="Smoothed")
      
      
      s5/s55
      
    } else {
      make_placeholder(yr1, "Scatter")
    }
    
    g1_right <- if (ok12&ok22){
      
      s6 <- scatterPlot(
        res1=res_adm12$res.admin2,
        res2=new$res.admin2,
        value1="direct.est",
        value2="mean",
        by.res1="admin2.name.full",
        by.res2="admin2.name.full",
        title= paste0(yr2," Prevalence"),
        label1="Unsmoothed",
        label2="Smoothed")
      
      
      
      s66 <- scatterPlot(
        res1=res_adm12$res.admin2,
        res2=new$res.admin2,
        value1="width",
        value2="width",
        by.res1="admin2.name.full",
        by.res2="admin2.name.full",
        title= paste0(yr2," 90% CI width"),
        label1="Unsmoothed",
        label2="Smoothed")
      
      s6/s66
    } else {
      make_placeholder(yr2, "Scatter")
    }
    
    scatter=g1_left|g1_right
    
  }
  
  
  
  ggsave(scatter,
         filename = file.path(plot_path_c, paste0("scatter-", indicator, ".png")),
         width = 10, height = 10, dpi = 300)
  
  
  
  
  
}
}



savescatter( country= "Nigeria",
             ad2_name= "new_FH_adm2_fix_nest-",
             ad2_name_dir=  "new_res_adm2_fix-",
             middle_path="Gates-results/Results",
             plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))




countryList <- unique(surveys$country)
for (ctry in countryList) {
  message("Processing: ", ctry)
  try(savescatter(ctry), silent = TRUE)
}



