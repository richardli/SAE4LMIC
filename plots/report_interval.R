library(RColorBrewer)

library(dplyr)
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




country="Malawi"


indicatorlist=infolist$ID

  
  
saveinterval_overlay<-function( 
    country="Malawi",
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
    
    qfile_adm1 <- resolve_qs(results_path_yr1, ad2_name, indicator)
    qfile_adm2 <- resolve_qs(results_path_yr2, ad2_name, indicator)
    
    
    old <- tryCatch(qs::qread(qfile_adm1), error = function(e) { message("Failed to read ", qfile_adm1, ": ", e$message); return("failed") })
    new <- tryCatch(qs::qread(qfile_adm2), error = function(e) { message("Failed to read ", qfile_adm2, ": ", e$message); return("failed") })
    
    qfile_adm11 <- resolve_qs(results_path_yr1, ad1_name, indicator)
    qfile_adm22 <- resolve_qs(results_path_yr2, ad1_name, indicator)
    
    
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
 
 
 
 
  
 saveinterval_overlay(country=country,
          ad2_name="FH_adm2_fix_nest-",
          ad1_name="res_adm1-",
          indicatorlist =indicatorlist,
          middle_path="Gates-results/Results",
          plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))
 
 
 
 
 
 
 
 
 saveinterval_overlay(country=country,
                      ad2_name="new_FH_adm2_fix_nest-",
                      ad1_name="new_res_adm1-",
                      indicatorlist ="ML_NETC_C_ITN",
                      middle_path="Gates-results/Results",
                      plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country))
 
 
 
 
 countryList <- unique(surveys$country)
 for (ctry in countryList) {
   message("Processing: ", ctry)
   try(saveinterval(ctry), silent = TRUE)
 }
 
 
 
  
  