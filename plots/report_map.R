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





library(rmapshaper)
library(SUMMER)
library(surveyPrev)
library(qs)
library(ggplot2)
library(RColorBrewer)
library(patchwork)


source_path<- "/Users/qianyu/Dropbox/binary_code/pcg/GATES/"
infolist <- read.csv(file.path(source_path, "infolist.csv"))
surveys <- read.csv(file.path(source_path, "surveyslist.csv"))
indicatorlist=infolist$ID







out3 <- file.path(source_path, "Gates-results/Results", country, paste0("fh_fix_nest-ad2", ".qs"))
ad2_store <- qs::qread(out3)

out1 <- file.path(source_path, "Gates-results/Results", country, paste0("dir-ad1", ".qs"))
ad1_store <- qs::qread(out1)








poly.adm2 <- ms_simplify(poly.adm2, keep = 0.1, keep_shapes = TRUE)  # keep ~10% of vertices
# # try keep = 0.2, 0.05, etc. until it looks good
# format(object.size(poly.adm2), "MB")
# format(object.size(poly_s),    "MB")
#
# yr1="previous"


yr1<-min(surveys[surveys$country==country,]$year)
yr2<-max(surveys[surveys$country==country,]$year)
iso3<-unique(surveys[surveys$country==country,]$iso3)

for (indicator in indicatorlist) {
  
  if (infolist[infolist$ID == indicator, ]$direction == -1) {
    color.paletteHERE <- rev(brewer.pal(5, "RdYlGn"))
  } else {
    color.paletteHERE <- brewer.pal(5, "RdYlGn")
  }
  

  old <- ad2_store[[iso3]][[as.character(yr1)]][[indicator]]
  new <- ad2_store[[iso3]][[as.character(yr2)]][[indicator]]
  
  ok11 <- has_data(old)
  ok12 <- has_data(new)
  
  # CI <- 0.9
  # 
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
      geo    = poly.adm2,
      by.data= "admin2.name.full",
      by.geo = "admin2.name.full",
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
    
    
    # old$res.admin2$cv1 <-  with(old$res.admin2,
    #                             sd / (mean * (1 - mean)))
    # 
    # 
    
    # qs11 <- apply(old$admin2_post, 2, quantile,
    #               probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
    # old$res.admin2$lower <- qs11[1, ]
    # old$res.admin2$upper <- qs11[2, ]
    
    out1 <- old$res.admin2[, c("admin2.name.full", "mean", "sd",
                               "cv", "lower", "upper", "cv1")]
    colnames(out1)[c(2, 3, 5, 6)] <- c("Prevalence", "sd", "lower", "upper")
    out1$version <- yr1
    out1$width <- out1$upper - out1$lower
    out1$Prevalence= out1$Prevalence*100
    out1$upper= out1$upper*100
    out1$lower= out1$lower*100
  }
  
  if (ok12) {
    # new$res.admin2$cv1 <- with(new$res.admin2,
    #                                  pmax(sd / mean, sd / (1 - mean))
    # )
    new$res.admin2$cv1 <-  with(new$res.admin2,
                                sd / (mean * (1 - mean)))
    
    
    qs12 <- apply(new$admin2_post, 2, quantile,
                  probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
    new$res.admin2$lower <- qs12[1, ]
    new$res.admin2$upper <- qs12[2, ]
    
    out11 <- new$res.admin2[, c("admin2.name.full", "mean", "sd",
                                "cv.median", "lower", "upper", "cv1")]
    colnames(out11)[c(2, 3, 4, 5, 6)] <- c("Prevalence", "sd", "cv","lower", "upper")
    out11$version <- yr2
    out11$width <- out11$upper - out11$lower
    out11$Prevalence= out11$Prevalence*100
    out11$upper= out11$upper*100
    out11$lower= out11$lower*100
  }
  
  
  if (ok11 && ok12) {
    # ----- both years available -----
    outadm2 <- rbind(out1, out11)
    outadm2$version <- factor(outadm2$version, levels = unique(outadm2$version))
    outadm2$width <- outadm2$upper - outadm2$lower
    
    
    
    g1 <- mapPlot(data = outadm2, geo = poly.adm2,
                  by.data = "admin2.name.full",  by.geo = "admin2.name.full", is.long = TRUE,
                  variable = "version", value = "Prevalence", legend.label = "Prevalence",
                  direction = -1, ncol = 2, size = .05, border = "gray50") +
      scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence")
    
    # g2 <- mapPlot(data = outadm2, geo = poly.adm2,
    #               by.data = "admin2.name.full",  by.geo = "admin2.name.full", is.long = TRUE,
    #               variable = "version", value = "sd", legend.label = "sd",
    #               ncol = 2, size = .05, border = "gray50") +
    #   scale_fill_viridis_c("sd", option = "F", direction = -1)
    #
    # g3 <- mapPlot(data = outadm2, geo = poly.adm2,
    #               by.data = "admin2.name.full",  by.geo = "admin2.name.full", is.long = TRUE,
    #               variable = "version", value = "cv", legend.label = "CV",
    #               ncol = 2, size = .05, border = "gray50") +
    #   scale_fill_viridis_c("CV", option = "G", direction = -1)
    
    g4 <- mapPlot(data = outadm2, geo = poly.adm2,
                  by.data = "admin2.name.full",  by.geo = "admin2.name.full", is.long = TRUE,
                  variable = "version", value = "width", legend.label = "90% CI\nwidth",
                  ncol = 2, size = .05, border = "gray50") +
      scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth")
    
    
    
    
    thre1=tab2[tab2$ID==indicator,]$previous_est
    thre2=tab2[tab2$ID==indicator,]$current_est
    old$admin2_post= old$admin2_post*100
    new$admin2_post= new$admin2_post*100
    
    g5 <- exceedPlot1(
      x = list(old,  new),
      facet_labels = c(
        paste0(yr1, " National: ", thre1, "%"),
        paste0(yr2," National: " ,  thre2, "%")
      ),
      threshold = c(thre1, thre2),
      exceed = TRUE, direction = infolist[infolist$ID == indicator, ]$direction ,
      geo = poly.adm2, by.geo = "admin2.name.full",
      facet_var = "year",                # column name to facet on
      ncol = 2, size = 0.05, border = "gray50"
    ) +
      scale_fill_distiller(name = "Exceedance\nProbability",
                           palette = "Spectral", direction = infolist[infolist$ID == indicator, ]$direction,
                           limits = c(0,1), breaks = c(0.25,0.5,0.75))
    
    
    
    
    
  } else {
    # ----- at least one year missing -----
    # ----- compute metrics only when present -----
    
    
    
    # Prevalence
    g1_left <- if (ok11) {
      plot_one_year(out1, yr1, "Prevalence", "Prevalence",
                    scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence"))
    } else {
      make_placeholder(yr1, "Prevalence")
    }
    g1_right <- if (ok12) {
      plot_one_year(out11, yr2, "Prevalence", "Prevalence",
                    scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence"))
    } else {
      make_placeholder(yr2, "Prevalence")
    }
    g1 <- g1_left | g1_right
    #
    #     # sd
    #     g2_left <- if (ok11) {
    #       plot_one_year(out1, yr1, "sd", "sd",
    #                     scale_fill_viridis_c("sd", option = "F", direction = -1))
    #     } else {
    #       make_placeholder(yr1, "sd")
    #     }
    #     g2_right <- if (ok12) {
    #       plot_one_year(out11, yr2, "sd", "sd",
    #                     scale_fill_viridis_c("sd", option = "F", direction = -1))
    #     } else {
    #       make_placeholder(yr2, "sd")
    #     }
    #     g2 <- g2_left | g2_right
    
    # # CV
    # g3_left <- if (ok11) {
    #   plot_one_year(out1, yr1, "cv", "CV",
    #                 scale_fill_viridis_c("CV", option = "G", direction = -1))
    # } else {
    #   make_placeholder(yr1, "CV")
    # }
    # g3_right <- if (ok12) {
    #   plot_one_year(out11, yr2, "cv", "CV",
    #                 scale_fill_viridis_c("CV", option = "G", direction = -1))
    # } else {
    #   make_placeholder(yr2, "CV")
    # }
    # g3 <- g3_left | g3_right
    
    # 90% CI width
    g4_left <- if (ok11) {
      plot_one_year(out1, yr1, "width", "90% CI\nwidth",
                    scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth"))
    } else {
      make_placeholder(yr1, "90% CI width")
    }
    g4_right <- if (ok12) {
      plot_one_year(out11, yr2, "width", "90% CI\nwidth",
                    scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth"))
    } else {
      make_placeholder(yr2, "90% CI width")
    }
    g4 <- g4_left | g4_right
    
    
    
    # CV
    g5_left <- if (ok11) {
      
      
      thre1=tab2[tab2$ID==indicator,]$previous_est
      # thre2=tab2[tab2$ID==indicator,]$current_est
      old$admin2_post=old$admin2_post*100
      # res_adm12$admin1_post=res_adm12$admin1_post*100
      
      
      legend <- paste0("Probability\nExceeding ", thre1, "%\n")
      exceedPlot(old, threshold = thre1,
                 exceed = TRUE,
                 value_col = as.character(yr1)
                 ,  border = "gray80", size = 0,
                 geo = poly.adm2, by.geo = "admin2.name.full", ylim = c(0, 1)) +
        scale_fill_distiller(legend, palette = "Spectral",
                             direction =  infolist[infolist$ID == indicator, ]$direction     )
      
      
      
      
      
    } else {
      make_placeholder(yr1, "Exceedance")
    }
    g5_right <- if (ok12) {
      
      # thre1=tab2[tab2$ID==indicator,]$previous_est
      thre2=tab2[tab2$ID==indicator,]$current_est
      # res_adm11$admin1_post=res_adm11$admin1_post*100
      new$admin2_post=new$admin2_post*100
      
      legend <- paste0("Probability\nExceeding ", thre2, "%\n")
      exceedPlot(new, threshold = thre2,
                 exceed = TRUE,
                 value_col = as.character(yr2),
                 direction =infolist[infolist$ID == indicator, ]$direction
                 ,  border = "gray80", size = 0,
                 geo = poly.adm2, by.geo = "admin2.name.full", ylim = c(0, 1)) +
        scale_fill_distiller(legend, palette = "Spectral",
                             direction =  infolist[infolist$ID == indicator, ]$direction     )
      
    } else {
      make_placeholder(yr2, "Exceedance")
    }
    g5 <-   g5_left |   g5_right
    
    
    
    
  }
  
  final_plot <- (g1 / g4/ g5) + plot_layout(heights = c(1,1,1))
  
  
  
  
  # stack the four rows
  
  ggsave(final_plot,
         filename = file.path(plot_path_c, paste0("ad2_map_", indicator, ".png")),
         width = 6, height = 9, dpi = 300)
}







poly.adm1 <- ms_simplify(poly.adm1, keep = 0.1, keep_shapes = TRUE)  # keep ~10% of vertices

#ad1
for (indicator in indicatorlist ) {
  
  if (infolist[infolist$ID == indicator, ]$direction == -1) {
    color.paletteHERE <- rev(brewer.pal(5, "RdYlGn"))
  } else {
    color.paletteHERE <- brewer.pal(5, "RdYlGn")
  }
  
  
  res_adm11=res_adm12=c()
  res_adm11 <- ad1_store[[iso3]][[as.character(yr1)]][[indicator]]
  res_adm12 <- ad1_store[[iso3]][[as.character(yr2)]][[indicator]]

  
  
  
  ok11 <- has_data(res_adm11)
  ok12 <- has_data(res_adm12)
  
  CI <- 0.9
  
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
    res_adm11$res.admin1$cv1 <- with(res_adm11$res.admin1,
                                     pmax(direct.se / direct.est, direct.se / (1 - direct.est))
    )
    res_adm11$admin1_post= redrawsample(res_adm11,nsim=1000)
    
    res_adm11$res.admin1$cv1 <-  with(res_adm11$res.admin1,
                                      direct.se / (direct.est * (1 - direct.est)))
    
    
    
    qs11 <- apply(res_adm11$admin1_post, 2, quantile,
                  probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
    res_adm11$res.admin1$direct.lower <- qs11[1, ]
    res_adm11$res.admin1$direct.upper <- qs11[2, ]
    
    out1 <- res_adm11$res.admin1[, c("admin1.name", "direct.est", "direct.se",
                                     "cv", "direct.lower", "direct.upper","cv1")]
    colnames(out1)[c(2, 3, 5, 6)] <- c("Prevalence", "sd", "lower", "upper")
    out1$version <- yr1
    out1$width <- out1$upper - out1$lower
    out1$Prevalence= out1$Prevalence*100
    out1$upper= out1$upper*100
    out1$lower= out1$lower*100
  }
  
  if (ok12) {
    # res_adm12$res.admin1$cv1 <- with(res_adm12$res.admin1,
    #                                  pmax(direct.se / direct.est, direct.se / (1 - direct.est))
    # )
    res_adm12$admin1_post= redrawsample(res_adm12,nsim=1000)
    
    res_adm12$res.admin1$cv1 <-  with(res_adm12$res.admin1,
                                      direct.se / (direct.est * (1 - direct.est)))
    
    
    qs12 <- apply(res_adm12$admin1_post, 2, quantile,
                  probs = c((1 - CI) / 2, 1 - (1 - CI) / 2))
    res_adm12$res.admin1$direct.lower <- qs12[1, ]
    res_adm12$res.admin1$direct.upper <- qs12[2, ]
    
    out11 <- res_adm12$res.admin1[, c("admin1.name", "direct.est", "direct.se",
                                      "cv", "direct.lower", "direct.upper", "cv1")]
    colnames(out11)[c(2, 3, 5, 6)] <- c("Prevalence", "sd", "lower", "upper")
    out11$version <- yr2
    out11$width <- out11$upper - out11$lower
    
    out11$Prevalence= out11$Prevalence*100
    out11$upper= out11$upper*100
    out11$lower= out11$lower*100
    
  }
  
  
  if (ok11 && ok12) {
    # ----- both years available -----
    outadm2 <- rbind(out1, out11)
    outadm2$version <- factor(outadm2$version, levels = unique(outadm2$version))
    # outadm2$Prevalence= outadm2$Prevalence*100
    # outadm2$upper= outadm2$upper*100
    # outadm2$lower= outadm2$lower*100
    
    outadm2$width <- outadm2$upper - outadm2$lower
    g1 <- mapPlot(data = outadm2, geo = poly.adm1,
                  by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
                  variable = "version", value = "Prevalence", legend.label = "Prevalence",
                  direction = -1, ncol = 2, size = .05, border = "gray50") +
      scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence")
    # theme_bw() +
    # theme(
    #   panel.grid = element_blank(),                 # no grid lines
    #   # panel.border = element_rect(color = "black", fill = NA, size = 1), # black frame
    #   strip.background = element_blank(),           # no grey facet bar
    #   strip.text = element_text(size = 12), # year title on top
    #   axis.text = element_blank(),                  # remove lat/lon text
    #   axis.ticks = element_blank(),                 # remove ticks
    #   axis.title = element_blank(),                 # remove axis titles
    #   legend.position = "right"
    # )
    
    # g2 <- mapPlot(data = outadm2, geo = poly.adm1,
    #               by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
    #               variable = "version", value = "sd", legend.label = "sd",
    #               ncol = 2, size = .05, border = "gray50") +
    #   scale_fill_viridis_c("sd", option = "F", direction = -1)
    
    
    g4 <- mapPlot(data = outadm2, geo = poly.adm1,
                  by.data = "admin1.name",  by.geo = "NAME_1", is.long = TRUE,
                  variable = "version", value = "width", legend.label = "90% CI\nwidth",
                  ncol = 2, size = .05, border = "gray50") +
      scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth")
    # scale_fill_viridis_c("90% CI\nwidth", option = "B", direction = -1)+
    # theme_bw() +
    # theme(
    #   panel.grid = element_blank(),                 # no grid lines
    #   # panel.border = element_rect(color = "black", fill = NA, size = 1), # black frame
    #   strip.background = element_blank(),           # no grey facet bar
    #   strip.text = element_text(size = 12), # year title on top
    #   axis.text = element_blank(),                  # remove lat/lon text
    #   axis.ticks = element_blank(),                 # remove ticks
    #   axis.title = element_blank(),                 # remove axis titles
    #   legend.position = "right"
    # )
    
    
    
    
    thre1=tab2[tab2$ID==indicator,]$previous_est
    thre2=tab2[tab2$ID==indicator,]$current_est
    res_adm11$admin1_post=res_adm11$admin1_post*100
    res_adm12$admin1_post=res_adm12$admin1_post*100
    
    g5 <- exceedPlot1(
      x = list(res_adm11,  res_adm12),
      facet_labels = c(
        paste0(yr1, " National: ", thre1, "%"),
        paste0(yr2," National: " ,  thre2, "%")
      ),
      threshold = c(thre1, thre2),
      exceed = TRUE, direction = infolist[infolist$ID == indicator, ]$direction ,
      geo = poly.adm1, by.geo = "NAME_1",
      facet_var = "year",                # column name to facet on
      ncol = 2, size = 0.05, border = "gray50"
    ) +
      scale_fill_distiller(name = "Exceedance\nProbability",
                           palette = "Spectral", direction = infolist[infolist$ID == indicator, ]$direction ,
                           limits = c(0,1), breaks = c(0.25,0.5,0.75))
    
    
    
    
    
    
  } else {
    # ----- at least one year missing -----
    # ----- compute metrics only when present -----
    
    
    
    # Prevalence
    g1_left <- if (ok11) {
      plot_one_year(out1, yr1, "Prevalence", "Prevalence",
                    scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence"))
    } else {
      make_placeholder(yr1, "Prevalence")
    }
    g1_right <- if (ok12) {
      plot_one_year(out11, yr2, "Prevalence", "Prevalence",
                    scale_fill_gradientn(colors = color.paletteHERE, name = "Prevalence"))
    } else {
      make_placeholder(yr2, "Prevalence")
    }
    g1 <- g1_left | g1_right
    
    
    
    # 90% CI width
    g4_left <- if (ok11) {
      plot_one_year(out1, yr1, "width", "90% CI\nwidth",
                    scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth"))
      
    } else {
      make_placeholder(yr1, "90% CI width")
    }
    g4_right <- if (ok12) {
      plot_one_year(out11, yr2, "width", "90% CI\nwidth",
                    scale_fill_gradientn(  colours =  RColorBrewer::brewer.pal(9, "Blues"),  name = "90% CI\nwidth"))
    } else {
      make_placeholder(yr2, "90% CI width")
    }
    g4 <- g4_left | g4_right
    
    
    # CV
    g5_left <- if (ok11) {
      
      
      thre1=tab2[tab2$ID==indicator,]$previous_est
      # thre2=tab2[tab2$ID==indicator,]$current_est
      res_adm11$admin1_post=res_adm11$admin1_post*100
      # res_adm12$admin1_post=res_adm12$admin1_post*100
      
      
      legend <- paste0("Probability\nExceeding ", thre1, "%\n")
      exceedPlot(res_adm11, threshold = thre1,
                 exceed = TRUE,
                 value_col = as.character(yr1)
                 ,  border = "gray80", size = 0,
                 geo = poly.adm1, by.geo = "NAME_1", ylim = c(0, 1)) +
        scale_fill_distiller(legend, palette = "Spectral",
                             direction =  infolist[infolist$ID == indicator, ]$direction     )
      
      
      
      
      
    } else {
      make_placeholder(yr1, "Exceedance")
    }
    g5_right <- if (ok12) {
      
      # thre1=tab2[tab2$ID==indicator,]$previous_est
      thre2=tab2[tab2$ID==indicator,]$current_est/100
      # res_adm11$admin1_post=res_adm11$admin1_post*100
      # res_adm12$admin12_post=res_adm12$admin1_post*100
      
      legend <- paste0("Probability\nExceeding ", thre2*100, "%\n")
      exceedPlot(res_adm12, threshold = thre2,
                 exceed = TRUE,
                 value_col = as.character(yr2),
                 direction =       infolist[infolist$ID == indicator, ]$direction
                 ,  border = "gray80", size = 0,
                 geo = poly.adm1, by.geo = "NAME_1", ylim = c(0, 1)) +
        scale_fill_distiller(legend, palette = "Spectral",
                             direction =  infolist[infolist$ID == indicator, ]$direction     )
      
    } else {
      make_placeholder(yr2, "Exceedance")
    }
    g5 <-   g5_left |   g5_right
    
    
    
    
  }
  
  
  
  
  
  final_plot <- (g1/ g4/ g5) + plot_layout(heights = c(1,1,1))
  
  #
  #   final_plot <- (g1 / g4 / g5) +
  #     plot_layout(heights = c(1, 1, 1), guides = "collect") &
  #     theme(
  #       # legend.position = "right"
  #       # legend.justification = "center"  # centers the legends across rows
  #     )
  
  final_plot
  
  
  # stack the four rows
  
  ggsave(final_plot,
         filename = file.path(plot_path_c, paste0("ad1_map_", indicator, ".png")),
         width = 6, height = 9, dpi = 300)
}


