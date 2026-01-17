source('fct_plotting.R')
source("fct_reading_result.R")

load_and_gen_plots <- function(ctry, static_dir,
                               html_dir,
                               palette = rev(RColorBrewer::brewer.pal(11, "RdYlGn")),
                               static = TRUE,
                               html = TRUE,
                               ridge = TRUE,
                               ridge_diff = TRUE,
                               indicator_to_plot = character(0)) {
  message(paste0("At country: ", ctry))
  surveylist <- read.csv(file.path(git_path, "info", "surveyslist.csv"))
  countryInfo <- get_country_info(surveylist, ctry)
  year1 <- as.character(countryInfo$year1)
  year2 <- as.character(countryInfo$year2)
  country <- countryInfo$iso3
  
  res_data <-load_data(ctry, country, year1, year2)
  gen_all_map(res_data,
              country, year1, year2,
              static_dir,
              html_dir, 
              palette = palette,
              static = static,
              html = html,
              ridge = ridge,
              ridge_diff = ridge,
              indicator_to_plot = indicator_to_plot)
}

gen_all_map <- function(res_data, country, year1, year2, static_dir, html_dir, palette = rev(RColorBrewer::brewer.pal(11, "RdYlGn")),
                        static,
                        html,
                        ridge,
                        ridge_diff,
                        indicator_to_plot){
  i = 0

  
  shp_name <- paste0(country, "_shp")
  infolist <- read.csv(file.path(git_path, "info", "infolist.csv"))
  indicators <- infolist$ID
  
  if(length(indicator_to_plot) > 0) {
    indicators <- indicator_to_plot
  }
  
  rev_vec <- ifelse(infolist$direction == 1, TRUE, FALSE)
  
  admin1height <- max(10, dim(res_data[[country]][[year2]][[1]]$adm1$res.admin1)[1] / 2)
  
  if(country %in% c("MOZ", "KEN")){
    static.height = 12
  } else if (country == "BFA") {
    static.height = 6
  } else if (country == "COD") {
    static.height = 9
  } else if (country == "MLI") {
    static.height = 8
  } else {
    static.height = 7
  }

  for (ind in indicators) {
    if(is.null(res_data[[country]][[year2]][[ind]]) && is.null(res_data[[country]][[year1]][[ind]]))  next
    print(ind)
    if(ind %in% c("CM_ECMR_C_NNF")) {
      percentage <- FALSE
    } else {
      percentage <- TRUE
    }
    i = i+1
    # get range
    range_low <- min(
      res_data[[country]][[year2]][[ind]]$adm1$res.admin1$direct.est,
      res_data[[country]][[year2]][[ind]]$adm2$res.admin2$mean,
      res_data[[country]][[year1]][[ind]]$adm1$res.admin1$direct.est,
      res_data[[country]][[year1]][[ind]]$adm2$res.admin2$mean,
      na.rm = TRUE
    )
    range_high <- max(
      res_data[[country]][[year2]][[ind]]$adm1$res.admin1$direct.est,
      res_data[[country]][[year2]][[ind]]$adm2$res.admin2$mean,
      res_data[[country]][[year1]][[ind]]$adm1$res.admin1$direct.est,
      res_data[[country]][[year1]][[ind]]$adm2$res.admin2$mean,
      na.rm = TRUE
    )
    plot_range <- c(range_low, range_high)
    
    # get range of CI
    ci_range_low <- min(
      res_data[[country]][[year1]][[ind]]$adm1$res.admin1$direct.upper - res_data[[country]][[year1]][[ind]]$adm1$res.admin1$direct.lower,
      res_data[[country]][[year2]][[ind]]$adm1$res.admin1$direct.upper - res_data[[country]][[year2]][[ind]]$adm1$res.admin1$direct.lower,
      res_data[[country]][[year1]][[ind]]$adm2$res.admin2$upper - res_data[[country]][[year1]][[ind]]$adm2$res.admin2$lower,
      res_data[[country]][[year2]][[ind]]$adm2$res.admin2$upper - res_data[[country]][[year2]][[ind]]$adm2$res.admin2$lower,
      na.rm = TRUE)
    
    ci_range_high <- max(
      res_data[[country]][[year1]][[ind]]$adm1$res.admin1$direct.upper - res_data[[country]][[year1]][[ind]]$adm1$res.admin1$direct.lower,
      res_data[[country]][[year2]][[ind]]$adm1$res.admin1$direct.upper - res_data[[country]][[year2]][[ind]]$adm1$res.admin1$direct.lower,
      res_data[[country]][[year1]][[ind]]$adm2$res.admin2$upper - res_data[[country]][[year1]][[ind]]$adm2$res.admin2$lower,
      res_data[[country]][[year2]][[ind]]$adm2$res.admin2$upper - res_data[[country]][[year2]][[ind]]$adm2$res.admin2$lower,
      na.rm = TRUE)
    ci_plot_range <- c(ci_range_low * 0.99, ci_range_high * 1.01)
    
    # palette
    pal <- palette
    ci_pal <- brewer.pal(9, "Blues")
    pal_dif <- rev(RColorBrewer::brewer.pal(9, "Blues"))
    
    #get reverse
    ###########
    reverse <- rev_vec[i]
    
    if(reverse) {
      pal <- rev(pal)
    } else {
      pal <- pal
    }
    
    
    if(static){
      # static prevmap in overview
      # map of mean
      # ---- static maps ----
      p1 <- if(!is.null(res_data[[country]][[year1]][[ind]]$adm1)) {
        prevMap.static(res_data[[country]][[year1]][[ind]]$adm1, poly_shp[[shp_name]]$adm1,
                       value.range = plot_range, color.palette = pal, color.reverse = reverse)
      } else make_no_data_plot()
      
      p2 <- if(!is.null(res_data[[country]][[year2]][[ind]]$adm1)) {
        prevMap.static(res_data[[country]][[year2]][[ind]]$adm1, poly_shp[[shp_name]]$adm1,
                       value.range = plot_range, color.palette = pal, color.reverse = reverse)
      } else make_no_data_plot()
      
      p3 <- if(!is.null(res_data[[country]][[year1]][[ind]]$adm2)) {
        prevMap.static(res_data[[country]][[year1]][[ind]]$adm2, poly_shp[[shp_name]]$adm2,
                       value.range = plot_range, color.palette = pal, color.reverse = reverse)
      } else make_no_data_plot()
      
      p4 <- if(!is.null(res_data[[country]][[year2]][[ind]]$adm2)) {
        prevMap.static(res_data[[country]][[year2]][[ind]]$adm2, poly_shp[[shp_name]]$adm2,
                       value.range = plot_range, color.palette = pal, color.reverse = reverse)
      } else make_no_data_plot()
      
      # CI width maps
      p5 <- if(!is.null(res_data[[country]][[year1]][[ind]]$adm1)) {
        prevMap.static(res_data[[country]][[year1]][[ind]]$adm1, poly_shp[[shp_name]]$adm1,
                       value.range = ci_plot_range, color.palette = ci_pal, color.reverse = reverse, value.to.plot = "CI.width")
      } else make_no_data_plot()
      
      p6 <- if(!is.null(res_data[[country]][[year2]][[ind]]$adm1)) {
        prevMap.static(res_data[[country]][[year2]][[ind]]$adm1, poly_shp[[shp_name]]$adm1,
                       value.range = ci_plot_range, color.palette = ci_pal, color.reverse = reverse, value.to.plot = "CI.width")
      } else make_no_data_plot()
      
      p7 <- if(!is.null(res_data[[country]][[year1]][[ind]]$adm2)) {
        prevMap.static(res_data[[country]][[year1]][[ind]]$adm2, poly_shp[[shp_name]]$adm2,
                       value.range = ci_plot_range, color.palette = ci_pal, color.reverse = reverse, value.to.plot = "CI.width")
      } else make_no_data_plot()
      
      p8 <- if(!is.null(res_data[[country]][[year2]][[ind]]$adm2)) {
        prevMap.static(res_data[[country]][[year2]][[ind]]$adm2, poly_shp[[shp_name]]$adm2,
                       value.range = ci_plot_range, color.palette = ci_pal, color.reverse = reverse, value.to.plot = "CI.width")
      } else make_no_data_plot()
      
      # put together
      plots <- lapply(list(p1, p2, p3, p4, p5, p6, p7, p8), shrink_plot)
      
      # patchwork
      synced_maps <- patchwork::wrap_plots(plots, ncol = 4, nrow = 2)
      
      #save
      ggsave(
        filename = paste0(static_dir, "/", country, "/", ind, ".png"),
        plot = synced_maps,
        width = 16,
        height = static.height
      )
      
      
      legend
      colours_use <- pal
      
      if(percentage){
        p <- ggplot(data.frame(x = 1, y = 1, z = 1), aes(x, y, fill = z)) +
          geom_tile() +
          scale_fill_gradientn(
            colours = colours_use,
            limits = plot_range,
            name = "Prevalence    ",
            labels = scales::label_percent(accuracy = 1)
          ) +
          guides(fill = guide_colorbar(barheight = 10, barwidth = 1)) +
          theme_void() +
          theme(legend.position = "right")
      } else {
        p <- ggplot(data.frame(x = 1, y = 1, z = 1), aes(x, y, fill = z)) +
          geom_tile() +
          scale_fill_gradientn(
            colours = colours_use,
            limits = plot_range,
            name = "Rate             ",
            labels = scales::label_number(
              scale = 1000,
              suffix = "",
              accuracy = 1
            )
          ) +
          guides(fill = guide_colorbar(barheight = 10, barwidth = 1)) +
          theme_void() +
          theme(legend.position = "right")
      }
      
      
      
      legend <- cowplot::get_legend(p)
      legend <- ggplotify::as.ggplot(legend)
      
      ggsave(
        filename = paste0(static_dir, "/", country, "/", ind, "_legend.png"),
        plot = legend,
        height = 3, width = 1
      )
      
      
      if(percentage) {
        p <- ggplot(data.frame(x = 1, y = 1, z = 1), aes(x, y, fill = z)) +
          geom_tile() +
          scale_fill_gradientn(
            colours = ci_pal,
            limits = ci_plot_range,
            name = "90% CI width",
            labels = scales::label_percent(accuracy = 1)
          ) +
          guides(fill = guide_colorbar(barheight = 10, barwidth = 1)) +
          theme_void() +
          theme(legend.position = "right")
      } else {
        p <- ggplot(data.frame(x = 1, y = 1, z = 1), aes(x, y, fill = z)) +
          geom_tile() +
          scale_fill_gradientn(
            colours = ci_pal,
            limits = ci_plot_range,
            name = "90% CI width",
            labels = scales::label_number(
              scale = 1000,
              suffix = "",
              accuracy = 1
            )
          ) +
          guides(fill = guide_colorbar(barheight = 10, barwidth = 1)) +
          theme_void() +
          theme(legend.position = "right")
      }
      
      
      legend <- cowplot::get_legend(p)
      legend <- ggplotify::as.ggplot(legend)
      
      ggsave(
        filename = paste0(static_dir, "/", country, "/", ind, "_ci_legend.png"),
        plot = legend,
        height = 3, width = 1
      )
    }


    if(html){
      #detailed map
      # message("generating detailed interactive map")
      pal <- palette
      
      plot_list <- list()
      plot_list[[1]] <- if(!is.null(res_data[[country]][[year1]][[ind]]$adm1)) {
        prevMap.leaflet(res_data[[country]][[year1]][[ind]]$adm1, poly_shp[[shp_name]]$adm1,
                        value.range = plot_range, color.palette = pal, color.reverse = reverse, use.basemap="OSM", percentage = percentage)
      } else make_no_data_leaflet(poly_shp[[shp_name]]$adm1)
      
      plot_list[[2]] <- if(!is.null(res_data[[country]][[year2]][[ind]]$adm1)) {
        prevMap.leaflet(res_data[[country]][[year2]][[ind]]$adm1, poly_shp[[shp_name]]$adm1,
                        value.range = plot_range, color.palette = pal, color.reverse = reverse, legend.appear = T, use.basemap="OSM", percentage = percentage)
      } else make_no_data_leaflet(poly_shp[[shp_name]]$adm1)
      
      plot_list[[3]] <- if(!is.null(res_data[[country]][[year1]][[ind]]$adm2)) {
        prevMap.leaflet(res_data[[country]][[year1]][[ind]]$adm2, poly_shp[[shp_name]]$adm2,
                        value.range = plot_range, color.palette = pal, color.reverse = reverse, use.basemap="OSM", percentage = percentage)
      } else make_no_data_leaflet(poly_shp[[shp_name]]$adm2)
      
      plot_list[[4]] <- if(!is.null(res_data[[country]][[year2]][[ind]]$adm2)) {
        prevMap.leaflet(res_data[[country]][[year2]][[ind]]$adm2, poly_shp[[shp_name]]$adm2,
                        value.range = plot_range, color.palette = pal, color.reverse = reverse, legend.appear = T, use.basemap="OSM", percentage = percentage)
      } else make_no_data_leaflet(poly_shp[[shp_name]]$adm2)
      
      detailed_maps <- leafsync::latticeView(
        plot_list,
        ncol = 2,
        sync = "all"
      )
      if (!dir.exists(paste0(html_dir, "/", country))) {
          dir.create(paste0(html_dir, "/", country))
        }
      save_html(detailed_maps,
                file = paste0(html_dir, "/", country, "/", ind, "_detail.html"))
      
      
      plot_list <- list()
      ci_pal <- rev(ci_pal)
      plot_list[[1]] <- if (!is.null(res_data[[country]][[year1]][[ind]]$adm1)) {
        prevMap.leaflet(res_data[[country]][[year1]][[ind]]$adm1, poly_shp[[shp_name]]$adm1,
                        value.range = ci_plot_range, color.palette = ci_pal,
                        use.basemap = "OSM", value.to.plot = "CI.width", percentage = percentage)
      } else make_no_data_leaflet(poly_shp[[shp_name]]$adm1)
      
      plot_list[[2]] <- if (!is.null(res_data[[country]][[year2]][[ind]]$adm1)) {
        prevMap.leaflet(res_data[[country]][[year2]][[ind]]$adm1, poly_shp[[shp_name]]$adm1,
                        value.range = ci_plot_range, color.palette = ci_pal,
                        legend.appear = T, use.basemap = "OSM", value.to.plot = "CI.width", percentage = percentage)
      } else make_no_data_leaflet(poly_shp[[shp_name]]$adm1)
      
      plot_list[[3]] <- if (!is.null(res_data[[country]][[year1]][[ind]]$adm2)) {
        prevMap.leaflet(res_data[[country]][[year1]][[ind]]$adm2, poly_shp[[shp_name]]$adm2,
                        value.range = ci_plot_range, color.palette = ci_pal,
                        use.basemap = "OSM", value.to.plot = "CI.width", percentage = percentage)
      } else make_no_data_leaflet(poly_shp[[shp_name]]$adm2)
      
      plot_list[[4]] <- if (!is.null(res_data[[country]][[year2]][[ind]]$adm2)) {
        prevMap.leaflet(res_data[[country]][[year2]][[ind]]$adm2, poly_shp[[shp_name]]$adm2,
                        value.range = ci_plot_range, color.palette = ci_pal,
                        legend.appear = T, use.basemap = "OSM", value.to.plot = "CI.width", percentage = percentage)
      } else make_no_data_leaflet(poly_shp[[shp_name]]$adm2)
      
      detailed_ci_maps <- leafsync::latticeView(
        plot_list,
        ncol = 2,
        sync = "all"
      )
      
      save_html(detailed_ci_maps,
                file = paste0(html_dir, "/", country, "/", ind, "_detail_ci.html"))
      
    }

    

    if(ridge){
      # generate ridge plot
      # palette
      pal <- palette
      reverse <- rev_vec[i]
      
      if(reverse) {
        pal <- rev(pal)
      }
      
      if(!is.null(res_data[[country]][[year2]][[ind]])){
        if (!percentage) {
          ridge_adm1 <- posterior_ridge_plot(res_data[[country]][[year2]][[ind]]$adm1, plot.extreme.num = NA, palette = pal, legend.label = "Rate (per 1,000)", percentage = FALSE)
          ridge_adm2 <- posterior_ridge_plot(res_data[[country]][[year2]][[ind]]$adm2, plot.extreme.num = 10, palette = pal, legend.label = "Rate (per 1,000)", percentage = FALSE)
        } else {
          ridge_adm1 <- posterior_ridge_plot(res_data[[country]][[year2]][[ind]]$adm1, plot.extreme.num = NA, palette = pal, legend.label = "Prevalence (%)")
          ridge_adm2 <- posterior_ridge_plot(res_data[[country]][[year2]][[ind]]$adm2, plot.extreme.num = 10, palette = pal, legend.label = "Prevalence (%)")
        }
        
        ridge_adm1 <- cowplot::ggdraw(ridge_adm1)
        ggsave(paste0(
          static_dir, "/", country, "/", ind, "_ridge_adm1.png"),
          plot = ridge_adm1,
          height = admin1height,
          width = 10
        )
        
        ridge_adm2 <- cowplot::ggdraw(ridge_adm2)
        ggsave(paste0(
          static_dir, "/", country, "/", ind, "_ridge_adm2.png"),
          plot = ridge_adm2,
          height = 30,
          width = 10
        )
      }
    }
    
    if(ridge_diff){
      pal_dif <- rev(RColorBrewer::brewer.pal(9, "Blues"))
      
      #get reverse
      ###########
      reverse <- rev_vec[i]
      
      if(reverse) {
        pal_dif <- rev(pal_dif)
      }
      
      
      
      if(!is.null(res_data[[country]][[year1]][[ind]]$adm1) && !is.null(res_data[[country]][[year2]][[ind]]$adm1)){
        
        diff_adm1 <- res_data[[country]][[year2]][[ind]]$adm1$admin1_post - res_data[[country]][[year1]][[ind]]$adm1$admin1_post
        new <- res_data[[country]][[year2]][[ind]]$adm1
        new$admin1_post <- diff_adm1
        ind_ridge_diff1 <- posterior_ridge_plot(new, plot.extreme.num = NA, color.reverse = reverse, palette = pal_dif, plot.difference = T, legend.label = "Change", percentage = percentage)
        
        ind_ridge_diff1 <- cowplot::ggdraw(ind_ridge_diff1)
        ggsave(paste0(
          static_dir, "/", country, "/", ind,"_ridge_diff_adm1.png"),
          plot = ind_ridge_diff1,
          height = admin1height,
          width = 10
        )
      }
      
      if(!is.null(res_data[[country]][[year1]][[ind]]$adm2) && !is.null(res_data[[country]][[year2]][[ind]]$adm2)){
        diff_adm2 <- res_data[[country]][[year2]][[ind]]$adm2$admin2_post - res_data[[country]][[year1]][[ind]]$adm2$admin2_post
        new <- res_data[[country]][[year2]][[ind]]$adm2
        new$admin2_post <- diff_adm2
        ind_ridge_diff2 <- posterior_ridge_plot(new, plot.extreme.num = 10, color.reverse = reverse, palette = pal_dif, plot.difference = T, legend.label = "Change", percentage = percentage)
        
        ind_ridge_diff2 <- cowplot::ggdraw(ind_ridge_diff2)
        ggsave(paste0(
          static_dir, "/", country, "/", ind,"_ridge_diff_adm2.png"),
          plot = ind_ridge_diff2,
          height = 30,
          width = 10
        )
      }
    }
  }
}

get_country_info <- function(df, country_name) {
  df %>%
    filter(country == country_name) %>%
    summarise(
      year1 = min(year, na.rm = TRUE),
      year2 = max(year, na.rm = TRUE),
      iso3  = first(iso3)
    )
}