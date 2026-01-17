###############################################################
### leaflet (interactive) prevalence map for any subnational level
###############################################################
#'
#' @description produce interactive map for any subnational level
#'
#' @param res.obj result object from surveyPrev
#'
#' @param poly.shp polygon file for plotting
#' 
#' @param admin1.focus whether to display the map of a specific admin 1 area, defult to NULL to display whole country
#'
#' @param color.palette which palette to use for plotting
#'
#' @param color.reverse whether to use reverse color scale
#'
#' @param value.range what range to plot, useful if want to compare plots using the same scale
#'
#' @param num_bins number of bins on the legend (what displayed might not be exact)
#'
#' @param legend.label label for the legend, such as 'Coefficient of Variation'
#'
#' @param no.hatching whether to hatch region with problematic uncertainties, recommend F (depends on rgeos package, set to T if not installed)
#'
#' @param map.title title for the map
#'
#' @param use.basemap what basemap to use 'OSM', if NULL, no basemap
#'
#' @param threshold.p cutoff for the exceedance probability map
#'
#' @param polygon.weight width of the border line of each admin
#'
#' @param percentage whether to display percentage or by 1000 in hover in and in legend
#'
#' @return leaflet map object
#' 
#'
#'
#'


prevMap.leaflet <- function(res.obj,
                            poly.shp,
                            admin1.focus = NULL,
                            color.palette = NULL,
                            value.to.plot = 'mean',
                            value.range = NULL,
                            num_bins=NULL,
                            legend.label = 'Estimates',
                            map.title = NULL,
                            color.reverse = T,
                            no.hatching= F,
                            hatching.density=12,
                            use.basemap= NULL,
                            threshold.p=NULL,
                            legend.color.reverse= T,
                            legend.appear = F,
                            polygon.weight = 1,
                            percentage = TRUE){
  
  ########################################################
  ### check required packages
  ########################################################
  
  ### get admin levels from admin.info, by surveyPrev definition
  by.adm2 <- res.obj$admin.info$by.adm
  by.adm1 <- ifelse(is.null(res.obj$admin.info$by.adm.upper), by.adm2, 
                    res.obj$admin.info$by.adm.upper)
  adm_level <- res.obj$admin
  
  if (!requireNamespace("leaflegend", quietly = TRUE)) {
    stop("Package 'leaflegend' is required for this function. Please install it with install.packages('leaflegend').")
  }
  
  if (!requireNamespace("viridisLite", quietly = TRUE)) {
    stop("Package 'viridisLite' is required for this function. Please install it with install.packages('viridisLite').")
  }
  
  ########################################################
  ### initialize parameters
  ########################################################
  
  poly.shp <- sf::st_as_sf(poly.shp)
  
  
  ########################################################
  ### prepare to.plot data set
  ########################################################
  ##
  
  survey.res <- res.obj[[paste0("res.admin", adm_level)]]
  survey.res$n.clusters <- res.obj$data.info$n_clusters
  
  if (adm_level == "0") {
    post.samp.mat <- NULL
  } else {
    post.samp.mat <- res.obj[[paste0("admin", adm_level, "_post")]]
  }
  
  
  res.to.plot <- harmonize_all_cols(survey.res=survey.res)
  #res.to.plot$n.clusters <- res.obj$data.info$n_clusters
  
  ### prepare exceedance probabilities
  if(value.to.plot=='exceed_prob'){
    
    post.samp.mat <- res.obj[[paste0('admin',adm_level,'_post')]]
    
    if(is.null(post.samp.mat)){stop('No posterior samples provided, cannot produce exceedance probability map.')}
    if(is.null(threshold.p)){stop('No threshold provided, cannot produce exceedance probability map.')}
    
    ### process posterior samples to be in the correct format
    post.samp.mat <- as.matrix(post.samp.mat)
    
    n.samp= 1000
    if(dim(post.samp.mat)[2]==n.samp&dim(post.samp.mat)[1]<dim(post.samp.mat)[2]){
      post.samp.mat <- t(post.samp.mat)
    }
    
    # threshold.p <- 0.1
    
    res.to.plot$exceed_prob <- apply(post.samp.mat,2,function(x){sum(x>threshold.p)/length(x)})
    
    ### if no valid uncertainty measure, assign NA to exceedance probability
    if(sum(is.na(res.to.plot$var))>0){
      res.to.plot[is.na(res.to.plot$var),]$exceed_prob <- NA
    }
    
  }
  
  
  ########################################################
  ### merge results with spatial dataset
  ########################################################
  ##
  ##  if user hope to create map of specific admin 1 area, process the            shapefile accordingly
  if (!is.null(admin1.focus)) {
    poly.shp <- poly.shp[poly.shp[[by.adm1]] == admin1.focus, ]
    res.to.plot <- res.to.plot[res.to.plot$upper.adm.name == admin1.focus, ]
  }
  
  ## merge results
  if (adm_level == 0) {
    res.to.plot$region.name <- 'National'
    poly.shp$full_name <- 'National'
    
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    gadm.with.res$region.name=gadm.with.res$full_name
  }
  if (adm_level == 1) {
    poly.shp$full_name <- poly.shp[[by.adm2]]
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    gadm.with.res$region.name=gadm.with.res$full_name
  } 
  
  if (adm_level == 2){
    poly.shp$full_name <- paste0(poly.shp[[by.adm1]], "_", poly.shp[[by.adm2]])
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name.full"))
    
    gadm.with.res$region.name <- gadm.with.res[[by.adm2]]
    gadm.with.res$upper.adm.name <- gadm.with.res[[by.adm1]]
  }
  
  
  ### modify the format of numeric values and add warning messages
  gadm.with.res$warnings <- NA
  
  ### problematic sd, warnings and set uncertainty related measure to NA
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(warnings = dplyr::if_else(!is.na(mean) &is.na(sd),
                                            "Data in this region are insufficient for <br/> reliable estimates with the current method.",
                                            NA)
    )
  
  ### no data warning
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(warnings = dplyr::if_else(is.na(mean),
                                            "No data in this region",
                                            warnings))
  
  gadm.with.res$value <- gadm.with.res[[value.to.plot]] ### name the variable to plot as value
  
  ### cv to %
  gadm.with.res <- gadm.with.res %>%
    dplyr::mutate(cv = sprintf("%.1f%%", cv * 100))
  
  
  if(value.to.plot=='exceed_prob'){
    gadm.with.res <- gadm.with.res %>%
      dplyr::mutate(exceed_prob = sprintf("%.1f%%", exceed_prob * 100))
  }
  
  ### formatting numeric variables to 2 decimal places
  if(percentage){
    gadm.with.res <- gadm.with.res %>%
      dplyr::mutate(across(c(mean, var, lower, upper,CI.width), ~sprintf("%.2f", .)))
  } else {
    gadm.with.res <- gadm.with.res %>%
      dplyr::mutate(across(c(mean, var, lower, upper,CI.width), ~sprintf("%.3f", .)))
  }
  
  
  
  ########################################################
  ### hatching for problematic sd
  ########################################################
  
  hatching.ind <- T
  
  hatching.gadm <- gadm.with.res %>%
    subset( is.na(sd) & (!is.na(value)))
  
  ### no hatching if all regions have reasonable sd or manually set
  if(dim(hatching.gadm)[1]==0 | (no.hatching)){
    hatching.ind <- F
  }else{
    
    ### setup hatching polygons
    hatching.regions <- hatched.SpatialPolygons(hatching.gadm,
                                                density = c(hatching.density), angle = c(45))
    
    ### setup hatching legend
    warning.icon <- leaflet::awesomeIconList(
      'Sparse Data' =leaflet::makeAwesomeIcon(icon = "align-justify", library = "glyphicon",
                                              iconColor = 'gray',
                                              markerColor = 'white',
                                              squareMarker = TRUE, iconRotate = 135)
    )
  }
  
  #############################################
  ### parameters for color scale and breaks
  #############################################
  
  ### determine color palette for statistics, if not pre-specified
  ### r built-in palette
  
  if(is.null(color.palette)){
    color.palette='viridis'
    if(value.to.plot==c('mean')){
      color.palette = 'viridis'
    }
    if(value.to.plot==c('cv')){
      #color.palette = viridisLite::inferno(10)[2:10]
      color.palette = viridisLite::mako(10)[2:10]
    }
    if(value.to.plot==c('CI.width')){
      #color.palette = viridisLite::inferno(10)[2:10]
      color.palette = viridisLite::plasma(10)[3:10]
    }
    if(value.to.plot==c('exceed_prob')){
      #color.palette = 'cividis'
      #color.palette = viridisLite::cividis(10)
      color.palette = viridisLite::rocket(10)[3:10]
    }
  }
  
  ### determine value range if not specified, also create legend data
  if(is.null(value.range)){
    
    value.range <-  gadm.with.res$value
    
    if(value.to.plot=='exceed_prob'){
      value.range <- c(-0.001,1.001)
    }
    
    ### if no range specified, use data to determine limits for color schemes
    legend.dat <- gadm.with.res
    
    if(max(gadm.with.res$value,na.rm=T)-min(gadm.with.res$value,na.rm=T)<0.005){
      new.max <- min(1, max(gadm.with.res$value,na.rm=T)+0.005)
      new.min <- max(0, min(gadm.with.res$value,na.rm=T)-0.005)
      
      legend.dat <- data.frame(value=seq(new.min,new.max,length.out	=10),ID=c(1:10))
      
    }
    
  }else{
    ### if range specified, use range determine limits for color schemes
    legend.dat <- data.frame(value=seq(value.range[1],value.range[2],length.out	=10),
                             ID=c(1:10))
    
  }
  
  ### number of ticks on the legend
  if(is.null(num_bins)){
    if(value.to.plot=='exceed_prob'){
      num_bins <- 6
      
    }else{
      num_bins <- 4
    }
  }
  
  ### color palette
  if(value.to.plot=='exceed_prob'){
    pal <- leaflet::colorNumeric(palette = color.palette,
                                 domain = value.range,
                                 #na.color = '#9370DB',
                                 na.color = '#AEAEAE',
                                 reverse = color.reverse)
    
    ### whether to reverse color scheme on legend (for fixing bugs)
    if(!legend.color.reverse){legend.reverse=color.reverse}else{legend.reverse=!color.reverse}
    
    pal.legend <- leaflet::colorNumeric(palette = color.palette,
                                        domain = value.range,
                                        #na.color = '#9370DB',
                                        na.color = '#AEAEAE',
                                        reverse = legend.reverse)
    
    
  }else{
    pal <- leaflet::colorNumeric(palette = color.palette,
                                 domain = value.range,
                                 na.color = '#AEAEAE',
                                 reverse = color.reverse)
    
    ### whether to reverse color scheme on legend (for fixing bugs)
    
    if(!legend.color.reverse){legend.reverse=color.reverse}else{legend.reverse=!color.reverse}
    
    pal.legend <- leaflet::colorNumeric(palette = color.palette,
                                        domain = value.range,
                                        #na.color = '#9370DB',
                                        na.color = '#AEAEAE',
                                        reverse = legend.reverse)
    
  }
  
  numberFormat = function(x) {
    prettyNum(x, format = "f", big.mark = ",", digits =
                3, scientific = FALSE)
  }
  
  if(value.to.plot %in% c('cv','exceed_prob')){
    
    numberFormat = function(x) {
      paste0(formatC(100 * x, format = 'f', digits = 1), "%")
    }
    
  }
  
  ###############################################
  ### hovering effect, information to display
  ###############################################
  hover_labels <- gadm.with.res %>%
    dplyr::rowwise() %>%
    dplyr::mutate(hover_label = {
      label <- paste0('Region: ', region.name, '<br/>')
      
      if (!is.null(by.adm1) && !is.null(by.adm2) && by.adm1 != by.adm2) {
        label <- paste0(label,  'Upper Admin: ', upper.adm.name, '<br/>')
      }
      
      if(value.to.plot=='exceed_prob'){
        label <- paste0(label,  'Prob (prevalence > ',threshold.p,') = ', exceed_prob, '<br/>')
      }
      
      if(percentage) {
        label <- paste0(label,
                        'Estimate (90% CI): ', as.character(round(as.double(mean)*100, 1)), '% (', 
                        as.character(round(as.double(lower)*100, 1)), '%, ', 
                        as.character(round(as.double(upper)*100, 1)), '%)', '<br/>',
                        'Number of Clusters: ', n.clusters,'<br/>')
      } else {
        label <- paste0(label,
                        'Rate (90% CI): ', as.character(round(as.double(mean)*1000, 1)), ' (', 
                        as.character(round(as.double(lower)*1000, 1)), ', ', 
                        as.character(round(as.double(upper)*1000, 1)), ')', '<br/>',
                        'Number of Clusters: ', n.clusters,'<br/>')
      }

      
      if (!is.na(warnings) && warnings != "") {
        label <- paste0(label, '<span style="color: red;">Warning: ', warnings, '</span><br/>')
      }
      htmltools::HTML(label)  # Ensure that HTML rendering is applied
    }) %>%
    dplyr::ungroup() %>%
    dplyr::pull(hover_label)
  
  ###############################################
  ### assemble
  ###############################################
  
  ### base map
  adm.map <- gadm.with.res  %>% leaflet::leaflet(
    options = leaflet::leafletOptions(zoomControl = FALSE, attributionControl = FALSE, scrollWheelZoom = FALSE))
  adm.map <- add_basemap(original.map=adm.map,
                         static.ind= F,
                         basemap.type =use.basemap)
  
  #if(use.basemap=='OSM'){ adm.map <- adm.map %>%  leaflet::addTiles()}
  
  
  adm.map <- adm.map %>%
    leaflet::addPolygons(
      fillColor = ~pal(value),
      weight = polygon.weight,
      color = "gray",
      fillOpacity = 1,
      opacity = 1,
      label = ~ hover_labels, # display hover label
      labelOptions = leaflet::labelOptions(
        style = list("color" ="black"),  # Text color
        direction = "auto",
        textsize = "12px",
        noHide = F,  # Label disappears when not hovering
        offset = c(0,0)  # Adjust label position if necessary
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.75,
        bringToFront = TRUE,
        sendToBack=T)
    )
  
  
  ### add legend
  missingLabel <- ifelse(value.to.plot=='mean', 'No Data', 'N/A')
  if(legend.appear) {
    if(percentage){
      adm.map <- adm.map %>%
        leaflegend::addLegendNumeric(pal = pal.legend, values = ~value, title =  htmltools::HTML(legend.label),
                                     tickWidth = 0,#tickLength = 0, 
                                     orientation = 'vertical', fillOpacity = .7,
                                     position = 'bottomright', group = 'Symbols',
                                     width=15,height=80,naLabel = missingLabel,
                                     data=legend.dat,
                                     bins = num_bins, # Custom tick positions
                                     numberFormat=scales::label_percent(accuracy = 1),
                                     decreasing=T
        ) 
    } else {
      adm.map <- adm.map %>%
        leaflegend::addLegendNumeric(pal = pal.legend, values = ~value, title =  htmltools::HTML(legend.label),
                                     tickWidth = 0,#tickLength = 0, 
                                     orientation = 'vertical', fillOpacity = .7,
                                     position = 'bottomright', group = 'Symbols',
                                     width=15,height=80,naLabel = missingLabel,
                                     data=legend.dat,
                                     bins = num_bins, # Custom tick positions
                                     numberFormat = scales::label_number(
                                       scale = 1000,
                                       suffix = "",
                                       accuracy = 1
                                     ),
                                     decreasing=T
        ) 
    }
    
  }
  
  if(hatching.ind){
    
    adm.map <- adm.map %>% leaflet::addPolylines(
      data = hatching.regions,
      color = c( "gray"),
      weight = 2.0,
      opacity = 0.8
    )
    # adm.map <- adm.map %>% leaflegend::addLegendAwesomeIcon(iconSet = warning.icon,
    #                                                         title = 'Interpret with caution:',
    #                                                         position = 'bottomright')
    
  }
  
  ### add title
  
  if(!is.null(map.title)){
    
    tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    background: rgba(255,255,255,0.65);
    font-weight: bold;
    font-size: 20px;
    }
    "))
    
    title <- tags$div(
      tag.map.title, HTML(paste0(map.title))
    )
    
    adm.map <- adm.map %>%
      leaflet::addControl(title, position = "topleft", className="map-title")
  }
  
  # make background transparent
  adm.map <- htmlwidgets::onRender(adm.map, "function(el, x) {
    el.style.background = 'transparent';
  }")
  
  # remove ticks on legend
  adm.map <- adm.map %>%
    leaflet::addControl(
      html = htmltools::HTML(
        "<style>
        /* kill legend axis ticks (SVG lines only inside leaflegend) */
        .leaflegend svg line {
          stroke: none !important;
        }
      </style>"
      ),
      position = "topright"
    )
  
  return(adm.map)
}

prevMap.static <- function(res.obj,
                           poly.shp ,
                           admin1.focus = NULL,
                           value.to.plot = 'mean',
                           map.title = NULL,
                           legend.label = 'Estimates',
                           threshold.p = NULL,
                           color.palette = NULL,
                           color.reverse = T,
                           value.range=NULL,
                           show.legend=F,
                           show.hatching = F,
                           ...){
  
  
  ########################################################
  ### initialize parameters
  ########################################################
  
  poly.shp <- sf::st_as_sf(poly.shp)
  
  ### get admin levels from admin.info, by surveyPrev definition
  by.adm2 <- res.obj$admin.info$by.adm
  by.adm1 <- ifelse(is.null(res.obj$admin.info$by.adm.upper), by.adm2, 
                    res.obj$admin.info$by.adm.upper)
  adm_level <- res.obj$admin
  
  ### determine color scheme if not specified
  
  if(is.null(color.palette)){
    
    color.palette = viridisLite::viridis(10)
    
    if(value.to.plot==c('mean')){
      color.palette = viridisLite::viridis(10)
    }
    if(value.to.plot==c('cv')){
      color.palette = viridisLite::mako(10)[2:10]
    }
    if(value.to.plot==c('CI.width')){
      #color.palette = viridisLite::inferno(10)[2:10]
      color.palette = viridisLite::plasma(10)[3:10]
    }
    if(value.to.plot==c('exceed_prob')){
      #color.palette = viridisLite::cividis(10)
      color.palette = viridisLite::rocket(10)[3:10]
      
    }
    
    ### whether to reverse color scale
    if(color.reverse){color.palette <- rev(color.palette)}
    
  }
  
  ########################################################
  ### prepare to.plot data set
  ########################################################
  
  survey.res <- res.obj[[paste0('res.admin',adm_level)]]
  
  res.to.plot <- harmonize_all_cols(survey.res=survey.res)
  
  ### prepare exceedance probabilities
  if(value.to.plot=='exceed_prob'){
    
    post.samp.mat <- res.obj[[paste0('admin',adm_level,'_post')]]
    
    if(is.null(post.samp.mat)){stop('No posterior samples provided, cannot produce exceedance probability map.')}
    if(is.null(threshold.p)){stop('No threshold provided, cannot produce exceedance probability map.')}
    
    ### process posterior samples to be in the correct format
    post.samp.mat <- as.matrix(post.samp.mat)
    
    n.samp= 1000
    if(dim(post.samp.mat)[2]==n.samp&dim(post.samp.mat)[1]<dim(post.samp.mat)[2]){
      post.samp.mat <- t(post.samp.mat)
    }
    
    # threshold.p <- 0.1
    
    res.to.plot$exceed_prob <- apply(post.samp.mat,2,function(x){sum(x>threshold.p)/length(x)})
    
  }
  
  ########################################################
  ### merge to spatial data
  ########################################################
  
  # focus on given admin 1 level area map if user specified
  if (!is.null(admin1.focus)) {
    poly.shp <- poly.shp[poly.shp[[by.adm1]] == admin1.focus, ]
    res.to.plot <- res.to.plot[res.to.plot$upper.adm.name == admin1.focus, ]
  }
  
  if(adm_level==0){
    res.to.plot$region.name <- 'National'
    poly.shp$full_name <- 'National'
    
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    gadm.with.res$region.name=gadm.with.res$full_name
    
  }
  
  if(adm_level==1){
    poly.shp$full_name <- poly.shp[[by.adm2]]
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name"))
    
  }
  
  if(adm_level==2){
    poly.shp$full_name <- paste0(poly.shp[[by.adm1]], "_", poly.shp[[by.adm2]])
    gadm.with.res <- poly.shp %>%
      dplyr::left_join(res.to.plot, by = c("full_name" = "region.name.full"))
  }
  
  
  gadm.with.res$value <- gadm.with.res[[value.to.plot]] ### name the variable to plot as value
  gadm.with.res$method <- ''
  
  
  ########################################################
  ### hatching for problematic sd (same logic as leaflet)
  ########################################################
  hatching.ind <- TRUE
  hatching.gadm <- gadm.with.res %>%
    subset(is.na(sd) & (!is.na(value)))
  
  if (nrow(hatching.gadm) == 0 | show.hatching) {
    hatching.ind <- FALSE
  }
  
  ### determine value range if not specified, also create legend data
  if(is.null(value.range)){
    
    value.range <-  c(min(gadm.with.res$value,na.rm=T),
                      max(gadm.with.res$value,na.rm=T))
    
    if(value.to.plot=='exceed_prob'){
      value.range <- c(0,1)
    }
    
  }
  
  
  
  static.map <- SUMMER::mapPlot(gadm.with.res, variables = "method", values = "value",
                                by.data = "full_name", geo = poly.shp,
                                by.geo = "full_name", is.long = TRUE,
                                removetab = T,...)+
    ggplot2::theme (legend.text=ggplot2::element_text(size=12),
                    legend.title = ggplot2::element_text(size=14),
                    strip.text.x = ggplot2::element_text(size = 12),
                    legend.key.height = ggplot2::unit(1,'cm') )
  
  ########################################################
  ### add hatching layer if needed
  ########################################################
  if (hatching.ind) {
    # make sure ggpattern is available
    if (!requireNamespace("ggpattern", quietly = TRUE)) {
      stop("Package 'ggpattern' is required for hatching. Please install it with install.packages('ggpattern')")
    }
    
    static.map <- static.map +
      ggpattern::geom_sf_pattern(
        data = hatching.gadm,
        fill = NA,
        pattern = "stripe",
        pattern_angle = 45,
        pattern_density = 0.2,
        pattern_spacing = 0.03,
        pattern_fill = "black",
        color = "black",
        inherit.aes = FALSE,
        linewidth = 0.2,
        show.legend = FALSE
      )
  }
  
  na.color = 'gray50'
  
  if (show.legend) {
    if (value.to.plot %in% c('cv','exceed_prob')) {
      static.map <- static.map +
        ggplot2::scale_fill_gradientn(
          colours = color.palette,
          limits = value.range,
          labels = scales::label_percent(),
          name = paste0(legend.label,'\n'),
          na.value = na.color
        )
    } else {
      static.map <- static.map +
        ggplot2::scale_fill_gradientn(
          colours = color.palette,
          limits = value.range,
          labels = scales::label_number(accuracy = 0.01),
          name = paste0(legend.label,'\n'),
          na.value = na.color
        )
    }
  } else {
    if (value.to.plot %in% c('cv','exceed_prob')) {
      static.map <- static.map +
        ggplot2::scale_fill_gradientn(
          colours = color.palette,
          limits = value.range,
          labels = NULL,
          name = NULL,
          na.value = na.color,
          guide = "none"
        )
    } else {
      static.map <- static.map +
        ggplot2::scale_fill_gradientn(
          colours = color.palette,
          limits = value.range,
          labels = NULL,
          name = NULL,
          na.value = na.color,
          guide = "none"
        )
    }
  }
  
  
  
  
  return(static.map)
  
  
}

posterior_ridge_plot <- function(res.obj,
                                 admin1.focus=NA ,
                                 plot.extreme.num=8,
                                 legend.label = 'Value',
                                 color.reverse= T,
                                 plot.format = c('Long','Wide')[1],
                                 top.bottom.label=c('Top','Bottom'),
                                 palette = viridisLite::viridis(10),
                                 plot.difference = F,
                                 percentage = TRUE
){
  ########################################################
  ### initialize parameters
  ########################################################
  adm_level <- res.obj$admin
  
  # color scheme
  if(color.reverse){direction=-1}else{direction=1}
  
  ########################################################
  ### prepare posterior sample data set
  ########################################################
  n.samp=1000
  survey.res <- res.obj[[paste0('res.admin',adm_level)]]
  res.summary <- harmonize_all_cols(survey.res=survey.res)
  post.samp.mat <- res.obj[[paste0('admin',adm_level,'_post')]]
  
  post.samp.mat <- as.matrix(post.samp.mat)
  if(dim(post.samp.mat)[2]==n.samp & dim(post.samp.mat)[1]<dim(post.samp.mat)[2]){
    post.samp.mat <- t(post.samp.mat)
  }
  
  if(adm_level==1){
    by.res = 'region.name'
    if(percentage) {
      res.to.plot <- tidyr::pivot_longer(
        as.data.frame(post.samp.mat),
        cols = dplyr::everything(),
        names_to = "region.name",
        values_to = "value"
      )
    } else {
      res.to.plot <- tidyr::pivot_longer(
        as.data.frame(post.samp.mat * 1000),
        cols = dplyr::everything(),
        names_to = "region.name",
        values_to = "value"
      )
    }
    
    res.summary.to.match <- as.data.frame(res.summary)
    res.summary.to.match$order.name <- res.summary.to.match[,by.res]
  }
  
  if(adm_level==2){
    by.res = 'region.name.full'
    if(!is.na(admin1.focus)){
      res.summary[,by.res] <- paste0(res.summary$region.name)
    } else {
      res.summary[,by.res] <- paste0(res.summary$region.name,' (',res.summary$upper.adm.name,')')
    }
    
    if(percentage) {
      res.to.plot <- data.frame(region.name = rep(res.summary[, by.res], each=n.samp),
                                upper.adm.name = rep(res.summary[, 'upper.adm.name'], each=n.samp),
                                value = as.numeric(post.samp.mat))
    } else {
      res.to.plot <- data.frame(region.name = rep(res.summary[, by.res], each=n.samp),
                                upper.adm.name = rep(res.summary[, 'upper.adm.name'], each=n.samp),
                                value = as.numeric(post.samp.mat) * 1000)
    }
    
    
    if(!is.na(admin1.focus)){
      res.to.plot <- res.to.plot[res.to.plot$upper.adm.name == admin1.focus,]
      if(dim(res.to.plot)[1]==0){
        message(paste0('wrong upper admin name - cannot plot ',admin1.focus))
        return()
      }
      res.summary.to.match <- as.data.frame(res.summary)
      res.summary.to.match <- res.summary.to.match[res.summary.to.match$upper.adm.name == admin1.focus,]
      res.summary.to.match$order.name <- res.summary.to.match[,by.res]
    } else {
      res.summary.to.match <- as.data.frame(res.summary)
      res.summary.to.match$order.name <- res.summary.to.match[,by.res]
    }
  }
  
  ########################################################
  ### prepare order and plot data set
  ########################################################
  n.regions <- dim(res.summary.to.match)[1]
  
  if(!is.na(plot.extreme.num)){
    if(n.regions < 2*plot.extreme.num+1){
      plot.extreme.num=NA
    }
  }
  
  order.num <- order(res.summary.to.match[['median']], decreasing = TRUE)
  region.name.vec <- res.summary.to.match[['order.name']]
  res.to.plot.order <- region.name.vec[order.num]
  
  if(!is.na(plot.extreme.num)){
    n.levels <- plot.extreme.num*2
    ridge_top_k_order <- res.to.plot.order[1:plot.extreme.num]
    ridge_bottom_k_order <- res.to.plot.order[(n.regions-plot.extreme.num+1):n.regions]
    
    top_k_plot_dt <- res.to.plot[res.to.plot$region.name %in% ridge_top_k_order, ]
    top_k_plot_dt$region.name = factor(top_k_plot_dt$region.name, levels = rev(ridge_top_k_order))
    top_k_plot_dt$rank <-  paste0(plot.extreme.num, top.bottom.label[1])
    
    bottom_k_plot_dt <- res.to.plot[res.to.plot$region.name %in% ridge_bottom_k_order, ]
    bottom_k_plot_dt$region.name = factor(bottom_k_plot_dt$region.name, levels = rev(ridge_bottom_k_order))
    bottom_k_plot_dt$rank <- paste0(plot.extreme.num, top.bottom.label[2])
    
    res.to.plot <- rbind(top_k_plot_dt,bottom_k_plot_dt)
    res.to.plot$rank <- factor(res.to.plot$rank, 
                               levels = c(paste0(plot.extreme.num, top.bottom.label[1]),
                                          paste0(plot.extreme.num, top.bottom.label[2])))
  }
  
  if(is.na(plot.extreme.num)){
    n.levels <- n.regions
    res.to.plot$region.name = factor(res.to.plot$region.name, levels = rev(res.to.plot.order))
  }
  
  ########################################################
  ### set up ranges of values
  ########################################################
  ridge.max <- max(res.to.plot$value, na.rm = TRUE)*1.03
  if(ridge.max>0.95 && ridge.max <= 1){ridge.max=1}
  ridge.min <- min(res.to.plot$value, na.rm = TRUE)*0.97
  if(ridge.min<0.05){ridge.min=0}
  
  if(plot.difference){
    ridge.max <- max(res.to.plot$value, na.rm = TRUE)*1.03
    ridge.min <- min(res.to.plot$value, na.rm = TRUE)*0.97
  }
  
  ########################################################
  ### plot posterior density
  ########################################################
  if (percentage) {
    ridge.plot.adm <- ggplot2::ggplot(res.to.plot, ggplot2::aes(x = value, y = region.name)) +
      ggridges::geom_density_ridges_gradient(ggplot2::aes(fill = ggplot2::after_stat(x)),
                                             scale= max(15/n.levels,3)) +
      ggplot2::scale_fill_gradientn(
        colors = palette,
        limits = c(ridge.min, ridge.max),
        name = legend.label,
        labels = scales::label_percent(accuracy = 1)   # format legend ticks as percentages
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::label_percent(accuracy = 1)   # format x-axis ticks as percentages
      )
  } else {
    ridge.plot.adm <- ggplot2::ggplot(res.to.plot, ggplot2::aes(x = value, y = region.name)) +
      ggridges::geom_density_ridges_gradient(ggplot2::aes(fill = ggplot2::after_stat(x)),
                                             scale= max(15/n.levels,3)) +
      ggplot2::scale_fill_gradientn(
        colors = palette,
        limits = c(ridge.min, ridge.max),
        name = legend.label,
        labels = scales::label_number(accuracy = 1)   # format legend ticks as number
      ) +
      ggplot2::scale_x_continuous(
        labels = scales::label_number(accuracy = 1)   # format x-axis ticks as number
      )
  }
  
  
  if(!is.na(plot.extreme.num)){
    if(is.null(plot.format)){ plot.format='Long' }
    
    if(plot.format=='Long'){
      ridge.plot.adm <- ridge.plot.adm +
        ggplot2::facet_grid(rank ~ ., scales = "free") +
        ggplot2::theme(panel.spacing = ggplot2::unit(1.5, "lines"))
    }
    if(plot.format=='Wide'){
      ridge.plot.adm <- ridge.plot.adm +
        ggplot2::facet_wrap(rank ~ ., scales = "free") +
        ggplot2::theme(panel.spacing = ggplot2::unit(1.5, "lines"))
    }
  }
  
  ########################################################
  ### styles for the plot
  ########################################################
  if(!is.na(plot.extreme.num) & plot.format=='Wide' & adm_level==2){
    y.text.size = 13
  } else { y.text.size=16 }
  
  ylabel <- ifelse(!is.na(admin1.focus), admin1.focus, "Region name")
  
  ridge.plot.adm <- ridge.plot.adm +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5,size=20),
                   text = ggplot2::element_text(size=16),
                   strip.text = ggplot2::element_text(size=18),
                   axis.text.y = ggplot2::element_text(size = y.text.size,
                                                       margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0))) +
    ggplot2::xlab("") +
    ggplot2::ylab(ylabel) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "left",
                                                    title.hjust = 1,
                                                    title=legend.label,
                                                    label.position = "bottom")) +
    ggplot2::theme (legend.position = 'top',
                    legend.key.height= ggplot2::unit(0.5,'cm'),
                    legend.text= ggplot2::element_text(size=16),
                    legend.key.width = ggplot2::unit(2,'cm'),
                    legend.title = ggplot2::element_text(size=16,
                                                         margin = ggplot2::margin(r = 30)),
                    legend.box.margin= ggplot2::margin(-15,0,0,0),
                    legend.margin=ggplot2::margin(0,100,0,0),
                    panel.background = ggplot2::element_rect(fill = "white", colour = NA), 
                    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),  
                    panel.grid = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank(),
                    plot.margin = margin(t = 30))
  
  if (plot.difference) {
    ridge.plot.adm <- ridge.plot.adm +
      ggplot2::geom_vline(xintercept = 0,
                          linetype = "dashed",
                          colour = "red",
                          size = 0.6)
  }
  
  return(ridge.plot.adm)
}





#' visualization_helpers
#'
#' @description make the naming for one column the same for different methods
#'
#' @param survey.res result summary data.frame
#'
#' @param from_col possible column names for a feature, such as c("direct.est", "mean")
#'
#' @param to_col harmonized name for the column
#'
#' @return data.frame with modified column names
#'
#' @noRd
#'
#'

harmonize_one_col <- function(survey.res,
                              from_col,
                              to_col){
  
  # Find the first matching column in the list
  existingCol <- from_col[from_col %in% names(survey.res)][1]
  
  # Create harmonized column if found match, else assign NA
  survey.res[to_col] <- if (!is.null(existingCol)) survey.res[[existingCol]] else NA
  
  
  return(survey.res)
}


###############################################################
### harmonize column names
###############################################################
#' visualization_helpers
#'
#' @description make the naming for all columns the same for different methods and admin levels
#'
#' @param survey.res result summary data.frame
#'
#' @return data.frame with modified column names
#'
#' @noRd
#'
#'

harmonize_all_cols <- function(survey.res){
  
  ###### harmonize summary statistics
  
  stat_var <- c('mean','median','sd','var','lower','upper','CI.width','cv', 'n.clusters')
  ### mean
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.est','mean'),
                                 to_col = 'mean')
  
  ### sd
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.se','sd'),
                                 to_col = 'sd')
  
  ### var
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.var','var'),
                                 to_col = 'var')
  
  ### coefficient of variation
  survey.res$cv <- survey.res$sd/survey.res$mean
  
  
  ### lower CI
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.lower','lower'),
                                 to_col = 'lower')
  
  ### upper CI
  survey.res<- harmonize_one_col(survey.res=survey.res,
                                 from_col = c('direct.upper','upper'),
                                 to_col = 'upper')
  
  
  ### CI width
  survey.res$CI.width <- survey.res$upper-survey.res$lower
  
  
  ### set problematic uncertainties to NA
  survey.res <- survey.res %>%
    dplyr::mutate(var = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,var),
                  lower = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,lower),
                  upper = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,upper),
                  cv = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,cv),
                  CI.width = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,CI.width)
    )
  survey.res <- survey.res %>%
    dplyr::mutate(sd = dplyr::if_else(sd < 1e-08|sd > 1e10,NA,sd))
  
  
  ### for direct estimates, mean is median
  if(!'median' %in% names(survey.res)){
    survey.res$median <- survey.res$mean
  }
  
  ###### harmonize region variables
  
  ### national estimates
  if(!'admin1.name' %in% names(survey.res)& !'admin2.name.full'%in% names(survey.res)){
    
    survey.res <- survey.res[, stat_var[stat_var %in% names(survey.res)], drop = FALSE]
    
    return(survey.res)
  }
  
  
  ### estimates not finer than stratification level
  
  if(!'admin2.name.full' %in% names(survey.res)){
    
    survey.res$region.name <- survey.res$admin1.name
    
    res.var <- c('region.name',stat_var)
    
    survey.res <- survey.res[, res.var[res.var %in% names(survey.res)], drop = FALSE]
    
    return(survey.res)
  }
  
  ### estimates finer than stratification level
  
  if('admin2.name.full' %in% names(survey.res)){
    
    if('admin1.name' %in% names(survey.res)){
      survey.res$region.name <-  survey.res$admin2.name
      survey.res$upper.adm.name <- survey.res[['admin1.name']]
    }else{
      survey.res <- survey.res %>%
        tidyr::separate(admin2.name.full, into = c("upper.adm.name", "region.name"), sep = "_", remove = FALSE)
    }
    
    survey.res$region.name.full <- survey.res[['admin2.name.full']]
    
    res.var <- c('region.name',stat_var,'upper.adm.name','region.name.full')
    
    survey.res <- survey.res[, res.var[res.var %in% names(survey.res)], drop = FALSE]
    
    return(survey.res)
  }
  
}

###############################################################
### function to add basemap
###############################################################
#' @description produce interactive map for country boundaries
#'
#' @param original.map the object to add basemap on
#'
#' @param static.ind indicator of static (ggplot2) or interactive map (leaflet)
#'
#' @param basemap.type what basemap to use 'OSM' or 'WHO'
#'
#' @return leaflet/ggplot2 object
#'
#' @noRd
#'

add_basemap <- function(original.map,
                        static.ind= F,
                        basemap.type =NULL){
  
  
  if(is.null(basemap.type)){
    return(original.map)
  }
  
  if(basemap.type=='OSM'&static.ind==F){
    
    return.map <- tryCatch({
      original.map %>%  leaflet::addTiles()
    },error = function(e) {
      message(e$message)
      message('basemap did not load successfully')
      return.map <<- original.map
    })
    
  }else{
    
    return.map <- original.map
    
  }
  
}

# helper function to plots

shrink_plot <- function(p) {
  p + theme(plot.margin = margin(t = 0, r = 2, b = 0, l = 2))
}

# helper: no data ggplot
make_no_data_plot <- function(){
  ggplot() + 
    geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
              fill = NA, color = "black", linewidth = 0.8) +
    annotate("text", x = 0.5, y = 0.5, label = "No Data", size = 6, hjust = 0.5, vjust = 0.5) +
    theme_void()
}

# helper: grey leaflet when no data
make_no_data_leaflet <- function(poly_shp, label = "No Data") {
  leaflet::leaflet(poly_shp, options = leafletOptions(
    zoomControl = FALSE,   
    dragging = FALSE,
    scrollWheelZoom = FALSE,
    doubleClickZoom = FALSE,
    touchZoom = FALSE
  )) %>%
    leaflet::addPolygons(
      fillColor = "transparent",
      color = "transparent",
      weight = 0,
      opacity = 0,
      fillOpacity = 0
    )%>%
    leaflet::addLabelOnlyMarkers(
      lng = mean(sf::st_coordinates(sf::st_centroid(sf::st_union(poly_shp)))[,1]),
      lat = mean(sf::st_coordinates(sf::st_centroid(sf::st_union(poly_shp)))[,2]),
      label = label,
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        textOnly = TRUE,
        style = list(
          "font-size" = "18px",
          "font-weight" = "bold",
          "color" = "black",
          "background" = "rgba(255,255,255,0.6)",
          "padding" = "4px"
        )
      )
    )
}