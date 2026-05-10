


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





library(dplyr)



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


saveclustermap(
  country= "Malawi",
  middle_path="Gates-results/Results",
  plot_path_c=file.path(source_path,"Gates-results/ReportPlots",country),
  indicatorlist =infolist$ID  
)
