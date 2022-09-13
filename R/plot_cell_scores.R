



plot_cell_scores <- function(data_list, isos, legend_size = 2, font_size = 18){
  
  #' Creates ggplot map of cell-scores
  #' 
  #' @description
  #' Turns the output_list from get_cell_scores into a ggplot map
  #' 
  #' @usage
  #' plot_cell_scores(data_list, isos, legend_size = 2, font_size = 18)
  #' 
  #' @param data_list list: list of 3 sf dataframes obtained from get_cell_scores names 'cells', 'events', and 'adm'
  #' @param isos character/numeric: A vector of numeric iso3n values or character iso3c values for plotting country boundaries
  #' @param legend_size numeric: numeric value to be passed into the ggplot call for the legend key size
  #' @param font_size numeric: numeric value to be passed into the ggplot call for the legend's font size
  #' 
  #' @returns
  #' ggplot object
  #' 
  #' @examples
  #' data_list <- get_cell_scores(iso = ison("Western Asia"), years = 2020, weights)
  #' plot_cell_scores(data_list = data2, isos = ison("Western Asia"))
  #' 
  #' @details
  #' Dependencies: ggplot, sf, dplyr, viridis
  #' Uses: sanitize_iso()
  
  #browser()
  isos <- sanitize_iso(isos)
  #isos <- ison(isos)
  adm0 <- filter(adm0_cgaz, iso3n %in% isos)
  out_plot <- ggplot() +
    geom_sf(data = data_list$cells, aes(fill = score), color = NA) +
    geom_sf(data = data_list$events, aes(col = int_cat)) +
    geom_sf(data = data_list$adm, col = "grey", size = 0.5, fill = NA) +
    geom_sf(data = adm0) +
    scale_fill_viridis_c() +
    theme(legend.key.size = unit(legend_size, 'cm'),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="#FFEBCD"))
  
  return(out_plot)
}
