



plot_adm <- function(df, isos, adm1, id_col = "capa_id", legend_size = 2, font_size = 18){
  
  #' Creates ggplot map of conflict data
  #' 
  #' @description
  #' Turns the output_list from get_cell_scores into a ggplot map
  #' 
  #' @usage
  #' plot_adm(df, isos, id_col = "capa_id", legend_size = 2, font_size = 18)
  #' 
  #' @param df dataframe: dataframe returned from a 'get' function (get_standard_aggregation, get_temporal, etc.)
  #' @param isos character/numeric: A vector of numeric iso3n values or character iso3c values for plotting country boundaries
  #' @param id_col character: name of the column containing the unique ADM1 identifier
  #' @param legend_size numeric: numeric value to be passed into the ggplot call for the legend key size
  #' @param font_size numeric: numeric value to be passed into the ggplot call for the legend's font size
  #' 
  #' @returns
  #' ggplot object
  #' 
  #' @examples
  #' data <- get_standard_aggregation(iso = ison("World"), years = 2020, period = "yearly", adm1 = FALSE, weights = test_weights, threshold = 1)
  #' plot_adm(df = data, isos = ison("World"), adm1 = FALSE)
  #' 
  #' data <- get_standard_aggregation(iso = "SYR", years = 2020, period = "yearly", adm1 = TRUE, weights = test_weights, threshold = 1)
  #' plot_adm(df = data, isos = "SYR", adm1 = TRUE, id_col = "capa_id_adm1")
  #' 
  #' @details
  #' Dependencies: ggplot, sf, dplyr, viridis
  #' Uses: sanitize_iso(), isoc(), ison()
  
  isos <- sanitize_iso(isos) %>% isoc()
  if(adm1){
    isos <- ison(isos)
    df <- left_join(df, adm1_cgaz %>% dplyr::select(capa_id), by = setNames("capa_id", id_col)) %>% st_as_sf()
    y <- filter(adm0_cgaz, iso3n %in% isos)
    my_plot <- ggplot() +
      geom_sf(data = df, aes(fill = risk_pct), col = "grey", size = 0.5) +
      geom_sf(data = y, col = "black") +
      scale_fill_viridis_c(limits = c(0.0001,1)) +
      theme(legend.key.size = unit(legend_size, 'cm'),
            legend.text = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill="#FFEBCD"))
  }else{
    df <- df %>%
      mutate(iso3c = isoc(iso3n)) %>%
      left_join(nid_grid %>% dplyr::select(ISOCODE), by = c("iso3c" = "ISOCODE")) %>% st_as_sf()
    my_plot <- ggplot() +
      geom_sf(data = df, aes(fill = risk_pct), col = "black", size = 0.5) +
      scale_fill_viridis_c(limits = c(0.0001,1)) +
      theme(legend.key.size = unit(legend_size, 'cm'),
            legend.text = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill="#FFEBCD"))
  }
  
  return(my_plot)
  
}
