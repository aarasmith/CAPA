



get_cell_scores <- function(iso, years, weights, start_end = c(1,12), draw_adm1 = TRUE, draw_points = TRUE){
  
  #' Query the CAPA database for cell-scores aggregation
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs
  #' 
  #' @usage get_cell_scores(iso, years, weights, start_end = c(1,12), draw_adm1 = TRUE, draw_points = TRUE)
  #' 
  #' @param iso character/numeric: a single or vector of numeric iso3n values or character iso3c values
  #' @param years numeric: a single or vector containing a range of years
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param start_end numeric: vector of length 2 with the 2 numbers corresponding with the start month of the first year and end
  #' month of the final year
  #' @param draw_adm1 boolean: If TRUE will attach ADM1 geometry to the output list under out_list$adm and will be plotted when given to plot_cell_scores()
  #' @param draw_points boolean: If TRUE will attach GED events to the output list under out_list$events and will be plotted when given to plot_cell_scores()
  #' 
  #' @examples 
  #' get_cell_scores(iso = 760, 2019:2020, test_weights, start_end = c(2, 11))
  #' get_cell_scores(c("SYR", "AFG"), 2020, test_weights, start_end = c(1, 12))
  #'                          
  #' @returns list length 3 of sf dataframes. If draw_adm1 or draw_points are FALSE, they will be sf dataframes of nrow = 0
  #' 
  #' @details 
  #' Dependencies: dplyr, sf
  #' Uses: sanitize_iso(), connect_to_capa(), query_cell_scores(), cnames(), disconnect_from_capa()
  #' Used-with: plot_cell_scores()
  
  iso3n <- sanitize_iso(iso)
  
  capa_db <- connect_to_capa()
  
  data <- query_cell_scores(iso3n, years, start_end, weights, capa_db)
  
  data$score <- as.numeric(data$score)
  
  conflict_events <- filter(ged, iso3c %in% isoc(iso) & year %in% years) %>%
    filter(!(year == min(years) & month < start_end[1])) %>%
    filter(!(year == max(years) & month > start_end[2]))
  
  conflict_events$int_cat <- as.factor(conflict_events$int_cat)
  
  if(draw_points == FALSE){
    conflict_events <- head(conflict_events, n = 0)
  }
  
  #assign iso3n to iso3n_vector to avoid name confusion in the filter call
  iso3n_vector <- iso3n
  out_list <- list(cells = data %>% cnames(), adm = filter(adm1_cgaz, iso3n %in% iso3n_vector), events = conflict_events)
  
  if(draw_adm1 == FALSE){
    out_list['adm'] <- NULL
  }
  
  disconnect_from_capa(capa_db)
  
  return(out_list)
  
}
