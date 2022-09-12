



get_cell_scores <- function(iso, years, start_end = c(1,12), weights, draw_adm1 = TRUE, draw_points = TRUE){
  #browser()
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
