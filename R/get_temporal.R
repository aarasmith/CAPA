


#max periods needs should be calculated inside the funtion. It is currently calculated in the dashboard reactively based on inputs and then passed as an argument, but for non-dashboard usage this would be better
get_temporal <- function(iso, years, weights, period, start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = NA, max_periods = NULL){
  
  #' Query the CAPA database for the temporal aggregation
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs. If p_threshold = max_periods that is by definition a 'duration' calculation. Otherwise it is frequency
  #' 
  #' @usage get_temporal(iso, years, weights, period, start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = NA, max_periods = NULL)
  #' 
  #' @param iso character/numeric: a single or vector of numeric iso3n values or character iso3c values or exact region name matches
  #' @param years numeric: a single or vector containing a range of years
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' @param start_end numeric: vector of length 2 with the 2 numbers corresponding with the start period of the first year and end
  #' period of the final year
  #' @param adm1 boolean: if FALSE will use ADM0
  #' @param threshold numeric: a single whole number intensity-score threshold value
  #' @param p_threshold numeric: if not NA, will filter output to select only observations containing the risk_pop that experienced at least p_threshold periods at the threshold
  #' @param max_periods numeric: the maximum number of periods in the query. E.g. 2020 monthly = 12, 2019-2020 quarterly = 8, 2015-2019 yearly = 5
  #' 
  #' @examples 
  #' 
  #' get_temporal("AFG", 2017:2020, test_weights, period = "yearly", start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = 2, max_periods = 4)
  #' get_temporal(4, 2017:2020, test_weights, period = "monthly", start_end = c(1,12), adm1 = TRUE, threshold = 1, p_threshold = NA, max_periods = 48)
  #' get_temporal("World", 2017:2020, test_weights, period = "yearly", start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = 2, max_periods = 4)
  #' 
  #' @returns dataframe
  #' 
  #' @details 
  #' Dependencies: dplyr, tidyr, sf, DBI
  #' Uses: sanitize_iso(), sanitize_threshold(), connect_to_capa(), query_temporal_gv(), query_total_pop(), query_frequency()
  #'       create_placeholder_freq(), placeholder_join_freq(), cnames(), disconnect_from_capa()
  
  iso3n <- sanitize_iso(iso)
  
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- query_temporal_gv(adm1, period, start_end, years, capa_db)
  if("World" %in% iso){
    gv$region <- "World"
  }
  
  total_pop <- query_total_pop(iso3n, adm1, max(years), capa_db) %>%
    rename(capa_id = contains("capa_id")) %>% dplyr::select(-year)
  
  results <- query_frequency(iso3n, years, start_end, weights, threshold, gv, capa_db)
  
  ph_join <- create_placeholder_freq(iso3n, max_periods, adm1) %>%
    left_join(results, by = placeholder_join_freq(adm1)) %>%
    group_by(across(all_of(unlist(strsplit(gv[['grouping_vars']], ", "))))) %>%
    fill(everything(), .direction = "up") %>%
    mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>%
    ungroup()
  
  out_frame <- ph_join %>%
    rename(capa_id = contains("capa_id")) %>%
    left_join(total_pop, by = gv$tot_join_vars) %>%
    mutate(risk_pct = round(risk_pop/total_pop, 4)) %>%
    cnames()
  
  if(adm1){
    out_frame <- out_frame %>%
      mutate(capa_id = as.numeric(capa_id)) %>%
      left_join(dplyr::select(adm1_cgaz, capa_id, shape_name) %>% st_drop_geometry(), by = "capa_id")
  }
  
  if(!is.na(p_threshold)){
    out_frame <- out_frame %>%
      filter(n_periods == p_threshold)
  } 
  
  disconnect_from_capa(capa_db)
  
  return(out_frame)
  
}
