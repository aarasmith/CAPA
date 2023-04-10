



get_standard_aggregation <- function(iso, years, period = "monthly", adm1 = TRUE, weights, threshold = 1, selected_period = NA){
  
  #' Query the CAPA database for the standard aggregation
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs
  #' 
  #' @usage get_standard_aggregation(iso, years, period, adm1, weights, threshold, selected_period = NA)
  #' 
  #' @param iso character/numeric: a single or vector of numeric iso3n values or character iso3c values
  #' @param years numeric: a single or vector containing a range of years
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' @param adm1 boolean: if FALSE will use ADM0
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param threshold numeric: a single whole number intensity-score threshold value
  #' @param selected_period numeric: if not NA, will filter output to select a single month/quarter/half - this is used when plotting a map is desired
  #' 
  #' @examples 
  #' 
  #' #Get yearly aggregation for Syria and Afghanistan over the years 1990-2021 at the ADM0 level
  #' get_standard_aggregation(iso = c("SYR", "AFG"), years = 1990:2021, period = "yearly", adm1 = FALSE,
  #'                          weights = list(L25 = 1, L50 = 1, ..., int100 = 0), threshold = 1)
  #' 
  #' #Get quarterly aggregation for Syria Q4 2020 at the ADM1 level
  #' get_standard_aggregation(iso = 760, years = 2020, period = "quarterly", adm1 = TRUE,
  #'                          weights = list(L25 = 1, L50 = 1, ..., int100 = 0), threshold = 1, selected_period = 4)
  #'                          
  #' @returns dataframe
  #' 
  #' @details 
  #' Dependencies: dplyr
  #' Uses: sanitize_iso(), sanitize_threshold(), connect_to_capa(), query_standard_gv(), query_total_pop(), query_standard_aggregation()
  #'       create_placeholder(), placeholder_join(), cnames(), disconnect_from_capa()
  
  iso3n <- sanitize_iso(iso)
  
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- query_standard_gv(adm1, period)
  
  total_pop <- query_total_pop(iso3n, adm1, years, capa_db)
  
  data <- query_standard_aggregation(iso3n, years, weights, threshold, gv, capa_db, score = FALSE)
  ph <- create_placeholder(iso = iso3n, years = years, period = period, adm1 = adm1)
  
  #join data to the placeholder
  out_frame <- ph %>% left_join(data, by = placeholder_join(period, adm1)) %>%
    mutate(risk_pop = ifelse(is.na(risk_pop), 0, risk_pop)) %>%
    left_join(total_pop, by = gv$tot_join_vars) %>%
    mutate(risk_pct = risk_pop/total_pop)
  
  #This section is dependent on a supplied value to "selected_period" and is for filtering the df before handing to adm_plot() for mapping
  if(period == "monthly" & !is.na(selected_period)){
    out_frame <- out_frame %>% filter(month == selected_period)
  }
  if(period == "biannually" & !is.na(selected_period)){
    out_frame <- out_frame %>% filter(half == selected_period)
  }
  if(period == "quarterly" & !is.na(selected_period)){
    out_frame <- out_frame %>% filter(quarter == selected_period)
  }
  
  #cnames() adds iso3c and country name to the dataframe
  out_frame <- out_frame %>% within(risk_pct <- round(risk_pct, 4)) %>% cnames()
  
  disconnect_from_capa(capa_db)
  return(out_frame)
  
}
