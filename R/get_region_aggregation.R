


#needs GWNO support
get_region_aggregation <- function(region, years, weights, period, threshold = 1){
  
  #' Query the CAPA database for the regional aggregation
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs
  #' 
  #' @usage get_region_aggregation(region, years, weights, period, threshold = 1)
  #' 
  #' @param region character: a single character string of an exact region name from the UN geoscheme or (GWNO definition - future addition)
  #' @param years numeric: a single or vector containing a range of years
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' @param threshold numeric: a single whole number intensity-score threshold value
  #' 
  #' @examples 
  #' 
  #' get_region_aggregation(region = "World", years = 1990:2021, weights = list(L25 = 1, L50 = 1, ..., int100 = 0), period = "yearly", threshold = 1)
  #' get_region_aggregation(region = "Western Asia", years = 1990:2021, weights = list(L25 = 1, L50 = 1, ..., int100 = 0), period = "monthly", threshold = 1)
  #'                          
  #' @returns dataframe
  #' 
  #' @details 
  #' Dependencies: dplyr, DBI, glue
  #' Uses: sanitize_threshold(), connect_to_capa(), query_region_gv(), query_region_pop(), query_region_aggregation(), disconnect_from_capa()
  
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- query_region_gv(region, period)
  
  #could move this into query_region_gv? Or just improve it by removing the requirement for a DB query
  if(region == "World"){
    iso3n <- unique(dbGetQuery(capa_db, glue("SELECT * FROM region_key"))$iso3n)
  }else{
    iso3n <- dbGetQuery(capa_db, glue("SELECT * FROM region_key WHERE region = '{region}'"))$iso3n
  }
  
  total_pop <- query_region_pop(region, years, capa_db)
  
  data <- query_region_aggregation(iso3n, years, weights, threshold, gv, capa_db) %>%
    left_join(total_pop, by = "year") %>%
    mutate(risk_pct = round(risk_pop/total_pop, 4))
  
  disconnect_from_capa(capa_db)
  
  return(data) 
}
