


#THIS FUNCTION CURRENTLY ONLY WORKS WITH period = 'yearly'
get_CAR <- function(iso3c, years, period = "yearly", adm1, weights, threshold = 1,
                                 score_selection = c(1, 25, 100, 1000), cat_names = c("low", "medium", "high", "extreme"), exclusive = F){
  
  #' Query the CAPA database for children at risk (CAR) of conflict
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs
  #' 
  #' @usage get_CAR(iso3c, years, period, adm1, weights, threshold, score_selection, cat_names, exclusive)
  #' 
  #' @param iso3c character: a single or vector of character iso3c values or preferably just 'World' for the full data
  #' @param years numeric: a single or vector containing a range of years
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' THIS FUNCTION CURRENTLY ONLY WORKS WITH 'yearly'
  #' @param adm1 boolean: if FALSE will use ADM0
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param threshold numeric: a single whole number intensity-score threshold value
  #' @param score_selection numeric: a vector of numeric score values that are the lower-bounds of your conflict categories
  #' @param cat_names character: a vector of category names corresponding to the score_selection argument and equal in length
  #' @param exclusive boolean: if FALSE, each category will include all population over the lower-bound supplied in the 'score_selection' arg
  #' if TRUE, each category will use the following category's lower-bound as it's upper-bound except for the final category which will have no
  #' upper limit
  #' 
  #' @examples 
  #' 
  #' get_CAR("World", 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights, exclusive = T)
  #' get_CAR("World", 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights, exclusive = F)
  #' get_CAR("World", 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights,
  #'         score_selection = c(1, 100, 1000), cat_names = c("smol", "big", "bigly"), exclusive = F)
  #'                          
  #' @returns dataframe
  #' 
  #' @details 
  #' Dependencies: dplyr, data.table, DBI, countrycode, sf
  #' Uses: connect_to_capa(), get_score_aggregation(), cnames(), disconnect_from_capa()
  
  #browser()
  capa_db <- connect_to_capa()
  
  #get totals to update country totals for countries not in the conflict table
  country_tots <- dbGetQuery(capa_db, "SELECT * FROM country_pops")
  
  conf_long <- get_score_aggregation(iso = iso3c, years = years, period = period, adm1 = adm1, weights = weights, threshold = threshold, score_selection = score_selection)
  
  if(exclusive){
    conf_long <- conf_long %>%
      group_by(iso3n, year) %>%
      mutate(risk_pop = risk_pop - lead(risk_pop, default = 0),
             risk_pct = round(risk_pop/total_pop, 4)) %>%
      ungroup()
  }
  
  #generate a placeholder that includes all categories for all countries for all years
  category_join <- data.table(iso3n = ison(iso3c)) %>%
    .[rep(1:.N, length(cat_names))] %>%
    .[ , category := cat_names, by = iso3n] %>%
    .[ , score := score_selection, by = iso3n] %>%
    .[rep(1:.N, length(years))] %>%
    .[ , year := years, by = list(iso3n, category, score)]
  
  #join the data to the placeholder and pivot wide
  conf_wide <- category_join %>% left_join(conf_long, by = c("iso3n", "year", "score")) %>%
    dplyr::select(-score) %>%
    mutate(risk_pop = ifelse(is.na(risk_pop), 0, risk_pop),
           risk_pct = ifelse(is.na(risk_pct), 0, risk_pct)) %>%
    rows_update(country_tots, by = c("iso3n", "year"), unmatched = "ignore") %>%
    cnames() %>%
    pivot_wider(names_from = category, values_from = c(risk_pop, risk_pct))
  
  #add demographic data and calculate the children stuff
  conf <- conf_wide %>%
    left_join(un_demos, by = c("iso3n", "year")) %>%
    mutate(total_children = round(total_pop * child_pct))
  
  child_names <- paste("risk_children", cat_names, sep = "_")
  pop_names <- paste("risk_pop", cat_names, sep = "_")
  for(i in 1:length(cat_names)){
    conf <- conf %>% mutate(!!sym(child_names[i]) := round(!!sym(pop_names[i]) * child_pct))
  }
  
  child_pct_names <- paste("risk_children", cat_names, "share", sep = "_")
  
  for(i in 1:length(cat_names)){
    conf <- conf %>% mutate(!!sym(child_pct_names[i]) := round(!!sym(child_names[i]) / total_children, 4))
  }
  
  conf <- conf %>%
    mutate(iso3c = isoc(iso3n)) %>%
    arrange(iso3c, year) %>%
    dplyr::select(iso3c, iso3n, year, total_pop, un_total, child_pct, total_children, everything())
  
  #attach b_deaths
  b_deaths <- ged %>% st_drop_geometry() %>% group_by(iso3c, year) %>% summarize(battle_deaths = sum(best, na.rm = T))
  conf_out <- conf %>% left_join(b_deaths, by = c("iso3c", "year"))
  
  disconnect_from_capa(capa_db)
  
  conf_out$country <- countrycode(conf_out$iso3c, origin = "iso3c", destination = "country.name", custom_match = c("KOS" = "Kosovo"))
  conf_out <- conf_out %>% dplyr::select(country, everything())
  
  return(conf_out)
  
}

#aggregates output of get_CAR() to the regional (level = "Region") or global (level = "Global") level
agg_car <- function(car_data, level){
  
  if(level == "Region"){
    gv <- c("region", "year")
    region_key <- dbGetQuery(connect_to_capa(), "SELECT * FROM region_key WHERE region != 'World'")
    car_data <- car_data %>% left_join(region_key, by = "iso3n")
  }else if(level == "Global"){
    gv <- c("year")
  }
  
  car_data <- car_data %>%
    group_by_at(gv) %>%
    summarize(across((contains("children") & !contains("share")) | contains("total") | contains("battle_deaths"), sum, na.rm = T)) %>%
    mutate(across(contains("risk_children"), ~ round(.x / total_children, 4), .names = "{.col}_share"))
  
  return(car_data)
  
}
