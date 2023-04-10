



create_placeholder_freq <- function(iso3n, max_periods, adm1){
  
  #' Create a placeholder for frequency data query
  #' 
  #' @description Creates a placeholder for the query_frequency call so that iso-periods that don't have conflict are present in the final data
  #' as observations with 0's
  #' 
  #' @usage create_placeholder_freq(iso3n, max_periods, adm1)
  #' 
  #' @param iso3n numeric: a single or vector of numeric iso3n values
  #' @param max_periods numeric: a numeric value indicating the maximum number of periods in the timeframe which need to be generated as observations
  #' @param adm1 boolean: If TRUE, will generate placeholder observations at the adm1 level, else it will operate at the ADM0 level
  #' 
  #' @examples 
  #' 
  #' create_placeholder_freq(760, max_periods = 32, adm1 = FALSE)
  #' create_placeholder_freq(ison("World"), max_periods = 32, adm1 = TRUE)
  #'                          
  #' @returns dataframe
  #' 
  #' @details 
  #' Dependencies: data.table, dplyr, sf
  #' Used-in: get_temporal_aggregation()
  
  placeholder <- data.table(iso3n = iso3n) %>%
    .[rep(1:.N, max_periods)] %>%
    .[ , n_periods := 1:max_periods, by = list(iso3n)] %>%
    setorder(iso3n, n_periods)
  
  if(adm1){
    placeholder <- placeholder %>%
      left_join(dplyr::select(adm1_cgaz, iso3n, capa_id) %>% st_drop_geometry(), by = "iso3n") %>%
      rename(capa_id_adm1 = capa_id)
  }
  
  return(placeholder)
  
}

placeholder_join_freq <- function(adm1){
  
  #' Create join criteria for the frequency placeholder
  #' 
  #' @description Create join criteria for joining data from query_frequency to the placeholder data
  #' 
  #' @usage placeholder_join_freq(adm1)
  #' 
  #' @param adm1 boolean: If TRUE, will generate placeholder observations at the adm1 level, else it will operate at the ADM0 level
  #' 
  #' @examples 
  #' 
  #' placeholder_join_freq(adm1 = TRUE)
  #'                          
  #' @returns character vector
  #' 
  #' @details 
  #' Dependencies: none
  #' Used-in: get_temporal_aggregation()
  
  ph_join_vars <- c("iso3n", "n_periods")
  
  if(adm1){
    ph_join_vars <- c(ph_join_vars, "capa_id_adm1")
  }
  
  return(ph_join_vars)
  
}

#create a placeholder dataframe to join with returned conflict data so that observations that did not have any conflict are retained in the data structure
create_placeholder <- function(iso3n, years, period, adm1){
  
  #' Create a placeholder for standard aggregation data query
  #' 
  #' @description Creates a placeholder for the query_standard_aggregation call so that iso-periods that don't have conflict are present in the 
  #' final data as observations with 0's
  #' 
  #' @usage create_placeholder(iso3n, years, period, adm1)
  #' 
  #' @param iso3n numeric: a single or vector of numeric iso3n values
  #' @param years numeric: a single or vector containing a range of years
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' @param adm1 boolean: If TRUE, will generate placeholder observations at the adm1 level, else it will operate at the ADM0 level
  #' 
  #' @examples 
  #' 
  #' create_placeholder(760, years = 1990:2021, period = "yearly", adm1 = FALSE)
  #' create_placeholder(ison("World"), years = 1990:2021, period = "quarterly", adm1 = TRUE)
  #'                          
  #' @returns dataframe
  #' 
  #' @details 
  #' Dependencies: data.table, dplyr, sf
  #' Used-in: get_standard_aggregation(), get_score_aggregation()
  
  if(period == "yearly"){
    placeholder <- data.table(iso3n = iso3n) %>%
      .[rep(1:.N, length(years))] %>%
      .[ , year := years, by = list(iso3n)] %>%
      setorder(iso3n, year)
  }else{
    if(period == "monthly"){
      p_num <- 12
      p_name <- "month"
    }else if(period == "quarterly"){
      p_num <- 4
      p_name <- "quarter"
    }else if(period == "biannually"){
      p_num <- 2
      p_name <- "half"
    }
    
    placeholder <- data.table(iso3n = iso3n) %>%
      .[rep(1:.N, length(years))] %>%
      .[ , year := years, by = list(iso3n)] %>%
      .[rep(1:.N, p_num)] %>%
      .[ , period := c(1:p_num), by = list(iso3n, year)] %>%
      setorder(iso3n, year, period)
    names(placeholder)[3] <- p_name
    
  }
  
  if(adm1){
    placeholder <- placeholder %>%
      left_join(dplyr::select(adm1_cgaz, iso3n, capa_id, shape_name) %>% st_drop_geometry(), by = "iso3n") %>%
      rename(capa_id_adm1 = capa_id)
  }
  
  return(placeholder)
  
}

#func for generating the "by" arg in the placeholder join to the data
placeholder_join <- function(period, adm1){
  
  #' Create join criteria for the placeholder
  #' 
  #' @description Create join criteria for joining data from query_standard_aggregation to the placeholder data
  #' 
  #' @usage placeholder_join(period, adm1)
  #' 
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' @param adm1 boolean: If TRUE, will generate placeholder observations at the adm1 level, else it will operate at the ADM0 level
  #' 
  #' @examples 
  #' 
  #' placeholder_join(period = "yearly", adm1 = TRUE)
  #'                          
  #' @returns character vector
  #' 
  #' @details 
  #' Dependencies: none
  #' Used-in: get_standard_aggregation(), get_score_aggregation()
  
  if(period == "yearly"){
    ph_join_vars <- c("iso3n", "year")
  }else if(period == "biannually"){
    ph_join_vars <- c("iso3n", "year", "half")
  }else if(period == "quarterly"){
    ph_join_vars <- c("iso3n", "year", "quarter")
  }else if(period == "monthly"){
    ph_join_vars <- c("iso3n", "year", "month")
  }
  
  if(adm1){
    ph_join_vars <- c(ph_join_vars, "capa_id_adm1")
  }
  
  return(ph_join_vars)
  
}