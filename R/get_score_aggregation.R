



get_score_aggregation <- function(iso, years, period, adm1, weights, threshold, cap = NA, score_selection = NULL){
  
  #' Query the CAPA database for the score aggregation
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs
  #' 
  #' @usage get_score_aggregation(iso, years, period, adm1, weights, threshold, cap = NA, score_selection = NA)
  #' 
  #' @param iso character/numeric: a single or vector of numeric iso3n values or character iso3c values
  #' @param years numeric: a single or vector containing a range of years
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' @param adm1 boolean: if FALSE will use ADM0
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param threshold numeric: a single whole number intensity-score threshold value
  #' @param cap numeric: an upper-bound for a maximum intensity score - used if you want to standardize calculated indices against very high
  #' intensity outliers
  #' @param score_selection numeric: a vector of numeric score values to return instead of every single score from threshold to max score
  #' 
  #' @examples 
  #' 
  #' get_score_aggregation(iso = "AFG", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = test_weights,
  #'                       threshold = 1, cap = 1000, score_selection = c(1, 25, 100, 1000))
  #' 
  #' get_score_aggregation(iso = ison("Western Asia"), years = 1990:2021, period = "yearly", adm1 = TRUE, weights = test_weights,
  #'                       threshold = 1)
  #'                          
  #' @returns dataframe
  #' 
  #' @details 
  #' Dependencies: dplyr, data.table
  #' Uses: sanitize_iso(), sanitize_threshold(), connect_to_capa(), query_standard_gv(), query_total_pop(), query_standard_aggregation()
  #'       cnames(), disconnect_from_capa()
  #' Used-in: get_CAR()
  
  iso3n <- sanitize_iso(iso)
  
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- query_standard_gv(adm1, period)
  
  total_pop <- query_total_pop(iso3n, adm1, years, capa_db)
  
  data <- query_standard_aggregation(iso3n, years, weights, threshold, gv, capa_db, score = TRUE)
  
  data <- data %>%
    mutate(score = as.numeric(score)) %>%
    left_join(total_pop, by = gv$tot_join_vars) %>%
    mutate(risk_pct = risk_pop/total_pop)
  
  max_scores <- data %>% group_by_at(gv$grouping_vars) %>% summarize(max = max(score)) %>% filter(max != 0)
  
  out_frame <- max_scores[rep(seq_len(dim(max_scores)[1]), max_scores$max), ] %>%
    group_by_at(gv$grouping_vars) %>%
    mutate(score = dplyr::row_number()) %>%
    ungroup() %>%
    left_join(data, by = c("score", names(max_scores)[-ncol(max_scores)])) %>%
    fill(iso3n, risk_pop, risk_pct, total_pop, .direction = "up") %>%
    dplyr::select(-max)
  
  if(!is.na(cap)){
    out_frame <- out_frame %>% filter(score <= cap)
  }
  if(!is.null(score_selection)){
    out_frame <- out_frame %>% filter(score %in% score_selection)
  }
  
  out_frame <- out_frame %>% within(risk_pct <- round(risk_pct, 4)) %>% cnames()
  
  disconnect_from_capa(capa_db)
  return(out_frame)
  
}
