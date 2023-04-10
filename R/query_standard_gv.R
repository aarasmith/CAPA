



query_standard_gv <- function(adm1, period){
  
  #' Create grouping variables for standard aggregation query
  #' 
  #' @description
  #' Returns a list of vectors used for grouping in the SQL query depending on spatial and temporal parameters
  #' 
  #' @usage
  #' query_standard_gv(adm1, period)
  #' 
  #' @param adm1 boolean: if false will use ADM0
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' 
  #' @returns
  #' list of length 3 containing character vectors
  #' 
  #' @examples
  #' query_standard_gv(adm1 = TRUE, period = "yearly")
  #' 
  #' @details
  #' Dependencies: none
  #' Used-in: get_standard_aggregation(), get_score_aggregation() 
  
  gv <- list()
  
  gv$tot_join_vars <- c("iso3n", "year")
  if(period == "monthly"){
    gv$table <- "cell_stats"
    gv$grouping_vars <- c("iso3n", "year", "month")
  }else if(period == "quarterly"){
    gv$table <- "cell_stats_qu"
    gv$grouping_vars <- c("iso3n", "year", "quarter")
  }else if(period == "biannually"){
    gv$table <- "cell_stats_bi"
    gv$grouping_vars <- c("iso3n", "year", "half")
  }else if(period == "yearly"){
    gv$table <- "cell_stats_yr"
    gv$grouping_vars <- c("iso3n", "year")
  }
  
  if(adm1){
    gv$tot_join_vars <- c(gv$tot_join_vars, "capa_id_adm1")
    gv$grouping_vars <- c(gv$grouping_vars, "capa_id_adm1")
  }
  return(gv)
}
