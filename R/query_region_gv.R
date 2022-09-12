



query_region_gv <- function(region, period){
  
  #' Create grouping variables for regional aggregation query
  #' 
  #' @description
  #' Returns a list of vectors used for grouping in the SQL query depending on spatial and temporal parameters
  #' 
  #' @usage
  #' query_region_gv(region, period)
  #' 
  #' @param region character: A single string matching one of the regions in the 'region_pops' materialized view
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' 
  #' @returns
  #' list of length 3 containing character vectors
  #' 
  #' @examples
  #' query_region_gv(region = "World", period = "yearly")
  #' query_region_gv(region = "Western Africa", period = "biannually")
  #' 
  #' @details
  #' Dependencies: none
  #' Used-in: get_region_aggregation()
  
  gv <- list()
  gv$region <- region
  if(period == "monthly"){
    gv$table <- "cell_stats"
    gv$grouping_vars <- c("year", "month")
  }else if(period == "quarterly"){
    gv$table <- "cell_stats_qu"
    gv$grouping_vars <- c("year", "quarter")
  }else if(period == "biannually"){
    gv$table <- "cell_stats_bi"
    gv$grouping_vars <- c("year", "half")
  }else{
    gv$table <- "cell_stats_yr"
    gv$grouping_vars <- c("year")
  }
  
  return(gv)
  
}
