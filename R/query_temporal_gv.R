



query_temporal_gv <- function(adm1, period, start_end, years, capa_db){
  
  #' Create grouping variables for temporal (frequency) aggregation query
  #' 
  #' @description
  #' Returns a list of vectors used for grouping in the SQL query depending on spatial and temporal parameters
  #' 
  #' @usage
  #' query_temporal_gv(adm1, period, start_end, years, capa_db)
  #' 
  #' @param adm1 boolean: if false will use ADM0
  #' @param period character: a character string of either "yearly", "biannually, "quarterly", or "monthly"
  #' @param start_end numeric: vector of length 2 with the 2 numbers corresponding with the start period of the first year and end
  #' period of the final year
  #' @param years numeric: single year or vector range of years
  #' @param capa_db DB connection: connection to the capa_db or any PostgreSQL connection (this is used in the glue_sql call)
  #' 
  #' @returns
  #' list of length 6 containing 5 character vectors and 1 partial sql query
  #' 
  #' @examples
  #' query_temporal_gv(adm1 = TRUE, period = "yearly", start_end = c(1, 12), years = 1990:2021, capa_db = conn)
  #' 
  #' @details
  #' Dependencies: DBI, glue
  #' Used-in: get_frequency() 
  
  gv <- list()
  gv$region <- "not_world"
  
  if(adm1){
    gv$grouping_vars <- c("iso3n", "capa_id_adm1")
    gv$tot_join_vars <- c("iso3n", "capa_id")
  }else{
    gv$grouping_vars <- c("iso3n")
    gv$tot_join_vars <- c("iso3n")
  }
  
  if(period == "monthly"){
    gv$table <- "cell_stats"
    gv$period <- "month"
  }else if(period == "quarterly"){
    gv$table <- "cell_stats_qu"
    gv$period <- "quarter"
  }else if(period == "biannually"){
    gv$table <- "cell_stats_bi"
    gv$period <- "half"
  }else if(period == "yearly"){
    gv$table <- "cell_stats_yr"
    gv$period <- "year"
  }
  
  if(period == "yearly"){
    gv$start_end <- DBI::SQL('')
  }else{
    gv$start_end <- glue_sql("AND
        NOT ({`gv$period`} < {start_end[1]} AND year = {min(years)}) AND
        NOT ({`gv$period`} > {start_end[2]} AND year = {max(years)})",
                             .con = capa_db)
  }
  
  return(gv)
}
