



query_region_aggregation <- function(iso3n, years, weights, threshold, gv, capa_db){
  
  #' Query the CAPA database for the regional aggregation
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs
  #' 
  #' @usage query_region_aggregation(iso3n, years, weights, threshold, gv, capa_db)
  #' 
  #' @param iso3n numeric: vector of numeric iso3n values of countries in the region
  #' @param years numeric: a single or vector containing a range of years
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param threshold numeric: a single whole number score threshold value
  #' @param gv list: a list of grouping variables obtained from the query_region_gv function
  #' @param capa_db connection: a connection to the database hosting the CAPA data
  #' 
  #' @examples 
  #' query_frequency(iso3n = ison("Africa"), years = 1990:2021, weights = list(L25 = 1, L50 = 1, ..., int100 = 0), threshold = 1,
  #'                 gv = query_region_gv(region = "Africa", period = "monthly"), capa_db = con)
  #' 
  #' @details 
  #' Dependencies: DBI, glue, dplyr
  #' Uses: query_region_gv()
  #' Used-in: get_region_aggregation()
  
  if(gv$region == "World"){
    iso_compare <- DBI::SQL('')
  }else{
    iso_compare <- glue_sql("iso3n IN ({iso3n*}) AND",
                            .con = capa_db)
  }
  
  sql_query <- glue_sql(
    "SELECT
        {`gv$grouping_vars`*}, SUM(cell_pop) AS risk_pop
      FROM 
        (
        SELECT 
          {`gv$grouping_vars`*},
          cell_pop,
          (lo_25 * {weights['L25']}) +
          ((lo_50 - lo_25) * {weights['L50']}) +
          ((lo_100 - lo_50) * {weights['L100']}) +
          (md_25 * {weights['M25']}) +
          ((md_50 - md_25) * {weights['M50']}) +
          ((md_100 - md_50) * {weights['M100']}) +
          (hi_25 * {weights['H25']}) +
          ((hi_50 - hi_25) * {weights['H50']}) +
          ((hi_100 - hi_50) * {weights['H100']}) +
          (int_25 * {weights['int25']}) +
          ((int_50 - int_25) * {weights['int50']}) +
          ((int_100 - int_50) * {weights['int100']}) AS score 
        FROM {`gv$table`}
        WHERE {iso_compare}
          year >= {min(years)} AND
          year <= {max(years)}
        ) agg
      WHERE score >= {threshold}
      GROUP BY {`gv$grouping_vars`*}",
    .con = capa_db
  )
  
  data <- dbGetQuery(capa_db, sql_query) %>%
    within(risk_pop <- as.numeric(risk_pop))
  return(data)
}
