



#x <- query_standard_aggregation(4, 1990, test_weights, 1, query_standard_gv(TRUE, "yearly"), capa_db, TRUE)

query_standard_aggregation <- function(iso3n, years, weights, threshold, gv, capa_db, score = FALSE){
  
  #' Query the CAPA database for the standard aggregation
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs
  #' 
  #' @usage query_standard_aggregation(iso3n, years, weights, threshold, gv, capa_db, score = FALSE)
  #' 
  #' @param iso3n numeric: a single or vector of numeric iso3n values
  #' @param years numeric: a single or vector containing a range of years
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param threshold numeric: a single whole number score threshold value
  #' @param gv list: a list of grouping variables obtained from the query_standard_gv function
  #' @param capa_db connection: a connection to the database hosting the CAPA data
  #' @param score boolean: if TRUE will group by cell-scores and return risk_pop at all levels of cell-score rather than
  #' aggregating risk_pop to a single value above the specified threshold
  #' 
  #' @examples 
  #' query_standard_aggregation(iso3n = c(760, 4), years = 1990:2021, weights = list(L25 = 1, L50 = 1, ..., int100 = 0), threshold = 1,
  #'                            gv = query_standard_gv(adm1 = FALSE, period = "yearly"), capa_db = conn, score = FALSE)
  #' 
  #' @details 
  #' Dependencies: DBI, glue, dplyr
  #' Uses: query_standard_gv()
  #' Used-in: get_standard_aggregation(), get_score_aggregation()
  
  if(score){
    select_score <- DBI::SQL(", score")
  }else{
    select_score <- DBI::SQL('')
  }
  
  #differentiating between score = T and score = F has little impact on speed, but will have impact on memory usage as app scales up
  
  sql_query <- glue_sql(
    "SELECT
        {`gv$grouping_vars`*}, SUM(cell_pop) AS risk_pop{select_score}
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
        WHERE iso3n IN ({iso3n*}) AND
          year >= {min(years)} AND
          year <= {max(years)}
        ) agg
      WHERE score >= {threshold}
      GROUP BY {`gv$grouping_vars`*}",
    .con = capa_db
  )
  
  sql_query_score <- glue_sql(
    "SELECT
      {`gv$grouping_vars`*}, SUM(risk_pop) OVER (PARTITION BY {`gv$grouping_vars`*} ORDER BY score DESC) AS risk_pop, score
    FROM
      (
      
    {sql_query}
      
      , score) foobar",
    .con = capa_db
  )
  
  if(score){
    sql_query <- sql_query_score
  }
  
  data <- dbGetQuery(capa_db, sql_query) %>% mutate(across(.fns = ~as.numeric(.)))
  return(data)
}
