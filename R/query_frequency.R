



query_frequency <- function(iso3n, years, start_end, weights, threshold, gv, capa_db){
  
  #' Query the CAPA database for the frequency aggregation
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs
  #' 
  #' @usage query_frequency(iso3n, years, start_end, weights, threshold, gv, capa_db)
  #' 
  #' @param iso3n numeric: a single or vector of numeric iso3n values
  #' @param years numeric: a single or vector containing a range of years
  #' @param start_end numeric: vector of length 2 with the 2 numbers corresponding with the start period of the first year and end
  #' period of the final year
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param threshold numeric: a single whole number score threshold value
  #' @param gv list: a list of grouping variables obtained from the query_temporal_gv function
  #' @param capa_db connection: a connection to the database hosting the CAPA data
  #' 
  #' @examples 
  #' query_frequency(iso3n = c(760, 4), years = 1990:2021, start_end = c(1, 12) weights = list(L25 = 1, L50 = 1, ..., int100 = 0), threshold = 1,
  #'                            gv = query_temporal_gv(adm1 = FALSE, period = "monthly"), capa_db = conn)
  #' 
  #' @details 
  #' Dependencies: DBI, glue, dplyr
  #' Uses: query_temporal_gv()
  #' Used-in: get_temporal()
  
  if(gv$region == "World"){
    iso_compare <- DBI::SQL('')
  }else{
    iso_compare <- glue_sql("iso3n IN ({iso3n*}) AND",
                            .con = capa_db)
  }
  
  sql_query <- glue_sql(
    "SELECT
      {`gv$grouping_vars`*}, n_periods, SUM(risk_pop) OVER(PARTITION BY {`gv$grouping_vars`*} ORDER BY n_periods DESC) AS risk_pop
    FROM
      (
      SELECT
        {`gv$grouping_vars`*}, n_periods, SUM(cell_pop) as risk_pop
      FROM
        (
        SELECT
          iso3n, sid, capa_id_adm1, COUNT(sid) AS n_periods,
          MAX(cell_pop) FILTER (WHERE year = {max(years)}) as cell_pop
        FROM 
         (
          SELECT 
            iso3n, sid, capa_id_adm1, cell_pop, year,
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
          WHERE
            {iso_compare}
            year >= {min(years)} AND
            year <= {max(years)}
            {gv$start_end}
          ) agg
          WHERE score >= {threshold}
          GROUP BY iso3n, sid, capa_id_adm1
        ) freq
          
      GROUP BY {`gv$grouping_vars`*}, n_periods
      ) pre_cumsum",
    .con = capa_db
  )
  
  data <- dbGetQuery(capa_db, sql_query) %>% mutate(across(.fns = ~as.numeric(.)))
  return(data)
}
