

#x <- query_cell_scores(760, 2020, start_end = c(1,12), test_weights, connect_to_capa())

query_cell_scores <- function(iso3n, years, start_end, weights, capa_db){
  
  #' Query the CAPA database for the individual cell-intensity scores
  #' 
  #' @description Writes and sends an SQL query to the CAPA db based on user supplied inputs for mapping conflict intensity for a snapshot in time
  #' 
  #' @usage query_cell_scores(iso3n, years, start_end, weights, capa_db)
  #' 
  #' @param iso3n numeric: a single or vector of numeric iso3n values
  #' @param years numeric: a single or vector containing a range of years
  #' @param start_end numeric: vector of length 2 with the 2 numbers corresponding with the start period of the first year and end
  #' period of the final year
  #' @param weights list: a list of named weight elements containing a single whole number value
  #' @param capa_db connection: a connection to the database hosting the CAPA data
  #' 
  #' @examples 
  #' query_cell_scores(iso3n = c(760, 4), years = 2020:2021, start_end = c(3, 12) weights = list(L25 = 1, L50 = 1, ..., int100 = 0), capa_db = con)
  #' 
  #' @returns sf object with cell-intensity-scores and geometry aggregated over the time-period to provide a single score per cell geometry
  #' 
  #' @details 
  #' Dependencies: DBI, glue, sf
  #' Used-in: get_cell_scores()
  
  if(start_end[1] == 1 & start_end[2] == 12){
    table <- "cell_stats_yr"
    start_end <- DBI::SQL('')
  }else{
    table <- "cell_stats"
    start_end <- glue_sql("AND
        NOT (month < {start_end[1]} AND year = {min(years)}) AND
        NOT (month > {start_end[2]} AND year = {max(years)})",
                          .con = capa_db)
  }
  
  sql_query <- glue_sql(
    "SELECT
      iso3n,
      score,
      geometry
    FROM
      (
      SELECT
        iso3n,
        sid,
        SUM((lo_25 * {weights['L25']}) +
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
          ((int_100 - int_50) * {weights['int100']})) AS score
      FROM {`table`}
      WHERE iso3n IN ({iso3n*}) AND
        year >= {min(years)} AND
        year <= {max(years)}
        {start_end}
      GROUP BY iso3n, sid
      ) stats_sub
      
      LEFT JOIN
      
      (
      SELECT sid, geometry
      FROM cell_geos
      WHERE iso3n IN ({iso3n*})
      ) cells_sub
      
      ON stats_sub.sid = cells_sub.sid",
    .con = capa_db
  )
  
  data <- st_read(capa_db, query = sql_query)
  return(data)
}
