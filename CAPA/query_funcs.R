


get_grouping_vars <- function(adm1, monthly){
  if(adm1){
    tot_group_vars <- "iso3n, year, capa_id_adm1"
    tot_join_vars <- c("iso3n", "year", "capa_id_adm1")
    if(monthly){
      table <- "cell_stats"
      grouping_vars <- "iso3n, year, month, capa_id_adm1"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("iso3n", "year", "month", "capa_id_adm1")
    }else{
      table <- "cell_stats_yr"
      grouping_vars <- "iso3n, year, capa_id_adm1"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- c("iso3n", "year", "capa_id_adm1")
    }
  }else{
    tot_group_vars <- "iso3n, year"
    tot_join_vars <- c("iso3n", "year")
    if(monthly){
      table <- "cell_stats"
      grouping_vars <- "iso3n, year, month"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("iso3n", "year", "month")
    }else{
      table <- "cell_stats_yr"
      grouping_vars <- "iso3n, year"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- c("iso3n", "year")
    }
  }
  return(list(tot_group_vars = tot_group_vars, tot_join_vars = tot_join_vars, grouping_vars = grouping_vars, table = table, dplyr_group_vars = dplyr_group_vars))
}

get_total_pop <- function(iso3n, years, gv, capa_db){
  
  iso3n <- paste(iso3n, collapse = ", ")
  
  total_query <- glue(
    "SELECT {gv['tot_group_vars']}, SUM(cell_pop) AS total_pop
    FROM cell_pops
    WHERE iso3n IN ({iso3n}) AND
      year >= {years[1]} AND
      year <= {years[length(years)]}
    GROUP BY {gv['tot_group_vars']}"
  )
  
  total_pop <- dbGetQuery(capa_db, total_query)
  return(total_pop)
}

get_cell_stats <- function(iso3n, years, weights, threshold, gv, capa_db, score = FALSE){
  
  iso3n <- paste(iso3n, collapse = ", ")
  
  if(score){
    select_score <- ", score"
  }else{
    select_score <- ""
  }
  
  #differentiating between score = T and score = F has little impact on speed, but will have impact on memory usage as app scales up
  
  sql_query <- glue(
    "SELECT
        {gv['grouping_vars']}, SUM(cell_pop) AS risk_pop{select_score}
      FROM 
        (
        SELECT 
          {gv['grouping_vars']},
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
        FROM {gv['table']}
        WHERE iso3n IN ({iso3n}) AND
          year >= {years[1]} AND
          year <= {years[length(years)]}
        ) agg
      WHERE score >= {threshold}
      GROUP BY {gv['grouping_vars']}"
  )
  
  sql_query_score <- glue(
    "SELECT
      {gv['grouping_vars']}, SUM(risk_pop) OVER (PARTITION BY {gv['grouping_vars']} ORDER BY score DESC) AS risk_pop, score
    FROM
      (
      
    {sql_query}
      
      , score) foobar"
  )
  
  if(score){
    sql_query <- sql_query_score
  }
  
  data <- dbGetQuery(capa_db, sql_query)
  return(data)
}
