


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


####plot_score_sql####
get_cell_scores <- function(iso3n, years, start_end, weights){
  
  if(start_end[1] == 1 & start_end[2] == 12){
    table <- "cell_stats_yr"
    start_end <- ""
  }else{
    table <- "cell_stats"
    start_end <- glue(" AND
        NOT (month < {start_end[1]} AND year = {min(years)}) AND
        NOT (month > {start_end[2]} AND year = {max(years)})
                      ")
  }
  
  sql_query <- glue(
    "SELECT
      score,
      geometry
    FROM
      (
      SELECT
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
      FROM {table}
      WHERE iso3n = {iso3n} AND
        year >= {years[1]} AND
        year <= {years[length(years)]}
        {start_end}
      GROUP BY sid
      ) stats_sub
      
      LEFT JOIN
      
      (
      SELECT sid, geometry
      FROM cell_geos
      WHERE iso3n = {iso3n}
      ) cells_sub
      
      ON stats_sub.sid = cells_sub.sid"
  )
  
  data <- st_read(capa_db, query = sql_query)
  return(data)
}


get_duration_gv <- function(adm, monthly, start_end, years){
  #creates grouping variables for the SQL query based on whether the query is adm0/adm1 and yearly/monthly. gv['start_end'] is a partial SQL statement added to the middle of sql_query
  
  gv <- list()
  if(adm){
    gv['grouping_vars'] <- "iso3n, capa_id_adm1"
    gv['tot_group_vars'] <- "iso3n, capa_id_adm1"
    gv[['tot_join_vars']] <- c("iso3n", "capa_id")
  }else{
    gv['grouping_vars'] <- "iso3n"
    gv['tot_group_vars'] <- "iso3n"
    gv[['tot_join_vars']] <- c("iso3n")
  }
  if(!monthly){
    gv['table'] <- "cell_stats_yr"
    gv['start_end'] <- ""
  }else{
    gv['table'] <- "cell_stats"
    gv['start_end'] <- glue(" AND
        NOT (month < {start_end[1]} AND year = {min(years)}) AND
        NOT (month > {start_end[2]} AND year = {max(years)})
                      ")
  }
  
  return(gv)
}

get_duration <- function(iso3n, years, start_end, weights, threshold, gv, capa_db){
  
  iso3n <- paste(iso3n, collapse = ", ")
  
  sql_query <- glue(
    "SELECT
      {gv['grouping_vars']}, SUM(cell_pop) as risk_pop
    FROM
      (
      SELECT
          iso3n, sid, capa_id_adm1, MIN(score) AS min_score
        FROM 
          (
          SELECT 
            iso3n, sid, capa_id_adm1,
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
            {gv['start_end']}
          ) agg
        WHERE score >= {threshold}
        GROUP BY iso3n, sid, capa_id_adm1
      ) min_query
      
      LEFT JOIN
      
      (
      SELECT
        sid,
        cell_pop
      FROM cell_pops
      WHERE
        iso3n IN ({iso3n}) And
        year = {max(years)}
      ) pops
    
    ON min_query.sid = pops.sid
    WHERE min_score >= {threshold}
    GROUP BY {gv['grouping_vars']}"
  )
  
  data <- dbGetQuery(capa_db, sql_query)
  return(data)
  
}

system.time({x <- get_duration(iso3n, years = 2014:2018, start_end = c(1,12), adm = T, weights, threshold = 100, gv, monthly = T, capa_db)})
system.time({y <- get_duration(iso3n, years = 2014:2018, start_end = c(1,12), adm = T, weights, threshold = 100, gv, monthly = F, capa_db)})


get_frequency <- function(iso3n, years, start_end, weights, threshold, p_threshold, gv, capa_db){
  
  iso3n <- paste(iso3n, collapse = ", ")
  
  sql_query <- glue(
    "SELECT
      {gv['grouping_vars']}, n_periods, SUM(risk_pop) OVER(PARTITION BY {gv['grouping_vars']} ORDER BY n_periods ASC) AS risk_pop
    FROM
      (
      SELECT
        {gv['grouping_vars']}, n_periods, SUM(cell_pop) as risk_pop
      FROM
        (
        SELECT
            iso3n, sid, capa_id_adm1, COUNT(sid) AS n_periods
          FROM 
            (
            SELECT 
              iso3n, sid, capa_id_adm1,
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
              {gv['start_end']}
            ) agg
          WHERE score >= {threshold}
          GROUP BY iso3n, sid, capa_id_adm1
        ) freq_query
        
        LEFT JOIN
        
        (
        SELECT
          sid,
          cell_pop
        FROM cell_pops
        WHERE
          iso3n IN ({iso3n}) And
          year = {max(years)}
        ) pops
      
      ON freq_query.sid = pops.sid
      WHERE n_periods >= {p_threshold}
      GROUP BY {gv['grouping_vars']}, n_periods
      ) pre_cumsum"
  )
  
  data <- dbGetQuery(capa_db, sql_query)
  return(data)
}
