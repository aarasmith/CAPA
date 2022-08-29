

#this generates variables for the sql queries and some of the data processing in dplyr. the logic is more or less duplicated for adm1 = T and adm1 = F
##it could also be cleaned up further by taking advantage of sql_glue() '*' to unpack vectors into appropriate form e.g. sql_glue({c("iso3n", "year", "month")*}) -> ('iso3n', 'year', 'month')
get_standard_gv <- function(adm1, period){
  if(adm1){
    tot_group_vars <- "iso3n, year, capa_id_adm1"
    tot_join_vars <- c("iso3n", "year", "capa_id_adm1")
    if(period == "monthly"){
      table <- "cell_stats"
      grouping_vars <- "iso3n, year, month, capa_id_adm1"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("iso3n", "year", "month", "capa_id_adm1")
    }else if(period == "quarterly"){
      table <- "cell_stats_qu"
      grouping_vars <- "iso3n, year, quarter, capa_id_adm1"
      grouping_vars2 <- "sid, year, quarter"
      dplyr_group_vars <- c("iso3n", "year", "quarter", "capa_id_adm1")
    }else if(period == "biannually"){
      table <- "cell_stats_bi"
      grouping_vars <- "iso3n, year, half, capa_id_adm1"
      grouping_vars2 <- "sid, year, half"
      dplyr_group_vars <- c("iso3n", "year", "half", "capa_id_adm1")
    }else if(period == "yearly"){
      table <- "cell_stats_yr"
      grouping_vars <- "iso3n, year, capa_id_adm1"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- c("iso3n", "year", "capa_id_adm1")
    }
  }else{
    tot_group_vars <- "iso3n, year"
    tot_join_vars <- c("iso3n", "year")
    if(period == "monthly"){
      table <- "cell_stats"
      grouping_vars <- "iso3n, year, month"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("iso3n", "year", "month")
    }else if(period == "quarterly"){
      table <- "cell_stats_qu"
      grouping_vars <- "iso3n, year, quarter"
      grouping_vars2 <- "sid, year, quarter"
      dplyr_group_vars <- c("iso3n", "year", "quarter")
    }else if(period == "biannually"){
      table <- "cell_stats_bi"
      grouping_vars <- "iso3n, year, half"
      grouping_vars2 <- "sid, year, half"
      dplyr_group_vars <- c("iso3n", "year", "half")
    }else if(period == "yearly"){
      table <- "cell_stats_yr"
      grouping_vars <- "iso3n, year"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- c("iso3n", "year")
    }
  }
  return(list(tot_group_vars = tot_group_vars, tot_join_vars = tot_join_vars, grouping_vars = grouping_vars, table = table, dplyr_group_vars = dplyr_group_vars))
}

query_total_pop <- function(iso3n, adm1, years, gv, capa_db){
  
  iso3n <- paste(iso3n, collapse = ", ")
  
  if(adm1){
    table <- "adm1_pops"
  }else{
    table <- "country_pops"
  }
  
  total_query <- glue(
    "SELECT *
    FROM {table}
    WHERE iso3n IN ({iso3n}) AND
      year >= {years[1]} AND
      year <= {years[length(years)]}"
  )
  
  total_pop <- dbGetQuery(capa_db, total_query) %>% mutate(across( , ~as.numeric(.)))
  return(total_pop)
}

query_standard_aggregation <- function(iso3n, years, weights, threshold, gv, capa_db, score = FALSE){
  
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
  
  data <- dbGetQuery(capa_db, sql_query) %>% mutate(across( , ~as.numeric(.)))
  return(data)
}


####plot_score_sql####
query_cell_scores <- function(iso3n, years, start_end, weights, capa_db){
  #browser()
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
  
  iso3n <- paste(iso3n, collapse = ", ")
  
  sql_query <- glue(
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
      FROM {table}
      WHERE iso3n IN ({iso3n}) AND
        year >= {years[1]} AND
        year <= {years[length(years)]}
        {start_end}
      GROUP BY iso3n, sid
      ) stats_sub
      
      LEFT JOIN
      
      (
      SELECT sid, geometry
      FROM cell_geos
      WHERE iso3n IN ({iso3n})
      ) cells_sub
      
      ON stats_sub.sid = cells_sub.sid"
  )
  
  data <- st_read(capa_db, query = sql_query)
  return(data)
}


get_temporal_gv <- function(adm, period, start_end, years){
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
  if(period != "monthly"){
    gv['start_end'] <- ""
  }else{
    gv['start_end'] <- glue(" AND
        NOT (month < {start_end[1]} AND year = {min(years)}) AND
        NOT (month > {start_end[2]} AND year = {max(years)})
                      ")
  }
  
  if(period == "monthly"){
    gv['table'] <- "cell_stats"
    gv['period'] <- "month"
    gv['max_periods'] <- (max(years) - min(years) + 1) * 12
  }else if(period == "quarterly"){
    gv['table'] <- "cell_stats_qu"
    gv['period'] <- "quarter"
    gv['max_periods'] <- (max(years) - min(years) + 1) * 4
  }else if(period == "biannually"){
    gv['table'] <- "cell_stats_bi"
    gv['period'] <- "half"
    gv['max_periods'] <- (max(years) - min(years) + 1) * 2
  }else if(period == "yearly"){
    gv['table'] <- "cell_stats_yr"
    gv['period'] <- "year"
    gv['max_periods'] <- (max(years) - min(years) + 1)
  }
  
  # if(period == "yearly"){
  #   gv['start_end'] <- ""
  # }else{
  #   gv['start_end'] <- glue(" AND
  #       NOT ({gv[['period']]} < {start_end[1]} AND year = {min(years)}) AND
  #       NOT ({gv[['period']]} > {start_end[2]} AND year = {max(years)})
  #                     ")
  # }
  
  return(gv)
}

query_duration <- function(iso3n, years, start_end, weights, threshold, gv, capa_db){
  #This one is tricky, because the way the data is structured only cells that experienced conflict are in the stats tables, so any non-conflict periods will be missing and won't trigger
  # a 0 min for the score. To address this, I've included the 'HAVING COUNT' clause to make sure every SID has the same number of entries as the number of possible periods. If this is
  # less than the maximum number of periods in the time-range, that implies there was at least 1 period where that cell did not experience conflict, and had it been included in the stats
  # table (which it is not, to save space) it would have dropped the min to 0 and filtered out in the first part of the HAVING clause that filters out min_scores under the threshold.
  #browser()
  if(gv[['region']] == "World"){
    iso_compare <- ""
  }else{
    iso3n <- paste(iso3n, collapse = ", ")
    iso_compare <- glue("iso3n IN ({iso3n}) AND")
  }
  
  sql_query <- glue(
    "SELECT
      {gv['grouping_vars']}, SUM(cell_pop) as risk_pop
    FROM
      (
      SELECT
        iso3n, sid, capa_id_adm1,
        MIN(
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
          ((int_100 - int_50) * {weights['int100']})
        ) AS min_score,
        SUM(cell_pop) FILTER (WHERE year = {max(years)}) as cell_pop
        FROM {gv['table']}
        WHERE
          {iso_compare}
          year >= {min(years)} AND
          year <= {max(years)}
          {gv['start_end']}
        GROUP BY iso3n, sid, capa_id_adm1
        HAVING
          MIN(
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
            ((int_100 - int_50) * {weights['int100']})
          ) >= {threshold}
          AND COUNT({gv['period']}) = {gv['max_periods']}
      ) min_query
    
    GROUP BY {gv['grouping_vars']}"
  )
  
  data <- dbGetQuery(capa_db, sql_query) %>%
    within(risk_pop <- as.numeric(risk_pop)) %>% mutate(across( , ~as.numeric(.)))
  return(data)
  
}

# system.time({x <- query_duration(iso3n, years = 2014:2018, start_end = c(1,12), adm = T, weights, threshold = 100, gv, monthly = T, capa_db)})
# system.time({y <- query_duration(iso3n, years = 2014:2018, start_end = c(1,12), adm = T, weights, threshold = 100, gv, monthly = F, capa_db)})


query_frequency <- function(iso3n, years, start_end, weights, threshold, gv, capa_db){
  
  if(gv[['region']] == "World"){
    iso_compare <- ""
  }else{
    iso3n <- paste(iso3n, collapse = ", ")
    iso_compare <- glue("iso3n IN ({iso3n}) AND")
  }
  
  #browser()
  sql_query1 <- glue(
    "SELECT
      {gv['grouping_vars']}, n_periods, SUM(risk_pop) OVER(PARTITION BY {gv['grouping_vars']} ORDER BY n_periods DESC) AS risk_pop
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
      GROUP BY {gv['grouping_vars']}, n_periods
      ) pre_cumsum"
  )
  
  sql_query <- glue(
    "SELECT
      {gv['grouping_vars']}, n_periods, SUM(risk_pop) OVER(PARTITION BY {gv['grouping_vars']} ORDER BY n_periods DESC) AS risk_pop
    FROM
      (
      SELECT
        {gv['grouping_vars']}, n_periods, SUM(cell_pop) as risk_pop
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
          FROM {gv['table']}
          WHERE
            {iso_compare}
            year >= {years[1]} AND
            year <= {years[length(years)]}
            {gv['start_end']}
          ) agg
          WHERE score >= {threshold}
          GROUP BY iso3n, sid, capa_id_adm1
        ) freq
          
      GROUP BY {gv['grouping_vars']}, n_periods
      ) pre_cumsum"
    )
  
  data <- dbGetQuery(capa_db, sql_query) %>%
    within(risk_pop <- as.numeric(risk_pop)) %>% mutate(across( , ~as.numeric(.)))
  return(data)
}


query_region_pop <- function(region, years, capa_db){
  
  #iso3n <- paste(iso3n, collapse = ", ")
  
  region_query <- glue(
    "SELECT
    	region,
    	year,
    	total_pop
    FROM
    	region_pops
    WHERE
      region = '{region}' AND
      year >= {years[1]} AND
      year <= {years[length(years)]}"
  )
  
  region_pop <- dbGetQuery(capa_db, region_query)
  return(region_pop)
}

query_region_aggregation <- function(iso3n, years, weights, threshold, gv, capa_db){
  
  if(gv[['region']] == "World"){
    iso_compare <- ""
  }else{
    iso3n <- paste(iso3n, collapse = ", ")
    iso_compare <- glue("iso3n IN ({iso3n}) AND")
  }
  
  sql_query <- glue(
    "SELECT
        {gv['grouping_vars']}, SUM(cell_pop) AS risk_pop
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
        WHERE {iso_compare}
          year >= {years[1]} AND
          year <= {years[length(years)]}
        ) agg
      WHERE score >= {threshold}
      GROUP BY {gv['grouping_vars']}"
  )
  
  data <- dbGetQuery(capa_db, sql_query) %>%
    within(risk_pop <- as.numeric(risk_pop))
  return(data)
}

query_custom_region_pop <- function(isos, years, capa_db){
  
  iso3n <- paste(isos, collapse = ", ")
  
  region_query <- glue(
    "SELECT
    	year,
    	sum(total_pop) AS total_pop
    FROM
    	country_pops
    WHERE
      iso3n IN ({iso3n}) AND
      year >= {years[1]} AND
      year <= {years[length(years)]}
    GROUP BY year"
  )
  
  region_pop <- dbGetQuery(capa_db, region_query) %>%
    within(total_pop <- as.numeric(total_pop))
  return(region_pop)
}
