connect_to_capa <- function(){
  #hdr_db <- dbConnect(RSQLite::SQLite(), "C:/Users/andara/Documents/hdr_db_v2(2).sqlite")
  #hdr_db <- dbConnect(drv = RPostgres::Postgres(), host = post_host, dbname = "HDR", user = post_user, password = post_pass, port = post_port)
  capa_db <- dbConnect(drv = RPostgres::Postgres(), host = prio_host, dbname = "capa", user = prio_user, password = prio_pass, port = prio_port)
  return(capa_db)
}
disconnect_from_capa <- function(x){
  dbDisconnect(x)
}

ison <- function(iso3c){
  iso3n <- countrycode(iso3c, origin = "iso3c", destination = "iso3n")
  if(is.na(iso3n)){
    iso3n <- 899
  }
  return(iso3n)
}

L25_weight <- 1
L50_weight <- 1
L100_weight <- 1
M25_weight <- 1
M50_weight <- 1
M100_weight <- 1
H25_weight <- 1
H50_weight <- 1
H100_weight <- 1
int25_weight <- 0
int50_weight <- 0
int100_weight <- 0

weights <- list(L25_weight, L50_weight, L100_weight, M25_weight, M50_weight, M100_weight, H25_weight, H50_weight, H100_weight,
                int25_weight, int50_weight, int100_weight)

weights <- list(L25 = L25_weight, L50 = L50_weight, L100 = L100_weight, M25 = M25_weight, M50 = M50_weight, M100 = M100_weight, H25 = H25_weight, H50 = H50_weight, H100 = H100_weight,
                int25 = int25_weight, int50 = int50_weight, int100 = int100_weight)

worst_adm <- function(iso, years, monthly = TRUE, adm1 = TRUE, weights, threshold = 1, cap = NA){
  
  iso3n <- ison(iso)
  
  #browser()
  capa_db <- connect_to_capa()
  
  if(adm1){
    tot_group_vars <- "year, capa_id_adm1"
    tot_join_vars <- c("year", "capa_id_adm1")
    if(monthly){
      grouping_vars <- "year, month, capa_id_adm1"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("year", "month", "capa_id_adm1")
    }else{
      grouping_vars <- "year, capa_id_adm1"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- c("year", "capa_id_adm1")
    }
  }else{
    tot_group_vars <- "year"
    tot_join_vars <- "year"
    if(monthly){
      grouping_vars <- "year, month"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("year", "month")
    }else{
      grouping_vars <- "year"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- "year"
    }
  }

  
  total_query <- glue(
    "SELECT {tot_group_vars}, SUM(cell_pop) AS total_pop
    FROM cell_pops
    WHERE iso3n = {iso3n} AND
      year >= {years[1]} AND
      year <= {years[length(years)]}
    GROUP BY {tot_group_vars}"
  )
  
  total_pop <- dbGetQuery(capa_db, total_query)
  
  sql_query <- glue(
    "
    SELECT
      iso3n, {grouping_vars}, SUM(cell_pop) OVER (PARTITION BY {grouping_vars} ORDER BY score DESC) AS risk_pop, score
    FROM
      (
      SELECT
        iso3n, {grouping_vars}, SUM(cell_pop) AS cell_pop, score
      FROM 
        (
        SELECT 
          iso3n, {grouping_vars},
          AVG(cell_pop) as cell_pop,
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
        FROM cell_stats
        WHERE iso3n = {iso3n} AND
          year >= {years[1]} AND
          year <= {years[length(years)]}
        GROUP BY iso3n, sid, {grouping_vars}
        ) agg
      WHERE score >= {threshold}
      GROUP BY iso3n, {grouping_vars}, score
      ) foobar
    "
  )
  
  data <- dbGetQuery(capa_db, sql_query) %>%
    left_join(total_pop, by = tot_join_vars) %>%
    mutate(risk_pct = risk_pop/total_pop, score = as.numeric(score))
  
  max_scores <- data %>% group_by_at(dplyr_group_vars) %>% summarize(max = max(score)) %>% filter(max != 0)
  
  out_frame <- max_scores[rep(seq_len(dim(max_scores)[1]), max_scores$max), ] %>%
    group_by_at(dplyr_group_vars) %>%
    mutate(score = dplyr::row_number()) %>%
    ungroup() %>%
    left_join(data, by = c("score", names(max_scores)[-ncol(max_scores)])) %>%
    fill(iso3n, risk_pop, risk_pct, total_pop, .direction = "up") %>%
    dplyr::select(-max)
  
  if(!is.na(cap)){
    out_frame <- out_frame %>% filter(score <= cap)
  }
  
  if(threshold != ""){
    #threshold <- as.numeric(strsplit(threshold, split = ",")[[1]])
    out_frame <- out_frame %>% filter(score %in% threshold)
  }
  
  if(adm1){
    out_frame <- left_join(out_frame %>% within(capa_id_adm1 <- as.numeric(capa_id_adm1)), dplyr::select(adm1_cgaz, capa_id, shape_name) %>% st_drop_geometry(), by = c("capa_id_adm1" = "capa_id"))
  }
  
  disconnect_from_capa(capa_db)
  return(out_frame %>% within(risk_pct <- round(risk_pct, 4)))
  
}

system.time({x <- worst_adm("CAF", 2011:2015, monthly = T, adm1 = T, weights)})

plot_score_sql <- function(iso, years, start_end = c(1,12), weights, draw_points = TRUE){
  
  iso3n <- ison(iso)
  
  weight_list <- weights
  L25_weight <- weight_list[[1]]
  L50_weight <- weight_list[[2]]
  L100_weight <- weight_list[[3]]
  M25_weight <- weight_list[[4]]
  M50_weight <- weight_list[[5]]
  M100_weight <- weight_list[[6]]
  H25_weight <- weight_list[[7]]
  H50_weight <- weight_list[[8]]
  H100_weight <- weight_list[[9]]
  int25_weight <- weight_list[[10]]
  int50_weight <- weight_list[[11]]
  int100_weight <- weight_list[[12]]
  
  #browser()
  capa_db <- connect_to_capa()
  
  #years_string <- paste(years, collapse = ", ")
  
  sql_query <- glue(
    "SELECT
      score,
      geometry
    FROM
      (
      SELECT
        sid,
        SUM(((lo_25 * {L25_weight}) +
        ((lo_50 - lo_25) * {L50_weight}) +
        ((lo_100 - lo_50) * {L100_weight}) +
        (md_25 * {M25_weight}) +
        ((md_50 - md_25) * {M50_weight}) +
        ((md_100 - md_50) * {M100_weight}) +
        (hi_25 * {H25_weight}) +
        ((hi_50 - hi_25) * {H50_weight}) +
        ((hi_100 - hi_50) * {H100_weight}) +
        (int_25 * {int25_weight}) +
        ((int_50 - int_25) * {int50_weight}) +
        ((int_100 - int_50) * {int100_weight}))) AS score
      FROM cell_stats
      WHERE iso3n = {iso3n} AND
        year >= {years[1]} AND
        year <= {years[length(years)]} AND
        NOT (month < {start_end[1]} AND year = {min(years)}) AND
        NOT (month > {start_end[2]} AND year = {max(years)})
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
  
  data$score <- as.numeric(data$score)
  
  conflict_events <- filter(ged, iso3c == iso & year %in% years) %>%
    filter(!(year == min(years) & month < start_end[1])) %>%
    filter(!(year == max(years) & month > start_end[2]))

  conflict_events$int_cat <- as.factor(conflict_events$int_cat)
  
  if(draw_points == FALSE){
    conflict_events <- head(conflict_events, n = 0)
  }
  
  out_list <- list(data, filter(adm1_cgaz, shape_group == iso), conflict_events)
  
  disconnect_from_capa(capa_db)
  
  return(out_list)
  
}

plot_score <- function(x, legend_size = 2, font_size = 18){
  #browser()
  out_plot <- ggplot(x[[2]]) +
    geom_sf(data = x[[1]], aes(fill = score), color = NA) +
    geom_sf(data = x[[3]], aes(col = int_cat)) +
    geom_sf(fill = NA) +
    scale_fill_viridis_c() +
    theme(legend.key.size = unit(legend_size, 'cm'),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size))
  
  return(out_plot)
}
