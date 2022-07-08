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
  iso3n <- ifelse(is.na(iso3n), 899, iso3n)
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

worst_adm <- function(iso, years, monthly = TRUE, adm1 = TRUE, weights, threshold = 1, cap = NA, score = FALSE){
  
  iso3n <- ison(iso)

  capa_db <- connect_to_capa()
  
  gv <- get_grouping_vars(adm1, monthly)
  
  total_pop <- get_total_pop(iso3n, years, gv, capa_db)
  
  data <- get_cell_stats(iso3n, years, weights, threshold, gv, capa_db) %>%
    left_join(total_pop, by = gv[['tot_join_vars']]) %>%
    mutate(risk_pct = risk_pop/total_pop)
  
  if(score){
    data <- data %>%
      mutate(score = as.numeric(score))
    
    max_scores <- data %>% group_by_at(gv[['dplyr_group_vars']]) %>% summarize(max = max(score)) %>% filter(max != 0)
    
    out_frame <- max_scores[rep(seq_len(dim(max_scores)[1]), max_scores$max), ] %>%
      group_by_at(gv[['dplyr_group_vars']]) %>%
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
  }else{
    out_frame <- data
  }
 
  if(adm1){
    out_frame <- left_join(out_frame %>% within(capa_id_adm1 <- as.numeric(capa_id_adm1)),
                           dplyr::select(adm1_cgaz, capa_id, shape_name) %>% st_drop_geometry(),
                           by = c("capa_id_adm1" = "capa_id"))
  }
  
  disconnect_from_capa(capa_db)
  return(out_frame %>% within(risk_pct <- round(risk_pct, 4)))
  
}

system.time({x <- worst_adm(c("AFG", "IRQ", "MEX", "CAF"), 1990:2020, monthly = T, adm1 = T, weights)})
system.time({x <- worst_adm("AFG", 1990:2020, monthly = T, adm1 = F, weights)})
system.time({x <- worst_adm("AFG", 1990:2020, monthly = F, adm1 = T, weights)})
system.time({x <- worst_adm(c("AFG", "IRQ", "MEX", "CAF"), 1990:2020, monthly = F, adm1 = F, weights)})
system.time({x <- worst_adm("AFG", 1990:2020, monthly = T, adm1 = T, weights, score = T)})
system.time({x <- worst_adm("AFG", 1990:2020, monthly = T, adm1 = F, weights, score = T)})
system.time({x <- worst_adm("AFG", 1990:2020, monthly = F, adm1 = T, weights, score = T)})
system.time({x <- worst_adm(c("AFG", "IRQ"), 1990:2020, monthly = F, adm1 = F, weights, score = T)})

plot_score_sql <- function(iso, years, start_end = c(1,12), weights, draw_points = TRUE){
  
  iso3n <- ison(iso)

  capa_db <- connect_to_capa()
  
  data <- get_cell_scores(iso3n, years, start_end, weights)
  
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

system.time({x <- plot_score_sql("AFG", 2011, start_end = c(1,11), weights, draw_points = F)})

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
