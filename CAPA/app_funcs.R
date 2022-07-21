#next up
#probably best to force period threshold
#There's something weird where logical choices from the UI are getting passed as strings - need to fix up and cleanup all the temporary as.logical() calls
#fixed broken bit in duration, but might need closer investigation to make sure
#allow ADM0 for adm_map
#weight presets
#simplify or split adm0 rds
#explore allowing multi-country input for the cell_score map
#give more flexibility for when ADM2 is integrated
#think about adding total pops for countries alongside regions to allow for a custom region selection

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
  individual_isos <- iso3c[str_length(iso3c) == 3]
  iso3n <- countrycode(individual_isos, origin = "iso3c", destination = "iso3n")
  iso3n <- ifelse(is.na(iso3n), 899, iso3n)
  
  regions <- iso3c[str_length(iso3c) > 3]
  for(region in regions){
    iso3n <- append(iso3n, ison_region(region))
  }
  return(unique(iso3n))
}

ison_region <- function(region){
  capa_db <- connect_to_capa()
  if(region == "World"){
    iso3n <- unique(dbGetQuery(capa_db, glue("SELECT * FROM region_key"))$iso3n)
  }else{
    iso3n <- dbGetQuery(capa_db, glue("SELECT * FROM region_key WHERE region = '{region}'"))$iso3n
  }
  disconnect_from_capa(capa_db)
  return(iso3n)
}

sanitize_weights <- function(x){
  
  weight_list <- lapply(x, function(x){if(!is.numeric(x)){return(0)}else{return(x)}})
  return(weight_list)
  
}

sanitize_threshold <- function(x){
  if(!is.numeric(x)){return(0)}else{return(x)}
}

# L25_weight <- 1
# L50_weight <- 1
# L100_weight <- 1
# M25_weight <- 1
# M50_weight <- 1
# M100_weight <- 1
# H25_weight <- 1
# H50_weight <- 1
# H100_weight <- 1
# int25_weight <- 0
# int50_weight <- 0
# int100_weight <- 0
# 
# weights <- list(L25_weight, L50_weight, L100_weight, M25_weight, M50_weight, M100_weight, H25_weight, H50_weight, H100_weight, int25_weight, int50_weight, int100_weight)
# 
# weights <- list(L25 = L25_weight, L50 = L50_weight, L100 = L100_weight, M25 = M25_weight, M50 = M50_weight, M100 = M100_weight, H25 = H25_weight, H50 = H50_weight, H100 = H100_weight,
#                 int25 = int25_weight, int50 = int50_weight, int100 = int100_weight)

get_standard_aggregation <- function(iso, years, monthly = TRUE, adm1 = TRUE, weights, threshold = 1, cap = NA, score = FALSE, selected_month = NA){
  #browser()
  if(is.numeric(iso)){
    iso3n <- iso
  }else{
    iso3n <- ison(iso)
  }
  
  threshold <- sanitize_threshold(threshold)

  capa_db <- connect_to_capa()
  
  gv <- get_standard_gv(adm1, monthly)
  
  total_pop <- query_total_pop(iso3n, years, gv, capa_db)
  
  data <- query_standard_aggregation(iso3n, years, weights, threshold, gv, capa_db) %>%
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
  
  if(as.logical(monthly) & !is.na(selected_month)){
    out_frame <- out_frame %>% filter(month == selected_month)
  }
  
  disconnect_from_capa(capa_db)
  return(out_frame %>% within(risk_pct <- round(risk_pct, 4)))
  
}

# system.time({x <- get_standard_aggregation(c("AFG", "IRQ", "MEX", "CAF"), 1990:2020, monthly = T, adm1 = T, weights)})
# system.time({x <- get_standard_aggregation("AFG", 1990:2020, monthly = T, adm1 = F, weights)})
# system.time({x <- get_standard_aggregation("AFG", 1990:2020, monthly = F, adm1 = T, weights)})
# system.time({x <- get_standard_aggregation(c("AFG", "IRQ", "MEX", "CAF"), 1990:2020, monthly = F, adm1 = F, weights)})
# system.time({x <- get_standard_aggregation("AFG", 1990:2020, monthly = T, adm1 = T, weights, score = T)})
# system.time({x <- get_standard_aggregation("AFG", 1990:2020, monthly = T, adm1 = F, weights, score = T)})
# system.time({x <- get_standard_aggregation("AFG", 1990:2020, monthly = F, adm1 = T, weights, score = T)})
# system.time({x <- get_standard_aggregation(c("AFG", "IRQ"), 1990:2020, monthly = F, adm1 = F, weights, score = T)})
# 
# iso3n_wa <- ison_region("Western Asia")
# system.time({x <- get_standard_aggregation(iso3n_wa, 1990:2020, monthly = F, adm1 = F, weights)})
# system.time({x <- get_standard_aggregation(iso3n_wa, 1990:2020, monthly = F, adm1 = T, weights)})
# system.time({x <- get_standard_aggregation(iso3n_wa, 1990:2020, monthly = T, adm1 = F, weights)})
# system.time({x <- get_standard_aggregation(iso3n_wa, 1990:2020, monthly = T, adm1 = T, weights)})

get_cell_scores <- function(iso, years, start_end = c(1,12), weights, draw_adm1 = TRUE, draw_points = TRUE){
  #browser()
  if(is.numeric(iso)){
    iso3n <- iso
  }else{
    iso3n <- ison(iso)
  }

  capa_db <- connect_to_capa()
  
  data <- query_cell_scores(iso3n, years, start_end, weights, capa_db)
  
  data$score <- as.numeric(data$score)
  
  conflict_events <- filter(ged, iso3c == iso & year %in% years) %>%
    filter(!(year == min(years) & month < start_end[1])) %>%
    filter(!(year == max(years) & month > start_end[2]))
  
  conflict_events$int_cat <- as.factor(conflict_events$int_cat)
  
  if(draw_points == FALSE){
    conflict_events <- head(conflict_events, n = 0)
  }
  
  #to avoid confusion in the filter call
  iso3n_vector <- iso3n
  out_list <- list(cells = data, adm = filter(adm1_cgaz, iso3n %in% iso3n_vector), events = conflict_events)
  
  if(draw_adm1 == FALSE){
    out_list['adm'] <- NULL
  }
  
  disconnect_from_capa(capa_db)
  
  return(out_list)
  
}

# system.time({x <- get_cell_scores("AFG", 2011, start_end = c(1,11), weights, draw_points = F)})

plot_cell_scores <- function(x, isos, legend_size = 2, font_size = 18){
  #browser()
  isos <- ison(isos)
  adm0 <- filter(adm0_cgaz, iso3n %in% isos)
  out_plot <- ggplot() +
    geom_sf(data = x[['cells']], aes(fill = score), color = NA) +
    geom_sf(data = x[['events']], aes(col = int_cat)) +
    geom_sf(data = x[['adm']], col = "grey", size = 0.5, fill = NA) +
    geom_sf(data = adm0) +
    scale_fill_viridis_c() +
    theme(legend.key.size = unit(legend_size, 'cm'),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="#FFEBCD"))
  
  return(out_plot)
}

# plot_cell_scores(x)


get_temporal <- function(type, iso, years, weights, monthly = FALSE, start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = NA){
  #not ready to implement the p_threshold just yet. Will be easier once I've gone through the stage to create place-holder 0 entries for periods not matching the thresh condition
  #browser()
  if(type %!in% c("duration", "frequency")){
    stop("type must be either 'duration' or 'frequency'")
  }
  
  if(is.numeric(iso)){
    iso3n <- iso
  }else{
    iso3n <- ison(iso)
  }
  
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- get_temporal_gv(adm1, monthly, start_end, years)
  
  total_pop <- query_total_pop(iso3n, years[length(years)], gv, capa_db) %>%
    rename(capa_id = contains("capa_id"))
  
  if(type == "frequency"){
    data <- query_frequency(iso3n, years, start_end, weights, threshold, gv, capa_db) %>%
      within(n_periods <- as.numeric(n_periods))
  }
  if(type == "duration"){
    data <- query_duration(iso3n, years, start_end, weights, threshold, gv, capa_db) %>%
      within(risk_pop <- as.numeric(risk_pop))
  }
  
  out_frame <- data %>%
    rename(capa_id = contains("capa_id")) %>%
    left_join(total_pop, by = gv[['tot_join_vars']]) %>%
    mutate(risk_pct = risk_pop/total_pop)
  
  if(!is.null(out_frame$capa_id)){
    out_frame$capa_id <- as.numeric(out_frame$capa_id)
  }
  
  if(adm1){
    out_frame <- left_join(out_frame, dplyr::select(adm1_cgaz, capa_id, shape_name) %>% st_drop_geometry(), by = "capa_id")
    if(!is.na(p_threshold)){
      out_frame <- out_frame %>%
        filter(n_periods >= p_threshold) %>%
        group_by(capa_id) %>%
        filter(n_periods == min(n_periods)) %>%
        ungroup()
    }
  }
  
  disconnect_from_capa(capa_db)
  return(out_frame %>% within(risk_pct <- round(risk_pct, 4)))
  
}

# system.time({x <- get_temporal("duration", c("SYR", "IRQ"), years = 2014:2015, monthly = F, start_end = c(1,12), adm1 = T, weights, threshold = 50)})
# system.time({y <- get_temporal("duration", c("SYR", "IRQ"), years = 2014:2015, monthly = T, start_end = c(1,12), adm1 = T, weights, threshold = 50)})
# system.time({z <- get_temporal("duration", c("SYR", "IRQ"), years = 2014:2015, monthly = T, start_end = c(1,12), adm1 = F, weights, threshold = 50)})
# system.time({zz <- get_temporal("duration", c("SYR", "IRQ"), years = 2014:2015, monthly = F, start_end = c(1,12), adm1 = T, weights, threshold = 50)})
# 
# 
# system.time({x <- get_temporal("frequency", c("SYR", "IRQ"), years = 2014:2015, monthly = F, start_end = c(1,12), adm1 = F, weights, threshold = 50)})
# system.time({y <- get_temporal("frequency", c("SYR", "IRQ"), years = 2015, monthly = T, start_end = c(1,12), adm1 = T, weights, threshold = 50)})
# system.time({z <- get_temporal("frequency", c("SYR", "IRQ"), years = 2014:2015, monthly = F, start_end = c(1,12), adm1 = T, weights, threshold = 50)})
# system.time({zz <- get_temporal("frequency", c("SYR", "IRQ"), years = 2015, monthly = T, start_end = c(1,12), adm1 = F, weights, threshold = 50)})
# 
# system.time({z <- get_temporal("frequency", c("SYR", "IRQ"), years = 2014:2016, monthly = F, start_end = c(1,12), adm1 = T, weights, threshold = 50, p_threshold = 1)})
# my_plot <- adm_plot(z)
# my_plot
# system.time({z <- get_temporal("frequency", iso3n_a, years = 2014:2015, monthly = T, start_end = c(1,12), adm1 = T, weights, threshold = 10, p_threshold = 2)})
# my_plot <- adm_plot(z)
# my_plot

get_region_aggregation <- function(region, years, weights, monthly = TRUE, threshold = 1){
  
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- list()
  gv['region'] <- region
  if(monthly){
    gv['table'] <- "cell_stats"
    gv['grouping_vars'] <- "year, month"
  }else{
    gv['table'] <- "cell_stats_yr"
    gv['grouping_vars'] <- "year"
  }
  
  if(region == "World"){
    iso3n <- unique(dbGetQuery(capa_db, glue("SELECT * FROM region_key"))$iso3n)
  }else{
    iso3n <- dbGetQuery(capa_db, glue("SELECT * FROM region_key WHERE region = '{region}'"))$iso3n
  }
  
  total_pop <- query_region_pop(region, years, capa_db)
  
  
  
  data <- query_region_aggregation(iso3n, years, weights, threshold, gv, capa_db) %>%
    left_join(total_pop, by = "year") %>%
    mutate(risk_pct = risk_pop/total_pop)
  
  return(data) 
}

# system.time({x <- get_region_aggregation("Western Asia", 2014:2015, weights, monthly = F)})
# system.time({x <- get_region_aggregation("Western Asia", 2014:2015, weights, monthly = T)})
# system.time({x <- get_region_aggregation("Western Asia", 1990:2020, weights, monthly = F)})
# system.time({x <- get_region_aggregation("Western Asia", 1990:2020, weights, monthly = T)})
# system.time({x <- get_region_aggregation("World", 2014:2015, weights, monthly = F)})
# system.time({x <- get_region_aggregation("World", 2014:2015, weights, monthly = T)})
# system.time({x <- get_region_aggregation("World", 1990:2020, weights, monthly = F)})
# system.time({x <- get_region_aggregation("World", 1990:2020, weights, monthly = T)})


adm_plot <- function(x, isos, id_col = "capa_id", legend_size = 2, font_size = 18){
  #browser()
  isos <- ison(isos)
  x <- left_join(x, adm1_cgaz %>% dplyr::select(capa_id), by = setNames("capa_id", id_col)) %>% st_as_sf()
  #isos <- isos[isos %!in% x$iso3n]
  y <- filter(adm0_cgaz, iso3n %in% isos)
  my_plot <- ggplot() +
    geom_sf(data = x, aes(fill = risk_pct), col = "grey", size = 0.5) +
    geom_sf(data = y, col = "black") +
    scale_fill_viridis_c(limits = c(0,1)) +
    theme(legend.key.size = unit(legend_size, 'cm'),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="#FFEBCD"))
  
  return(my_plot)
  
}

# system.time({x <- get_standard_aggregation(iso3n_wa, 2018, monthly = F, adm1 = T, weights)})
# iso3n_a <- ison_region("Africa")
# system.time({x <- get_standard_aggregation(iso3n_a, 2018, monthly = F, adm1 = T, weights)})
# system.time({my_plot <- adm_plot(x, iso3n_wa, "capa_id_adm1")})
# my_plot

toggle_inputs <- function(input_list,enable_inputs=T,only_buttons=TRUE){
  # Subset if only_buttons is TRUE.
  if(only_buttons){
    buttons <- which(sapply(input_list,function(x) {any(grepl('Button',attr(x,"class")))}))
    input_list = input_list[buttons]
  }
  
  # Toggle elements
  for(x in names(input_list))
    if(enable_inputs){
      shinyjs::enable(x)} else {
        shinyjs::disable(x) }
}