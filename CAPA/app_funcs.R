#To Do
#App funcs
  #frequency weird stuff happening with ADM1 selected (or is there?) - fixed I think
  #add less than yearly for CAR
  #implement starting and ending period for frequency/duration
  #probably best to force period threshold
#Refactoring
  #clean up gv for global agg
  #There's something weird where logical choices from the UI are getting passed as strings - need to fix up and cleanup all the temporary as.logical() calls
#Usability
  #create tool-tip for weight presets
  #maybe generate a dynamic problem statement based on inputs
  #restrict allowing multi-country input for the cell_score map
  #move plot options to their own tab and make them reactive
#Backend
  #month_abs either needs removing or fixing for when updating stats
  #simplify or split adm0 rds
  #give more flexibility for when ADM2 is integrated


####Utilities####
connect_to_capa <- function(){
  #hdr_db <- dbConnect(RSQLite::SQLite(), "C:/Users/andara/Documents/hdr_db_v2(2).sqlite")
  #hdr_db <- dbConnect(drv = RPostgres::Postgres(), host = post_host, dbname = "HDR", user = post_user, password = post_pass, port = post_port)
  capa_db <- dbConnect(drv = RPostgres::Postgres(), host = prio_host, dbname = "capa", user = prio_user, password = prio_pass, port = prio_port)
  return(capa_db)
}
disconnect_from_capa <- function(x){
  dbDisconnect(x)
}

ison <- function(iso3c, vectorized = FALSE){
  individual_isos <- iso3c[str_length(iso3c) == 3]
  iso3n <- countrycode(individual_isos, origin = "iso3c", destination = "iso3n", custom_match = c("KOS" = 899))
  iso3n <- ifelse(is.na(iso3n), 899, iso3n)
  
  regions <- iso3c[str_length(iso3c) > 3]
  for(region in regions){
    iso3n <- append(iso3n, ison_region(region))
  }
  if(vectorized){
    return(iso3n)
  }else{
    return(unique(iso3n))
  }
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

isoc <- function(iso3n){
  iso3c <- countrycode(iso3n, origin = "iso3n", destination = "iso3c", custom_match = c("KOS" = 899))
  iso3c <- ifelse(is.na(iso3c), "KOS", iso3c)
  return(iso3c)
}

sanitize_weights <- function(x){
  
  weight_list <- lapply(x, function(x){if(!is.numeric(x)){return(0)}else{return(x)}})
  return(weight_list)
  
}

sanitize_threshold <- function(x){
  if(!is.numeric(x)){return(0)}else{return(x)}
}

sanitize_iso <- function(iso){
  if(is.numeric(iso)){
    iso3n <- iso
  }else{
    iso3n <- ison(iso)
  }
  
  return(iso3n)
}

create_placeholder_freq <- function(iso, max_periods, adm1){
  placeholder <- data.table(iso3n = iso) %>%
    .[rep(1:.N, max_periods)] %>%
    .[ , n_periods := 1:max_periods, by = list(iso3n)] %>%
    setorder(iso3n, n_periods)
  
  if(adm1){
    placeholder <- placeholder %>%
      left_join(dplyr::select(adm1_cgaz, iso3n, capa_id) %>% st_drop_geometry(), by = "iso3n") %>%
      rename(capa_id_adm1 = capa_id)
  }
  
  return(placeholder)
  
}

placeholder_join_freq <- function(adm1){
  
  ph_join_vars <- c("iso3n", "n_periods")
  
  if(adm1){
    ph_join_vars <- c(ph_join_vars, "capa_id_adm1")
  }
  
  return(ph_join_vars)
  
}

create_placeholder_dur <- function(iso, adm1){
  placeholder <- data.table(iso3n = iso)
  
  if(adm1){
    placeholder <- placeholder %>%
      left_join(dplyr::select(adm1_cgaz, iso3n, capa_id) %>% st_drop_geometry(), by = "iso3n") %>%
      rename(capa_id_adm1 = capa_id)
  }
  
  return(placeholder)
  
}

placeholder_join_dur <- function(adm1){
  
  ph_join_vars <- c("iso3n")
  
  if(adm1){
    ph_join_vars <- c(ph_join_vars, "capa_id_adm1")
  }
  
  return(ph_join_vars)
  
}

#create a placeholder dataframe to join with returned conflict data so that observations that did not have any conflict are retained in the data structure
create_placeholder <- function(iso, years, period, adm1){

  if(period == "yearly"){
    placeholder <- data.table(iso3n = iso) %>%
      .[rep(1:.N, length(years))] %>%
      .[ , year := years, by = list(iso3n)] %>%
      setorder(iso3n, year)
  }else{
    if(period == "monthly"){
      p_num <- 12
      p_name <- "month"
    }else if(period == "quarterly"){
      p_num <- 4
      p_name <- "quarter"
    }else if(period == "biannually"){
      p_num <- 2
      p_name <- "half"
    }
    
    placeholder <- data.table(iso3n = iso) %>%
      .[rep(1:.N, length(years))] %>%
      .[ , year := years, by = list(iso3n)] %>%
      .[rep(1:.N, p_num)] %>%
      .[ , period := c(1:p_num), by = list(iso3n, year)] %>%
      setorder(iso3n, year, period)
    names(placeholder)[3] <- p_name
    
  }
  
  if(adm1){
    placeholder <- placeholder %>%
      left_join(dplyr::select(adm1_cgaz, iso3n, capa_id, shape_name) %>% st_drop_geometry(), by = "iso3n") %>%
      rename(capa_id_adm1 = capa_id)
  }
  
  return(placeholder)
  
}

#func for generating the "by" arg in the placeholder join to the data
placeholder_join <- function(period, adm1){
  if(period == "yearly"){
    ph_join_vars <- c("iso3n", "year")
  }else if(period == "biannually"){
    ph_join_vars <- c("iso3n", "year", "half")
  }else if(period == "quarterly"){
    ph_join_vars <- c("iso3n", "year", "quarter")
  }else if(period == "monthly"){
    ph_join_vars <- c("iso3n", "year", "month")
  }
  
  if(adm1){
    ph_join_vars <- c(ph_join_vars, "capa_id_adm1")
  }
  
  return(ph_join_vars)
  
}


####Work Horses####
get_standard_aggregation <- function(iso, years, period = "monthly", adm1 = TRUE, weights, threshold = 1, selected_period = NA){
  #browser()
  iso3n <- sanitize_iso(iso)
  
  threshold <- sanitize_threshold(threshold)

  capa_db <- connect_to_capa()
  
  gv <- get_standard_gv(adm1, period)
  
  total_pop <- query_total_pop(iso3n, adm1, years, gv, capa_db)
  
  data <- query_standard_aggregation(iso3n, years, weights, threshold, gv, capa_db, score = FALSE)
  ph <- create_placeholder(iso = iso3n, years = years, period = period, adm1 = adm1)
  
  out_frame <- ph %>% left_join(data, by = placeholder_join(period, adm1)) %>%
    mutate(risk_pop = ifelse(is.na(risk_pop), 0, risk_pop)) %>%
    left_join(total_pop, by = gv[['tot_join_vars']]) %>%
    mutate(risk_pct = risk_pop/total_pop)
  
  #This section is dependent on a supplied value to "selected_period" and is for filtering the df before plotting
  if(period == "monthly" & !is.na(selected_period)){
    out_frame <- out_frame %>% filter(month == selected_period)
  }
  if(period == "biannually" & !is.na(selected_period)){
    out_frame <- out_frame %>% filter(half == selected_period)
  }
  if(period == "quarterly" & !is.na(selected_period)){
    out_frame <- out_frame %>% filter(quarter == selected_period)
  }
  
  out_frame <- out_frame %>% within(risk_pct <- round(risk_pct, 4))
  
  disconnect_from_capa(capa_db)
  return(out_frame)
  
}

get_score_aggregation <- function(iso, years, period = "monthly", adm1 = TRUE, weights, threshold = 1, cap = NA, score_selection = NULL){
  iso3n <- sanitize_iso(iso)
  
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- get_standard_gv(adm1, period)
  
  total_pop <- query_total_pop(iso3n, adm1, years, gv, capa_db)
  
  data <- query_standard_aggregation(iso3n, years, weights, threshold, gv, capa_db, score = TRUE)
  
  data <- data %>%
    mutate(score = as.numeric(score)) %>%
    left_join(total_pop, by = gv[['tot_join_vars']]) %>%
    mutate(risk_pct = risk_pop/total_pop)
  
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
  if(!is.null(score_selection)){
    out_frame <- out_frame %>% filter(score %in% score_selection)
  }
  
  out_frame <- out_frame %>% within(risk_pct <- round(risk_pct, 4))
  
  disconnect_from_capa(capa_db)
  return(out_frame)
  
}

get_cell_scores <- function(iso, years, start_end = c(1,12), weights, draw_adm1 = TRUE, draw_points = TRUE){
  #browser()
  iso3n <- sanitize_iso(iso)

  capa_db <- connect_to_capa()
  
  data <- query_cell_scores(iso3n, years, start_end, weights, capa_db)
  
  data$score <- as.numeric(data$score)
  
  conflict_events <- filter(ged, iso3c %in% iso & year %in% years) %>%
    filter(!(year == min(years) & month < start_end[1])) %>%
    filter(!(year == max(years) & month > start_end[2]))
  
  conflict_events$int_cat <- as.factor(conflict_events$int_cat)
  
  if(draw_points == FALSE){
    conflict_events <- head(conflict_events, n = 0)
  }
  
  #assign iso3n to iso3n_vector to avoid name confusion in the filter call
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

adm_plot <- function(x, isos, adm1, id_col = "capa_id", legend_size = 2, font_size = 18){
  #browser()
  if(adm1){
    isos <- ison(isos)
    x <- left_join(x, adm1_cgaz %>% dplyr::select(capa_id), by = setNames("capa_id", id_col)) %>% st_as_sf()
    y <- filter(adm0_cgaz, iso3n %in% isos)
    my_plot <- ggplot() +
      geom_sf(data = x, aes(fill = risk_pct), col = "grey", size = 0.5) +
      geom_sf(data = y, col = "black") +
      scale_fill_viridis_c(limits = c(0.0001,1)) +
      theme(legend.key.size = unit(legend_size, 'cm'),
            legend.text = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill="#FFEBCD"))
  }else{
    x <- x %>%
      mutate(iso3c = isoc(iso3n)) %>%
      left_join(nid_grid %>% dplyr::select(ISOCODE), by = c("iso3c" = "ISOCODE")) %>% st_as_sf()
    my_plot <- ggplot() +
      geom_sf(data = x, aes(fill = risk_pct), col = "black", size = 0.5) +
      scale_fill_viridis_c(limits = c(0.0001,1)) +
      theme(legend.key.size = unit(legend_size, 'cm'),
            legend.text = element_text(size = font_size),
            legend.title = element_text(size = font_size),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill="#FFEBCD"))
  }
  
  return(my_plot)
  
}

adm0_plot <- function(x, legend_size = 2, font_size = 18){
  #browser()
  #isos <- ison(isos)
  x <- x %>%
    mutate(iso3c = isoc(iso3n)) %>%
    left_join(nid_grid %>% dplyr::select(ISOCODE), by = c("iso3c" = "ISOCODE")) %>% st_as_sf() #%>% st_simplify(preserveTopology = T, dTolerance = 0.25)
  #isos <- isos[isos %!in% x$iso3n]
  #y <- filter(adm0_cgaz, iso3n %in% isos)
  my_plot <- ggplot() +
    geom_sf(data = x, aes(fill = risk_pct), col = "black", size = 0.5) +
    scale_fill_viridis_c(limits = c(0,1)) +
    theme(legend.key.size = unit(legend_size, 'cm'),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="#FFEBCD"))
  # my_plot <- tm_shape(x) +
  #   tm_polygons("risk_pct")
  return(my_plot)
  
}



# system.time({x <- get_standard_aggregation("World", 1990:2020, period = "yearly", adm1 = F, weights)})
# adm0_plot(x)
# z <- nid_grid %>% st_simplify(preserveTopology = T, dTolerance = 0.1)
# plot(z['geometry'])

get_temporal <- function(type, iso, years, weights, period = "yearly", start_end = c(1,12), adm1 = FALSE, threshold = 1, p_threshold = NA, max_periods = NULL){
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
  
  gv <- get_temporal_gv(adm1, period, start_end, years)
  if("World" %in% iso){
    gv[['region']] <- "World"
  }else{
    gv[['region']] <- "not_world"
  }
  
  total_pop <- query_total_pop(iso3n, adm1, years[length(years)], gv, capa_db) %>%
    rename(capa_id = contains("capa_id")) %>% dplyr::select(-year)
  
  if(type == "frequency"){
    data <- query_frequency(iso3n, years, start_end, weights, threshold, gv, capa_db) %>%
      within(n_periods <- as.numeric(n_periods))
    temp_out <- create_placeholder_freq(iso3n, max_periods, adm1) %>%
      left_join(data, by = placeholder_join_freq(adm1)) %>%
      group_by(across(all_of(unlist(strsplit(gv[['grouping_vars']], ", "))))) %>%
      fill(everything(), .direction = "up") %>%
      mutate(across(.cols = everything(), .fns = ~replace_na(., 0))) %>%
      ungroup()
  }
  if(type == "duration"){
    num_p_in_year <- 1
    if(period == "monthly"){
      num_p_in_year <- 12
    }
    if(period == "quarterly"){
      num_p_in_year <- 4
    }
    if(period == "biannually"){
      num_p_in_year <- 2
    }
    data <- query_duration(iso3n, years, start_end, weights, threshold, gv, capa_db) %>%
      within(risk_pop <- as.numeric(risk_pop) / num_p_in_year)
    temp_out <- create_placeholder_dur(iso3n, adm1) %>%
      left_join(data, by = placeholder_join_dur(adm1)) %>%
      mutate(across(.cols = everything(), .fns = ~replace_na(., 0)))
  }
  
  out_frame <- temp_out %>%
    rename(capa_id = contains("capa_id")) %>%
    left_join(total_pop, by = gv[['tot_join_vars']]) %>%
    mutate(risk_pct = risk_pop/total_pop)
  
  if(!is.null(out_frame$capa_id)){
    out_frame$capa_id <- as.numeric(out_frame$capa_id)
  }
  
  if(adm1){
    out_frame <- left_join(out_frame, dplyr::select(adm1_cgaz, capa_id, shape_name) %>% st_drop_geometry(), by = "capa_id")
  }
  
  if(!is.na(p_threshold)){
    out_frame <- out_frame %>%
      filter(n_periods == p_threshold)
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

get_region_aggregation <- function(region, years, weights, period = "monthly", threshold = 1){
  
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- list()
  gv['region'] <- region
  if(period == "monthly"){
    gv['table'] <- "cell_stats"
    gv['grouping_vars'] <- "year, month"
  }else if(period == "quarterly"){
    gv['table'] <- "cell_stats_qu"
    gv['grouping_vars'] <- "year, quarter"
  }else if(period == "biannually"){
    gv['table'] <- "cell_stats_bi"
    gv['grouping_vars'] <- "year, half"
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
    mutate(risk_pct = round(risk_pop/total_pop, 4))
  
  disconnect_from_capa(capa_db)
  
  return(data) 
}

get_custom_region_aggregation <- function(isos, years, weights, period = "monthly", threshold = 1){
  threshold <- sanitize_threshold(threshold)
  
  capa_db <- connect_to_capa()
  
  gv <- list()
  gv['region'] <- "Custom"
  if(period == "monthly"){
    gv['table'] <- "cell_stats"
    gv['grouping_vars'] <- "year, month"
  }else if(period == "quarterly"){
    gv['table'] <- "cell_stats_qu"
    gv['grouping_vars'] <- "year, quarter"
  }else if(period == "biannually"){
    gv['table'] <- "cell_stats_bi"
    gv['grouping_vars'] <- "year, half"
  }else{
    gv['table'] <- "cell_stats_yr"
    gv['grouping_vars'] <- "year"
  }
  
  total_pop <- query_custom_region_pop(isos, years, capa_db)
  
  data <- query_region_aggregation(isos, years, weights, threshold, gv, capa_db) %>%
    left_join(total_pop, by = "year") %>%
    mutate(risk_pct = round(risk_pop/total_pop, 4))
  
  disconnect_from_capa(capa_db)
  
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




# system.time({x <- get_standard_aggregation(iso3n_wa, 2018, monthly = F, adm1 = T, weights)})
# iso3n_a <- ison_region("Africa")
# system.time({x <- get_standard_aggregation(iso3n_a, 2018, monthly = F, adm1 = T, weights)})
# system.time({my_plot <- adm_plot(x, iso3n_wa, "capa_id_adm1")})
# my_plot

children_in_conflict <- function(iso3c, years, period, adm1, weights, threshold = 1,
                                 score_selection = c(1, 25, 100, 1000), cat_names = c("low", "medium", "high", "extreme"), exclusive = F, level = "Country"){
  #browser()
  capa_db <- connect_to_capa()
  
  #get totals to update country totals for countries not in the conflict table
  country_tots <- dbGetQuery(capa_db, "SELECT * FROM country_pops")
  
  conf_long <- get_score_aggregation(iso = iso3c, years = years, period = period, adm1 = adm1, weights = weights, threshold = threshold, score_selection = score_selection)
  
  if(exclusive){
    conf_long <- conf_long %>%
      group_by(iso3n, year) %>%
      mutate(risk_pop = risk_pop - lead(risk_pop, default = 0),
             risk_pct = round(risk_pop/total_pop, 4)) %>%
      ungroup()
  }
  
  #generate a placeholder that includes all categories for all countries for all years
  category_join <- data.table(iso3n = ison(iso3c)) %>%
    .[rep(1:.N, length(cat_names))] %>%
    .[ , category := cat_names, by = iso3n] %>%
    .[ , score := score_selection, by = iso3n] %>%
    .[rep(1:.N, length(years))] %>%
    .[ , year := years, by = list(iso3n, category, score)]
  
  #join the data to the placeholder and pivot wide
  conf_wide <- category_join %>% left_join(conf_long, by = c("iso3n", "year", "score")) %>%
    dplyr::select(-score) %>%
    mutate(risk_pop = ifelse(is.na(risk_pop), 0, risk_pop),
           risk_pct = ifelse(is.na(risk_pct), 0, risk_pct)) %>%
    rows_update(country_tots, by = c("iso3n", "year"), unmatched = "ignore") %>%
    pivot_wider(names_from = category, values_from = c(risk_pop, risk_pct))
  
  #add demographic data and calculate the children stuff
  conf <- conf_wide %>%
    left_join(un_demos, by = c("iso3n", "year")) %>%
    mutate(total_children = round(total_pop * child_pct))
  
  child_names <- paste("risk_children", cat_names, sep = "_")
  pop_names <- paste("risk_pop", cat_names, sep = "_")
  for(i in 1:length(cat_names)){
    conf <- conf %>% mutate(!!sym(child_names[i]) := round(!!sym(pop_names[i]) * child_pct))
  }
  
  child_pct_names <- paste("risk_children", cat_names, "share", sep = "_")
  
  for(i in 1:length(cat_names)){
    conf <- conf %>% mutate(!!sym(child_pct_names[i]) := round(!!sym(child_names[i]) / total_children, 4))
  }
  
  conf <- conf %>%
    mutate(iso3c = isoc(iso3n)) %>%
    arrange(iso3c, year) %>%
    dplyr::select(iso3c, iso3n, year, total_pop, un_total, child_pct, total_children, everything())
  
  #attach region names
  region_key <- dbGetQuery(capa_db, "SELECT * FROM region_key WHERE region != 'World'")
  region1 <- region_key[!duplicated(region_key$iso3n), ]
  
  region2 <- region_key[region_key$region %!in% region1$region, ]
  region2 <- region2[!duplicated(region2$iso3n), ]
  
  region3 <- region_key[region_key$region %!in% region1$region, ]
  region3 <- region3[region3$region %!in% region2$region, ]
  
  region1 <- region1 %>% rename(region1 = region)
  region2 <- region2 %>% rename(region2 = region)
  region3 <- region3 %>% rename(region3 = region)
  
  conf_out <- conf %>% left_join(region1, by = "iso3n") %>% left_join(region2, by = "iso3n") %>% left_join(region3, by = "iso3n")
  
  #attach b_deaths
  b_deaths <- ged %>% st_drop_geometry() %>% group_by(iso3c, year) %>% summarize(battle_deaths = sum(best, na.rm = T))
  conf_out <- conf_out %>% left_join(b_deaths, by = c("iso3c", "year"))
  
  disconnect_from_capa(capa_db)
  
  conf_out$country <- countrycode(conf_out$iso3c, origin = "iso3c", destination = "country.name")
  conf_out$country[is.na(conf_out$country)] <- "Kosovo"
  conf_out <- conf_out %>% dplyr::select(country, everything())
  
  # if(level != "Country"){
  #   conf_out <- agg_car(conf_out, level = level)
  # }
  
  conf_out <- conf_out #%>% dplyr::select(-matches("region\\d"), -contains("risk_pop"), -contains("risk_pct")) %>% arrange(country)
  
  
  return(conf_out)
  
}

agg_car <- function(car_data, level){
  
  if(level == "Region"){
    gv <- c("region", "year")
    region1_car <- car_data %>% dplyr::select(-region2, -region3) %>% rename(region = region1) %>% filter(!is.na(region))
    region2_car <- car_data %>% dplyr::select(-region1, -region3) %>% rename(region = region2) %>% filter(!is.na(region))
    region3_car <- car_data %>% dplyr::select(-region1, -region2) %>% rename(region = region3) %>% filter(!is.na(region))
    car_data <- bind_rows(region1_car, region2_car, region3_car)
  }else if(level == "Global"){
    gv <- c("year")
  }
  
  car_data <- car_data %>%
    group_by_at(gv) %>%
    summarize(across((contains("children") & !contains("share")) | contains("total") | contains("battle_deaths"), sum, na.rm = T)) %>%
    mutate(across(contains("risk_children"), ~ round(.x / total_children, 4), .names = "{.col}_share"))
  
  return(car_data)
  
}


####Dashboard utilities####
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

weight_presets <- function(session, weight_list, preset){
  weight_list <- weight_list[order(names(weight_list))]
  for(i in seq(weight_list)){
    updateNumericInput(session, names(weight_list)[i], value = weight_presets_list[[preset]][i])
  }
}