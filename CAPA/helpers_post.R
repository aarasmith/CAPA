


worst_adm <- function(iso, years, monthly = TRUE, adm1 = TRUE, L25_weight = 4, L50_weight = 2, L100_weight = 1,
                      M25_weight = 8, M50_weight = 4, M100_weight = 2, H25_weight = 16, H50_weight = 8, H100_weight = 4,
                      int25_weight = 0, int50_weight = 0, int100_weight = 0, threshold = "", cap = NA, acled = FALSE){
  
  threshold <- sanitize_threshold(threshold)
  
  weights <- list(L25_weight, L50_weight, L100_weight, M25_weight, M50_weight, M100_weight, H25_weight, H50_weight, H100_weight,
                  int25_weight, int50_weight, int100_weight)
  weight_list <- sanitize_weights(weights)
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
  hdr_db <- connect_to_hdr_db()
  
  table_name <- "comb"
  if(acled == TRUE){
    table_name <- "comb_acled"
  }
  
  if(adm1){
    tot_group_vars <- "year, shape_id"
    tot_join_vars <- c("year", "shape_id")
    if(monthly){
      grouping_vars <- "year, month, shape_id"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("year", "month", "shape_id")
    }else{
      grouping_vars <- "year, shape_id"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- c("year", "shape_id")
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
  
  years_str <- paste(years, collapse = ", ")
  
  total_query <- glue(
    "SELECT
    	{tot_group_vars},
    	sum(cell_pop) AS total_pop
    FROM
    	(
    		SELECT
    			sid,
    			year,
    			cell_pop
    		FROM cell_pops
    		WHERE
    			iso3c = '{iso}' AND
    			year IN ({years_str})
    	) comb_sub
    	
    	LEFT JOIN
    	
    	(
    		SELECT
    			sid,
    			shape_id
    		FROM cgaz
    		WHERE
    			iso3c = '{iso}'
    	) cgaz_sub
    	
    	ON
    		comb_sub.sid = cgaz_sub.sid
    	GROUP BY
    		{tot_group_vars}"
  )
  
  total_pop <- dbGetQuery(hdr_db, total_query)
  
  
  sql_query <- glue(
    "SELECT
      score,
      {grouping_vars},
      cell_pop,
      risk_pop
    FROM
    (
      SELECT
        score,
        {grouping_vars},
        cell_pop,
        sum(cell_pop) OVER (PARTITION BY {grouping_vars} ORDER BY score DESC) AS risk_pop
      FROM
      (
        SELECT
          score,
          {grouping_vars},
          sum(cell_pop) AS cell_pop
        FROM
        (
          SELECT
            sid,
            {grouping_vars},
            cell_pop,
            ((L25 * {L25_weight}) +
            ((L50 - L25) * {L50_weight}) +
            ((L100 - L50) * {L100_weight}) +
            (M25 * {M25_weight}) +
            ((M50 - M25) * {M50_weight}) +
            ((M100 - M50) * {M100_weight}) +
            (H25 * {H25_weight}) +
            ((H50 - H25) * {H50_weight}) +
            ((H100 - H50) * {H100_weight}) +
            (int25 * {int25_weight}) +
            (int50 * {int50_weight}) +
            (int100 * {int100_weight})) AS score
          FROM
      	  (
            (
              SELECT
                {grouping_vars2},
                avg (cell_pop) as cell_pop,
            	  sum (lo_25) AS L25,
                sum (md_25) AS M25,
                sum (hi_25) AS H25,
                sum (lo_50) AS L50,
                sum (md_50) AS M50,
                sum (hi_50) AS H50,
                sum (lo_100) AS L100,
                sum (md_100) AS M100,
                sum (hi_100) AS H100,
                sum (int_25) AS int25,
                sum (int_50) AS int50,
                sum (int_100) AS int100
              FROM comb
              WHERE
            	  iso3c = '{iso}' AND
            	  year IN ({years_str})
              GROUP BY {grouping_vars2}
            ) comb_sub
      	  
      	    LEFT JOIN
      
          	(
          		SELECT
          			sid as sid_b,
          			shape_id
          		FROM cgaz
          		WHERE
          			iso3c = '{iso}'
          	) cgaz_sub
          	
          	ON
          		comb_sub.sid = cgaz_sub.sid_b
        	)
    	  
        ) foo
        GROUP BY score, {grouping_vars}
      ) comb_agg
    ) bar"
  )
  
  
  data <- dbGetQuery(hdr_db, sql_query)
  
  
  data <- data %>% left_join(total_pop, by = tot_join_vars) %>%
    mutate(risk_pct = risk_pop/total_pop, score = as.numeric(score))
  
  max_scores <- data %>% group_by_at(dplyr_group_vars) %>% summarize(max = max(score)) %>% filter(max != 0)
  
  out_frame <- max_scores[rep(seq_len(dim(max_scores)[1]), max_scores$max), ] %>%
    group_by_at(dplyr_group_vars) %>%
    mutate(score = dplyr::row_number()) %>%
    ungroup() %>%
    left_join(data, by = c("score", names(max_scores)[-ncol(max_scores)])) %>%
    fill(risk_pop, risk_pct, total_pop, .direction = "up") %>%
    dplyr::select(-max)
  
  if(!is.na(cap)){
    out_frame <- out_frame %>% filter(score <= cap)
  }
  
  if(threshold != ""){
    threshold <- as.numeric(strsplit(threshold, split = ",")[[1]])
    out_frame <- out_frame %>% filter(score %in% threshold)
  }
  
  if(adm1){
    out_frame <- left_join(out_frame, dplyr::select(adm1_cgaz, shape_id, shape_name), by = "shape_id")
  }
  
  disconnect_from_hdr_db(hdr_db)
  return(out_frame)
  
}


plot_score_sql <- function(iso, years, start_end = c(1,12), acled = FALSE,
                           L25_weight = 4, L50_weight = 2, L100_weight = 1,
                           M25_weight = 8, M50_weight = 4, M100_weight = 2, H25_weight = 16, H50_weight = 8, H100_weight = 4,
                           int25_weight = 0, int50_weight = 0, int100_weight = 0, draw_points = TRUE){
  
  weights <- list(L25_weight, L50_weight, L100_weight, M25_weight, M50_weight, M100_weight, H25_weight, H50_weight, H100_weight,
                  int25_weight, int50_weight, int100_weight)
  weight_list <- sanitize_weights(weights)
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
  hdr_db <- connect_to_hdr_db()
  
  table_name <- "comb"
  if(acled == TRUE){
    table_name <- "comb_acled"
  }
  
  start_month_str <- c("13")
  end_month_str <- c("13")
  
  if(start_end[1] > 1){
    start_month <- start_end[1]
    start_month_str <- str_pad(as.character(1:(start_month - 1)), "2", "left", pad = "0")
    start_month_str <- paste(start_month_str, collapse = "', '")
  }
  if(start_end[2] < 12){
    end_month <- start_end[2]
    end_month_str <- str_pad(as.character((end_month + 1):12), "2", "left", pad = "0")
    end_month_str <- paste(end_month_str, collapse = "', '")
  } 
  
    years_string <- paste(years, collapse = ", ")
    sql_agg <- glue(
      "SELECT
        ((L25 * {L25_weight}) +
        ((L50 - L25) * {L50_weight}) +
        ((L100 - L50) * {L100_weight}) +
        (M25 * {M25_weight}) +
        ((M50 - M25) * {M50_weight}) +
        ((M100 - M50) * {M100_weight}) +
        (H25 * {H25_weight}) +
        ((H50 - H25) * {H50_weight}) +
        ((H100 - H50) * {H100_weight}) +
        (int25 * {int25_weight}) +
        (int50 * {int50_weight}) +
        (int100 * {int100_weight})) AS score,
        geometry
      FROM
      (
      SELECT 
        sid,
        avg (cell_pop) as cell_pop,
    	  sum (lo_25) AS L25,
        sum (md_25) AS M25,
        sum (hi_25) AS H25,
        sum (lo_50) AS L50,
        sum (md_50) AS M50,
        sum (hi_50) AS H50,
        sum (lo_100) AS L100,
        sum (md_100) AS M100,
        sum (hi_100) AS H100,
        sum (int_25) AS int25,
        sum (int_50) AS int50,
        sum (int_100) AS int100
      FROM {table_name}
      WHERE
    	  iso3c = '{iso}' AND
    	  year IN ({years_string}) AND
    	  NOT (month IN ('{start_month_str}') AND year = {min(years)}) AND
      	NOT (month IN ('{end_month_str}') AND year = {max(years)})
      GROUP BY sid
      ) comb_sub
      
      LEFT JOIN
      
      (
      SELECT sid, geometry
      FROM cells
      WHERE
    	  iso3c = '{iso}'
      ) cells_sub
      ON
    	  comb_sub.sid = cells_sub.sid"
    )
    
    
    data <- st_read(hdr_db, query = sql_agg)
    
    data$score <- as.numeric(data$score)
  
  
  if(acled){
    conflict_events <- filter(acled_af, iso3c == iso & YEAR %in% years) %>%
      filter(!(YEAR == min(years) & as.numeric(month) < start_end[1])) %>%
      filter(!(YEAR == max(years) & as.numeric(month) > start_end[2]))
  }else{
    conflict_events <- filter(ged, iso3c == iso & year %in% years) %>%
      filter(!(year == min(years) & as.numeric(month) < start_end[1])) %>%
      filter(!(year == max(years) & as.numeric(month) > start_end[2]))
  }
  
  conflict_events$int_cat <- as.factor(conflict_events$int_cat)
  
  if(draw_points == FALSE){
    conflict_events <- head(conflict_events, n = 0)
  }
  
  # out_plot <- ggplot(filter(adm1_cgaz, shape_group == iso)) +
  #         geom_sf(data = data, aes(fill = score), color = NA) +
  #         geom_sf(data = conflict_events, aes(col = int_cat)) +
  #         geom_sf(fill = NA) +
  #         scale_fill_viridis_c()
  
  out_list <- list(data, filter(adm1_cgaz, shape_group == iso), conflict_events)
  
  disconnect_from_hdr_db(hdr_db)
  return(out_list)
  
}

plot_score <- function(x, legend_size, font_size){
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


long_duration <- function(iso, years, monthly = FALSE, start_end = NULL, period = NA, adm1 = FALSE, L25_weight = 4, L50_weight = 2, L100_weight = 1,
                          M25_weight = 8, M50_weight = 4, M100_weight = 2, H25_weight = 16, H50_weight = 8, H100_weight = 4,
                          int25_weight = 0, int50_weight = 0, int100_weight = 0, cap = NA, acled = FALSE, threshold = ""){
  
  threshold <- sanitize_threshold(threshold)
  
  weights <- list(L25_weight, L50_weight, L100_weight, M25_weight, M50_weight, M100_weight, H25_weight, H50_weight, H100_weight,
                  int25_weight, int50_weight, int100_weight)
  weight_list <- sanitize_weights(weights)
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
  hdr_db <- connect_to_hdr_db()
  
  table_name <- "comb"
  if(acled == TRUE){
    table_name <- "comb_acled"
  }
  
  months <- str_pad(as.character(1:12), "2", "left", pad = "0")
  months_str <- paste(months, collapse = "', '")
  years_str <- paste(years, collapse = ", ")
  
  if(period != "year"){
    monthly <- TRUE
  }
  
  if(monthly){
    grouping_vars <- "year, month"
  }else{
    grouping_vars <- "year"
  }
  
  start_month_str <- c("13")
  end_month_str <- c("13")
  
  if(start_end[1] > 1){
    start_month <- start_end[1]
    start_month_str <- str_pad(as.character(1:(start_month - 1)), "2", "left", pad = "0")
    start_month_str <- paste(start_month_str, collapse = "', '")
  }
  if(start_end[2] < 12){
    end_month <- start_end[2]
    end_month_str <- str_pad(as.character((end_month + 1):12), "2", "left", pad = "0")
    end_month_str <- paste(end_month_str, collapse = "', '")
  }  
    #start_end_vals <- start_end
  
  
  start_end_periods <- glue(
    "NOT (month IN ('{start_month_str}') AND year = {min(years)}) AND
    NOT (month IN ('{end_month_str}') AND year = {max(years)})"
  )
  
  if(adm1){
    total_query <- glue(
      "SELECT
    	shape_id,
    	sum(cell_pop) AS total_pop
    FROM
    	(
    		SELECT
    			sid,
    			year,
    			cell_pop
    		FROM cell_pops
    		WHERE
    			iso3c = '{iso}' AND
    			year IN ({max(years)})
    	) comb_sub
    	
    	LEFT JOIN
    	
    	(
    		SELECT
    			sid,
    			shape_id
    		FROM cgaz
    		WHERE
    			iso3c = '{iso}'
    	) cgaz_sub
    	
    	ON
    		comb_sub.sid = cgaz_sub.sid
    	GROUP BY
    		shape_id"
    )
  }else{
    total_query <- glue(
      "SELECT
      sum (cell_pop) AS cell_pop
    FROM cell_pops
    WHERE
      iso3c = '{iso}' AND
      year IN ({max(years)})
    GROUP BY year"
    )
  }
  
  total_pop <- dbGetQuery(hdr_db, total_query)
  
  pop_query <- glue(
    "SELECT
      sid,
      cell_pop
    FROM cell_pops
    WHERE
      iso3c = '{iso}' And
      year IN ({max(years)})"
  )
  
  cell_pops <- dbGetQuery(hdr_db, pop_query)
  
  if(!is.na(period)){
    if(period %in% c("quarter", "half")){
      p_top <- glue(
        "SELECT
          sid,
          year,
          period,
          SUM(score) AS score,
          shape_id
        FROM
        (
          ("
      )
      
      p_bottom <- glue(
        ") AS comb_period
    
        LEFT JOIN
        
        (
          SELECT month, period
          FROM {period}
        ) p_table
        ON comb_period.month = p_table.month
        )
        GROUP BY sid, year, period, shape_id"
      )
      
      # start_end_periods <- glue(
      #   "NOT (period < ({start_end_vals[1]}) AND year = {min(years)}) AND
      #   NOT (period > ({start_end_vals[2]}) AND year = {max(years)})"
      #)
      
    }else{
      p_top <- ''
      p_bottom <- ''
    }
  }
  
  sql_query <- glue(
    "SELECT
      sid,
      MIN(score) AS min_score,
      shape_id
    FROM
    (
      {p_top}
      SELECT
        sid,
        {grouping_vars},
        ((L25 * {L25_weight}) +
        ((L50 - L25) * {L50_weight}) +
        ((L100 - L50) * {L100_weight}) +
        (M25 * {M25_weight}) +
        ((M50 - M25) * {M50_weight}) +
        ((M100 - M50) * {M100_weight}) +
        (H25 * {H25_weight}) +
        ((H50 - H25) * {H50_weight}) +
        ((H100 - H50) * {H100_weight}) +
        (int25 * {int25_weight}) +
        (int50 * {int50_weight}) +
        (int100 * {int100_weight})) AS score,
        shape_id
      FROM
      (
        SELECT
          {grouping_vars},
          sid,
          avg (cell_pop) as cell_pop,
      	  sum (lo_25) AS L25,
          sum (md_25) AS M25,
          sum (hi_25) AS H25,
          sum (lo_50) AS L50,
          sum (md_50) AS M50,
          sum (hi_50) AS H50,
          sum (lo_100) AS L100,
          sum (md_100) AS M100,
          sum (hi_100) AS H100,
          sum (int_25) AS int25,
          sum (int_50) AS int50,
          sum (int_100) AS int100
        FROM {table_name}
        WHERE
      	  iso3c = '{iso}' AND
      	  year IN ({years_str}) AND
      	  month IN ('{months_str}') AND
      	  NOT (month IN ('{start_month_str}') AND year = {min(years)}) AND
      	  NOT (month IN ('{end_month_str}') AND year = {max(years)})
        GROUP BY {grouping_vars}, sid
        ) comb_sub
        
        LEFT JOIN
        
        (
        SELECT sid AS sid_b, shape_id
        FROM cgaz
        WHERE
      	  iso3c = '{iso}'
        ) cgaz_sub
        ON
      	  comb_sub.sid = cgaz_sub.sid_b
      {p_bottom}
    ) foo
    GROUP BY sid, shape_id"
  )
  
  
  
  data <- dbGetQuery(hdr_db, sql_query)
  
  data$min_score <- as.numeric(data$min_score)
  
  if(adm1){
    risk_pop <- data  %>%
      dplyr::select(sid, shape_id, min_score) %>% left_join(cell_pops, by = "sid") %>% group_by(shape_id, min_score) %>%
      summarize(cell_pop = sum(cell_pop)) %>%
      arrange(desc(min_score)) %>%
      mutate(risk_pop = cumsum(cell_pop)) %>% filter(min_score != 0)
    
    max_min <- risk_pop %>% group_by(shape_id) %>% summarize(max = max(min_score))
    
    out_frame <- max_min[rep(seq_len(dim(max_min)[1]), max_min$max), ] %>%
      group_by(shape_id) %>%
      mutate(min_score = dplyr::row_number()) %>%
      ungroup() %>%
      left_join(risk_pop, by = c("min_score", "shape_id")) %>%
      fill(risk_pop, .direction = "up") %>%
      dplyr::select(-max) %>%
      left_join(total_pop, by = "shape_id") %>%
      mutate(risk_pct = risk_pop/total_pop)
  }else{
    risk_pop <- data  %>%
      dplyr::select(sid, min_score) %>% left_join(cell_pops, by = "sid") %>% group_by(min_score) %>%
      summarize(cell_pop = sum(cell_pop)) %>%
      arrange(desc(min_score)) %>%
      mutate(risk_pop = cumsum(cell_pop))
    
    out_frame <- data.frame(min_score = 1:max(risk_pop$min_score)) %>%
      left_join(risk_pop, by = "min_score") %>%
      fill(risk_pop, .direction = "up") %>%
      mutate(total_pop = total_pop$cell_pop,
             risk_pct = risk_pop/total_pop)
  }
  
  if(!is.na(cap)){
    out_frame <- out_frame %>% filter(min_score <= cap)
  }
  if(threshold != ""){
    threshold <- as.numeric(strsplit(threshold, split = ",")[[1]])
    out_frame <- out_frame %>% filter(min_score %in% threshold)
  }
  
  if(adm1){
    # score_frame <- out_frame %>%
    #   group_by(shape_id) %>%
    #   summarize(score = trapz(min_score, risk_pct)) %>%
    #   right_join(filter(adm1_cgaz, shape_group == iso), by = "shape_id") %>%
    #   mutate(score = ifelse(is.na(score), 0, score)) %>%
    #   st_as_sf()
    
    out_frame <- left_join(out_frame, dplyr::select(adm1_cgaz, shape_id, shape_name), by = "shape_id")
  }
  
  disconnect_from_hdr_db(hdr_db)
  return(out_frame)
  
}



long_freq <- function(iso, years, start_end = c(1,12), period = NA, adm1 = FALSE, L25_weight = 4, L50_weight = 2, L100_weight = 1,
                      M25_weight = 8, M50_weight = 4, M100_weight = 2, H25_weight = 16, H50_weight = 8, H100_weight = 4,
                      int25_weight = 0, int50_weight = 0, int100_weight = 0, cap = NA, acled = FALSE, threshold = NA){
  
  weights <- list(L25_weight, L50_weight, L100_weight, M25_weight, M50_weight, M100_weight, H25_weight, H50_weight, H100_weight,
                  int25_weight, int50_weight, int100_weight)
  weight_list <- sanitize_weights(weights)
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
  hdr_db <- connect_to_hdr_db()
  
  #table call
  table_name <- "comb"
  if(acled == TRUE){
    table_name <- "comb_acled"
  }
  
  #default months
  months <- str_pad(as.character(1:12), "2", "left", pad = "0")
  months_str <- paste(months, collapse = "', '")
  
  monthly <- FALSE
  if(period != "year"){
    monthly <- TRUE
  }
  
  start_month_str <- c("13")
  end_month_str <- c("13")
  
  if(start_end[1] > 1){
    start_month <- start_end[1]
    start_month_str <- str_pad(as.character(1:(start_month - 1)), "2", "left", pad = "0")
    start_month_str <- paste(start_month_str, collapse = "', '")
  }
  if(start_end[2] < 12){
    end_month <- start_end[2]
    end_month_str <- str_pad(as.character((end_month + 1):12), "2", "left", pad = "0")
    end_month_str <- paste(end_month_str, collapse = "', '")
  }
  
  #years to string
  years_str <- paste(years, collapse = ", ")
  
  if(adm1){  
    dplyr_group_vars <- c("shape_id", "sid")
    dplyr_group_vars2 <- c("shape_id", "n_periods")
    tot_group_vars <- "shape_id"
    if(monthly){  
      grouping_vars <- "year, month"
      grouping_vars2 <- "shape_id, year, month"
    }else{
      grouping_vars <- "year"
      grouping_vars2 <- "shape_id, year"
    }
    
    total_query <- glue(
      "SELECT
    	{tot_group_vars},
    	sum(cell_pop) AS total_pop
    FROM
    	(
    		SELECT
    			sid,
    			year,
    			cell_pop
    		FROM cell_pops
    		WHERE
    			iso3c = '{iso}' AND
    			year IN ({max(years)})
    	) comb_sub
    	
    	LEFT JOIN
    	
    	(
    		SELECT
    			sid,
    			shape_id
    		FROM cgaz
    		WHERE
    			iso3c = '{iso}'
    	) cgaz_sub
    	
    	ON
    		comb_sub.sid = cgaz_sub.sid
    	GROUP BY
    		{tot_group_vars}"
    )
    
    total_pop <- dbGetQuery(hdr_db, total_query)
    
  }else{
    dplyr_group_vars <- "sid"
    dplyr_group_vars2 <- "n_periods"
    tot_group_vars <-
      if(monthly){
        grouping_vars <- "year, month"
        grouping_vars2 <- "shape_id, year, month"
      }else{
        grouping_vars <- "year"
        grouping_vars2 <- "shape_id, year"
      }
    
    total_query <- glue(
      "SELECT
        sum (cell_pop) AS total_pop
      FROM cell_pops
      WHERE
        iso3c = '{iso}' AND
        year IN ({max(years)})
      GROUP BY year"
    )
    
    total_pop <- dbGetQuery(hdr_db, total_query) %>% mutate(shape_id = iso)
  }
  
  
  
  pop_query <- glue(
    "SELECT
      sid,
      cell_pop
    FROM cell_pops
    WHERE
      iso3c = '{iso}' And
      year IN ({max(years)})"
  )
  
  cell_pops <- dbGetQuery(hdr_db, pop_query)
  
  if(period %in% c("quarter", "half")){
    p_top <- glue(
      "SELECT
          sid,
          year,
          period,
          SUM(score) AS score,
          shape_id
        FROM
        (
          ("
    )
    
    p_bottom <- glue(
      ") AS comb_period
    
        LEFT JOIN
        
        (
          SELECT month, period
          FROM {period}
        ) p_table
        ON comb_period.month = p_table.month
        )
        GROUP BY sid, year, period, shape_id"
    )
    
    # start_end_periods <- glue(
    #   "NOT (period < ({start_end_vals[1]}) AND year = {min(years)}) AND
    #   NOT (period > ({start_end_vals[2]}) AND year = {max(years)})"
    #)
    
  }else{
    p_top <- ''
    p_bottom <- ''
  }
  
  sql_query <- glue(
    "{p_top}
    SELECT
        sid,
        {grouping_vars2},
        ((L25 * {L25_weight}) +
        ((L50 - L25) * {L50_weight}) +
        ((L100 - L50) * {L100_weight}) +
        (M25 * {M25_weight}) +
        ((M50 - M25) * {M50_weight}) +
        ((M100 - M50) * {M100_weight}) +
        (H25 * {H25_weight}) +
        ((H50 - H25) * {H50_weight}) +
        ((H100 - H50) * {H100_weight}) +
        (int25 * {int25_weight}) +
        (int50 * {int50_weight}) +
        (int100 * {int100_weight})) AS score
      FROM
      (
        (
          SELECT
            {grouping_vars},
            sid,
            avg (cell_pop) as cell_pop,
        	  sum (lo_25) AS L25,
            sum (md_25) AS M25,
            sum (hi_25) AS H25,
            sum (lo_50) AS L50,
            sum (md_50) AS M50,
            sum (hi_50) AS H50,
            sum (lo_100) AS L100,
            sum (md_100) AS M100,
            sum (hi_100) AS H100,
            sum (int_25) AS int25,
            sum (int_50) AS int50,
            sum (int_100) AS int100
          FROM {table_name}
          WHERE
        	  iso3c = '{iso}' AND
        	  year IN ({years_str}) AND
        	  NOT (month IN ('{start_month_str}') AND year = {min(years)}) AND
      	    NOT (month IN ('{end_month_str}') AND year = {max(years)})
          GROUP BY {grouping_vars}, sid
        ) comb_sub
        
        LEFT JOIN
        
        (
          SELECT
            sid as sid_b,
            shape_id
          FROM cgaz
          WHERE
            iso3c = '{iso}'
        ) cgaz_sub
        
        ON
          comb_sub.sid = cgaz_sub.sid_b
      ) foo
    {p_bottom}"
  )
  
  max_months <- length(years)
  if(period == "month"){
    max_months <- length(years) * 12
  }
  if(period == "quarter"){
    max_months <- length(years) * 4
  }
  if(period == "half"){
    max_months <- length(years) * 2
  }
  
  
  
  data <- dbGetQuery(hdr_db, sql_query)
  
  data$score <- as.numeric(data$score)
  
  risk_pop <- data %>% group_by_at(dplyr_group_vars) %>%
    summarize(n_periods = sum(score > threshold)) %>%
    left_join(cell_pops, by = "sid") %>%
    group_by_at(dplyr_group_vars2) %>%
    summarize(cell_pop = sum(cell_pop)) %>%
    arrange(desc(n_periods)) %>%
    mutate(risk_pop = cumsum(cell_pop))
  
  if(adm1){
    max_periods <- risk_pop %>% group_by(shape_id) %>% summarize(max = max(n_periods, na.rm = TRUE)) %>% filter(max != 0)
    
    out_frame <- max_periods[rep(seq_len(dim(max_periods)[1]), max_periods$max), ] %>%
      group_by(shape_id) %>%
      mutate(n_periods = dplyr::row_number()) %>%
      ungroup() %>%
      left_join(risk_pop, by = c("n_periods", "shape_id")) %>%
      fill(risk_pop, .direction = "up") %>%
      dplyr::select(-max) %>%
      left_join(total_pop, by = "shape_id") %>%
      mutate(risk_pct = risk_pop/total_pop) %>%
      left_join(dplyr::select(st_drop_geometry(adm1_cgaz), shape_id, shape_name), by = "shape_id") %>% dplyr::select(-shape_id)
  }else{
    out_frame <- data.frame(n_periods = 1:max(risk_pop$n_periods)) %>%
      left_join(risk_pop, by = "n_periods") %>%
      fill(risk_pop, .direction = "up") %>%
      mutate(total_pop = total_pop$total_pop,
             risk_pct = risk_pop/total_pop)
  }
  

  disconnect_from_hdr_db(hdr_db)
  return(out_frame)
  
}

long_plot <- function(x, legend_size = 2, font_size = 18){
  #browser()
  x <- left_join(x, adm1_cgaz, by = "shape_id") %>% st_as_sf()
  y <- filter(adm1_cgaz, shape_group %in% x$shape_group)
  my_plot <- ggplot() +
    geom_sf(data = y) +
    geom_sf(data = x, aes(fill = risk_pct)) +
    scale_fill_viridis_c(limits = c(0,1)) +
    theme(legend.key.size = unit(legend_size, 'cm'),
          legend.text = element_text(size = font_size),
          legend.title = element_text(size = font_size))
  
  return(my_plot)
  
}


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

ged_country_fix <- function(ged){
  ged_ssd <- ged %>% filter(country == "Sudan" & (year < 2012)) %>% # until july 2011
    filter(!(year == 2011 & month %in% c("08", "09", "10", "11", "12"))) %>%
    mutate(iso3c = "SSD")
  ged_yugo <- ged %>% filter(country_id == 345) #croatia/slovenia/serbia/nmacedonia/bosnia/
  ged_croatia <- ged_yugo %>% mutate(iso3c = "HRV")
  ged_slovenia <- ged_yugo %>% mutate(iso3c = "SVN")
  ged_serbia <- ged_yugo %>% mutate(iso3c = "SRB")
  ged_montenegro <- ged_yugo %>% mutate(iso3c = "MNE")
  ged_macedonia <- ged_yugo %>% mutate(iso3c = "MKD")
  ged_bosnia <- ged_yugo %>% mutate(iso3c = "BIH")
  ged_kosovo <- ged_yugo %>% mutate(iso3c = "KOS")
  ged_eritrea <- ged %>% filter(country_id == 530 & (year < 2001)) %>% #531 - until Dec 2000
    mutate(iso3c = "ERI")
  ged_namibia <- ged %>% filter(country_id == 560 & (year < 1991)) %>% #angola conflict gwnoa
    filter(!(year == 1990 & month %in% c("04", "05", "06", "07", "08", "09", "10", "11", "12"))) %>%
    mutate(iso3c = "NAM")
  ged_soviets <- ged %>% filter(country_id == 365) #azerbaijan armenia 1990/1991
  ged_azerbaijan <- ged_soviets %>% filter(year %in% c(1989, 1990, 1991)) %>% mutate(iso3c = "AZE")
  ged_azerbaijan2 <- filter(ged, iso3c == "ARM")
  ged_armenia <- ged_soviets %>% filter(year %in% c(1989, 1990, 1991)) %>% mutate(iso3c = "ARM")
  ged_armenia2 <- filter(ged, iso3c == "AZE")
  ged_palestine <- ged %>% filter(country_id == 666) %>% #palestine
    mutate(iso3c = "PSE")
  ged_wsh <- ged %>% filter(country_id == 600) %>%
    mutate(iso3c = "ESH")
  #ged_test1 <- ged %>% filter(country_id == 551)
  
  ged_list <- list(ged_ssd, ged_croatia, ged_slovenia, ged_serbia, ged_montenegro, ged_macedonia, ged_bosnia, ged_kosovo, ged_eritrea, ged_namibia, ged_azerbaijan, ged_armenia, ged_palestine,
                   ged_wsh, ged_azerbaijan2, ged_armenia2)
  ged_fix <- rbindlist(ged_list) %>% st_as_sf()
  ged_out <- bind_rows(ged, ged_fix)
  return(ged_out)
}

non_conflict <- function(ged){
  nc_names <- c("Australia", "Belgium", "France", "Netherlands", "United States", "Austria", "Germany", "United Arab Emirates", "Sweden", "Benin", "Bhutan", "Botswana", "Switzerland")
  nc_codes <- c(900, 211, 220, 210, 2, 305, 260, 696, 380, 434, 760, 571, 225)
  ged_nc <- ged %>% filter(country_id %!in% nc_codes)
  #Spain
  ged_nc <- ged_nc %>% filter(!(country_id == 230 & year > 2016))
  #uk
  ged_nc <- ged_nc %>% filter(!(country_id == 200 & year > 1999))
  #Tunisia
  ged_nc <- ged_nc %>% filter(!(country_id == 616 & year == 2002))
  
  return(ged_nc)
}

worst_global <- function(years, region = "World", monthly = TRUE, adm1 = FALSE, L25_weight = 4, L50_weight = 2, L100_weight = 1,
                      M25_weight = 8, M50_weight = 4, M100_weight = 2, H25_weight = 16, H50_weight = 8, H100_weight = 4,
                      int25_weight = 0, int50_weight = 0, int100_weight = 0, threshold = "", cap = NA, acled = FALSE){
  
  threshold <- sanitize_threshold(threshold)
  
  weights <- list(L25_weight, L50_weight, L100_weight, M25_weight, M50_weight, M100_weight, H25_weight, H50_weight, H100_weight,
                  int25_weight, int50_weight, int100_weight)
  weight_list <- sanitize_weights(weights)
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
  hdr_db <- connect_to_hdr_db()
  
  if(region != "World"){
    region_keys <- dbGetQuery(hdr_db, glue("SELECT * FROM region_key WHERE region = '{region}'"))
  }else{
    region_keys <- dbGetQuery(hdr_db, glue("SELECT * FROM region_key"))
  }
  
  iso_str <- paste(unique(region_keys$iso3c), collapse =  "', '")
  
  table_name <- "comb"
  
  if(adm1){
    tot_group_vars <- "year, shape_id"
    tot_join_vars <- c("year", "shape_id")
    if(monthly){
      grouping_vars <- "year, month, shape_id"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("year", "month", "shape_id")
    }else{
      grouping_vars <- "year, shape_id"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- c("year", "shape_id")
    }
  }else{
    tot_group_vars <- "year"
    tot_join_vars <- c("year")
    if(monthly){
      grouping_vars <- "year, month"
      grouping_vars2 <- "sid, year, month"
      dplyr_group_vars <- c("year", "month")
    }else{
      grouping_vars <- "year"
      grouping_vars2 <- "sid, year"
      dplyr_group_vars <- c("year")
    }
  }
  
  years_str <- paste(years, collapse = ", ")
  
  total_query <- glue(
    "SELECT
    	region,
    	year,
    	total_pop
    FROM
    	region_pops
    WHERE
      region = '{region}' AND
      year IN ({years_str})"
  )
  
  total_pop <- dbGetQuery(hdr_db, total_query)
  
  
  sql_query <- glue(
    "SELECT
      score,
      {grouping_vars},
      cell_pop,
      risk_pop
    FROM
    (
      SELECT
        score,
        {grouping_vars},
        cell_pop,
        sum(cell_pop) OVER (PARTITION BY {grouping_vars} ORDER BY score DESC) AS risk_pop
      FROM
      (
        SELECT
          score,
          {grouping_vars},
          sum(cell_pop) AS cell_pop
        FROM
        (
          SELECT
            sid,
            {grouping_vars},
            cell_pop,
            ((L25 * {L25_weight}) +
            ((L50 - L25) * {L50_weight}) +
            ((L100 - L50) * {L100_weight}) +
            (M25 * {M25_weight}) +
            ((M50 - M25) * {M50_weight}) +
            ((M100 - M50) * {M100_weight}) +
            (H25 * {H25_weight}) +
            ((H50 - H25) * {H50_weight}) +
            ((H100 - H50) * {H100_weight}) +
            (int25 * {int25_weight}) +
            (int50 * {int50_weight}) +
            (int100 * {int100_weight})) AS score
          FROM
            (
              SELECT
                {grouping_vars2},
                avg (cell_pop) as cell_pop,
            	  sum (lo_25) AS L25,
                sum (md_25) AS M25,
                sum (hi_25) AS H25,
                sum (lo_50) AS L50,
                sum (md_50) AS M50,
                sum (hi_50) AS H50,
                sum (lo_100) AS L100,
                sum (md_100) AS M100,
                sum (hi_100) AS H100,
                sum (int_25) AS int25,
                sum (int_50) AS int50,
                sum (int_100) AS int100
              FROM comb
              WHERE
            	  year IN ({years_str}) AND
            	  iso3c IN ('{iso_str}')
              GROUP BY {grouping_vars2}
            ) comb_sub
    	  
        ) foo
        GROUP BY score, {grouping_vars}
      ) comb_agg
    ) bar"
  )
  
  
  
  data <- dbGetQuery(hdr_db, sql_query)
  
  #data$score <- as.numeric(data$score)
  
  # if(region != "World"){
  #   region_keys <- dbGetQuery(hdr_db, glue("SELECT * FROM region_key WHERE region = '{region}'"))
  #   data <- data %>% filter(iso3c %in% region_keys$iso3c)
  # }
  
  
  
  data <- data %>% left_join(total_pop, by = "year") %>%
    mutate(risk_pct = risk_pop/total_pop, score = as.numeric(score))
  
  max_scores <- data %>% group_by_at(dplyr_group_vars) %>% summarize(max = max(score)) %>% filter(max != 0)
  
  out_frame <- max_scores[rep(seq_len(dim(max_scores)[1]), max_scores$max), ] %>%
    group_by_at(dplyr_group_vars) %>%
    mutate(score = dplyr::row_number()) %>%
    ungroup() %>%
    left_join(data, by = c("score", names(max_scores)[-ncol(max_scores)])) %>%
    # fill(risk_pop, .direction = "up") %>%
    # group_by(year, score) %>%
    # summarize(risk_pop = sum(risk_pop)) %>%
    # left_join(total_pop, by = "year") %>%
    # mutate(risk_pct = risk_pop/total_pop) %>%
    fill(risk_pop, risk_pct, total_pop, .direction = "up") %>%
    dplyr::select(-max)
  
  if(!is.na(cap)){
    out_frame <- out_frame %>% filter(score <= cap)
  }
  
  if(threshold != ""){
    threshold <- as.numeric(strsplit(threshold, split = ",")[[1]])
    out_frame <- out_frame %>% filter(score %in% threshold)
  }
  
  if(adm1){
    out_frame <- left_join(out_frame, dplyr::select(adm1_cgaz, shape_id, shape_name), by = "shape_id")
  }
  
  disconnect_from_hdr_db(hdr_db)
  return(out_frame)
  
}


connect_to_hdr_db <- function(){
  #hdr_db <- dbConnect(RSQLite::SQLite(), "C:/Users/andara/Documents/hdr_db_v2(2).sqlite")
  #hdr_db <- dbConnect(drv = RPostgres::Postgres(), host = post_host, dbname = "HDR", user = post_user, password = post_pass, port = post_port)
  hdr_db <- dbConnect(drv = RPostgres::Postgres(), host = aws_host, dbname = "postgres", user = aws_user, password = aws_pass, port = aws_port)
  return(hdr_db)
}
disconnect_from_hdr_db <- function(x){
  dbDisconnect(x)
}



sanitize_weights <- function(x){
  
  weight_list <- lapply(x, function(x){if(is.na(x)){return(0)}else{return(x)}})
  return(weight_list)
  
}

sanitize_threshold <- function(x){
  x <- gsub("\\s*", "", x)
  return(x)
}

plot_global_buffers <- function(){
  
}