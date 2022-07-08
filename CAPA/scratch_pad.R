library(raster)

idn_comb <- idn_comb %>% rename(lo_25 = L25, lo_50 = L50, lo_100 = L100, md_25 = M25, md_50 = M50, md_100 = M100, hi_25 = H25, hi_50 = H50, hi_100 = H100, int_25 = int25, int_50 = int50, int_100 = int100)

all_cells <- dbGetQuery(hdr_db, "SELECT * FROM cells")
all_cells <- all_cells %>% dplyr::select(-geometry)
all_cells <- reshape2::melt(all_cells, id.vars = c(1:4), variable.name = "year", value.name = "cell_pop")
all_cells$year <- as.numeric(substr(all_cells$year, 5, 8))

dbWriteTable(hdr_db, "cell_pops", all_cells)

all_cells <- st_read(hdr_db_old, query = "SELECT * FROM cells")
all_cells <- dplyr::select(all_cells, -contains("ipop"))
st_write(all_cells, hdr_db_post, "cells")

dbWriteTable(hdr_db_post, "cgaz", cgaz_data)

cell_pops <- dbGetQuery(hdr_db, "SELECT * FROM cell_pops")
dbWriteTable(hdr_db_post, "cell_pops", cell_pops)

comb <- dbGetQuery(hdr_db, "SELECT * FROM comb")
dbWriteTable(hdr_db_post, "comb", comb)

region_pops <- dbGetQuery(hdr_db, "SELECT * FROM region_pops")
dbWriteTable(hdr_db_post, "region_pops", region_pops)

region_key <- dbGetQuery(hdr_db, "SELECT * FROM region_key")
dbWriteTable(hdr_db_post, "region_key", region_key)

grid <- st_read(hdr_db_old, query = "SELECT * FROM grid")
#dbWriteTable(hdr_db_post, "grid", grid)
st_write(grid, hdr_db_post, "grid")


library(RSQLite)
library(tidyverse)
library(data.table)
adm2 <- readRDS("C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/adm2.RDS")
path <- "C:/my/local/path/to/database/hdr_db.sqlite"
hdr_db <- dbConnect(RSQLite::SQLite(), "C:/Users/andara/Documents/hdr_db_v2(1).sqlite")
hdr_db_2 <- dbConnect(RSQLite::SQLite(), "C:/Users/andara/Documents/hdr_db_v2(2).sqlite")
hdr_db_3 <- dbConnect(RSQLite::SQLite(), paste0(drop_path, "hdr_db_new.sqlite"))
capa_db <- dbConnect(drv = RPostgres::Postgres(), host = prio_host, dbname = "capa", user = prio_user, password = prio_pass, port = prio_port)

############################
#     YEARLY ESTIMATES     #
############################
risk_100_year <- dbGetQuery(hdr_db, "SELECT iso3c, year, sum(cell_pop) AS risk_pop_100 FROM (SELECT iso3c, cid, year, cell_pop, sum (int_100) AS score FROM comb GROUP BY iso3c, year, cid, cell_pop) WHERE score > 0 GROUP BY iso3c, year")
risk_50_year <- dbGetQuery(hdr_db, "SELECT iso3c, year, sum(cell_pop) AS risk_pop_50 FROM (SELECT iso3c, cid, year, cell_pop, sum (int_50) AS score FROM comb GROUP BY iso3c, year, cid, cell_pop) WHERE score > 0 GROUP BY iso3c, year")
system.time({risk_25_year <- dbGetQuery(hdr_db, "SELECT iso3c, year, sum(cell_pop) AS risk_pop_25 FROM (SELECT iso3c, cid, year, cell_pop, sum (int_25) AS score FROM comb GROUP BY iso3c, year, cid, cell_pop) WHERE score > 0 GROUP BY iso3c, year")})
system.time({risk_25_year_2 <- dbGetQuery(hdr_db_2, "SELECT iso3n, year, sum(cell_pop) AS risk_pop_25 FROM (SELECT iso3n, cid, year, cell_pop, sum (int_25) AS score FROM comb GROUP BY iso3n, year, cid, cell_pop) WHERE score > 0 GROUP BY iso3n, year")})

total_pop <- dbGetQuery(hdr_db, "SELECT iso3c, year, sum(cell_pop) AS total_pop FROM (SELECT iso3c, year, cell_pop FROM cell_pops) GROUP BY iso3c, year")
conflict_table_yearly <- total_pop %>% left_join(risk_100_year, by = c("iso3c", "year")) %>% left_join(risk_50_year, by = c("iso3c", "year")) %>% left_join(risk_25_year, by = c("iso3c", "year"))
conflict_table_yearly <- conflict_table_yearly %>%
  mutate(pct_risk_25 = risk_pop_25/total_pop,
         pct_risk_50 = risk_pop_50/total_pop,
         pct_risk_100 = risk_pop_100/total_pop)
conflict_table_yearly[, 3:9] <- nafill(conflict_table_yearly[, 3:9], fill = 0)


#############################
#     MONTHLY ESTIMATES     #
#############################
risk_100_month <- dbGetQuery(hdr_db, "SELECT iso3c, year, month, sum(cell_pop) AS risk_pop_100 FROM (SELECT iso3c, cid, year, month, cell_pop, sum (int_100) AS score FROM comb GROUP BY iso3c, year, month, cid, cell_pop) WHERE score > 0 GROUP BY iso3c, year, month")
risk_50_month <- dbGetQuery(hdr_db, "SELECT iso3c, year, month, sum(cell_pop) AS risk_pop_50 FROM (SELECT iso3c, cid, year, month, cell_pop, sum (int_50) AS score FROM comb GROUP BY iso3c, year, month, cid, cell_pop) WHERE score > 0 GROUP BY iso3c, year, month")
system.time({risk_25_month <- dbGetQuery(hdr_db, "SELECT iso3c, year, month, sum(cell_pop) AS risk_pop_25 FROM (SELECT iso3c, cid, year, month, cell_pop, sum (int_25) AS score FROM comb GROUP BY iso3c, year, month, cid, cell_pop) WHERE score > 0 GROUP BY iso3c, year, month")})
system.time({risk_25_month_2 <- dbGetQuery(hdr_db_2, "SELECT iso3n, year, month, sum(cell_pop) AS risk_pop_25 FROM (SELECT iso3n, cid, year, month, cell_pop, sum (int_50) AS score FROM comb WHERE iso3n = 760 GROUP BY iso3n, year, month, cid, cell_pop) WHERE score > 0 GROUP BY iso3n, year, month")})
system.time({risk_25_month <- dbGetQuery(capa_db, glue("SELECT 
iso3n, year, sum(cell_pop) AS risk_pop_25
FROM 
(
SELECT 
iso3n, year, month, cell_pop, capa_id_adm1,
((lo_25 * {L25_weight}) +
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
        ((int_100 - int_50) * {int100_weight})) AS score 
FROM cell_stats
) foo
WHERE score > 0
GROUP BY iso3n, year"))})

dbExecute(capa_db, "SET enable_seqscan = OFF")
dbExecute(capa_db, "SET enable_seqscan = ON")

system.time({x <- st_read(capa_db, query = "SELECT * FROM cell_geos WHERE iso3n = 760")})
x <- dbGetQuery(capa_db, "EXPLAIN ANALYZE SELECT * FROM cell_geos WHERE iso3n = 760")

system.time({risk_25_month <- dbGetQuery(capa_db, glue("SELECT 
sid, sum(score) as score
FROM 
(
SELECT 
sid, year,
((lo_25 * {L25_weight}) +
                ((lo_50 - lo_25) * {L50_weight}) +
                ((lo_100 - lo_50) * {L100_weight}) +
                (md_25 * {M25_weight}) +
                ((md_50 - md_25) * {M50_weight}) +
                ((md_100 - md_50) * {M100_weight}) +
                (hi_25 * {H25_weight}) +
                ((hi_50 - hi_25) * {H50_weight}) +
                ((hi_100 - hi_50) * {H100_weight})) AS score 
FROM cell_stats
WHERE iso3n = 760
) foo
WHERE score > 0 AND year = 2015
GROUP BY sid"))})

y <- left_join(risk_25_month, x, by = "sid") %>% st_as_sf()
y$score <- as.integer.integer64(y$score)
plot((filter(adm1_cgaz, shape_group == "UKR")$geometry))
plot(y['score'], add = T)
ggplot() +
  geom_sf(data = y, aes(fill = score), color = NA) +
  geom_sf(data = filter(adm1_cgaz, shape_group == "SYR"), fill = NA) +
  scale_fill_viridis_c()

total_pop <- dbGetQuery(hdr_db, "SELECT iso3c, year, sum(cell_pop) AS total_pop FROM (SELECT iso3c, year, cell_pop FROM cell_pops) GROUP BY iso3c, year") %>%
  as.data.table()
months <- str_pad(as.character(1:12), "2", "left", pad = "0")
total_pop_months <- total_pop[rep(1:.N, 12)][, month := months, by = list(iso3c, year)]
setorder(total_pop_months, iso3c, year, month)


conflict_table_monthly <- total_pop_months %>% left_join(risk_100_month, by = c("iso3c", "year", "month")) %>%
  left_join(risk_50_month, by = c("iso3c", "year", "month")) %>%
  left_join(risk_25_month, by = c("iso3c", "year", "month"))

conflict_table_monthly <- conflict_table_monthly %>%
  mutate(pct_risk_25 = risk_pop_25/total_pop,
         pct_risk_50 = risk_pop_50/total_pop,
         pct_risk_100 = risk_pop_100/total_pop)

conflict_table_monthly[, 5:10] <- nafill(conflict_table_monthly[, 5:10], fill = 0)


unique_countries <- unique(x$iso3c)
unique_iso3n <- countrycode(unique_countries, origin = "iso3c", destination = "iso3n")
unique_iso3n[107] <- 999
iso_switch <- data.frame(iso3c = unique_countries, iso3n = unique_iso3n)
new_data <- left_join(x, iso_switch, by = "iso3c")
new_data$month <- as.numeric(new_data$month)
dbWriteTable(hdr_db, "comb", new_data, overwrite = T)



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
      grouping_vars <- "iso3n, year, month"
      grouping_vars2 <- "iso3n, cid, year, month"
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
            iso3n,
            {grouping_vars},
            sum (cell_pop) AS cell_pop,
            score
          FROM
      	  (
            (
              SELECT
                {grouping_vars2},
                sid,
                cell_pop,
            	  ((lo_25 * {L25_weight}) +
                ((lo_50 - lo_25) * {L50_weight}) +
                ((lo_100 - lo_50) * {L100_weight}) +
                (md_25 * {M25_weight}) +
                ((md_50 - md_25) * {M50_weight}) +
                ((md_100 - md_50) * {M100_weight}) +
                (hi_25 * {H25_weight}) +
                ((hi_50 - hi_25) * {H50_weight}) +
                ((hi_100 - hi_50) * {H100_weight})) AS score
              FROM comb
              WHERE
            	  iso3n = {iso} AND
            	  year > {min(years)-1} AND
            	  year < {max(years)+1}
            ) comb_sub
      	  
      	    LEFT JOIN
      
          	(
          		SELECT
          			sid as sid_b,
          			shape_id
          		FROM cgaz
          		WHERE
          			iso3n = 'SYR'
          	) cgaz_sub
          	
          	ON
          		comb_sub.sid = cgaz_sub.sid_b
        	)
    	    GROUP BY {grouping_vars}, score
        ) foo
        
      
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



+
  (int_25 * {int25_weight}) +
  ((int_50 - int_25) * {int50_weight}) +
  ((int_100 - int_50) * {int100_weight})



system.time({risk_25_month <- dbGetQuery(hdr_db_2, glue(
"SELECT
iso3n, year, month, sum(cell_pop) AS risk_pop_25
FROM
(
SELECT 
iso3n, year, month, cell_pop,
((lo_25 * {L25_weight}) +
((lo_50 - lo_25) * {L50_weight}) +
((lo_100 - lo_50) * {L100_weight}) +
(md_25 * {M25_weight}) +
((md_50 - md_25) * {M50_weight}) +
((md_100 - md_50) * {M100_weight}) +
(hi_25 * {H25_weight}) +
((hi_50 - hi_25) * {H50_weight}) +
((hi_100 - hi_50) * {H100_weight})) AS score
FROM 
(
SELECT 
iso3n, cid, year, month, cell_pop,
lo_25, lo_50, lo_100,
md_25, md_50, md_100,
hi_25, hi_50, hi_100
FROM comb 
WHERE iso3n in (760)
)
)
WHERE score > 0 
GROUP BY iso3n, year, month"))})


system.time({risk_25_month <- dbGetQuery(hdr_db_2, glue("SELECT 
iso3n, year, month, sum(cell_pop) AS risk_pop_25, shape_id 
FROM 
(
SELECT 
iso3n, sid, cid, year, month, cell_pop,
((lo_25 * {L25_weight}) +
                ((lo_50 - lo_25) * {L50_weight}) +
                ((lo_100 - lo_50) * {L100_weight}) +
                (md_25 * {M25_weight}) +
                ((md_50 - md_25) * {M50_weight}) +
                ((md_100 - md_50) * {M100_weight}) +
                (hi_25 * {H25_weight}) +
                ((hi_50 - hi_25) * {H50_weight}) +
                ((hi_100 - hi_50) * {H100_weight})) AS score 
FROM comb
) comb_sub
LEFT JOIN
      
          	(
          		SELECT
          			sid as sid_b,
          			shape_id
          		FROM cgaz
          		WHERE
          			iso3c = 'AFG'
          	) cgaz_sub
          	
          	ON
          		comb_sub.sid = cgaz_sub.sid_b
WHERE iso3n = 004 AND score > 0
GROUP BY iso3n, year, month, shape_id"))})

system.time({x <- dbGetQuery(hdr_db_2, glue("SELECT * FROM comb WHERE iso3n = 760"))})
dbSendStatement(hdr_db_2, "ANALYZE comb")


L25_weight <- 1
L50_weight <- 1
L100_weight <- 1
M25_weight <- 1
M50_weight <- 1
M100_weight <- 1
H25_weight <- 1
H50_weight <- 1
H100_weight <- 1
int25_weight <- 1
int50_weight <- 1
int100_weight <- 1


system.time({risk_25_month <- dbGetQuery(hdr_db_2, glue("
SELECT 
iso3n, year, month, sum(cell_pop) AS risk_pop_25, shape_id 
FROM 
  (
  SELECT 
  iso3n, sid, cid, year, month, cell_pop, shape_id,
  ((lo_25 * {L25_weight}) +
  ((lo_50 - lo_25) * {L50_weight}) +
  ((lo_100 - lo_50) * {L100_weight}) +
  (md_25 * {M25_weight}) +
  ((md_50 - md_25) * {M50_weight}) +
  ((md_100 - md_50) * {M100_weight}) +
  (hi_25 * {H25_weight}) +
  ((hi_50 - hi_25) * {H50_weight}) +
  ((hi_100 - hi_50) * {H100_weight})) AS score 
  FROM
    (
    SELECT * FROM comb
    LEFT JOIN
      (SELECT sid as sid_b, shape_id FROM cgaz) cgaz_sub
    ON
      comb.sid = cgaz_sub.sid_b
    )
  )
WHERE iso3n = 004 AND score > 0
GROUP BY iso3n, year, month, shape_id"))})



#SQL TUNING
#optimal yma
"SELECT
    iso3n, year, month, capa_id_adm1, SUM(cell_pop) AS cell_pop
  FROM 
    (
    SELECT
      iso3n, year, month, capa_id_adm1,
      cell_pop,
      ((lo_25 * 1) +
      ((lo_50 - lo_25) * 1) +
      ((lo_100 - lo_50) * 1) +
      (md_25 * 1) +
      ((md_50 - md_25) * 1) +
      ((md_100 - md_50) * 1) +
      (hi_25 * 1) +
      ((hi_50 - hi_25) * 1) +
      ((hi_100 - hi_50) * 1) +
      (int_25 * 0) +
      ((int_50 - int_25) * 0) +
      ((int_100 - int_50) * 0)) AS score 
    FROM cell_stats
    WHERE iso3n = 4 AND
      year >= 2011 AND
      year <= 2015
    ) agg
  WHERE score >= 1
  GROUP BY iso3n, year, month, capa_id_adm1"

#optimal ya
" SELECT
    iso3n, year, month, capa_id_adm1, SUM(cell_pop) AS cell_pop
  FROM 
    (
    SELECT 
      iso3n, year, month, capa_id_adm1,
      AVG(cell_pop) as cell_pop,
      SUM((lo_25 * 1) +
      ((lo_50 - lo_25) * 1) +
      ((lo_100 - lo_50) * 1) +
      (md_25 * 1) +
      ((md_50 - md_25) * 1) +
      ((md_100 - md_50) * 1) +
      (hi_25 * 1) +
      ((hi_50 - hi_25) * 1) +
      ((hi_100 - hi_50) * 1) +
      (int_25 * 0) +
      ((int_50 - int_25) * 0) +
      ((int_100 - int_50) * 0)) AS score 
    FROM cell_stats
    WHERE iso3n = 4 AND
      year >= 2011 AND
      year <= 2015
    GROUP BY iso3n, sid, year, month, capa_id_adm1
    ) agg
  WHERE score >= 1
  GROUP BY iso3n, year, month, capa_id_adm1"

#optimal ym
" SELECT
    iso3n, year, month, SUM(cell_pop) AS cell_pop
  FROM 
    (
    SELECT
      iso3n, year, month,
      cell_pop,
      ((lo_25 * 1) +
      ((lo_50 - lo_25) * 1) +
      ((lo_100 - lo_50) * 1) +
      (md_25 * 1) +
      ((md_50 - md_25) * 1) +
      ((md_100 - md_50) * 1) +
      (hi_25 * 1) +
      ((hi_50 - hi_25) * 1) +
      ((hi_100 - hi_50) * 1) +
      (int_25 * 0) +
      ((int_50 - int_25) * 0) +
      ((int_100 - int_50) * 0)) AS score 
    FROM cell_stats
    WHERE iso3n = 4 AND
      year >= 2011 AND
      year <= 2015
    ) agg
  WHERE score >= 1
  GROUP BY iso3n, year, month"

#optimal y
#depends...DISTINCT ON approach is best for countries with not that much conflict - also faster on global...
#new cell_stats_yearly table?
# agg to cell_stats_yr
"SELECT
    sid, iso3n, capa_id_adm1, year, SUM(cell_pop) AS cell_pop,
    MAX(lo_25) as lo_25,
    MAX(lo_50) as lo_50,
    MAX(lo_100) as lo_100,
    MAX(md_25) as md_25,
    MAX(md_50) as md_50,
    MAX(md_100) as md_100,
    MAX(hi_25) as hi_25,
    MAX(hi_50) as hi_50,
    MAX(hi_100) as hi_100,
    MAX(int_25) as int_25,
    MAX(int_50) as int_50,
    MAX(int_100) as int_100
  FROM 
    (
    SELECT 
      sid, iso3n, capa_id_adm1, year,
      MAX(cell_pop) as cell_pop,
      SUM(lo_25) as lo_25,
      SUM(lo_50) as lo_50,
      SUM(lo_100) as lo_100,
      SUM(md_25) as md_25,
      SUM(md_50) as md_50,
      SUM(md_100) as md_100,
      SUM(hi_25) as hi_25,
      SUM(hi_50) as hi_50,
      SUM(hi_100) as hi_100,
      SUM(int_25) as int_25,
      SUM(int_50) as int_50,
      SUM(int_100) as int_100 
    FROM cell_stats
    GROUP BY iso3n, capa_id_adm1, year, sid
    ) agg
  GROUP BY iso3n, capa_id_adm1, year, sid"
