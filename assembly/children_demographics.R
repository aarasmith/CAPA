


demos_est <- readxl::read_excel(paste0(drop_path, "WPP2019_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx"), skip = 16)
names(demos_est)[9:29] <- paste("ages", gsub("-", "_", names(demos_est)[9:29]), sep = "_")
demos_est[, 9:29] <- sapply(demos_est[, 9:29], as.numeric)
demos_est[, 9:29] <- demos_est[, 9:29] * 1000
demos_est <- demos_est %>% mutate(un_total = rowSums(.[9:29]))
names(demos_est)[8] <- "year"
names(demos_est)[5] <- "iso3n"
demos_est <- demos_est %>% mutate(ages_15_18 = ages_15_19 * (4/5)) %>%
  mutate(child_pop = ages_0_4 + ages_5_9 + ages_10_14 + ages_15_18) %>%
  mutate(child_pct = child_pop/un_total)


demos_proj <- readxl::read_excel(paste0(drop_path, "WPP2019_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx"), sheet = "MEDIUM VARIANT", skip = 16)
names(demos_proj)[9:29] <- paste("ages", gsub("-", "_", names(demos_proj)[9:29]), sep = "_")
demos_proj[, 9:29] <- sapply(demos_proj[, 9:29], as.numeric)
demos_proj[, 9:29] <- demos_proj[, 9:29] * 1000
demos_proj <- demos_proj %>% mutate(un_total = rowSums(.[9:29]))
names(demos_proj)[8] <- "year"
names(demos_proj)[5] <- "iso3n"
demos_proj <- demos_proj %>% mutate(ages_15_18 = ages_15_19 * (4/5)) %>%
  mutate(child_pop = ages_0_4 + ages_5_9 + ages_10_14 + ages_15_18) %>%
  mutate(child_pct = child_pop/un_total) %>%
  filter(year != 2020)

un_demos <- bind_rows(demos_est, demos_proj) %>%
  dplyr::select(iso3n, year, un_total, child_pct)


un_isos <- unique(demos$iso3n)
nid_isos <- unique(nid_grid$ISOCODE) %>% ison()
missing_isos <- nid_isos[nid_isos %!in% un_isos] %>% isoc()


children_in_conflict <- function(iso3c, years, period, adm1, weights, threshold = 1, score_selection = c(1, 25, 100, 1000), cat_names = c("low", "medium", "high", "extreme")){
  #browser()
  capa_db <- connect_to_capa()
  
  #get totals to update country totals for countries not in the conflict table
  country_tots <- dbGetQuery(capa_db, "SELECT * FROM country_pops")
  
  conf_long <- get_score_aggregation(iso = iso3c, years = years, period = period, adm1 = adm1, weights = weights, threshold = threshold, score_selection = score_selection)
  
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
    mutate(total_children = round(total_pop * child_pct)) %>%
    mutate(risk_children_low = round(risk_pop_low * child_pct),
           risk_children_medium = round(risk_pop_medium * child_pct),
           risk_children_high = round(risk_pop_high * child_pct),
           risk_children_extreme = round(risk_pop_extreme * child_pct)) %>%
    mutate(risk_children_low_pct = round(risk_children_low / total_children, 4),
           risk_children_medium_pct = round(risk_children_medium / total_children, 4),
           risk_children_high_pct = round(risk_children_high / total_children, 4),
           risk_children_extreme_pct = round(risk_children_extreme / total_children, 4)) %>%
    mutate(iso3c = isoc(iso3n)) %>%
    arrange(iso3c, year) %>%
    dplyr::select(iso3c, iso3n, year, total_pop, un_total, child_pct, total_children, everything())
  
  
  #attach region names
  region_key <- dbGetQuery(capa_db, "SELECT * FROM region_key")
  region1 <- region_key[!duplicated(region_key$iso3n), ]
  
  region2 <- region_key[region_key$region %!in% region1$region, ]
  region2 <- region2[!duplicated(region2$iso3n), ]
  
  region3 <- region_key[region_key$region %!in% region1$region, ]
  region3 <- region3[region3$region %!in% region2$region, ]
  
  region1 <- region1 %>% rename(region1 = region)
  region2 <- region2 %>% rename(region2 = region)
  region3 <- region3 %>% rename(region3 = region)
  
  conf_out <- conf %>% left_join(region1, by = "iso3n") %>% left_join(region2, by = "iso3n") %>% left_join(region3, by = "iso3n")
  
  disconnect_from_capa(capa_db)
  
  return(conf_out)
  
}


x <- children_in_conflict("World", 1990:2021, "yearly", FALSE, weights, score_selection = c(1, 10, 25, 100, 1000), cat_names = c("low", "low_mid", "medium", "high", "extreme"))


