


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

demos <- bind_rows(demos_est, demos_proj) %>%
  dplyr::select(iso3n, year, un_total, child_pct)


un_isos <- unique(demos$iso3n)
nid_isos <- unique(nid_grid$ISOCODE) %>% ison()
missing_isos <- nid_isos[nid_isos %!in% un_isos] %>% isoc()


children_in_conflict <- function(){
  
  country_tots <- dbGetQuery(capa_db, "SELECT * FROM country_pops")
  
  conf_lo <- get_standard_aggregation(iso = "World", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = weights, threshold = 1) %>%
    rename(risk_pop_low = risk_pop,
           risk_pct_low = risk_pct)
  conf_md <- get_standard_aggregation(iso = "World", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = weights, threshold = 25) %>%
    dplyr::select(-total_pop) %>%
    rename(risk_pop_mid = risk_pop,
           risk_pct_mid = risk_pct)
  conf_hi <- get_standard_aggregation(iso = "World", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = weights, threshold = 100) %>%
    dplyr::select(-total_pop) %>%
    rename(risk_pop_high = risk_pop,
           risk_pct_high = risk_pct)
  conf_ex <- get_standard_aggregation(iso = "World", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = weights, threshold = 1000) %>%
    dplyr::select(-total_pop) %>%
    rename(risk_pop_extreme = risk_pop,
           risk_pct_extreme = risk_pct)
  
  conf <- conf_lo %>%
    left_join(conf_md, by = c("iso3n", "year")) %>%
    left_join(conf_hi, by = c("iso3n", "year")) %>%
    left_join(conf_ex, by = c("iso3n", "year")) %>%
    rows_update(country_tots, by = c("iso3n", "year")) %>%
    left_join(demos, by = c("iso3n", "year")) %>%
    mutate(total_children = round(total_pop * child_pct)) %>%
    mutate(risk_children_low = round(risk_pop_low * child_pct),
           risk_children_mid = round(risk_pop_mid * child_pct),
           risk_children_high = round(risk_pop_high * child_pct),
           risk_children_extreme = round(risk_pop_extreme * child_pct)) %>%
    mutate(risk_children_low_pct = round(risk_children_low / total_children, 4),
           risk_children_mid_pct = round(risk_children_mid / total_children, 4),
           risk_children_high_pct = round(risk_children_high / total_children, 4),
           risk_children_extreme_pct = round(risk_children_extreme / total_children, 4)) %>%
    mutate(iso3c = isoc(iso3n))
  
}

x <- dbGetQuery(capa_db, "SELECT * FROM region_key")
region1 <- x[!duplicated(x$iso3n), ]

region2 <- x[x$region %!in% region1$region, ]
region2 <- region2[!duplicated(region2$iso3n), ]

region3 <- x[x$region %!in% region1$region, ]
region3 <- region3[region3$region %!in% region2$region, ]

region1 <- region1 %>% rename(region1 = region)
region2 <- region2 %>% rename(region2 = region)
region3 <- region3 %>% rename(region3 = region)


conf_out <- conf %>% left_join(region1, by = "iso3n") %>% left_join(region2, by = "iso3n") %>% left_join(region3, by = "iso3n")



