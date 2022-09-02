


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

un_demos_breakdown <- bind_rows(demos_est, demos_proj) %>%
  dplyr::select(iso3n, year, un_total, child_pct, ages_0_4:ages_10_14, ages_15_18) %>%
  mutate(across(contains("ages"), function(x) x/un_total))


un_isos <- unique(demos$iso3n)
nid_isos <- unique(nid_grid$ISOCODE) %>% ison()
missing_isos <- nid_isos[nid_isos %!in% un_isos] %>% isoc()


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
  
  if(level != "Country"){
    conf_out <- agg_car(conf_out, level = level)
  }
  
  conf_out <- conf_out %>% dplyr::select(-matches("region\\d"), -contains("risk_pop"), -contains("risk_pct")) %>% arrange(country)
  
  
  return(conf_out)
  
}


x1 <- children_in_conflict(iso3c = "World", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = weights,
                          score_selection = c(1, 25, 100, 1000), cat_names = c("low", "medium", "high", "extreme"), exclusive = T)
x2 <- children_in_conflict(iso3c = "World", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = weights,
                          score_selection = c(1, 25, 100, 1000), cat_names = c("low", "medium", "high", "extreme"), exclusive = T, level = "Region")
x3 <- children_in_conflict(iso3c = "World", years = 1990:2021, period = "yearly", adm1 = FALSE, weights = weights,
                          score_selection = c(1, 25, 100, 1000), cat_names = c("low", "medium", "high", "extreme"), exclusive = T, level = "world")
x1 <- children_in_conflict("AFG", 1990:2021, "yearly", FALSE, weights, score_selection = c(1, 10, 25, 100, 1000), cat_names = c("low", "low_mid", "medium", "high", "extreme"))

library(readxl)
x1 <- read_xlsx("C:\\Users\\andara\\Downloads\\CAR_data_siri2.xlsx")
x1 <- x %>% group_by(year) %>%
  summarize(total_pop = sum(total_pop, na.rm = T),
            un_total = sum(un_total, na.rm = T),
            total_children = sum(total_children, na.rm = T),
            tot_risk_kids = sum(risk_children_low + risk_children_medium + risk_children_high + risk_children_extreme, na.rm = T)) %>%
  mutate(risk_kids_pct = tot_risk_kids/total_children)

ged25 <- readRDS(paste0(drop_path, "ged25_22.RDS"))
x <- ged25 %>% st_drop_geometry() %>% group_by(iso3c, year) %>% summarize(battle_deaths = sum(best, na.rm = T))

x <- read_xlsx(paste0(drop_path, "car_data_2022_08_10.xlsx"))
x$country <- countrycode(x$iso3c, origin = "iso3c", destination = "country.name")
x$country[is.na(x$country)] <- "Kosovo"
x <- x %>% dplyr::select(country, everything())


world_car <- x %>%
  group_by(year) %>%
  summarize(total_pop = sum(total_pop, na.rm = T),
            un_total = sum(un_total, na.rm = T),
            total_children = sum(total_children, na.rm = T),
            risk_children_low = sum(risk_children_low, na.rm = T),
            risk_children_medium = sum(risk_children_medium, na.rm = T),
            risk_children_high = sum(risk_children_high, na.rm = T),
            risk_children_extreme = sum(risk_children_extreme, na.rm = T),
            risk_pop_low = sum(risk_pop_low, na.rm = T),
            risk_pop_medium = sum(risk_pop_medium, na.rm = T),
            risk_pop_high = sum(risk_pop_high, na.rm = T),
            risk_pop_extreme = sum(risk_pop_extreme, na.rm = T),
            battle_deaths = sum(battle_deaths, na.rm = T)) %>%
  mutate(risk_children_low_pct = risk_children_low/total_children,
         risk_children_medium_pct = risk_children_medium/total_children,
         risk_children_high_pct = risk_children_high/total_children,
         risk_children_extreme_pct = risk_children_extreme/total_children,
         risk_pop_low_pct = risk_pop_low/total_pop,
         risk_pop_medium_pct = risk_pop_medium/total_pop,
         risk_pop_high_pct = risk_pop_high/total_pop,
         risk_pop_extreme_pct = risk_pop_extreme/total_pop)

region1_car <- x %>% dplyr::select(-region2, -region3) %>% rename(region = region1) %>% filter(!is.na(region))
region2_car <- x %>% dplyr::select(-region1, -region3) %>% rename(region = region2) %>% filter(!is.na(region))
region3_car <- x %>% dplyr::select(-region1, -region2) %>% rename(region = region3) %>% filter(!is.na(region))
region_car <- bind_rows(region1_car, region2_car, region3_car)

region_car <- region_car %>%
  group_by(region, year) %>%
  summarize(total_pop = sum(total_pop, na.rm = T),
            un_total = sum(un_total, na.rm = T),
            total_children = sum(total_children, na.rm = T),
            risk_children_low = sum(risk_children_low, na.rm = T),
            risk_children_medium = sum(risk_children_medium, na.rm = T),
            risk_children_high = sum(risk_children_high, na.rm = T),
            risk_children_extreme = sum(risk_children_extreme, na.rm = T),
            risk_pop_low = sum(risk_pop_low, na.rm = T),
            risk_pop_medium = sum(risk_pop_medium, na.rm = T),
            risk_pop_high = sum(risk_pop_high, na.rm = T),
            risk_pop_extreme = sum(risk_pop_extreme, na.rm = T),
            battle_deaths = sum(battle_deaths, na.rm = T)) %>%
  mutate(risk_children_low_pct = risk_children_low/total_children,
         risk_children_medium_pct = risk_children_medium/total_children,
         risk_children_high_pct = risk_children_high/total_children,
         risk_children_extreme_pct = risk_children_extreme/total_children,
         risk_pop_low_pct = risk_pop_low/total_pop,
         risk_pop_medium_pct = risk_pop_medium/total_pop,
         risk_pop_high_pct = risk_pop_high/total_pop,
         risk_pop_extreme_pct = risk_pop_extreme/total_pop)

car_data <- list(Country = x, Region = region_car, World = world_car)
write_xlsx(car_data, path = paste0(drop_path, "CAR_data_2022_08_15.xlsx"))

agg_car <- function(car_data, level){
  
  if(level == "Region"){
    gv <- c("region", "year")
    region1_car <- car_data %>% dplyr::select(-region2, -region3) %>% rename(region = region1) %>% filter(!is.na(region))
    region2_car <- car_data %>% dplyr::select(-region1, -region3) %>% rename(region = region2) %>% filter(!is.na(region))
    region3_car <- car_data %>% dplyr::select(-region1, -region2) %>% rename(region = region3) %>% filter(!is.na(region))
    car_data <- bind_rows(region1_car, region2_car, region3_car)
  }else if(level == "world"){
    gv <- c("year")
  }
  
  car_data <- car_data %>%
    group_by_at(gv) %>%
    summarize(across((contains("children") & !contains("share")) | contains("total") | contains("battle_deaths"), sum, na.rm = T)) %>%
    mutate(across(contains("risk_children"), ~ round(.x / total_children, 4), .names = "{.col}_share"))
  
  return(car_data)
  
}

x2 <- agg_car(x1, level = "Region")

#new_regions is created in manual_regions.R
my_weights <- list(L25 = 1, L50 = 1, L100 = 0, M25 = 1, M50 = 1, M100 = 0, H25 = 1, H50 = 1, H100 = 0,
                   int25 = 0, int50 = 0, int100 = 0)
x <- get_standard_aggregation("World", 1990:2021, "yearly", FALSE, my_weights)
x1 <- x %>% left_join(un_demos_breakdown, by = c("iso3n", "year")) %>%
  mutate(total_children = round(child_pct * total_pop)) %>%
  mutate(across(contains(c("child_pct", "age")), function(x) round(x*risk_pop))) %>%
  rename(children_at_risk = child_pct) %>%
  left_join(new_regions, by = "iso3n")

age_group_tables <- list()
age_group_tables[["Country"]] <- x1 %>%
  mutate(
    across(
      contains(c("children_at_risk", "age")),
      function(x) round(x/total_children),
      .names = "{.col}_pct"
    )
  )
age_group_tables[["Country"]] <- x1 %>%
  mutate(iso3c = isoc(iso3n)) %>%
  dplyr::select(iso3c, iso3n, region, year, everything())
age_group_tables[["GWNO_regions"]] <- x1 %>%
  filter(!is.na(region)) %>%
  group_by(region, year) %>%
  summarize(across(risk_pop:total_children, function(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(risk_pct = round(risk_pop/total_pop, 4))
age_group_tables[["GWNO_all"]] <- x1 %>%
  filter(!is.na(region)) %>%
  group_by(year) %>%
  summarize(across(risk_pop:total_children, function(x) sum(x, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(risk_pct = round(risk_pop/total_pop, 4))

#write_xlsx(age_group_tables, paste0(drop_path, "gwno_CAR.xlsx"))
