


demos_est <- readxl::read_excel(paste0(drop_path, "WPP2019_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx"), skip = 16)
names(demos_est)[9:29] <- paste("ages", gsub("-", "_", names(demos_est)[9:29]), sep = "_")
demos_est[, 9:29] <- sapply(demos_est[, 9:29], as.numeric)
demos_est[, 9:29] <- demos_est[, 9:29] * 1000
demos_est <- demos_est %>% mutate(total = rowSums(.[9:29]))
names(demos_est)[8] <- "year"
names(demos_est)[5] <- "iso3n"
demos_est <- demos_est %>% mutate(ages_15_18 = ages_15_19 * (4/5)) %>%
  mutate(child_pop = ages_0_4 + ages_5_9 + ages_10_14 + ages_15_18) %>%
  mutate(child_pct = child_pop/total)


demos_proj <- readxl::read_excel(paste0(drop_path, "WPP2019_POP_F15_1_ANNUAL_POPULATION_BY_AGE_BOTH_SEXES.xlsx"), sheet = "MEDIUM VARIANT", skip = 16)
names(demos_proj)[9:29] <- paste("ages", gsub("-", "_", names(demos_proj)[9:29]), sep = "_")
demos_proj[, 9:29] <- sapply(demos_proj[, 9:29], as.numeric)
demos_proj[, 9:29] <- demos_proj[, 9:29] * 1000
demos_proj <- demos_proj %>% mutate(total = rowSums(.[9:29]))
names(demos_proj)[8] <- "year"
names(demos_proj)[5] <- "iso3n"
demos_proj <- demos_proj %>% mutate(ages_15_18 = ages_15_19 * (4/5)) %>%
  mutate(child_pop = ages_0_4 + ages_5_9 + ages_10_14 + ages_15_18) %>%
  mutate(child_pct = child_pop/total)

demos <- bind_rows(demos_est, demos_proj) %>%
  dplyr::select(iso3n, year, total, child_pct)


un_isos <- unique(demos$iso3n)
nid_isos <- unique(nid_grid$ISOCODE) %>% ison()
missing_isos <- nid_isos[nid_isos %!in% un_isos] %>% isoc()
