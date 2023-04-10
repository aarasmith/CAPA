# manual_regions <- list()
# 
# manual_regions[['middle_east_prio']] <- c("Yemen",
#                       "Oman",
#                       "Iran (Islamic Republic of)",
#                       "Israel",
#                       "Qatar",
#                       "Bahrain",
#                       "Syrian Arab Republic",
#                       "Turkey",
#                       "Palestine, State of",
#                       "United Arab Emirates",
#                       "Saudi Arabia",
#                       "Kuwait",
#                       "Iraq") %>% countrycode(origin = "country.name", destination = "iso3n")
# 
# 
# manual_regions[['asia_rest_prio']] <- c("Micronesia (Federated States of)",
#                     "Jordan",
#                     "Mongolia",
#                     "China",
#                     "Christmas Island",
#                     "Armenia",
#                     "India",
#                     "Georgia",
#                     "Pitcairn",
#                     "Heard Island and McDonald Islands",
#                     "Japan",
#                     "French Polynesia",
#                     "Kazakhstan",
#                     "Uzbekistan",
#                     "Macao",
#                     "Kyrgyzstan",
#                     "Palau",
#                     "Papua New Guinea",
#                     "Malaysia",
#                     "Myanmar",
#                     "Iraq",
#                     "Tonga",
#                     "Vanuatu",
#                     "New Zealand",
#                     "Timor-Leste",
#                     "Sri Lanka",
#                     "Thailand",
#                     "Fiji",
#                     "Marshall Islands",
#                     "Bangladesh",
#                     "Turkmenistan",
#                     "Korea (Democratic People's Republic of)",
#                     "Cocos (Keeling) Islands",
#                     "Samoa",
#                     "Northern Mariana Islands",
#                     "American Samoa",
#                     "Brunei Darussalam",
#                     "Cambodia",
#                     "United States Minor Outlying Islands",
#                     "Bhutan",
#                     "Tuvalu",
#                     "Singapore",
#                     "Lao People's Democratic Republic",
#                     "Indonesia",
#                     "Hong Kong",
#                     "Australia",
#                     "Guam",
#                     "Pakistan",
#                     "Tokelau",
#                     "Solomon Islands",
#                     "Niue",
#                     "Cook Islands",
#                     "Viet Nam",
#                     "Korea (Republic of)",
#                     "Philippines",
#                     "Lebanon",
#                     "Cyprus",
#                     "Azerbaijan",
#                     "Kiribati",
#                     "Nepal",
#                     "Afghanistan",
#                     "New Caledonia",
#                     "Taiwan, Province of China",
#                     "Norfolk Island",
#                     "Wallis and Futuna",
#                     "Maldives",
#                     "Tajikistan",
#                     "Nauru"
#                     ) %>% countrycode(origin = "country.name", destination = "iso3n")

library(readxl)
gwno_regions <- read_xlsx(paste0(drop_path, "USDP_GWNO_regions.xlsx"))
gwno_regions$iso3n <- countrycode(gwno_regions$code, origin = "gwc", destination = "iso3n", custom_match = c("KOS" = 899, "DRV"= 704, "YEM" = 887))
gwno_regions <- gwno_regions %>%
  filter(!is.na(iso3n))
gwno_region_names <- unique(gwno_regions$region)
manual_regions <- lapply(gwno_region_names, function(x) filter(gwno_regions, region == x) %>% pull(iso3n))
names(manual_regions) <- paste0("GWNO_", gwno_region_names)

new_regions <- gwno_regions %>% dplyr::select(iso3n, region) %>%
  mutate(region = gsub(" ", "_", paste0("GWNO_", region)))

old_key <- dbGetQuery(connect_to_capa(), "SELECT * FROM region_key")
dbWriteTable(connect_to_capa(), "region_key", new_regions, append = T)

#dbWriteTable(connect_to_capa(), "region_key", old_key, overwrite = T)
