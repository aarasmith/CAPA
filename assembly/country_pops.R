library(countrycode)
library(raster)
library(future)
library(future.apply)

nid_grid #in global.R
raster_files <- list.files("C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/new_rasters/ipolated", pattern = "tif", full.names = TRUE)
poprast_list <- lapply(raster_files, raster)

generate_country_pops <- function(iso3c, nid_grid, poprast_list, years = 1990:2020){
  print(iso3c)
  #data_generation.R
  poprast_c <- subset_rasters(iso3c, nid_grid, poprast_list)
  
  cpop <- lapply(poprast_c, cellStats, stat = sum) %>% unlist()
  
  #app_funcs.R utilities
  iso3n <- ison(iso3c)
  
  out_frame <- data.frame(iso3n = iso3n, year = years, total_pop = cpop)
  
  return(out_frame)
  
}

iso_list <- nid_grid$ISOCODE[nid_grid$ISOCODE != "SPR"]
future::plan(multisession)
country_pops <- lapply(iso_list, generate_country_pops, nid_grid = nid_grid, poprast_list = poprast_list) %>%
  rbindlist()

saveRDS(country_pops, "data/country_pops.RDS")
country_pops$total_pop <- round(country_pops$total_pop)

capa_db <- connect_to_capa()

#create table in DB
#country_pops_schema in schema.R
dbExecute(capa_db, country_pops_schema)

dbWriteTable(capa_db, "country_pops", country_pops, append = TRUE)

disconnect_from_capa(capa_db)
