#functions for migrating data from the hdr_app_v2 db to the CAPA db in new format

hdr_db <- dbConnect(drv = RPostgres::Postgres(), host = aws_host, dbname = "postgres", user = aws_user, password = aws_pass, port = aws_port)
capa_db <- dbConnect(drv = RPostgres::Postgres(), host = prio_host, dbname = "capa", user = prio_user, password = prio_pass, port = prio_port)

move_cell_geos <- function(hdr_db = hdr_db, capa_db = capa_db){
  new_cells <- st_read(hdr_db, query = "SELECT cid, gid, iso3c, geometry FROM cells") %>%
    mutate(iso3n = countrycode(iso3c, origin = "iso3c", destination = "iso3n")) %>%
    mutate(iso3n = ifelse(is.na(iso3n), 899, iso3n)) %>%
    mutate(sid = as.numeric(paste0(100 + iso3n, 1000000 + cid))) %>%
    dplyr::select(-cid, -iso3c)
  st_write(new_cells, dsn = capa_db, layer = "cell_geos", append = TRUE)
}

move_grid_geos <- function(hdr_db = hdr_db, capa_db = capa_db){
  new_grid <- st_read(hdr_db, query = "SELECT gid, iso3c, geometry FROM grid") %>%
    mutate(iso3n = countrycode(iso3c, origin = "iso3c", destination = "iso3n")) %>%
    mutate(iso3n = ifelse(is.na(iso3n), 899, iso3n)) %>%
    dplyr::select(-iso3c)
  st_write(new_grid, dsn = capa_db, layer = "grid_geos", append = TRUE)
}

#maybe need to add admin info
move_cell_pops <- function(hdr_db = hdr_db, capa_db = capa_db){
  new_cell_pops <- dbGetQuery(hdr_db, "SELECT cid, iso3c, cell_pop, year FROM cell_pops") %>%
    mutate(iso3n = countrycode(iso3c, origin = "iso3c", destination = "iso3n")) %>%
    mutate(iso3n = ifelse(is.na(iso3n), 899, iso3n)) %>%
    mutate(sid = as.numeric(paste0(100 + iso3n, 1000000 + cid))) %>%
    dplyr::select(-cid, -iso3c)
  dbWriteTable(capa_db, "cell_pops", new_cell_pops, append = TRUE)
}

move_cgaz <- function(hdr_db = hdr_db, capa_db = capa_db){
  new_cgaz <- dbGetQuery(hdr_db, "SELECT sid, shape_id, iso3c FROM cgaz") %>%
    mutate(iso3n = countrycode(iso3c, origin = "iso3c", destination = "iso3n")) %>%
    mutate(iso3n = ifelse(is.na(iso3n), 899, iso3n)) %>%
    mutate(cid = as.numeric(gsub("\\w{3}-\\d+-", "", sid))) %>%
    mutate(sid = as.numeric(paste0(100 + iso3n, 1000000 + cid))) %>%
    dplyr::select(-cid, -iso3c)
  #dbWriteTable(capa_db, "cell_pops", new_cell_pops, append = TRUE)
}

adm1_join <- dplyr::select(adm1_cgaz, shape_id, capa_id) %>% st_drop_geometry()
new_cgaz <- new_cgaz %>% left_join(adm1_join, by = "shape_id") %>%
  filter(!is.na(capa_id)) %>%
  dplyr::select(-shape_id, -iso3n) %>%
  rename(capa_id_adm1 = capa_id)
new_cgaz$sid <- as.integer64(new_cgaz$sid)


cell_pops <- dbGetQuery(capa_db, "SELECT * FROM cell_pops")
cell_pops <- cell_pops %>% left_join(new_cgaz, by = "sid")
dbWriteTable(capa_db, "cell_pops1", cell_pops, append = TRUE)

move_comb <- function(hdr_db = hdr_db, capa_db = capa_db){
  new_comb <- dbGetQuery(hdr_db, "SELECT cid, iso3c, year, month, month_abs, cell_pop, lo_25, lo_50, lo_100, md_25, md_50, md_100, hi_25, hi_50, hi_100, int_25, int_50, int_100 FROM comb") %>%
    mutate(iso3n = countrycode(iso3c, origin = "iso3c", destination = "iso3n")) %>%
    mutate(iso3n = ifelse(is.na(iso3n), 899, iso3n)) %>%
    mutate(sid = as.numeric(paste0(100 + iso3n, 1000000 + cid))) %>%
    dplyr::select(-cid) %>%
    mutate(sid = as.integer64(sid)) %>%
    left_join(new_cgaz, by = "sid")
  dbWriteTable(capa_db, "cell_stats", new_comb, append = TRUE)
}

dbExecute(capa_db, "CREATE TABLE region_key (iso3n SMALLINT, region TEXT)")
#requires ison function
move_region_key <- function(hdr_db = hdr_db, capa_db = capa_db){
  new_region_key <- dbGetQuery(hdr_db, "SELECT * FROM region_key") %>%
    filter(iso3c != "SPR") %>%
    mutate(iso3c = ison(iso3c)) %>%
    rename(iso3n = iso3c)
  dbWriteTable(capa_db, "region_key", new_region_key, append = TRUE)
}

dbExecute(capa_db, "CREATE TABLE region_pops (region TEXT, year SMALLINT, total_pop BIGINT)")
move_region_pops <- function(hdr_db = hdr_db, capa_db = capa_db){
  new_region_pops <- dbGetQuery(hdr_db, "SELECT * FROM region_pops") %>%
    mutate(total_pop = round(total_pop))
  dbWriteTable(capa_db, "region_pops", new_region_pops, append = TRUE)
}