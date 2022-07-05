#####Migration funcs#####


iso3n_transform <- function(vector){
  return(vector + 100)
}

iso3n_transform(x$iso3n)



cid3 + 100 + gid4 + iso3 + 100 + yr4 + month2
1,000,000,000,000

cid7 + 1,000,000 + iso3+100

iso3n, year, month, month_abs, sid, adm1, cell_pop, confvars 




generate_full <- function(iso, year_range = 1990:2020, nid_grid, poprast_list, ged25, ged50, ged100, adm1, write_db = NULL){
  
  iso3n <- countrycode(iso, origin = "iso3c", destination = "iso3n")
  if(is.na(iso3n)){
    iso3n <- 899
  }
  
  poprast_list_c <- subset_rasters(iso, nid_grid)
  
  geos <- generate_geos(iso3n, poprast_list_c)
  grid_geos <- geos[[1]]
  cell_geos <- geos[[2]]
  
  adm_join <- add_shapes(iso3n, adm1, cell_geos, grid_geos) %>%
    rename(capa_id_adm1 = capa_id)
  
  cell_pops <- generate_pops(cell_geos, poprast_list_c, year_range) %>%
    left_join(adm_join, by = "sid")
  
  cell_stats <- generate_stats(iso, year_range, ged25, ged50, ged100, grid_geos, cell_geos, cell_pops) %>%
    left_join(adm_join, by = "sid")
  
  if(!is.null(write_db)){
    st_write(grid_geos, write_db, "grid_geos", append = T)
    st_write(cell_geos, write_db, "cell_geos", append = T)
    dbWriteTable(write_db, "cell_pops", cell_pops, append = T)
    dbWriteTable(write_db, "cell_stats", cell_stats, append = T)
    return(iso)
  }else{
    out_list <- list(cell_stats, cell_geos, grid_geos, cell_pops)
    return(out_list)
  }
}

update_from_pops <- function(iso, year_range = c(2021), nid_grid, poprast_list, ged25, ged50, ged100, source_db = capa_db, write_db = NULL){
  
  #read
  cell_geos <- st_read(source_db, query = glue("SELECT * FROM cell_geos WHERE iso3n = '{iso3n}'")) %>%
    mutate(sid = as.numeric(sid))
  grid_geos <- st_read(source_db, query = glue("SELECT * FROM grid_geos WHERE iso3n = '{iso3n}'"))
  adm_join <- dbGetQuery(source_db, glue("SELECT sid, capa_id_adm1 FROM cell_geos WHERE iso3n = '{iso3n}'"))
  
  #create
  poprast_list_c <- subset_rasters(iso, nid_grid, poprast_list)
  cell_pops <- generate_pops(cell_geos, poprast_list_c, year_range) %>%
    left_join(adm_join, by = "sid")
  cell_stats <- generate_stats(iso, year_range, ged25, ged50, ged100, grid_geos, cell_geos, cell_pops) %>%
    left_join(adm_join, by = "sid")
  
  #write
  if(!is.null(write_db)){
    dbWriteTable(write_db, "cell_pops", cell_pops, append = T)
    dbWriteTable(write_db, "cell_stats", cell_stats, append = T)
  }else{
    return(list(cell_pops, cell_stats))
  }
  
}

update_from_stats <- function(iso, year_range = c(2021), nid_grid, ged25, ged50, ged100, source_db = capa_db, write_db = NULL){
  
  #read
  cell_geos <- st_read(source_db, query = glue("SELECT * FROM cell_geos WHERE iso3n = '{iso3n}'")) %>%
    mutate(sid = as.numeric(sid))
  grid_geos <- st_read(source_db, query = glue("SELECT * FROM grid_geos WHERE iso3n = '{iso3n}'"))
  cell_pops <- dbGetQuery(source_db, glue("SELECT * FROM cell_pops WHERE iso3n = '{iso3n}' AND year > {year_range[1] - 1} AND year < {year_range[length(year_range)] + 1}")) %>%
    mutate(sid = as.numeric(sid))
  adm_join <- dbGetQuery(source_db, glue("SELECT sid, capa_id_adm1 FROM cell_geos WHERE iso3n = '{iso3n}'"))
  
  #create
  cell_stats <- generate_stats(iso, year_range, ged25, ged50, ged100, grid_geos, cell_geos, cell_pops) %>%
    left_join(adm_join, by = "sid")
  
  #write
  if(!is.null(write_db)){
    dbWriteTable(write_db, "cell_stats", cell_stats, append = T)
  }else{
    return(cell_stats)
  }
}

update_pops <- function(iso, year_range = c(2021), nid_grid, poprast_list, source_db = capa_db, write_db = NULL){
  
  #read
  cell_geos <- st_read(source_db, query = glue("SELECT * FROM cell_geos WHERE iso3n = '{iso3n}'")) %>%
    mutate(sid = as.numeric(sid))
  grid_geos <- st_read(source_db, query = glue("SELECT * FROM grid_geos WHERE iso3n = '{iso3n}'"))
  adm_join <- dbGetQuery(source_db, glue("SELECT sid, capa_id_adm1 FROM cell_geos WHERE iso3n = '{iso3n}'"))
  
  #create
  poprast_list_c <- subset_rasters(iso, nid_grid, poprast_list)
  cell_pops <- generate_pops(cell_geos, poprast_list_c, year_range) %>%
    left_join(adm_join, by = "sid")
  
  #write
  if(!is.null(write_db)){
    dbWriteTable(write_db, "cell_pops", cell_pops, append = T)
  }else{
    return(cell_pops)
  }
  
}

#should probably write to staging db - hard-coded for cgaz adm1
update_adm <- function(iso, adm1, source_db = capa_db, write_db = NULL, init_column = FALSE){
  
  iso3n <- countrycode(iso, origin = "iso3c", destination = "iso3n")
  if(is.na(iso3n)){
    iso3n <- 899
  }
  
  #read
  cell_geos <- st_read(source_db, query = glue("SELECT * FROM cell_geos WHERE iso3n = '{iso3n}'")) %>%
    mutate(sid = as.numeric(sid))
  grid_geos <- st_read(source_db, query = glue("SELECT * FROM grid_geos WHERE iso3n = '{iso3n}'"))
  cell_stats <- dbGetQuery(source_db, "SELECT * FROM cell_stats WHERE iso3n = '{iso3n}'")
  
  adm_join <- add_shapes(iso3n, adm1, cell_geos, grid_geos) %>%
    rename(capa_id_adm1 = capa_id)
  
  if(!is.null(write_db)){
    if(init_column){
      dbExecute(write_db, glue("ALTER TABLE cell_geos ADD capa_id_adm1 BIGINT"))
      dbExecute(write_db, glue("ALTER TABLE cell_stats ADD capa_id_adm1 BIGINT"))
    }
    dbWriteTable(write_db, "temp", adm_join, overwrite = TRUE)
    dbExecute(write_db, "UPDATE cell_geos SET capa_id_adm1 = temp.capa_id FROM temp WHERE cell_geos.sid = temp.sid")
    dbExecute(write_db, "UPDATE cell_stats SET capa_id_adm1 = temp.capa_id FROM temp WHERE cell_stats.sid = temp.sid")
    return(iso)
  }else{
    
    if(!is.null(cell_geos$capa_id_adm1)){
      cell_geos <- dplyr::select(-capa_id_adm1)
    }
    cell_geos <- cell_geos %>%
      left_join(adm_join, by = "sid")
    
    if(!is.null(cell_stats$capa_id_adm1)){
      cell_stats <- dplyr::select(-capa_id_adm1)
    }
    cell_stats <- cell_stats %>%
      left_join(adm_join, by = "sid")
    
    out_list <- list(cell_geos, cell_stats)
    return(out_list)
  }
}


# dbWriteTable(hdr_db_2, "test_comb", test_comb, overwrite = TRUE)
# dbWriteTable(hdr_db, "temp", join)
# dbExecute(hdr_db_2, "UPDATE test_comb SET test_comb.capa_id_adm1 = temp.capa_id FROM test_comb LEFT JOIN temp ON test_comb.sid = temp.sid")

# dbExecute(hdr_db, "ALTER TABLE test_comb ADD capa_id_adm1 BIGINT")
# dbExecute(hdr_db, "UPDATE test_comb SET capa_id_adm1 = temp.capa_id FROM temp WHERE test_comb.sid = temp.sid")
