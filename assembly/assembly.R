source("assembly/data_generation.R")
source("adm_assembly.R")


#this is how isos are transformed in making the spatial ID's (sid) to ensure they're all 3 digits long (e.g. 4 -> 104)
#countries without iso numbers (currently just Kosovo) are assigned 899 as their iso number which transforms to 999
iso3n_transform <- function(vector){
  return(vector + 100)
}

iso3n_transform(x$iso3n)

####
# These functions are optimized to use parallel processing with the future and future.apply packages

# Consider setting future::plan(multisession) before running for dramatically increased individual performance

# If running for a global set, you should consider replacing the future_lapply() calls in the sub_functions (found in data_generation.R and adm_assembly.R) with regular lapply() 
# and running these primary functions over a vector of countries with future_lapply(), although I haven't tested whether this approach would actually be faster/slower
####

####
# If you need to generate the full dataset and commit it to a DB use generate_full()

# If you need to add new population data and then generate new conflict statistics use update_from_pops()
# If you need to generate new population tif's for a certain year see interpolate_rasters.R which utilizes the function in lintemp.R

# If you just need to update the conflict statistics use update_from_stats()

# If you want to update population data without then changing/updating the conflict statistics use update_pops()

# If you want to update ADM data which is attached to cell_stats and cell_pops use update_adm()
# This can be used to either initialize a new column in the DB tables for the generated ADM data or simply update the existing column with init_column = TRUE/FALSE
####

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
  
  iso3n <- ison(iso)
  
  #read
  cell_geos <- st_read(source_db, query = glue("SELECT * FROM cell_geos WHERE iso3n = {iso3n}")) %>%
    mutate(sid = as.numeric(sid))
  grid_geos <- st_read(source_db, query = glue("SELECT * FROM grid_geos WHERE iso3n = {iso3n}"))
  adm_join <- dbGetQuery(source_db, glue("SELECT sid, capa_id_adm1 FROM cell_pops WHERE iso3n = {iso3n}")) %>% distinct() %>% within(sid <- as.numeric(sid))
  
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
  
  iso3n <- ison(iso)
  source_db <- connect_to_capa()
  
  #read
  cell_geos <- st_read(source_db, query = glue("SELECT * FROM cell_geos WHERE iso3n = {iso3n}")) %>%
    mutate(sid = as.numeric(sid))
  grid_geos <- st_read(source_db, query = glue("SELECT * FROM grid_geos WHERE iso3n = {iso3n}"))
  cell_pops <- dbGetQuery(source_db, glue("SELECT * FROM cell_pops WHERE iso3n = {iso3n} AND year > {year_range[1] - 1} AND year < {year_range[length(year_range)] + 1}")) %>%
    mutate(sid = as.numeric(sid))
  adm_join <- dbGetQuery(source_db, glue("SELECT sid, capa_id_adm1 FROM cell_pops WHERE iso3n = {iso3n}")) %>% distinct() %>% within(sid <- as.numeric(sid))
  
  disconnect_from_capa(source_db)
  
  #create
  cell_stats <- generate_stats(iso, year_range, ged25, ged50, ged100, grid_geos, cell_geos, cell_pops) #%>%
    #left_join(adm_join, by = "sid")
  
  #write
  if(!is.null(write_db)){
    dbWriteTable(write_db, "cell_stats", cell_stats, append = T)
    return(iso)
  }else{
    return(cell_stats)
  }
}

#20 minutes with future_lapply on 1 year
#can't pass connection into future_lapply multisessions, therefore must connect within function
update_pops <- function(iso, year_range = c(2021), nid_grid, poprast_list, source_db = capa_db, write_db = NULL){
  
  iso3n <- ison(iso)
  source_db <- connect_to_capa()
  #read
  cell_geos <- st_read(source_db, query = glue("SELECT * FROM cell_geos WHERE iso3n = {iso3n}")) %>%
    mutate(sid = as.numeric(sid))
  grid_geos <- st_read(source_db, query = glue("SELECT * FROM grid_geos WHERE iso3n = {iso3n}"))
  adm_join <- dbGetQuery(source_db, glue("SELECT sid, capa_id_adm1 FROM cell_pops WHERE iso3n = {iso3n}")) %>% distinct() %>% within(sid <- as.numeric(sid))
  
  disconnect_from_capa(source_db)
  
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
update_adm <- function(iso, adm1, source_db = capa_db, write_db = NULL, adm_column = "capa_id_adm1", init_column = FALSE){
  
  iso3n <- countrycode(iso, origin = "iso3c", destination = "iso3n")
  if(is.na(iso3n)){
    iso3n <- 899
  }
  
  #read
  cell_geos <- st_read(source_db, query = glue("SELECT * FROM cell_geos WHERE iso3n = {iso3n}")) %>%
    mutate(sid = as.numeric(sid))
  grid_geos <- st_read(source_db, query = glue("SELECT * FROM grid_geos WHERE iso3n = {iso3n}"))
  cell_stats <- dbGetQuery(source_db, glue("SELECT * FROM cell_stats WHERE iso3n = {iso3n}"))
  
  adm_join <- add_shapes(iso3n, adm1, cell_geos, grid_geos)
  
  if(!is.null(write_db)){
    if(init_column){
      dbExecute(write_db, glue("ALTER TABLE cell_pops ADD {adm_column} BIGINT"))
      dbExecute(write_db, glue("ALTER TABLE cell_stats ADD {adm_column} BIGINT"))
    }
    dbWriteTable(write_db, "temp", adm_join, overwrite = TRUE)
    dbExecute(write_db, "UPDATE cell_pops SET {adm_column} = temp.capa_id FROM temp WHERE cell_pops.sid = temp.sid")
    dbExecute(write_db, "UPDATE cell_stats SET {adm_column} = temp.capa_id FROM temp WHERE cell_stats.sid = temp.sid")
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
