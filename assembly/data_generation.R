#Sys.setenv("_SF_USE_S2" = "false")
#parallel processes in future_lapply will use S2 (slower, unnecessary, changes results...) if this isn't set

nid_grid
poprast_list
library(countrycode)
library(raster)
library(exactextractr)
ged25 <- readRDS(paste0(drop_path, "ged25.RDS"))
ged50 <- readRDS(paste0(drop_path, "ged50.RDS"))
ged100 <- readRDS(paste0(drop_path, "ged100.RDS"))

raster_files <- list.files("C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/new_rasters/ipolated", pattern = "tif", full.names = TRUE)
poprast_list <- lapply(raster_files, raster)


generate_comb <- function(iso, year_range = 1990:2020){
  
  #Get the GPW country shape
  country_shape <- filter(nid_grid, ISOCODE == iso)
  
  #Crop/mask all the rasters to the country shape
  poprast_list_c <- lapply(poprast_list, crop, y = extent(country_shape))
  poprast_list_c <- lapply(poprast_list_c, mask, mask = country_shape)
  
  #Create grid-cell vector geometry out of rasters
  country_polys <- poprast_list_c[[31]] %>% as("SpatialPolygonsDataFrame") %>% st_as_sf() %>% cbind(data.frame(cid = 1:nrow(.))) %>% dplyr::select(-ipop_2020)
  
  #Create a meta-grid over the grid-cells
  country_grid <- st_make_grid(country_shape, cellsize = 0.5) %>%
    st_as_sf() %>%
    rename(geometry = x) %>%
    filter(st_intersects(geometry, country_shape, sparse = FALSE)) %>%
    mutate(gid = 1:nrow(.), iso3c = iso)
  
  #Function for joining the grid-cells to the meta-grid
  join_cells_to_grid <- function(){
    poly_join_with_dupes <- st_join(country_polys, country_grid) #%>% mutate(sid = paste(iso3c, gid, cid, sep = "-"))
    poly_dupes <- country_polys %>% filter(cid %in% poly_join_with_dupes$cid[duplicated(poly_join_with_dupes$cid)])
    poly_join_no_dupes <- poly_join_with_dupes %>% filter(cid %!in% poly_dupes$cid)
    dupe_join <- st_join(poly_dupes, country_grid, largest = TRUE) #%>% mutate(sid = paste(iso3c, gid, cid, sep = "-"))
    poly_join <- bind_rows(poly_join_no_dupes, dupe_join)
    
    return(poly_join)
  }
  
  #Join grid-cells to meta-grid and drop non-intersecting meta-grid entries
  country_cells <- join_cells_to_grid()
  country_grid <- country_grid %>% filter(gid %in% country_cells$gid)
  country_grid$gid <- 1:nrow(country_grid)
  country_cells <- join_cells_to_grid()
  
  country_cells <- country_cells %>%
    mutate(iso3c = countrycode(iso, origin = "iso3c", destination = "iso3n")) %>%
    rename(iso3n = iso3c) %>%
    mutate(cid = as.numeric(paste0(iso3n + 100, cid + 1000000))) %>%
    rename(sid = cid)
  
  cell_geos <- country_cells %>%
    dplyr::select(sid, gid, iso3n)
  
  #Extract pop data from the rasters to the vector-geometry grid-cells
  var_list <- paste0("ipop", year_range)
  

  i = 1:length(poprast_list)
  extract_pops <- function(i, country_cells, var_list, poprast_list_c){
    country_cells <- country_cells %>% mutate(!!var_list[i] := round(exact_extract(poprast_list_c[[i]], country_cells, fun = "sum", progress = FALSE))) %>% st_drop_geometry() %>%
      dplyr::select(sid, contains("ipop"))
    return(country_cells)
  }
  
  
  country_cells <- list(country_cells) %>%
    append(future_lapply(1:length(poprast_list), extract_pops, country_cells = country_cells, var_list = var_list, poprast_list_c = poprast_list_c)) %>%
    reduce(left_join, by = "sid")
  
  
  
  
  
  #Filter geds
  ged25_c <- ged25 %>% filter(iso3c == iso) %>% dplyr::select(int_cat, best, year, month, id)
  ged50_c <- ged50 %>% filter(iso3c == iso) %>% dplyr::select(int_cat, best, year, month, id)
  ged100_c <- ged100 %>% filter(iso3c == iso) %>% dplyr::select(int_cat, best, year, month, id)
  
  #subset ged by joining with meta-grid
  grid25 <- st_join(ged25_c, country_grid)
  grid25 <- grid25[!is.na(grid25$gid), ]
  grid50 <- st_join(ged50_c, country_grid)
  grid50 <- grid50[!is.na(grid50$gid), ]
  grid100 <- st_join(ged100_c, country_grid)
  grid100 <- grid100[!is.na(grid100$gid), ]
  
  #any meta-grid with no conflict intersections at the 100km distance does not need to be run
  grid_sub <- grid100 %>% st_drop_geometry()
  grid_sub <- unique(grid_sub$gid)
  
  if(sum(is.na(grid_sub)) > 0){
    print("FAILED")
    return(list(country_cells, country_grid))
  }
  
  #create the monthly framework for each grid-cell. Grid-cells in non-conflict meta-grids are excluded to save space/time
  cell_pops <- reshape2::melt(st_drop_geometry(filter(country_cells, gid %in% grid_sub)), id.vars = c(1:3), variable.name = "year", value.name = "cell_pop") %>%
    dplyr::select(-gid) %>%
    as.data.table()
  cell_pops <- cell_pops[ , year := substr(year, 5, 8)]
  cell_pops$year <- as.numeric(cell_pops$year)
  months <- 1:12
  cells <- cell_pops[rep(1:.N, 12)][ , month := months, by = list(sid, year)]
  setorder(cells, sid, year, month)
  cells <- cells[ , month_abs := c(1:(12 * length(year_range))), by = list(sid)]
  
  #Intersect conflict events with grid-cells with respect to time
  comb25 <- list()
  comb50 <- list()
  comb100 <- list()
  
  system.time({
  for(i_grid in grid_sub){
    ged25_sub <- filter(grid25, gid == i_grid)
    comb25_sub <- st_join(dplyr::select(st_as_sf(filter(cell_geos, gid == i_grid)), sid), ged25_sub) %>% st_drop_geometry()
    
    comb25[[i_grid]] <- comb25_sub
  }
  })
  
  ged_intersect <- function(i_grid, ged_grid, cell_geos){
    ged_sub <- filter(ged_grid, gid == i_grid)
    comb_sub <- st_join(dplyr::select(filter(cell_geos, gid == i_grid), sid), ged_sub) %>% st_drop_geometry()
    return(comb_sub)
  }
  plan(multisession)
  
  comb25 <- future_lapply(grid_sub, ged_intersect, ged_grid = grid25, cell_geos = cell_geos)
  comb25 <- rbindlist(comb251) %>% 
    group_by(sid, year, month) %>%
    summarize(L25 = sum(int_cat == 1, na.rm = TRUE),
              M25 = sum(int_cat == 2, na.rm = TRUE),
              H25 = sum(int_cat == 3, na.rm = TRUE),
              int25 = sum(best, na.rm = TRUE))
  
  
  comb50 <- future_lapply(grid_sub, ged_intersect, ged_grid = grid50, cell_geos = cell_geos)
  comb50 <- rbindlist(comb50) %>%
    group_by(sid, year, month) %>%
    summarize(L50 = sum(int_cat == 1, na.rm = TRUE),
              M50 = sum(int_cat == 2, na.rm = TRUE),
              H50 = sum(int_cat == 3, na.rm = TRUE),
              int50 = sum(best, na.rm = TRUE))
  
  
  comb100 <- future_lapply(grid_sub, ged_intersect, ged_grid = grid100, cell_geos = cell_geos)
  comb100 <- rbindlist(comb100) %>% 
    group_by(sid, year, month) %>%
    summarize(L100 = sum(int_cat == 1, na.rm = TRUE),
              M100 = sum(int_cat == 2, na.rm = TRUE),
              H100 = sum(int_cat == 3, na.rm = TRUE),
              int100 = sum(best, na.rm = TRUE))
  
  #join the intersected cells with "cells" and drop any non-conflict entries
  comb <- left_join(cells, comb25, by = c("sid", "year", "month")) %>%
    left_join(comb50, by = c("sid", "year", "month")) %>%
    left_join(comb100, by = c("sid", "year", "month")) %>%
    filter(int100 > 0) #%>%
  #replace(is.na(.), 0)
  comb[is.na(comb)] <- 0
  
  out_list <- list(comb, cell_geos, country_grid, cell_pops)
  # if(run_tests(out_list) == 9){
  #   dbWriteTable(hdr_db, "comb", out_list[[1]], append = TRUE)
  #   st_write(st_transform(out_list[[2]], crs = crs(out_list[[3]])), hdr_db, "cells", append = TRUE)
  #   st_write(out_list[[3]], hdr_db, "grid", append = TRUE)
  #   return(1)
  # }else{
    return(out_list)
  # }
}
