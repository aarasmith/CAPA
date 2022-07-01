

nid_grid
poprast_list



generate_comb <- function(iso){
  
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
    poly_join_with_dupes <- st_join(country_polys, country_grid) %>% mutate(sid = paste(iso3c, gid, cid, sep = "-"))
    poly_dupes <- country_polys %>% filter(cid %in% poly_join_with_dupes$cid[duplicated(poly_join_with_dupes$cid)])
    poly_join_no_dupes <- poly_join_with_dupes %>% filter(cid %!in% poly_dupes$cid)
    dupe_join <- st_join(poly_dupes, country_grid, largest = TRUE) %>% mutate(sid = paste(iso3c, gid, cid, sep = "-"))
    poly_join <- bind_rows(poly_join_no_dupes, dupe_join)
    
    return(poly_join)
  }
  
  #Join grid-cells to meta-grid and drop non-intersecting meta-grid entries
  country_cells <- join_cells_to_grid()
  country_grid <- country_grid %>% filter(gid %in% country_cells$gid)
  country_grid$gid <- 1:nrow(country_grid)
  country_cells <- join_cells_to_grid()
  
  #Extract pop data from the rasters to the vector-geometry grid-cells
  var_list <- paste0("ipop", 1990:2020)
  
  for(i in 1:31){
    country_cells <- country_cells %>% mutate(!!var_list[i] := round(exact_extract(poprast_list_c[[i]], country_cells, fun = "sum")))
  }
  
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
  
  #any grid with no conflict intersections at the 100km distance does not need to be run
  grid_sub <- grid100 %>% st_drop_geometry()
  grid_sub <- unique(grid_sub$gid)
  
  if(sum(is.na(grid_sub)) > 0){
    print("FAILED")
    return(list(country_cells, country_grid))
  }
  
  #create the monthly framework for each grid-cell. Grid-cells in non-conflict meta-grids are excluded to save space/time
  cells <- reshape2::melt(st_drop_geometry(filter(country_cells, gid %in% grid_sub)), id.vars = c(1:4), variable.name = "year", value.name = "cell_pop") %>% as.data.table()
  cells <- cells[, year := substr(year, 5, 8)]
  months <- str_pad(as.character(1:12), "2", "left", pad = "0")
  cells <- cells[rep(1:.N, 12)][, month := months, by = list(cid, year)]
  setorder(cells, cid, year, month)
  cells$year <- as.numeric(cells$year)
  cells <- cells[, month_abs := c(1:372), by = list(cid)]
  cells <- cells[, stid := paste(sid, year, month, sep = "-")]
  
  #Intersect conflict events with grid-cells with respect to time
  comb25 <- list()
  comb50 <- list()
  comb100 <- list()
  
  
  for(i_grid in grid_sub){
    ged25_sub <- filter(grid25, gid == i_grid)
    comb25_sub <- st_join(dplyr::select(st_as_sf(filter(country_cells, gid == i_grid)), cid), ged25_sub) %>% st_drop_geometry()
    
    comb25[[i_grid]] <- comb25_sub
  }
  
  comb25 <- rbindlist(comb25)
  #comb25 <- left_join(cells, dplyr::select(comb25, best, int_cat, year, month, cid), by = c("cid", "year", "month"))
  comb25 <- comb25 %>% group_by(cid, year, month) %>%
    summarize(L25 = sum(int_cat == 1, na.rm = TRUE),
              M25 = sum(int_cat == 2, na.rm = TRUE),
              H25 = sum(int_cat == 3, na.rm = TRUE),
              int25 = sum(best, na.rm = TRUE))
  
  
  
  for(i_grid in grid_sub){
    ged50_sub <- filter(grid50, gid == i_grid)
    comb50_sub <- st_join(dplyr::select(st_as_sf(filter(country_cells, gid == i_grid)), cid), ged50_sub) %>% st_drop_geometry()
    
    comb50[[i_grid]] <- comb50_sub
  }
  
  comb50 <- rbindlist(comb50)
  #comb50 <- left_join(cells, dplyr::select(comb50, best, int_cat, year, month, cid), by = c("cid", "year", "month"))
  comb50 <- comb50 %>% group_by(cid, year, month) %>%
    summarize(L50 = sum(int_cat == 1, na.rm = TRUE),
              M50 = sum(int_cat == 2, na.rm = TRUE),
              H50 = sum(int_cat == 3, na.rm = TRUE),
              int50 = sum(best, na.rm = TRUE))
  
  
  
  for(i_grid in grid_sub){
    ged100_sub <- filter(grid100, gid == i_grid)
    comb100_sub <- st_join(dplyr::select(st_as_sf(filter(country_cells, gid == i_grid)), cid), ged100_sub)
    
    comb100[[i_grid]] <- comb100_sub
  }
  
  comb100 <- rbindlist(comb100)
  #comb100 <- left_join(cells, dplyr::select(comb100, best, int_cat, year, month, cid), by = c("cid", "year", "month"))
  comb100 <- comb100 %>% group_by(cid, year, month) %>%
    summarize(L100 = sum(int_cat == 1, na.rm = TRUE),
              M100 = sum(int_cat == 2, na.rm = TRUE),
              H100 = sum(int_cat == 3, na.rm = TRUE),
              int100 = sum(best, na.rm = TRUE))
  
  #join the intersected cells with "cells" and drop any non-conflict entries
  comb <- left_join(cells, comb25, by = c("cid", "year", "month")) %>%
    left_join(comb50, by = c("cid", "year", "month")) %>%
    left_join(comb100, by = c("cid", "year", "month")) %>%
    filter(int100 > 0) #%>%
  #replace(is.na(.), 0)
  comb[is.na(comb)] <- 0
  
  out_list <- list(comb, country_cells, country_grid)
  if(run_tests(out_list) == 9){
    dbWriteTable(hdr_db, "comb", out_list[[1]], append = TRUE)
    st_write(st_transform(out_list[[2]], crs = crs(out_list[[3]])), hdr_db, "cells", append = TRUE)
    st_write(out_list[[3]], hdr_db, "grid", append = TRUE)
    return(1)
  }else{
    return(out_list)
  }
}
