Sys.setenv("_SF_USE_S2" = "false")
#parallel processes in future_lapply will use S2 (slower, unnecessary, changes results...) if this isn't set

nid_grid
library(countrycode)
library(raster)
library(exactextractr)
library(purrr)
library(future)
library(future.apply)
library(bit64)
ged25 <- readRDS(paste0(drop_path, "ged25.RDS"))
ged50 <- readRDS(paste0(drop_path, "ged50.RDS"))
ged100 <- readRDS(paste0(drop_path, "ged100.RDS"))
ged25 <- readRDS(paste0(drop_path, "ged25_22.RDS"))
ged50 <- readRDS(paste0(drop_path, "ged50_22.RDS"))
ged100 <- readRDS(paste0(drop_path, "ged100_22.RDS"))

raster_files <- list.files("C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/new_rasters/ipolated", pattern = "tif", full.names = TRUE)
poprast_list <- lapply(raster_files, raster)

new_poprast_list <- list(raster("C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/new_rasters1/ipolated/ipop_2021.tif"))

####STEP 1####
#Crop and mask global rasters to specified country
subset_rasters <- function(iso, nid_grid, poprast_list){
  
  #Get the GPW country shape
  country_shape <- filter(nid_grid, ISOCODE == iso)
  
  #Crop/mask all the rasters to the country shape
  poprast_list_c <- lapply(poprast_list, crop, y = extent(country_shape))
  poprast_list_c <- lapply(poprast_list_c, mask, mask = country_shape)
  
  return(poprast_list_c)
}

####STEP 2####
#Generate the cell and meta-grid geometries from the population rasters

##DEPENDENCY FUNCTIONS##

#Function for joining the grid-cells to the meta-grid
join_cells_to_grid <- function(country_polys, grid_geos){
  
  poly_join_with_dupes <- st_join(country_polys, grid_geos)
  poly_dupes <- country_polys %>% filter(cid %in% poly_join_with_dupes$cid[duplicated(poly_join_with_dupes$cid)])
  poly_join_no_dupes <- poly_join_with_dupes %>% filter(cid %!in% poly_dupes$cid)
  dupe_join <- st_join(poly_dupes, grid_geos, largest = TRUE)
  poly_join <- bind_rows(poly_join_no_dupes, dupe_join)
  
  return(poly_join)
}

##MAIN FUNCTION##
generate_geos <- function(iso3n, poprast_list_c){
  
  #Create grid-cell vector geometry out of rasters
  country_polys <- poprast_list_c[[length(poprast_list_c)]] %>% as("SpatialPolygonsDataFrame") %>% st_as_sf() %>% cbind(data.frame(cid = 1:nrow(.))) %>% dplyr::select(-contains("ipop"))
  
  #Create a meta-grid over the grid-cells
  grid_geos <- st_make_grid(country_shape, cellsize = 0.5) %>%
    st_as_sf() %>%
    rename(geometry = x) %>%
    filter(st_intersects(geometry, country_shape, sparse = FALSE)) %>%
    mutate(gid = 1:nrow(.), iso3n = iso3n)
  
  #Join grid-cells to meta-grid and drop non-intersecting meta-grid entries
  cell_geos <- join_cells_to_grid(country_polys, grid_geos)
  
  grid_geos <- grid_geos %>% filter(gid %in% cell_geos$gid)
  grid_geos$new_gid <- 1:nrow(grid_geos)
  cell_geos <- cell_geos %>%
    left_join(grid_geos %>% st_drop_geometry() %>% dplyr::select(gid, new_gid), by = "gid") %>%
    dplyr::select(-gid) %>%
    rename(gid = new_gid)
  
  cell_geos <- cell_geos %>%
    mutate(cid = as.numeric(paste0(iso3n + 100, cid + 1000000))) %>%
    rename(sid = cid)
  
  return(list(grid_geos, cell_geos))
  
}

####STEP 3####
#Generate the cell_pops table with yearly cell_pop counts

##DEPENDENCY FUNCTIONS##

#i = 1:length(poprast_list_c)
extract_pops <- function(i, cell_geos, var_list, poprast_list_c){
  cell_pops <- cell_geos %>% mutate(!!var_list[i] := round(exact_extract(poprast_list_c[[i]], cell_geos, fun = "sum", progress = FALSE))) %>%
    st_drop_geometry() %>%
    dplyr::select(sid, contains("ipop"))
  return(cell_pops)
}

##MAIN FUNCTION##
generate_pops <- function(cell_geos, poprast_list_c, year_range){
  
  #Extract pop data from the rasters to the vector-geometry grid-cells
  var_list <- paste0("ipop", year_range)
  
  cell_pops <- list(cell_geos) %>%
    append(future_lapply(1:length(poprast_list_c), extract_pops, cell_geos = cell_geos, var_list = var_list, poprast_list_c = poprast_list_c)) %>%
    reduce(left_join, by = "sid")

  cell_pops <- reshape2::melt(st_drop_geometry(cell_pops), id.vars = c(1:3), variable.name = "year", value.name = "cell_pop") %>%
    dplyr::select(-gid) %>%
    as.data.table() %>%
    .[ , year := as.numeric(substr(year, 5, 8))]
  #cell_pops$year <- as.numeric(cell_pops$year)
  
  return(cell_pops)
}

####STEP 4####
#Generate the cell_stats table

##DEPENDENCY FUNCTIONS##

subset_ged <- function(iso, ged, year_range){
  ged_c <- ged %>%
    filter(iso3c == iso) %>%
    filter(year %in% year_range) %>%
    dplyr::select(int_cat, best, year, month, id)
  
  return(ged_c)
}

ged_intersect <- function(i_grid, ged_grid, cell_geos){
  ged_sub <- filter(ged_grid, gid == i_grid)
  comb_sub <- st_join(dplyr::select(filter(cell_geos, gid == i_grid), sid), ged_sub) %>%
    st_drop_geometry()
  return(comb_sub)
}

generate_comb <- function(comb, ged_dist){
  #adds distance suffix to names 4-7, so change this selection if adding new variables
  comb <- rbindlist(comb) %>% 
    group_by(sid, year, month) %>%
    summarize(lo = sum(int_cat == 1, na.rm = TRUE),
              md = sum(int_cat == 2, na.rm = TRUE),
              hi = sum(int_cat == 3, na.rm = TRUE),
              int = sum(best, na.rm = TRUE))
  names(comb)[4:7] <- paste(names(comb)[4:7], ged_dist, sep = "_")
  
  return(comb)
}

##MAIN FUNCTION##
generate_stats <- function(iso, year_range, ged25, ged50, ged100, grid_geos, cell_geos, cell_pops){
  
  #Filter geds
  ged25_c <- subset_ged(iso, ged25, year_range)
  ged50_c <- subset_ged(iso, ged50, year_range)
  ged100_c <- subset_ged(iso, ged100, year_range)
  
  #subset ged by joining with meta-grid
  grid25 <- st_join(ged25_c, grid_geos) %>%
    .[!is.na(.$gid), ]
  grid50 <- st_join(ged50_c, grid_geos) %>%
    .[!is.na(.$gid), ]
  grid100 <- st_join(ged100_c, grid_geos) %>%
    .[!is.na(.$gid), ]
  
  #any meta-grid with no conflict intersections at the 100km distance does not need to be run
  grid_sub <- unique(grid100$gid)
  
  if(sum(is.na(grid_sub)) > 0){
    print("FAILED")
    return()
  }
  
  #DO NOT PASS A FUNCTION DEFINED WITHIN THIS FUNCTION HOLY SHIT JUST DECLARE IT OUTSIDE OR YOU WILL HAVE A BAD TIME
  comb25 <- future_lapply(grid_sub, ged_intersect, ged_grid = grid25, cell_geos = cell_geos) %>%
    generate_comb(ged_dist = "25")
  comb50 <- future_lapply(grid_sub, ged_intersect, ged_grid = grid50, cell_geos = cell_geos) %>%
    generate_comb(ged_dist = "50")
  comb100 <- future_lapply(grid_sub, ged_intersect, ged_grid = grid100, cell_geos = cell_geos) %>%
    generate_comb(ged_dist = "100")
  
  #create the monthly framework for each grid-cell. Grid-cells in non-conflict meta-grids are excluded to save space/time
  months <- 1:12
  cell_stats <- cell_pops[cell_pops$sid %in% st_drop_geometry(cell_geos)[cell_geos$gid %in% grid_sub, "sid"]] %>%
    .[rep(1:.N, 12)] %>%
    .[ , month := months, by = list(sid, year)] %>%
    setorder(sid, year, month) %>%
    .[ , month_abs := c(1:(12 * length(year_range))), by = list(sid)]
  
  #join the intersected cells with "cell_stats" and drop any non-conflict entries
  cell_stats <- list(cell_stats, comb25, comb50, comb100) %>%
    reduce(left_join, by = c("sid", "year", "month")) %>%
    filter(int_100 > 0)
  cell_stats[is.na(cell_stats)] <- 0
  
  return(cell_stats)
}

