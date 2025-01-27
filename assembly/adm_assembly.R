library(nngeo)
library(future)
library(future.apply)
library(countrycode)
library(bit64)


##### Function for spatially joining cells to ADM shapes by grid sub-setting - formatted for usage with lapply over a vector of GID's as "i"
cell_match <- function(i, grid_ref, cell_ref, adm_ref, join_type, shape_id_ref = NULL){
  grid_subset <- grid_ref[grid_ref$gid == i, ]
  cell_subset <- cell_ref[cell_ref$gid %in% grid_subset$gid, ]
  
  if(!is.null(shape_id_ref)){
    shape_id_subset <- filter(shape_id_ref, gid == i)$capa_id
  }else{
    shape_id_subset <- grid_subset$capa_id
  }
  adm_subset <- adm_ref[adm_ref$capa_id %in% shape_id_subset, ]
  
  if(join_type == "largest"){
    join <- st_join(cell_subset, adm_subset, largest = TRUE, left = FALSE)
  }else if(join_type == "nn"){
    join <- st_join(cell_subset, adm_subset, left = FALSE, join = st_nn, k = 1, progress = F)
  }else{
    stop("invalid join_type: must be 'largest' or 'nn'")
  }
  
  join <- join %>%
    st_drop_geometry()
  return(join)
}

add_shapes <- function(iso, adm, cell_geos, grid_geos){
  #' Attach ADM shapes to cells and load into DB
  #' 
  #' @description Attach ADM shapes to cells by various spatial joins
  #' 
  #' @param iso iso3n numeric for the desired country
  #' @param adm sf dataframe of 3 columns: iso3n, capa_id, and geometry
  #' @param cell_geos sf dataframe of the country indicated by iso: new schema format
  #' @param grid_geos sf dataframe of the country indicated by iso: new schema format
  #' 
  #' @details Attaches ADM info to cells by first intersecting the larger meta-grids with the ADM shapes. Meta-grids only matching one ADM shape have their cells automatically assigned
  #' to the corresponding ADM shape.
  #' 
  #' Meta-grids that overlap multiple ADM shapes are fed into the cell_match function. This iterates over each multi-match meta-grid and spatially
  #' compares the individual cells against a subset of ADM shapes that intersected the meta-grid using st_join(largest = TRUE) to compute coverage ratios and assign cells to the shape
  #' that they overlap with more than any others. 
  #' 
  #' Of that group, some individual cells do not intersect with ADM shapes (e.g. due to being coastal and not quite captured by the ADM shape).
  #' These cells are subset and fed back into cell_match using a nearest neighbor calculation (nngeos::st_nn). 
  #' 
  #' Some meta-grids are entirely outside of the ADM shapes and do not intersect
  #' at all (e.g. small islands, general imprecision, etc.). These meta-grids are compared to their 8 surrounding meta-grids (diagonals included). If any of the adjacent grids picked up
  #' an ADM assignment in the earlier steps, this ADM info is applied to the non-intersected meta-grid and its cells provided there is only one matched ADM.
  #' 
  #' The newly matched meta-grids are added to the list of matched meta-grids and the process is repeated once more to capture meta-grids that were two whole meta-grids away from an initially 
  #' matched meta-grid. 
  #' 
  #' For the meta-grids that matched with multiple ADMs during this process, they and their cells are fed into cell_match with an extra argument (shape_id_ref) for sub-setting the ADMs to compare
  #' and the cells are joined to an ADM using a nearest neighbor calculation.
  
  
  #browser()
  #plan(multisession)
  #####PART ONE#############################################################################################################################################################################
  ##SUBSET AND LOAD DATA
  adm_c <- filter(adm, iso3n == iso) %>%
    dplyr::select(capa_id)
  #country_cells <- st_read(hdr_db, query = glue("SELECT cid, gid, geometry FROM cells WHERE iso3c = '{iso}'"))
  #country_grid <- st_read(hdr_db, query = glue("SELECT gid, geometry FROM grid WHERE iso3c = '{iso}'"))
  cell_geos <- cell_geos %>% dplyr::select(-iso3n)
  grid_geos <- grid_geos %>% dplyr::select(-iso3n)
  
  
  #####PART TWO#############################################################################################################################################################################
  ##This section joins the ADM data to the country meta-grid and assigns ADM data to the first batch of cells whose meta-grids had unambiguous matches as join_grid_single_match
  ##It also defines grid_multi_match for use in Part 3, as well as matched_grids and missing_grids for use in Part 4
  
  ##
  grid_adm_join <- st_join(grid_geos, adm_c)
  
  matched_grids <- grid_adm_join[!is.na(grid_adm_join$capa_id), ]
  missing_grids <- grid_adm_join[is.na(grid_adm_join$capa_id), ]
  ##
  
  ##
  grid_match_count <- grid_adm_join %>%
    filter(!is.na(capa_id)) %>%
    st_drop_geometry() %>%
    group_by(gid) %>%
    summarize(count = n())
  
  grid_single_match <- grid_match_count %>% filter(count == 1)
  grid_multi_match <- grid_match_count %>% filter(count > 1)
  ##
  
  ##JOIN 1
  join_grid_single_match <- cell_geos[cell_geos$gid %in% grid_single_match$gid, ] %>%
    st_drop_geometry() %>%
    left_join(st_drop_geometry(grid_adm_join), by = "gid")
  
  #####PART THREE#########################################################################################################################################################################
  ##This section is for joining ADM data to cells in meta-grids that intersected multiple ADMs and coastal cells.
  ##It begins by (1) joining the meta-grid, country-cells, and ADM data (retaining the meta-grid geometry) as grid_cells_adm_join. This serves as the "grid_ref" argument in all 3
  ##  calls to the cell_match function (joins 2, 3, and 6).
  ##(2) Join 2 is made and (3) the results are added to the results from Join 1 as "join". This is used to (4) define missing_cells and then missing_cells_w_grid for use in (5) Join 3 which 
  ##  primarily targets coastal cells that belong to an ADM intersected meta-grid, but did not themselves intersect with an ADM shape
  
  ## (1)
  grid_cells_adm_join <- grid_geos %>%
    left_join(st_drop_geometry(cell_geos), by = "gid") %>%
    st_join(adm_c)
  ##
  
  ## (2) JOIN 2
  i <- unique(grid_multi_match$gid)
  join_grid_multi_match <- future_lapply(i, cell_match, grid_ref = grid_cells_adm_join, cell_ref = cell_geos, adm_ref = adm_c, join_type = "largest")
  ##
  
  ## (3)
  join <- join_grid_single_match %>% bind_rows(rbindlist(join_grid_multi_match))
  ##
  
  ## (4)
  missing_cells <- cell_geos[cell_geos$sid %!in% join$sid, ]
  missing_cells_w_grid <- missing_cells[missing_cells$gid %!in% missing_grids$gid, ]
  ##
  
  ## (5) JOIN 3
  i <- unique(missing_cells_w_grid$gid)
  join_coastal_cells <- future_lapply(i, cell_match, grid_ref = grid_cells_adm_join, cell_ref = missing_cells_w_grid, adm_ref = adm_c, join_type = "nn")
  ##
  
  #####PART 4#############################################################################################################################################################
  #This section is for matching grids that did not intersect with an ADM shape. It depends on missing_grids, matched_grids, country_cells, 
  ##
  missing_grids_ng <- st_join(dplyr::select(missing_grids, gid), dplyr::select(matched_grids, -gid)) %>%
    filter(!is.na(capa_id)) %>%
    filter(!duplicated(.)) %>%
    st_drop_geometry()
  missing_grids_count <- missing_grids_ng %>%
    group_by(gid) %>%
    summarize(count = n())
  ##
  
  ##
  grid_ng_match <- missing_grids_ng %>% filter(gid %in% filter(missing_grids_count, count == 1)$gid)
  grid_ng_multi <- missing_grids_ng %>% filter(gid %in% filter(missing_grids_count, count > 1)$gid)
  grid_ng_none <- missing_grids %>% filter(gid %!in% missing_grids_ng$gid)
  ##
  
  ##FOURTH JOIN
  join_grid_ng_match <- cell_geos[cell_geos$gid %in% grid_ng_match$gid, ] %>%
    st_drop_geometry() %>%
    left_join(filter(missing_grids_ng, gid %in% grid_ng_match$gid), by = "gid")
  ##
  
  ##
  grid_ng_match_round2 <- left_join(grid_ng_match, grid_geos, by = "gid") %>% st_as_sf()
  missing_grids_ng2 <- st_join(dplyr::select(grid_ng_none, gid), dplyr::select(grid_ng_match_round2, -gid)) %>%
    filter(!is.na(capa_id)) %>%
    filter(!duplicated(.)) %>%
    st_drop_geometry()
  ##
  
  ##FIFTH JOIN
  join_grid_ng_match2 <- cell_geos[cell_geos$gid %in% grid_ng_none$gid, ] %>%
    st_drop_geometry() %>%
    left_join(filter(missing_grids_ng2, gid %in% grid_ng_none$gid), by = "gid")
  ##
  
  ##SIXTH JOIN
  i <- unique(grid_ng_multi$gid)
  join_grid_ng_multi <- future_lapply(i, cell_match, grid_ref = grid_cells_adm_join, cell_ref = cell_geos, adm_ref = adm_c, shape_id_ref = grid_ng_multi, join_type = "nn")
  ##
  
  join <- join %>%
    bind_rows(rbindlist(join_coastal_cells)) %>%
    bind_rows(join_grid_ng_match) %>%
    bind_rows(join_grid_ng_match2) %>%
    bind_rows(rbindlist(join_grid_ng_multi))
  
  join <- join %>%
    dplyr::select(sid, capa_id)
  
  return(join)
  
}
