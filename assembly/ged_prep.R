




ged_country_fix <- function(ged){
  ged_ssd <- ged %>% filter(country == "Sudan" & (year < 2012)) %>% # until july 2011
    filter(!(year == 2011 & month > 7)) %>%
    mutate(iso3c = "SSD")
  ged_yugo <- ged %>% filter(country_id == 345) #croatia/slovenia/serbia/nmacedonia/bosnia/
  ged_croatia <- ged_yugo %>% mutate(iso3c = "HRV")
  ged_slovenia <- ged_yugo %>% mutate(iso3c = "SVN")
  ged_serbia <- ged_yugo %>% mutate(iso3c = "SRB")
  ged_montenegro <- ged_yugo %>% mutate(iso3c = "MNE")
  ged_macedonia <- ged_yugo %>% mutate(iso3c = "MKD")
  ged_bosnia <- ged_yugo %>% mutate(iso3c = "BIH")
  ged_kosovo <- ged_yugo %>% mutate(iso3c = "KOS")
  ged_eritrea <- ged %>% filter(country_id == 530 & (year < 2001)) %>% #531 - until Dec 2000
    mutate(iso3c = "ERI")
  ged_namibia <- ged %>% filter(country_id == 560 & (year < 1991)) %>% #angola conflict gwnoa
    filter(!(year == 1990 & month > 3)) %>%
    mutate(iso3c = "NAM")
  ged_soviets <- ged %>% filter(country_id == 365) #azerbaijan armenia 1990/1991
  ged_azerbaijan <- ged_soviets %>% filter(year %in% c(1989, 1990, 1991)) %>% mutate(iso3c = "AZE")
  ged_azerbaijan2 <- filter(ged, iso3c == "ARM")
  ged_armenia <- ged_soviets %>% filter(year %in% c(1989, 1990, 1991)) %>% mutate(iso3c = "ARM")
  ged_armenia2 <- filter(ged, iso3c == "AZE")
  ged_palestine <- ged %>% filter(country_id == 666) %>% #palestine
    mutate(iso3c = "PSE")
  ged_wsh <- ged %>% filter(country_id == 600) %>%
    mutate(iso3c = "ESH")
  #ged_test1 <- ged %>% filter(country_id == 551)
  
  ged_list <- list(ged_ssd, ged_croatia, ged_slovenia, ged_serbia, ged_montenegro, ged_macedonia, ged_bosnia, ged_kosovo, ged_eritrea, ged_namibia, ged_azerbaijan, ged_armenia, ged_palestine,
                   ged_wsh, ged_azerbaijan2, ged_armenia2)
  ged_fix <- rbindlist(ged_list) %>% st_as_sf()
  ged_out <- bind_rows(ged, ged_fix) %>%
    filter(!is.na(iso3c)) %>%
    filter(year > 1989)
  return(ged_out)
}

non_conflict <- function(ged){
  nc_names <- c("Australia", "Belgium", "France", "Netherlands", "United States", "Austria", "Germany", "United Arab Emirates", "Sweden", "Benin", "Bhutan", "Botswana", "Switzerland")
  nc_codes <- c(900, 211, 220, 210, 2, 305, 260, 696, 380, 434, 760, 571, 225)
  ged_nc <- ged %>% filter(country_id %!in% nc_codes)
  #Spain
  ged_nc <- ged_nc %>% filter(!(country_id == 230 & year > 2016))
  #uk
  ged_nc <- ged_nc %>% filter(!(country_id == 200 & year > 1999))
  #Tunisia
  ged_nc <- ged_nc %>% filter(!(country_id == 616 & year == 2002))
  
  return(ged_nc)
}

ged_path <- paste0(drop_path, "GEDEvent_v22_1.csv")

generate_ged <- function(ged_path, buffer_size = 50000, thresh = 1){
  #' Generate the GED file
  #' 
  #' @description Reads UCDP GED csv, converts to sf, buffers events, adds month and iso3c column, filters non-fatal events, fixes Yemen, and creates event-intensity categories 
  #' 
  #' @param ged_path character: path to ged csv
  #' @param buffer_size numeric: number of meters to buffer the event points by
  #' @param thresh numeric: minimum number of battle-related deaths in the 'best' column required (default = 1)
  #' 
  #' @details This function reads in a csv file of UCDP GED, converts it to an sf object, converts that to an sp object with meters as units, buffers by `buffer_size` using rgeos::gBuffer, and
  #' converts back to sf. It then creates a `month` column by taking a substring of the `date_start` column. Then it uses countrycode::countrycode to create an `iso3c` column before filtering
  #' out entries that have fatalities (`best` column) less than the `thresh` input and fixing the Yemen/North Yemen issue. Finally, it creates an `int_cat` column based on the number of fatalities
  #' per event.
  
  ged <- fread(ged_path)
  ged <- st_as_sf(ged, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
  if(buffer_size > 0){
    ged <- as_Spatial(ged)
    ged <- sp::spTransform(ged, CRSobj = "+proj=eck6 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
    ged <- rgeos::gBuffer(spgeom = ged, byid = TRUE, width = buffer_size)
    #ged <- sp::spTransform(ged,CRSobj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    ged <- st_as_sf(ged)
    ged <- st_transform(ged,crs = "WGS84")
  }
  
  ged$month <- as.numeric(substr(ged$date_start, 6, 7))
  ged$iso3c <- countrycode(ged$country_id, origin = "gwn", destination = "iso3c")
  
  ged <- filter(ged, best >= thresh)
  ged <- ged %>% mutate(iso3c = ifelse(country_id == 678, "YEM", iso3c))
  ged <- ged %>% mutate(int_cat = case_when(best <= 9 ~ 1,
                                            (best >= 10) & (best <= 24) ~ 2,
                                            best >= 25 ~ 3))
  return(ged)
}
