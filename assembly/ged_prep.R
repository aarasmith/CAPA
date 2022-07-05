




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


generate_ged <- function(drop_path, buffer_size = 50000, thresh = 1){
  ged <- fread(paste0(drop_path, "GEDEvent_v22_1.csv"))
  ged <- st_as_sf(ged, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
  ged <- as_Spatial(ged)
  ged <- sp::spTransform(ged, CRSobj = "+proj=eck6 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
  ged <- rgeos::gBuffer(spgeom = ged, byid = TRUE, width = buffer_size)
  #ged <- sp::spTransform(ged,CRSobj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  ged <- st_as_sf(ged)
  ged <- st_transform(ged,crs = "WGS84")
  
  ged$month <- as.numeric(substr(ged$date_start, 6, 7))
  ged$iso3c <- countrycode(ged$country_id, origin = "gwn", destination = "iso3c")
  
  ged <- filter(ged, best >= thresh)
  ged <- ged %>% mutate(iso3c = ifelse(country_id == 678, "YEM", iso3c))
  ged <- ged %>% mutate(int_cat = case_when(best <= 10 ~ 1,
                                            (best > 10) & (best < 25) ~ 2,
                                            best >= 25 ~ 3))
  return(ged)
}