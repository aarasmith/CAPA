# Population raster comparison for save the children
# Set working directory to data folder
#setwd("c:/users/andreas/PRIO Dropbox/Andreas For? Tollefsen/projects/CHILDREN_v2/data/")

library(raster)

pop1990 <- raster(paste0(drop_path, "new_rasters/glp90ag/w001001.adf"))  
pop1995 <- raster(paste0(drop_path, "new_rasters/glp95ag/w001001.adf"))  
pop2000 <- raster(paste0(drop_path, "new_rasters/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2000_2pt5_min.tif"))  
pop2005 <- raster(paste0(drop_path, "new_rasters/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2005_2pt5_min.tif"))   
pop2010 <- raster(paste0(drop_path, "new_rasters/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2010_2pt5_min.tif"))   
pop2015 <- raster(paste0(drop_path, "new_rasters/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2015_2pt5_min.tif"))   
pop2020 <- raster(paste0(drop_path, "new_rasters/gpw_v4_population_count_adjusted_to_2015_unwpp_country_totals_rev11_2020_2pt5_min.tif"))   

pop1990 <- crop(x = pop1990,y = extent(-180,180,-57,84))
pop1995 <- crop(x = pop1995,y = extent(-180,180,-57,84))
pop2000 <- crop(x = pop2000,y = extent(-180,180,-57,84))
pop2005 <- crop(x = pop2005,y = extent(-180,180,-57,84))
pop2010 <- crop(x = pop2010,y = extent(-180,180,-57,84))
pop2015 <- crop(x = pop2015,y = extent(-180,180,-57,84))
pop2020 <- crop(x = pop2020,y = extent(-180,180,-57,84))


s <- stack(pop1990,pop1995,pop2000,pop2005,pop2010,pop2015,pop2020)

source("assembly/lintemp.R")
interpolateTemporal(s = s,xin = c(1990,1995,2000,2005,2010,2015,2020),xout = seq(1990,2020,1),outdir = paste0(drop_path, "new_rasters/ipolated/"),prefix = "ipop",progress = TRUE)
