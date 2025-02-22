Author: Andrew Arasmith

Date: 2022/08/16

# Welcome to CAPA

## What is CAPA?

CAPA stands for Conflict Affected Populations App. It was developed as a new iteration of the scripts written by Andreas Foro Tollefson for the Children at Risk project in collaboration with Save The Children. This iteration is designed to be flexible, reproducible, and updatable. The current use-cases are for annually updating the SAVE project and for the UNDP HDRO project.

## What does CAPA do?

CAPA measures the number of people affected by conflict with respect to space (spatial) and time (temporal). **People** are measured with global grid-based (raster) data containing yearly population counts. Each **grid-cell** is 2.5 Arc-Minutes which translates to roughly 5 square kilometers at the equator. **Conflict** is measured using point-based conflict-event-location data. These points are "buffered" into 3 sets of polygons having 25km, 50km, and 100km radii for each event. The conflict events have estimates of the number of battle-related fatalities and a date associated with each event. These are used to create 3 separate **event-fatality** categories: low-fatality (1-9 deaths), medium-fatality (10-24 deaths), and high-fatality (25+ deaths).

The buffered conflict events are overlaid onto the population grid-cells and each grid receives a count of the number of low, medium, and high-fatality events that were within 25km, 50km, and 100km of the grid-cell (9 counts). The raw number of battle-deaths for each grid-cell is also counted at all 3 distances giving us a total of 12 counts. This is done on a monthly basis for each grid-cell.

Therefore the data at its core has a maximum *spatial* resolution of 2.5 Arc-Minutes and a maximum *temporal* resolution of "monthly". Using these 12 counts we can calculate sub-national **intensity of exposure**, **duration of exposure**, and **frequency of exposure**. We can also *temporally* aggregate upwards to quarters, halves, years, etc. and *spatially* aggregate upwards to Admin 2, Admin 1, Country, Region, or Global.

## What data does CAPA use

CAPA uses CIESIN Gridded Population of the World (GPW) data for the population grid-cell (raster) data. GPW V3 provides data for 1990 and 1995, while GPW v4 is used for 2000, 2005, 2010, 2015, and 2020. The intermediate years are determined using linear interpolation and assume constant rate of change in-between source data. This process takes place in the `lintemp.R` and `interpolate_rasters.R` files. Country boundaries use the GPW National Identifier Grid for determining which grid-cell belongs to which country.

UCDP GED V22.1 is used for the conflict event data. The data is buffered using a WGS84/EPSG:4326 projection. Some data is excluded due to not qualifying as part of an active conflict. Events that take place in a country do not affect populations in other countries (despite proximity) except in special cases. These augmentations take place in the `ged_prep.R` file.

geoBoundaries CGAZ data is used for global Administrative Level 1 boundaries for the purposes of sub-national aggregations. Preparations made to this data is found in the `cgaz.R` file.

For calculations involving Children at Risk, the UN World Population Prospectus 2019 is used. 1990-2020 are based on estimates and 2021 onward are based on projections.

## What does CAPA actually consist of?

CAPA is:

1.  A collection of R scripts that generate and assemble the data

2.  A dockerized PostgreSQL database running PostGIS for storing and serving the data

3.  A collection of R scripts using R and string-formatted dynamic SQL queries to aggregate, retrieve, and transform the data from the database into specific insights

4.  A collection of R scripts to create an R Shiny dashboard for end-users to access the data

5.  A Docker Image for an R Shiny app container

6.  A Docker Image for Shiny-proxy to load-balance and spin up app-containers for each user

7.  A Docker-compose file for deploying Shiny-proxy with Traefik to be available at hdro.prio.org - See Martin Tegnander.

8.  A bash script for redeploying the app when updates are made

9.  An environment file with secrets/credentials

## I just want to update the data for the new year. What do I do?

We have to start with the population rasters. Check to see if CIESIN has released new GPW data (doubtful unless you're living in the year 2026). To create a new pop_rast year we need to go to the `interpolate_rasters.R` script which uses `lintemp.R`. I didn't write the lintemp script (source: https://gist.github.com/johnbaums/10465462 ), but it's straight-forward.

Here is the code I used to update for 2021:
```
    library(raster)
    source('assembly/lintemp.R')

    #read in the last 2 years of pop data
    pop2019 <- raster(paste0(drop_path, "new_rasters/ipolated/ipop_2019.tif"))   
    pop2020 <- raster(paste0(drop_path, "new_rasters/ipolated/ipop_2020.tif"))

    #turn them into a raster stack
    s <- stack(pop2019, pop2020)

    #Run the function sourced from lintemp.R with your raster stack. xin is the years of the raster stack (s), xout is a sequence of years from beginning of the existing data to the end of the desired interpolation. I saved them in a different folder because it generates change-grids and the input rasters, so I save it all here and then move ipop_2021.tif to my folder containing all the pop rasters (new_rasters/ipolated/). Prefix should be "ipop" to match the pattern of the other pop rasts (ipop_1990.tif, ipop_1991.tif, etc.)

    interpolateTemporal(s = s,xin = c(2019,2020),xout = seq(2019,2021,1),outdir = paste0(drop_path, "new_rasters1/ipolated/"),prefix = "ipop",progress = TRUE)
```
Next we need the latest version of UCDP GED. For this we use `ged_prep.R`. The first function to use is `generate_ged`. We need to run it 3 times with the 3 different buffer distances and apply the cleaning functions `ged_country_fix` and `non_conflict`. `ged_country_fix` deals with things like fixing 'Yugoslavia' events so that they are duplicated for each former Yugoslav state and each duplicate given the appropriate iso3c code and creating dupes in a similar way to explicitly *pierce* national boundaries in cases like Israel-Palestine or pre-2011 Sudan/South Sudan. `non_conflict` removes events like the Manchester bombing, Madrid bombings, Pulse nightclub shooting, and other one-off terrorist events that shouldn't be considered as putting the entire city 'at risk of conflict' so to speak. These operations are all subjective and should be discussed by co-authors and edited/acknowledged accordingly.

The code for doing this looks like:
```
    ged_path <- paste0(drop_path, "GEDEvent_v22_1.csv")

    ged25 <- generate_ged(ged_path, buffer_size = 25000, thresh = 1) %>% ged_country_fix() %>% non_conflict()
    ged50 <- generate_ged(ged_path, buffer_size = 50000, thresh = 1) %>% ged_country_fix() %>% non_conflict()
    ged100 <- generate_ged(ged_path, buffer_size = 100000, thresh = 1) %>% ged_country_fix() %>% non_conflict()

    #I recommend saving these as .RDS for ease of use etc.
    saveRDS(ged25, paste0(drop_path, "ged25.RDS"))
```
Before updating the database I suggest making a backup. The simplest way to currently do this is to connect to the database in PG Admin, right click the database, and create a backup. It is probably best to make the backup without an owner or priviledges (uncheck these options). This will download a backup to the machine you're running PG Admin on. At the time of writing the DB is about 56GB and a backup takes about 5 minutes and yields a file around 1.5GB. Restoring will take about 1.5 hours, which is vastly shorter than the time required to rebuild the data. 

If you're tasked with maintaining this data over an extended period, I would highly suggest lobbying for the resources to implement a more robust backup/restore solution. This could be having the System Admin provision more storage on the server to run a secondary DB on the CAPA server, or better yet, a modestly priced cloud deployment into AWS RDS (~$10/month) where you can simply create on-demand snapshots.

To update the DB, first we want to load in the population raster/s into a list:
```
    poprast_list <- lapply(raster_files, raster)
    #e.g. raster_files <- list("C:/Users/andara/PRIO Dropbox/Andrew Arasmith/R Scripts/HDR/new_rasters1/ipolated/ipop_2021.tif")
```    
You can then pass all the data from these last few steps into `update_from_pops` function found in the `assembly.R file`. This is a wrapper function that calls the required functions from the `data_generation.R` file to update the DB. 

*NOTE* if there are events in the new GED data that take place in a country that has never previously been in the GED dataset (or it was previously excluded with the `non_conflict` function e.g. USA) *YOU MUST* generate grid-cells for the country and add them to the DB. You can check which countries have grid-cells by querying the DB:
```
    dbGetQuery(capa_db, "SELECT DISTINCT iso3n FROM grid_geos) %>% isoc()
    #isoc is a utility funciton that simplifies the iso3n -> iso3c conversion with the countrycodes package found in `CAPA/R/utils-iso_handlers.R`
```
If you need to add a country, you will have to run steps 2 and 3 in `data_generation.R` for each country you wish to add and append the resultant `cell_geos`, `grid_geos`, and `cell_pops` to the DB before continuing

Assuming all the required cell data is present in the DB, we will now use `update_from_pops` to update the data. If you want to be cautious, you can run this on 1 country or a small list of countries and set the `write_db` parameter to NULL in order to look at the data before actually committing it to the DB. Once you're satisfied, you can run lapply over the function with the `write_db` set and it will automatically generate everything and commit to the DB.

To update for 2021 I ran the following:
 ```   
    #test
    capa_db <- connect_to_capa() #source("CAPA/R/db_handlers.R") - creds found in the .env file - load_dot_env()
    nid_grid <- readRDS("data/nid_grid.RDS")
    vector_of_isos <- c("AFG", "SYR", "IRQ")
    test <- update_from_pops(vector_of_isos, year_range = c(2021), nid_grid, poprast_list, ged25, ged50, ged100, source_db = capa_db, write_db = NULL)
    
    #actual run
    lapply(vector_of_all_isos, update_from_pops, year_range = c(2021), nid_grid, poprast_list, ged25, ged50, ged100, source_db = capa_db, write_db = capa_db)
 ```   
If the GED update includes backchanges to previous years (very likely) you will need to update only the pops from the newest year and then entirely rebuild the stats table. You can do this with `update_pops` for the newest year and then `update_from_stats` for all years. You'll need to drop the cell_stats table before doing this (always good to backup). The cell_stats table has multiple dependent materialized views that will also need dropping and rebuilding. The rebuilds can be found in `CAPA/assembly/schema.R`. This will take some time to run and I suggest you use parallel processing by running plan(multisession) before executing the `update_from_stats`
```
    lapply(vector_of_all_isos, update_pops, year_range = c(2021), nid_grid, poprast_list, source_db = capa_db, write_db = capa_db)
    dbExecute(capa_db, "DROP TABLE cell_stats")
    source("assembly/schema.R")
    dbExecute(capa_db, cell_stats_schema)
    lapply(vector_of_all_isos, update_from_stats, year_range = c(1990:2021), nid_grid, ged25, ged50, ged100, source_db = capa_db, write_db = capa_db)
    dbExecute(capa_db, cell_stats_qu_view)
    dbExecute(capa_db, cell_stats_bi_view)
    dbExecute(capa_db, cell_stats_yr_view)
```

## Deployment administration notes

If you have the domain name (i.e. hdro.prio.org) set as the database host address in the .env file instead of the IP address (currently: 192.168.8.150), after the app crashes once, for some reason you will get `Error: could not translate host name 'hdro.prio.org' to address: Temporary failure in name resolution` for an unspecified period of time. This will not show up in any app logs you write, nor any shinyproxy logs. The only way to see these is to `sudo docker logs <container id>` of the broken container before it gets terminated.

*HOWEVER*, in my prior investigation, the app gets about to the 8th or 9th (out of 10) try to start during a normal launch, so if the app load time increases you may run into issues and you will need to update the application.yml in the shinyproxy folder to increase the `container-wait-time` parameter in order to ensure shinyproxy gives it enough time to spin up. I've increased this from 20 seconds to 30 seconds to provide some wiggle room.

## Troubleshooting

Here is the current workflow for debugging and troubleshooting issues on the server:

shinyproxy logs are not saving properly right now. To access shinyproxy logs you need to run `sudo docker logs <container id of shinyproxy>` . Shinyproxy must still be running for this and they will be deleted (or I suspect difficult to find) once the container is terminated. Although, historical versions of these logs will not be very interesting unless you're interested in monitoring who is logging in and using what. These logs will also show you startup info for the service and will show you the attempts to spin up app proxy containers. So if you're having problems accessing shinyproxy, launching it, or getting container timeouts, these will be helpful.

container logs: These logs are the most useful for figuring out why a container is experiencing issues or crashing *ESPECIALLY* if this is only happening when running online and not locally in R. They will save the std error and std out for the container session *inside the shinyproxy container*. This means you need to `sudo docker exec -it <container id of shinyproxy> bash` from the host to get a shell inside the container. This will drop you in /opt/shinyproxy and you will see the `container-logs` folder that contains all the container logs with the proxy instance ID and the date (YMD) as a filename. The std error is what you will be interested in.

app logs: The app is also using shinylogs to write out app logs in json format. Check out the shinylogs package documentation for details. This is mostly useful for if this gets scaled out and you need to analyze usage/access patterns. The error logging is very lackluster, so you will need container logs to figure out what the crash error was, but this will show you all the buttons your user pressed, inputs they made, etc. so you can figure out how they managed to crash it this time. The shiny app saves these into the /root/logs folder of the proxy container and this is mounted to the host server at /var/log/shinylogs

It is useful to note that container logs are configured in the application.yml file attached to the shinyproxy image when it is built. Shinyproxy logs would in theory be configured here as well if they were working. App logs are configured within the app itself and a destination directory for them is created when building the app docker image and then configured to mount onto the host in the shinyproxy's application.yml file.
