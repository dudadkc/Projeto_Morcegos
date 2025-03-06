#' ---
#' title: plataform sn - variavies - adjust
#' author: mauricio vancine
#' date: 2024-03-23
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(terra)

# options
options(timeout = 3e5)

# limit  ------------------------------------------------------------------

# import
neo <- terra::vect("01_data/01_variables/00_limits/neotropic_dissolved_fill_holes.shp")
neo
plot(neo)

# variables --------------------------------------------------------------

## climate ----
climate_list <- dir(path = "01_data/01_variables/01_climate/00_raw/",
                    pattern = ".tif", full.names = TRUE)
climate_list

climate <- terra::rast(climate_list)
climate
plot(climate[[1]])

## topography ----
topography_list <- dir(path = "01_data/01_variables/02_topography/00_raw/", 
                  pattern = ".tif", full.names = TRUE)
topography_list

topography <- terra::rast(topography_list)
topography
plot(topography[[1]])

## hydrology ----
hydrology <- terra::rast("01_data/01_variables/03_hidrology/00_raw/monthly_tmin_average.nc")[[1]]
hydrology
plot(hydrology)

## caves ----
caves <- terra::rast("01_data/01_variables/04_caves/00_raw/karst_resample_1km_wgs84.tif")
caves
plot(caves)

## adjust ----
climate_neo <- climate %>% 
    terra::crop(neo) %>% 
    terra::mask(neo)
climate_neo
plot(climate_neo[[1]])

topography_neo <- topography %>%
    terra::crop(neo) %>%
    terra::mask(neo)
topography_neo
plot(topography_neo[[1]])

caves_neo <- caves %>%
    terra::crop(neo) %>%
    terra::mask(neo)
plot(caves_neo)

hydrology_neo <- hydrology %>%
    terra::crop(neo) %>%
    terra::mask(neo)
hydrology_neo[!is.na(hydrology_neo)] <- 1
hydrology_neo
plot(hydrology_neo)

## distance ----
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  SG = hydrology_neo,
                  gisDbase = "01_data/01_variables/grassdb",
                  location = "newLocation",
                  mapset = "PERMANENT",
                  override = TRUE)

rgrass::execGRASS(cmd = "v.import", flags = "overwrite", input = "01_data/01_variables/00_limits/neotropic_dissolved_fill_holes.shp", output = "neo")
rgrass::write_RAST(x = hydrology_neo, flags = "overwrite", vname = "hydrology_neo")

rgrass::execGRASS(cmd = "r.grow.distance", flags = "overwrite", input = "hydrology_neo", distance = "hydrology_neo_distance", metric = "geodesic")
rgrass::execGRASS(cmd = "r.mapcalc", flags = "overwrite", expression = "hydrology_neo_distance_int=int(hydrology_neo_distance)")

rgrass::execGRASS(cmd = "r.mask", vector = "neo")

hydrology_neo <- rgrass::read_RAST("hydrology_neo_distance_int")

## names ----
names(climate_neo) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(climate_neo)
climate_neo

names(topography_neo) <- c("aspect", "elev", "slope")
names(topography_neo)
topography_neo

names(caves_neo) <- "caves"
names(caves_neo)
caves_neo

names(hydrology_neo) <- "hydrology"
names(hydrology_neo)
hydrology_neo

# export
terra::writeRaster(climate_neo, "01_data/01_variables/01_climate/01_adjusted/climate_neo.tif")
terra::writeRaster(topography_neo, "01_data/01_variables/02_topography/01_adjusted/topography_neo.tif")
terra::writeRaster(hydrology_neo, "01_data/01_variables/03_hidrology/01_adjusted/hydrology_neo.tif")
terra::writeRaster(caves_neo, "01_data/01_variables/04_caves/01_adjusted/caves_neo.tif")

# end ---------------------------------------------------------------------