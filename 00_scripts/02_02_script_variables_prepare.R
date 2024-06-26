#' ---
#' title: plataform sn - variavies - adjust
#' author: mauricio vancine
#' date: 2024-03-23
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(rgrass)

# options
options(timeout = 3e5)

# limit  ------------------------------------------------------------------

# import
neo <- terra::vect("02_data/02_variables/00_limit/neotropic_dissolved_fill_holes.shp")
neo
plot(neo)

# regional -----------------------------------------------------------------

## import ----
climate <- terra::rast(dir(path = "02_data/02_variables/01_regional/00_raw", pattern = "CHELSA_bio", full.names = TRUE))
climate
plot(climate[[1]])

topography <- terra::rast(dir(path = "02_data/02_variables/01_regional/00_raw", pattern = "GMTED", full.names = TRUE))
topography
plot(topography[[1]])

# adjust
climate_neo <- climate %>% 
    terra::crop(neo) %>% 
    terra::mask(neo)

topography_neo <- topography %>% 
    terra::crop(neo) %>% 
    terra::mask(neo)    

topography_neo <- terra::resample(topography_neo, climate_neo[[1]])
topography_neo

# regional
variables_regional <- c(climate_neo, topography_neo)
names(variables_regional) <- c(paste0("bio0", 1:9), paste0("bio", 10:19), "elev", "slope")
names(variables_regional)
variables_regional
plot(variables_regional[[1]])

# regional id
# variables_regional_id <- variables_regional[[1]]
# variables_regional_id[!is.na(variables_regional_id)] <- 1:ncell(variables_regional_id[!is.na(variables_regional_id)])    
# names(variables_regional_id) <- "id"
# plot(variables_regional_id)    

# export
terra::writeRaster(variables_regional_id, "02_data/02_variables/01_regional/01_adjusted/variables_regional_id.tif")
terra::writeRaster(variables_regional, "02_data/02_variables/01_regional/01_adjusted/variables_regional.tif")

# local ---------------------------------------------------------------

## geo ----
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "02_data/02_variables/02_local/grassdb",
                  location = "geo",
                  mapset = "PERMANENT",
                  override = TRUE)

rgrass::execGRASS("r.in.gdal", 
                  input = "02_data/02_variables/02_local/00_raw/brasil_coverage_2022.tif", 
                  output = "brasil_coverage_2022")

rgrass::execGRASS("r.in.gdal", 
                  input = "02_data/02_variables/02_local/00_raw/hii_2020-01-01.tif", 
                  output = "hfp_2020")

rgrass::execGRASS("v.in.ogr", 
                  flags = "overwrite",
                  input = "02_data/02_variables/00_limit/lm_bioma_250_sem_ilhas_diss.shp", 
                  output = "brasil")

# region
rgrass::execGRASS("g.region", flags = c("a", "p"), raster = "brasil_coverage_2022") 
rgrass::execGRASS("g.region", flags = c("a", "p"), res = "00:00:03.5") 

# mask
rgrass::execGRASS("r.mask", flags = "overwrite", vector = "brasil") 

# resample and habitat
rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = "brasil_coverage_2022_100m = brasil_coverage_2022") 
rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = "brasil_coverage_2022_100m_habitat = if(brasil_coverage_2022_100m == 3 | brasil_coverage_2022_100m == 4 | brasil_coverage_2022_100m == 5 | brasil_coverage_2022_100m == 6 | brasil_coverage_2022_100m == 10 | brasil_coverage_2022_100m == 11 | brasil_coverage_2022_100m == 12 | brasil_coverage_2022_100m == 13 | brasil_coverage_2022_100m == 32 | brasil_coverage_2022_100m == 49 | brasil_coverage_2022_100m == 50, 1, 0)")
rgrass::execGRASS("r.mapcalc", flags = c("overwrite"), expression = "brasil_coverage_2022_100m_habitat_null=if(brasil_coverage_2022_100m_habitat == 0, null(), 1)")

rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = "hfp_2020_100m = hfp_2020") 

## fragment area ----
rgrass::execGRASS("r.clump",
                  flags = c("d", "quiet", "overwrite"),
                  input = "brasil_coverage_2022_100m_habitat_null",
                  output = "brasil_coverage_2022_100m_habitat_fid")

rgrass::execGRASS("r.area",
                  flags = c("overwrite"),
                  input = "brasil_coverage_2022_100m_habitat_fid",
                  output = "brasil_coverage_2022_100m_habitat_area")

rgrass::execGRASS("r.mapcalc", 
                  flags = "overwrite", 
                  expression = "brasil_coverage_2022_100m_habitat_area = if(isnull(brasil_coverage_2022_100m_habitat_area), 0, brasil_coverage_2022_100m_habitat_area)")

## fragment distance ----
rgrass::execGRASS("g.region", flags = c("a", "p"), raster = "brasil_coverage_2022", res = "00:00:03.5") 

rgrass::execGRASS("r.grow.distance", flags = "overwrite", input = "brasil_coverage_2022_100m_habitat_null", distance = "brasil_coverage_2022_100m_habitat_distance_outside", metric = "geodesic")
rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("brasil_coverage_2022_100m_habitat_distance_outside=round(brasil_coverage_2022_100m_habitat_distance_outside)"))

rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("brasil_coverage_2022_100m_habitat_null_inv=if(isnull(brasil_coverage_2022_100m_habitat_null), 1, null())"))
rgrass::execGRASS("r.grow.distance", flags = "overwrite", input = "brasil_coverage_2022_100m_habitat_null_inv", distance = "brasil_coverage_2022_100m_habitat_distance_inside", metric = "geodesic")
rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("brasil_coverage_2022_100m_habitat_distance_inside=round(brasil_coverage_2022_100m_habitat_distance_inside*-1)"))

rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = paste0("brasil_coverage_2022_100m_habitat_distance=brasil_coverage_2022_100m_habitat_distance_outside + brasil_coverage_2022_100m_habitat_distance_inside"))

# export
rgrass::execGRASS("r.out.gdal", 
                  flags = "overwrite", 
                  input = "brasil_coverage_2022_100m_habitat", 
                  output = "02_data/02_variables/02_local/01_metrics/brasil_coverage_2022_100m_habitat.tif",
                  createopt = "COMPRESS=DEFLATE") 

rgrass::execGRASS("r.out.gdal", 
                  flags = "overwrite", 
                  input = "brasil_coverage_2022_100m_habitat_area", 
                  output = "02_data/02_variables/02_local/01_metrics/brasil_coverage_2022_100m_habitat_area.tif",
                  createopt = "COMPRESS=DEFLATE") 

rgrass::execGRASS("r.out.gdal", 
                  flags = "overwrite", 
                  input = "brasil_coverage_2022_100m_habitat_distance", 
                  output = "02_data/02_variables/02_local/01_metrics/brasil_coverage_2022_100m_habitat_distance.tif",
                  createopt = "COMPRESS=DEFLATE") 

rgrass::execGRASS("r.out.gdal", 
                  flags = "overwrite", 
                  input = "hfp_2020_100m", 
                  output = "02_data/02_variables/02_local/01_metrics/hfp_2020_100m.tif",
                  createopt = "COMPRESS=DEFLATE") 

# end ---------------------------------------------------------------------