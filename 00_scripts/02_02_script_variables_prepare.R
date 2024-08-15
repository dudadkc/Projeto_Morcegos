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
neo <- terra::vect("01_data/01_variables/limit/neotropic_dissolved_fill_holes.shp")
neo
plot(neo)

# regional -----------------------------------------------------------------

## import ----
climate <- geodata::worldclim_global(var = "bio", res = "10", path = "01_data/01_variables/")
climate
plot(climate[[1]])

# topography <- terra::rast(dir(path = "02_data/02_variables/01_regional/00_raw", pattern = "GMTED", full.names = TRUE))
# topography
# plot(topography[[1]])

# adjust
climate_neo <- climate %>% 
    terra::crop(neo) %>% 
    terra::mask(neo)

# topography_neo <- topography %>% 
#     terra::crop(neo) %>% 
#     terra::mask(neo)    

# topography_neo <- terra::resample(topography_neo, climate_neo[[1]])
# topography_neo

# regional
# variables_regional <- c(climate_neo, topography_neo)
variables_regional <- climate_neo
# names(variables_regional) <- c(paste0("bio0", 1:9), paste0("bio", 10:19), "elev", "slope")
names(variables_regional) <- c(paste0("bio0", 1:9), paste0("bio", 10:19))
names(variables_regional)
variables_regional
plot(variables_regional[[1]])

# regional id
# variables_regional_id <- variables_regional[[1]]
# variables_regional_id[!is.na(variables_regional_id)] <- 1:ncell(variables_regional_id[!is.na(variables_regional_id)])    
# names(variables_regional_id) <- "id"
# plot(variables_regional_id)    

# export
# terra::writeRaster(variables_regional_id, "02_data/02_variables/01_regional/01_adjusted/variables_regional_id.tif")
terra::writeRaster(variables_regional, "01_data/01_variables/climate/wc_21_neotropic.tif")

# end ---------------------------------------------------------------------