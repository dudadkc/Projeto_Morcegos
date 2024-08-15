#' ---
#' title: variavies - download
#' author: mauricio vancine
#' date: 2024-03-23
#' ---

# prepare r -------------------------------------------------------------

# packages
library(geodata)

# options
options(timeout = 3e5)

# download ----------------------------------------------------------------

## regional ----

# climate
geodata::worldclim_global(var = "bio", res = "10", path = "01_data/01_variables/")

# topography
# download.file(url = paste0("https://data.earthenv.org/topography/elevation_1KMmd_GMTEDmd.tif"), 
#               destfile = paste0("02_data/02_variables/01_regional/00_raw/elevation_1KMmd_GMTEDmd.tif"), 
#               mode = "wb")
# 
# download.file(url = paste0("https://data.earthenv.org/topography/slope_1KMmd_GMTEDmd.tif"), 
#               destfile = paste0("02_data/02_variables/01_regional/00_raw/slope_1KMmd_GMTEDmd.tif"), 
#               mode = "wb")

# end ---------------------------------------------------------------------