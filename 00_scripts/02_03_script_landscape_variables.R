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
library(lsmetrics)

# options
options(timeout = 3e5)

# import ------------------------------------------------------------------

## grassdb ----
rgrass::initGRASS(gisBase = "C:/Program Files/GRASS GIS 8.3",
                  gisDbase = "02_data/02_variables/02_landscape/grassdb",
                  location = "sirgas2000_brazil_policonic",
                  mapset = "PERMANENT",
                  override = TRUE)

# import limit brazil
rgrass::execGRASS("v.import", 
                  flags = "overwrite", 
                  input = "02_data/02_variables/00_limit/limit_br_island_removed.shp", 
                  output = "brasil",
                  snap = 0.001)

# import 
rgrass::execGRASS("r.import", 
                  input = "02_data/02_variables/02_landscape/00_raw/brasil_coverage_2022.tif", 
                  output = "brasil_coverage_2022")

# region 
rgrass::execGRASS("r.mask", flags = "r") 
rgrass::execGRASS("r.mask", flags = "overwrite", vector = "brasil") 
rgrass::execGRASS("g.region", flags = c("a", "p"), raster = "brasil_coverage_2022") 
rgrass::execGRASS("g.region", flags = c("a", "p"), res = "100") 

rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = "brasil_coverage_2022_100m = brasil_coverage_2022") 
rgrass::execGRASS("r.mapcalc", flags = "overwrite", expression = "brasil_coverage_2022_100m_habitat = if(brasil_coverage_2022_100m == 3 | brasil_coverage_2022_100m == 4 | brasil_coverage_2022_100m == 5 | brasil_coverage_2022_100m == 6 | brasil_coverage_2022_100m == 10 | brasil_coverage_2022_100m == 11 | brasil_coverage_2022_100m == 12 | brasil_coverage_2022_100m == 13 | brasil_coverage_2022_100m == 32 | brasil_coverage_2022_100m == 49 | brasil_coverage_2022_100m == 50, 1, 0)")

# habitat class
rgrass::execGRASS(cmd = "r.mapcalc", flags = c("overwrite"), expression = "brasil_coverage_2022_100m_habitat_null=if(brasil_coverage_2022_100m_habitat == 0, null(), 1)")

# metrics
lsmetrics::lsm_functional_connectivity(input = "brasil_coverage_2022_100m_habitat", gap_crossing = 100)
lsmetrics::lsm_perimeter(input = "brasil_coverage_2022_100m_habitat", perimeter_area_ratio = TRUE)
lsmetrics::lsm_fragment_area(input = "brasil_coverage_2022_100m_habitat", id = TRUE)
lsmetrics::lsm_distance(input = "brasil_coverage_2022_100m_habitat", distance_type = "both")

# export
rgrass::execGRASS("r.out.gdal", input = "brasil_coverage_2022_100m_habitat_fragment_area_ha", output = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_fragment_area_ha.tif", createopt = "COMPRESS=DEFLATE")
rgrass::execGRASS("r.out.gdal", input = "brasil_coverage_2022_100m_habitat_perimeter_area_ratio", output = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_perimeter_area_ratio.tif", createopt = "COMPRESS=DEFLATE")
rgrass::execGRASS("r.out.gdal", input = "brasil_coverage_2022_100m_habitat_perimeter", output = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_perimeter.tif", createopt = "COMPRESS=DEFLATE")
rgrass::execGRASS("r.out.gdal", input = "brasil_coverage_2022_100m_habitat_functional_connectivity200", output = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_functional_connectivity200.tif", createopt = "COMPRESS=DEFLATE")
rgrass::execGRASS("r.out.gdal", input = "brasil_coverage_2022_100m_habitat_distance_inside", output = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_distance_inside.tif", createopt = "COMPRESS=DEFLATE")


## grassdb ----
rgrass::initGRASS(gisBase = "C:/Program Files/GRASS GIS 8.3",
                  gisDbase = "02_data/02_variables/02_landscape/grassdb",
                  location = "geo",
                  mapset = "PERMANENT",
                  override = TRUE)

## import
rgrass::execGRASS("r.import", input = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_fragment_area_ha.tif", output = "brasil_coverage_2022_100m_habitat_fragment_area_ha")
rgrass::execGRASS("r.import", input = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_perimeter_area_ratio.tif", output = "brasil_coverage_2022_100m_habitat_perimeter_area_ratio")
rgrass::execGRASS("r.import", input = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_perimeter.tif", output = "brasil_coverage_2022_100m_habitat_perimeter")
rgrass::execGRASS("r.import", input = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_functional_connectivity200.tif", output = "brasil_coverage_2022_100m_habitat_functional_connectivity200")
rgrass::execGRASS("r.import", input = "02_data/02_variables/02_landscape/brasil_coverage_2022_100m_habitat_distance_inside.tif", output = "brasil_coverage_2022_100m_habitat_distance_inside_lsmetrics")

## import
rgrass::execGRASS("r.mapcalc", expression = "brasil_coverage_2022_100m_habitat_fragment_area_ha_geo=brasil_coverage_2022_100m_habitat_fragment_area_ha")
rgrass::execGRASS("r.mapcalc", expression = "brasil_coverage_2022_100m_habitat_perimeter_area_ratio_geo=brasil_coverage_2022_100m_habitat_perimeter_area_ratio")
rgrass::execGRASS("r.mapcalc", expression = "brasil_coverage_2022_100m_habitat_perimeter_geo=brasil_coverage_2022_100m_habitat_perimeter")
rgrass::execGRASS("r.mapcalc", expression = "brasil_coverage_2022_100m_habitat_functional_connectivity200_geo=brasil_coverage_2022_100m_habitat_functional_connectivity200")
rgrass::execGRASS("r.mapcalc", expression = "brasil_coverage_2022_100m_habitat_distance_inside_lsmetrics_geo=brasil_coverage_2022_100m_habitat_distance_inside_lsmetrics")

# end ---------------------------------------------------------------------