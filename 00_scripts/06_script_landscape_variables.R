#' ---
#' title: plataform sn - variavies - adjust
#' author: mauricio vancine
#' date: 2024-03-23
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(rgrass)
library(foreach)
library(future)
library(furrr)

# roads -------------------------------------------------------------------

# mun
mun <- sf::st_read("01_data/05_municipalities/BR_Municipios_2023.shp") %>% 
    sf::st_transform(crs = 4326)
mun

# roads
roads <- sf::st_read("01_data/04_landscape/00_roads/bc250_2023_11_23_roads.gpkg") %>% 
    sf::st_transform(crs = 4326)
roads

# sf::st_write(roads, "01_data/04_landscape/00_roads/bc250_2023_11_23_roads.gpkg")

# filter
roads_da <- sf::st_drop_geometry(roads)
roads_da

names(roads_da)

count(roads_da, tipovia)
count(roads_da, jurisdicao)
count(roads_da, administracao)
count(roads_da, concessionaria)
count(roads_da, revestimento)
count(roads_da, operacional)
count(roads_da, situacaofisica)
count(roads_da, canteirodivisorio)
count(roads_da, nrpistas)
count(roads_da, nrfaixas)
count(roads_da, trafego)
count(roads_da, tipopavimentacao)
count(roads_da, sigla)
count(roads_da, acostamento)
count(roads_da, codtrechorod)
count(roads_da, limitevelocidade)
count(roads_da, emperimetrourbano)

roads_filter <- roads %>% 
    filter(
        tipovia != "Autoestrada",
        # revestimento != "Pavimentado",
        canteirodivisorio != "Sim",
        nrpistas == 1 | NA, 
        nrfaixas < 3 | NA,
        # tipopavimentacao != "Asfalto"
    )
roads_filter

# density
mun_calculated <- dir("01_data/04_landscape/00_roads/", pattern = ".csv") %>% 
    stringr::str_replace_all("roads_data_dcmun", "") %>% 
    stringr::str_replace_all(".csv", "") %>% 
    as.numeric()
mun_calculated

mun_filtered <- mun %>% 
    dplyr::filter(!CD_MUN %in% mun_calculated)
mun_filtered

cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl)
foreach::foreach(i=1:nrow(mun_filtered)) %do% {
    
    print(i)
    mun_i <- mun_filtered[i,]
    
    roads_filter_i <- sf::st_intersection(roads_filter, mun_i)
    roads_filter_i_length <- (sum(sf::st_length(roads_filter_i))/1000)
    roads_filter_i_density <- round(roads_filter_i_length/mun_i$AREA_KM2, 4)
    # sf::st_write(roads_filter_i, paste0("01_data/04_landscape/00_roads/bc250_2023_11_23_roads_cdmun", mun_i$CD_MUN, ".gpkg"))
    
    roads_data_i <- tibble::tibble(
        CD_MUN = mun_i$CD_MUN, 
        mun_area = mun_i$AREA_KM2,
        roads_length_km = as.numeric(roads_filter_i_length),
        roads_density_km = as.numeric(roads_filter_i_density))
    readr::write_csv(roads_data_i, paste0("01_data/04_landscape/00_roads/roads_data_dcmun", mun_i$CD_MUN, ".csv"))
    
}
parallel::stopCluster(cl)

# bind
plan(multisession, workers = 6)
roads_data <- dir(path = "01_data/04_landscape/00_roads/", pattern = ".csv", full.names = TRUE) %>% 
    furrr::future_map_dfr(read_csv)
roads_data

# export
readr::write_csv(roads_data, "03_prioritization/data_roads.csv")

# pasture -----------------------------------------------------------------

# connect
rgrass::initGRASS(gisBase = system("grass --config path", inter = TRUE),
                  gisDbase = "01_data/04_landscape/01_pasture/grassdb",
                  location = "newProject",
                  mapset = "PERMANENT",
                  override = TRUE)

# import limit
rgrass::execGRASS("r.in.gdal",
                  flags = c("overwrite"),
                  input = "01_data/04_landscape/01_pasture/brasil_coverage_2023.tif",
                  output = "mapbiomas")

rgrass::execGRASS("g.region", flags = "a", raster = "mapbiomas")

rgrass::execGRASS("r.mapcalc",
                  flags = "overwrite",
                  expression = "mapbiomas_pasture = if(mapbiomas == 15, 1, 0)")

rgrass::execGRASS("r.in.gdal",
                  flags = "overwrite",
                  input = "02_results/03_01_sdms_v12/03_05_pred.tif",
                  output = "sdm")

rgrass::execGRASS("g.region", flags = "a", raster = "sdm")
rgrass::execGRASS("r.mapcalc",
                  flags = "overwrite",
                  expression = "mapbiomas_pasture_1km = mapbiomas_pasture")

mapbiomas_pasture_1km <- rgrass::read_RAST("mapbiomas_pasture_1km")
mapbiomas_pasture_1km

# municipalities
mun <- terra::vect("01_data/05_municipalities/BR_Municipios_2023.shp") %>% 
    terra::project(mapbiomas_pasture_1km)
mun

mun_calculated <- dir("01_data/04_landscape/01_pasture/", pattern = ".csv") %>% 
    stringr::str_replace_all("mapbiomas_mun_pasture_pct_data_dcmun", "") %>% 
    stringr::str_replace_all(".csv", "") %>% 
    as.numeric()
mun_calculated

mun_filtered <- mun %>% 
    tidyterra::filter(!CD_MUN %in% mun_calculated)
mun_filtered

# calculate
for(i in 1:nrow(mun_filtered)){
    
    print(i)
    mun_i <- mun_filtered[i,]
    
    mapbiomas_mun_pasture <- terra::crop(mapbiomas_pasture_1km, mun_i, mask= TRUE)
    
    terra::writeRaster(mapbiomas_mun_pasture, paste0("01_data/04_landscape/01_pasture/mapbiomas_2013_pasture_cdmun", mun_i$CD_MUN, ".tif"), overwrite = TRUE)
    
    mapbiomas_mun_pasture_pct <- terra::freq(mapbiomas_mun_pasture) %>% 
        dplyr::mutate(per = round(count/sum(count) * 100, 2)) %>% 
        dplyr::filter(value == 1) %>% 
        dplyr::pull(per)
    
    mapbiomas_mun_pasture_pct_data_i <- tibble::tibble(
        CD_MUN = mun_i$CD_MUN, 
        pasture_pct = as.numeric(mapbiomas_mun_pasture_pct))
    readr::write_csv(
        mapbiomas_mun_pasture_pct_data_i, 
        paste0("01_data/04_landscape/01_pasture/mapbiomas_mun_pasture_pct_data_dcmun", mun_i$CD_MUN, ".csv"))
}
parallel::stopCluster(cl)

# bind
pasture_data_list <- dir(path = "01_data/04_landscape/01_pasture/", pattern = ".csv", full.names = TRUE)
pasture_data_list
length(pasture_data_list)

pasture_data <- NULL
for(i in pasture_data_list){
    
    print(i)
    pasture_data_i <- readr::read_csv(i)
    
    if(nrow(pasture_data_i) == 0){
        pasture_data_i <- tibble::tibble(
            CD_MUN = as.numeric(stringr::str_extract(basename(i), "\\d+")),
            pasture_pct = 0)
    }
    pasture_data <- rbind(pasture_data, pasture_data_i)
    
}
data_pasture

# export
readr::write_csv(data_pasture, "03_prioritization/data_pasture.csv")

# end ---------------------------------------------------------------------