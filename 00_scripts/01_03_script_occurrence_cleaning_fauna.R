#' ---
#' title: plataforma sn - cleaning data
#' author: mauricio vancine
#' date: 2024-04-10
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(sf)
library(terra)
library(tmap)
library(CoordinateCleaner)

# options
tmap_options(check.and.fix = TRUE)
sf::sf_use_s2(FALSE)

# import ----------------------------------------------------------------

# import
occ_fauna <- readr::read_csv("02_data/01_occurrences/02_integrated/00_occ_raw_fauna.csv") %>% 
    tibble::rowid_to_column(var = "id")
occ_fauna

# information
nrow(occ_fauna)
length(unique(occ_fauna$species))

# brazil
br <- geobr::read_country() %>% 
    sf::st_transform(4326)
br

tm_shape(br) +
    tm_polygons()

# date filter -------------------------------------------------------------

# temporal

# fauna
occ_fauna_filter_date <- occ_fauna %>% 
    dplyr::mutate(date_filter = ifelse(year >= 1970 & year <= 2024 | is.na(year), TRUE, FALSE))
occ_fauna_filter_date

# precision ---------------------------------------------------------------

# fauna
occ_fauna_filter_date_precision <- occ_fauna_filter_date %>% 
    dplyr::mutate(precision_filter = ifelse(longitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3 &
                                                latitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3, 
                                            TRUE, FALSE)) %>% 
    dplyr::mutate(precision_filter_date = ifelse(date_filter, TRUE, ifelse(is.na(year) & precision_filter, TRUE, FALSE)))
occ_fauna_filter_date_precision

# citizen filter ----------------------------------------------------------

# fauna
occ_fauna_filter_date_precision_citizen <- occ_fauna_filter_date_precision %>% 
    dplyr::mutate(citizen_filter = case_when(source == "inat" ~ FALSE, .default = TRUE))
occ_fauna_filter_date_precision_citizen

# neotropical filter ------------------------------------------------------

# vector
neo <- sf::st_read("02_data/02_variables/00_limit/neotropic_dissolved_fill_holes.shp") %>% 
    dplyr::mutate(FID = 1)
neo
plot(neo, col = "gray")

# fauna
occ_fauna_filter_date_precision_citizen_neotropic <- occ_fauna_filter_date_precision_citizen %>% 
    dplyr::mutate(lon = longitude, lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    sf::st_join(neo) %>% 
    dplyr::mutate(neotropic_filter = ifelse(is.na(FID), FALSE, TRUE)) %>% 
    dplyr::select(-FID) %>% 
    sf::st_drop_geometry()
occ_fauna_filter_date_precision_citizen_neotropic

# iucn filter ------------------------------------------------------------

# polygon iucn
# iucn_amphibian <- readRDS("02_data/01_occurrences/tetrapods_vector/amphibians_iucn_2021.rds") %>%
#     dplyr::filter(binomial %in% fauna_list)
# birdlife_birds <- readRDS("02_data/01_occurrences/tetrapods_vector/birds_birdlife_2017.rds") %>%
#     dplyr::filter(SCINAME %in% fauna_list)
# iucn_mammals <- readRDS("02_data/01_occurrences/tetrapods_vector/mammals_iucn_2021.rds") %>%
#     dplyr::filter(binomial %in% fauna_list)
# iucn_reptiles <- readRDS("02_data/01_occurrences/tetrapods_vector/reptiles_iucn_2021.rds") %>%
#     dplyr::filter(binomial %in% fauna_list)

# export
# sf::st_write(iucn_amphibian, "02_data/01_occurrences/tetrapods_vector/iucn_amphibians.gpkg")
# sf::st_write(birdlife_birds, "02_data/01_occurrences/tetrapods_vector/birdlife_birds.gpkg")
# sf::st_write(iucn_mammals, "02_data/01_occurrences/tetrapods_vector/iucn_mammals.gpkg")
# sf::st_write(iucn_reptiles, "02_data/01_occurrences/tetrapods_vector/iucn_reptiles.gpkg")

# import
iucn_amphibians <- sf::st_read("02_data/01_occurrences/tetrapods_vector/iucn_amphibians.gpkg") %>%
    dplyr::select(binomial)
birdlife_birds <- sf::st_read("02_data/01_occurrences/tetrapods_vector/birdlife_birds.gpkg") %>%
    dplyr::select(SCINAME) %>%
    dplyr::rename(binomial = SCINAME)
iucn_mammals <- sf::st_read("02_data/01_occurrences/tetrapods_vector/iucn_mammals.gpkg") %>%
    dplyr::select(binomial)
iucn_reptiles <- sf::st_read("02_data/01_occurrences/tetrapods_vector/iucn_reptiles.gpkg") %>%
    dplyr::select(binomial)

iucn <- dplyr::bind_rows(iucn_amphibians, birdlife_birds, iucn_mammals, iucn_reptiles) %>% 
    dplyr::group_by(binomial) %>%
    dplyr::summarise(n = n()) %>% 
    dplyr::mutate(area = st_area(.))
iucn

iucn_br <- iucn %>% 
    sf::st_intersection(br) %>% 
    dplyr::mutate(area_br = st_area(.)) %>% 
    sf::st_drop_geometry()
iucn_br

iucn_endemic <- iucn %>% 
    sf::st_drop_geometry() %>% 
    dplyr::left_join(iucn_br) %>% 
    dplyr::mutate(endemic_per = round(as.numeric(area_br/area * 100), 2)) %>% 
    dplyr::mutate(endemic = ifelse(endemic_per >= 90, TRUE, FALSE)) %>% 
    dplyr::rename(species = 1) %>% 
    dplyr::select(1, 6)
iucn_endemic

# filter
occ_fauna_filter_date_precision_citizen_neotropic_iucn <- NULL
for(i in unique(occ_fauna$species)){
    
    # iucn species filter
    iucn_i <- iucn %>%
        dplyr::filter(binomial == i) %>%
        dplyr::mutate(fid = 1) %>%
        dplyr::select(fid)
    
    # export
    sf::st_write(iucn_i, paste0("02_data/01_occurrences/tetrapods_vector/species/", i, "_iucn.shp"), delete_dsn = TRUE, quiet = TRUE)
    
    # species filter
    occ_fauna_filter_date_precision_citizen_neotropic_i <- occ_fauna_filter_date_precision_citizen_neotropic %>%
        dplyr::filter(species == i)
    
    # spatial iucn filter
    occ_fauna_filter_date_precision_citizen_neotropic_iucn_i <- occ_fauna_filter_date_precision_citizen_neotropic_i %>%
        dplyr::mutate(lon = longitude, lat = latitude) %>%
        sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        sf::st_join(iucn_i, quiet = TRUE) %>%
        dplyr::mutate(iucn_filter = ifelse(is.na(fid), FALSE, TRUE)) %>%
        dplyr::select(-fid) %>%
        sf::st_drop_geometry()
    
    # bind
    occ_fauna_filter_date_precision_citizen_neotropic_iucn <- dplyr::bind_rows(
        occ_fauna_filter_date_precision_citizen_neotropic_iucn,
        occ_fauna_filter_date_precision_citizen_neotropic_iucn_i)
    
}
occ_fauna_filter_date_precision_citizen_neotropic_iucn

# salve filter ------------------------------------------------------------
# occ salve
occ_salve_fauna <- readr::read_csv("02_data/01_occurrences/02_integrated/occ_raw_salve_fauna.csv")
occ_salve_fauna

# filter
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve <- NULL
for(i in unique(occ_fauna$species)){
    
    # species filter
    occ_fauna_i <- occ_salve_fauna %>% 
        dplyr::filter(species == i) %>% 
        dplyr::select(2:3)
    
    if(nrow(occ_fauna_i) == 0){
        
        # species filter
        occ_fauna_filter_date_precision_citizen_neotropic_iucn_i <- occ_fauna_filter_date_precision_citizen_neotropic_iucn %>% 
            dplyr::filter(species == i)
        
        # spatial filter
        occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_i <- occ_fauna_filter_date_precision_citizen_neotropic_iucn_i %>% 
            dplyr::mutate(salve_filter = NA)
        
    } else{
        
        # species vector
        occ_fauna_i_v <- sf::st_as_sf(occ_fauna_i, coords = c("longitude", "latitude"), crs = 4326)
        
        # species buffer
        occ_fauna_i_v_buffer <- occ_fauna_i_v %>% 
            terra::vect() %>% 
            terra::buffer(100e3) %>% 
            sf::st_as_sf() %>% 
            sf::st_union() %>% 
            sf::st_as_sf() %>% 
            dplyr::mutate(fid = 1)
        
        # export
        sf::st_write(occ_fauna_i_v_buffer, paste0("02_data/01_occurrences/buffer_salve/", i, "_buffer100km_salve.shp"), delete_dsn = TRUE, quiet = TRUE)
        
        # species filter
        occ_fauna_filter_date_precision_citizen_neotropic_iucn_i <- occ_fauna_filter_date_precision_citizen_neotropic_iucn %>% 
            dplyr::filter(species == i)
        
        # spatial filter
        occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_i <- occ_fauna_filter_date_precision_citizen_neotropic_iucn_i %>% 
            dplyr::mutate(lon = longitude, lat = latitude) %>% 
            sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
            sf::st_join(occ_fauna_i_v_buffer, quiet = TRUE) %>% 
            dplyr::mutate(salve_filter = ifelse(is.na(fid), FALSE, TRUE)) %>% 
            dplyr::select(-fid) %>% 
            sf::st_drop_geometry()
        
    }
    
    # bind
    occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve <- dplyr::bind_rows(
        occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve,
        occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_i)
    
}
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve

# spatial filter -----------------------------------------------------

# raster
chelsa <- terra::rast("02_data/02_variables/01_regional/01_adjusted/variables_regional_id.tif")
chelsa

plot(chelsa)

# fauna
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_values <- terra::extract(
    x = chelsa, 
    y = occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve[, c("longitude", "latitude")], 
    ID = FALSE, cells = TRUE) %>% 
    tibble::as_tibble()
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_values

occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial <- occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve %>%
    dplyr::mutate(spatial_values = occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_values[, 1],
                  spatial_cells = occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_values[, 2]) %>% 
    tidyr::drop_na(spatial_values) %>% 
    dplyr::distinct(species, spatial_cells, .keep_all = TRUE) %>% 
    dplyr::mutate(spatial_filter = TRUE) %>% 
    dplyr::select(c(id, spatial_filter))
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial

occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial <- dplyr::left_join(occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve, occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial) %>% 
    dplyr::mutate(spatial_filter = ifelse(is.na(spatial_filter), FALSE, TRUE))
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial

# landscape filter -------------------------------------------------------------

# # Carregue o arquivo raster
# map <- terra::rast("02_data/02_variables/02_landscape/brasil_coverage_2022.tif")
# map
# map <- terra::rast("D:/mapbiomas/brasil_coverage_2022.tif")

# # resolucao 100 m
# map60 <- terra::aggregate(map, fun = "modal", cores = 10)
# map60
# 
# map100 <- terra::aggregate(map60, fun = "modal", cores = 10)
# map100
#  
# # Defina os valores que devem ser reclassificados como 1
# valores_1 <- c(3, 4, 5, 6, 49, 10, 11, 12, 32, 50, 13)
#  
# # Reclassifique o raster conforme a matriz de reclassificação
# map_binario <- map100 %in% valores_1
# map_binario
# 
# # Salve o novo raster em um arquivo
# writeRaster(map_binario, filename = "D:/mapbiomas/mapbiomas_binario_100m.tif")

# landscape
landscape <- terra::rast("02_data/02_variables/02_local/00_raw/brasil_coverage_2022_100m_habitat_wgs84_geo.tif")
landscape
plot(landscape)

# fauna
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_filter_values <- terra::extract(
    x = landscape, 
    y = occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve[, c("longitude", "latitude")]) %>% 
    tibble::as_tibble() %>% 
    dplyr::pull(brasil_coverage_2022_100m_habitat)
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_filter_values

occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape <- occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial %>%
    dplyr::mutate(landscape_filter = ifelse(is.na(occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_filter_values) | occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_filter_values == 0, FALSE, TRUE))
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape

# bias filter -------------------------------------------------------------

# bias
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_bias <- CoordinateCleaner::clean_coordinates(
    x = occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape, 
    species = "species",
    lon = "longitude", 
    lat = "latitude",
    tests = c("capitals", # radius around capitals
              "centroids", # radius around country and province centroids
              "duplicates", # records from one species with identical coordinates
              "equal", # equal coordinates
              "gbif", # radius around GBIF headquarters
              "institutions", # radius around biodiversity institutions
              # "outliers", # remove outliers
              "seas", # in the sea
              "urban", # within urban area
              "validity", # outside reference coordinate system
              "zeros" # plain zeros and lat = lon
    ),
    capitals_rad = 2000,
    centroids_rad = 2000,
    centroids_detail = "both",
    inst_rad = 100,
    outliers_method = "quantile",
    outliers_mtp = 5,
    outliers_td = 1000,
    outliers_size = 10,
    range_rad = 0,
    zeros_rad = 0.5,
    capitals_ref = NULL,
    centroids_ref = NULL,
    country_ref = NULL,
    country_refcol = "countryCode",
    inst_ref = NULL,
    range_ref = NULL,
    # seas_ref = continent_border,
    seas_scale = 110,
    urban_ref = NULL,
    value = "spatialvalid") %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(.cen = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .cen),
                  .summary = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .summary)) %>%     dplyr::mutate(lon = longitude, lat = latitude, bias_filter = .summary) %>% 
    dplyr::mutate(lon = longitude, lat = latitude, bias_filter = .summary) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_bias

# filter ------------------------------------------------------------------

## fauna group
fauna_order_group <- readr::read_csv("02_data/00_species_list/fauna_orders.csv")
fauna_order_group

# fauna
occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_bias_group_endemic <- occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_bias %>% 
    dplyr::left_join(fauna_order_group) %>% 
    dplyr::left_join(iucn_endemic) %>% 
    dplyr::relocate(group, .after = category)

occ_fauna_cleaned_regional_endemic <- occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_bias_group_endemic %>% 
    dplyr::mutate(endemic = case_when(group == "invertebrados" ~ TRUE, .default = endemic)) %>% 
    dplyr::filter(endemic == TRUE) %>% 
    dplyr::filter(salve_filter == TRUE)
occ_fauna_cleaned_regional_endemic

occ_fauna_cleaned_regional_not_endemic <- occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_bias_group_endemic %>% 
    dplyr::filter(endemic == FALSE) %>% 
    dplyr::filter(iucn_filter == TRUE)
occ_fauna_cleaned_regional_not_endemic

occ_fauna_cleaned_regional <- dplyr::bind_rows(occ_fauna_cleaned_regional_endemic, occ_fauna_cleaned_regional_not_endemic) %>% 
    dplyr::filter(precision_filter_date == TRUE,
                  citizen_filter == TRUE,
                  neotropic_iucn_filter = TRUE, 
                  spatial_filter == TRUE,
                  bias_filter == TRUE) %>% 
    dplyr::select(-id) %>% 
    tibble::rowid_to_column(var = "id")
occ_fauna_cleaned_regional

occ_fauna_cleaned_local <- occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_bias_group_endemic %>% 
    dplyr::filter(species %in% occ_fauna_cleaned_regional$species,
                  precision_filter_date == TRUE,
                  citizen_filter == TRUE,
                  neotropic_iucn_filter = TRUE, 
                  landscape_filter == TRUE,
                  spatial_filter == TRUE,
                  salve_filter == TRUE,
                  bias_filter == TRUE) %>% 
    dplyr::select(-id) %>% 
    tibble::rowid_to_column(var = "id")
occ_fauna_cleaned_local

# export ------------------------------------------------------------------

occ_fauna_filter_date_precision_citizen_neotropic_iucn_salve_spatial_landscape_bias_group_endemic %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(-c(.val:.summary)) %>% 
    dplyr::relocate(group, .after = source) %>% 
    readr::write_csv("02_data/01_occurrences/03_cleaned/occ_raw_cleaned_fauna.csv")

occ_fauna_cleaned_regional %>% 
    sf::st_drop_geometry() %>% 
    dplyr::relocate(group, .after = source) %>% 
    readr::write_csv("02_data/01_occurrences/03_cleaned/occ_cleaned_fauna_regional.csv")

occ_fauna_cleaned_local %>% 
    sf::st_drop_geometry() %>% 
    dplyr::relocate(group, .after = source) %>% 
    readr::write_csv("02_data/01_occurrences/03_cleaned/occ_cleaned_fauna_local.csv")


sp_reg <- occ_fauna_cleaned_regional %>% 
    sf::st_drop_geometry() %>% 
    dplyr::relocate(group, .after = source) %>%
    dplyr::filter(group == "invertebrados") %>% 
    dplyr::count(species) %>% 
    filter(n >= 10)
sp_reg

sp_loc <- occ_fauna_cleaned_local %>% 
    sf::st_drop_geometry() %>% 
    dplyr::relocate(group, .after = source) %>%
    dplyr::filter(group == "invertebrados") %>% 
    dplyr::count(species) %>% 
    filter(n >= 10)
sp_loc

# end ---------------------------------------------------------------------