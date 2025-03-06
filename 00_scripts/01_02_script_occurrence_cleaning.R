#' ---
#' title: occurrences - cleaning data
#' author: mauricio vancine
#' date: 2024-04-10
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(rnaturalearth)
library(sf)
library(terra)
library(tmap)
library(CoordinateCleaner)

# options
sf::sf_use_s2(FALSE)

# import ----------------------------------------------------------------

# import
occ <- readr::read_csv("01_data/00_occurrences/00_raw/00_occ_raw_bats.csv") %>% 
    tibble::rowid_to_column(var = "id")
occ

# date filter -------------------------------------------------------------

# temporal

# fauna
occ_filter_date <- occ %>% 
    dplyr::mutate(date_filter = ifelse(year >= 1970 & year <= 2024 | is.na(year), TRUE, FALSE))
occ_filter_date

# precision ---------------------------------------------------------------

# fauna
occ_filter_date_precision <- occ_filter_date %>% 
    dplyr::mutate(precision_filter = ifelse(longitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3 &
                                                latitude %>% as.character() %>% stringr::str_split_fixed(., pattern = "[.]", n = 2) %>% .[, 2] %>% stringr::str_length() >= 3, 
                                            TRUE, FALSE)) %>% 
    dplyr::mutate(precision_filter_date = ifelse(date_filter, TRUE, ifelse(is.na(year) & precision_filter, TRUE, FALSE)))
occ_filter_date_precision

# neotropical filter ---------------------------------------------------

# latin america
la <- dplyr::select(rnaturalearth::ne_countries(scale = 10, continent = "South America"), sov_a3, geometry) %>% 
    rbind(dplyr::select(rnaturalearth::ne_countries(scale = 10, country = "France"), sov_a3, geometry)) %>% 
    sf::st_crop(dplyr::select(rnaturalearth::ne_countries(scale = 10, continent = "South America"), geometry)) %>% 
    rbind(dplyr::select(rnaturalearth::ne_countries(scale = 10, continent = "North America"), sov_a3, geometry)) %>% 
    dplyr::filter(!sov_a3 %in% c("CAN", "US1", "DN1"))
la

br <- rnaturalearth::ne_states(country = "Brazil")
br

tm_shape(la) +
    tm_polygons() +
    tm_shape(br) +
    tm_borders(lty = 3, lwd = .4)

sf::st_write(la, "01_data/01_variables/00_limits/latin_america.shp", delete_dsn = TRUE)
sf::st_write(br, "01_data/01_variables/00_limits/brazil.shp", delete_dsn = TRUE)

# vector
neo <- sf::st_read("01_data/01_variables/00_limits/neotropic_dissolved_fill_holes.shp") %>% 
    dplyr::rename(fid = FID) %>% 
    dplyr::mutate(fid = 1)
neo

tm_shape(neo) +
    tm_polygons() +
    tm_shape(la) +
    tm_borders(lty = 3, lwd = .4)

# filter
occ_filter_date_precision_neo <- occ_filter_date_precision %>% 
    dplyr::mutate(lon = longitude, lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    sf::st_join(neo) %>% 
    dplyr::mutate(neo_filter = ifelse(is.na(fid), FALSE, TRUE)) %>% 
    dplyr::select(-fid) %>% 
    sf::st_drop_geometry()
occ_filter_date_precision_neo

# spatial filter -----------------------------------------------------

# spatial distance filter
raster_10km <- geodata::worldclim_global(var = "prec", res = 5, 
                                        path = "01_data/01_variables/climate/") 
raster_10km

raster_10km_neo <- raster_10km %>% 
    .[[1]] %>% 
    terra::crop(neo) %>% 
    terra::mask(neo)
raster_10km_neo

plot(raster_10km_neo)

raster_10km_neo_id <- raster_10km_neo
raster_10km_neo_id[!is.na(raster_10km_neo_id)] <- 1:ncell(raster_10km_neo_id[!is.na(raster_10km_neo_id)])
names(raster_10km_neo_id) <- "id"
plot(raster_10km_neo_id)

# spatial distance filter
occ_filter_date_precision_neo_spatial_values <- terra::extract(
    x = raster_10km_neo_id, 
    y = occ_filter_date_precision_neo[, c("longitude", "latitude")], 
    ID = FALSE, cells = TRUE) %>% 
    tibble::as_tibble() %>% 
    dplyr::select(2)
occ_filter_date_precision_neo_spatial_values

occ_filter_date_precision_neo_spatial <- occ_filter_date_precision_neo %>%
    dplyr::mutate(spatial_cells = occ_filter_date_precision_neo_spatial_values) %>% 
    dplyr::distinct(species, spatial_cells, .keep_all = TRUE) %>% 
    dplyr::mutate(spatial_filter = TRUE) %>% 
    dplyr::select(c(id, spatial_filter))
occ_filter_date_precision_neo_spatial

occ_filter_date_precision_neo_spatial <- occ_filter_date_precision_neo %>% 
    dplyr::left_join(occ_filter_date_precision_neo_spatial) %>% 
    dplyr::mutate(spatial_filter = ifelse(is.na(spatial_filter), FALSE, TRUE))
occ_filter_date_precision_neo_spatial

# bias filter -------------------------------------------------------------

# bias
occ_filter_date_precision_neo_spatial_bias <- CoordinateCleaner::clean_coordinates(
    x = occ_filter_date_precision_neo_spatial, 
    species = "species",
    lon = "longitude", 
    lat = "latitude",
    tests = c(
        # "capitals", # radius around capitals
        # "centroids", # radius around country and province centroids
        "duplicates", # records from one species with identical coordinates
        "equal", # equal coordinates
        # "gbif", # radius around GBIF headquarters
        # "institutions", # radius around biodiversity institutions
        # "outliers", # remove outliers
        # "seas", # in the sea
        # "urban", # within urban area
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
    dplyr::mutate(.summary = case_when(longitude == -52.8731 & latitude == -10.8339 ~ FALSE, .default = .summary)) %>%     dplyr::mutate(lon = longitude, lat = latitude, bias_filter = .summary) %>% 
    dplyr::mutate(bias_filter = .summary)
occ_filter_date_precision_neo_spatial_bias

# filter ------------------------------------------------------------------

# filter
occ_cleaned <- occ_filter_date_precision_neo_spatial_bias %>% 
    dplyr::filter(precision_filter_date == TRUE,
                  neo_filter == TRUE,
                  spatial_filter == TRUE,
                  bias_filter == TRUE) %>% 
    dplyr::select(-id) %>% 
    tibble::rowid_to_column(var = "id")
occ_cleaned

tm_shape(neo) +
    tm_polygons() +
    tm_shape(terra::vect(occ, geom = c("longitude", "latitude"), crs = "EPSG: 4326")) +
    tm_dots(fill = "red") +
    tm_shape(terra::vect(occ_cleaned, geom = c("longitude", "latitude"), crs = "EPSG: 4326")) +
    tm_dots()

# export ------------------------------------------------------------------

# export
readr::write_csv(occ_filter_date_precision_neo_spatial_bias, "01_data/00_occurrences/01_clean/00_occ_integrated.csv")
readr::write_csv(occ_cleaned, "01_data/00_occurrences/01_clean/01_occ_cleaned.csv")

# end ---------------------------------------------------------------------