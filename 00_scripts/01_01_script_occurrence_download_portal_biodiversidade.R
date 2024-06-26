#' ---
#' title: portalbioaform sn - integracao dados portal da biodiversidade
#' author: mauricio vancine
#' date: 2024-01-30
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(janitor)
library(parallelly)
library(future)
library(furrr)
library(tmap)

# options
options(timeout = 3e5)

# import data -------------------------------------------------------------

# import south america limite
li <- spData::world
li

tm_shape(li) +
    tm_polygons()

# species
fauna_list <- readr::read_csv("02_data/00_lista_especies/fauna_mma_2024_fitered.csv") %>% 
    dplyr::pull(species) %>% 
    sort()
fauna_list

flora_list <- readr::read_csv("02_data/00_lista_especies/flora_mma_2024_fitered.csv") %>% 
    dplyr::pull(species) %>% 
    sort()
flora_list

# import
list_files_portalbio <- dir(path = "02_data/01_occurrences/01_raw/05_plataforma_biodiversidade", pattern = ".csv", recursive = TRUE, full.names = TRUE) %>% 
    stringr::str_subset(pattern = "portalbio_export")
list_files_portalbio

occ_portalbio <- readr::read_csv("02_data/01_occurrences/01_raw/05_plataforma_biodiversidade/portalbio_one_species.csv")
for(i in list_files_portalbio){
    
    occ_portalbio_i <- readr::read_delim(i, delim = ";", col_types = cols()) %>% 
        janitor::clean_names() %>%
        dplyr::mutate(species = especie,
                      longitude = as.numeric(longitude),
                      latitude = as.numeric(latitude),
                      year = lubridate::year(lubridate::dmy(data_do_registro)),
                      source = "portalbio") %>% 
        dplyr::select(species, longitude, latitude, year, source)
    occ_portalbio <- dplyr::bind_rows(occ_portalbio, occ_portalbio_i)
    
}
occ_portalbio

# filter
occ_portalbio_fauna <- occ_portalbio %>% 
    dplyr::filter(species %in% fauna_list)
occ_portalbio_fauna

occ_portalbio_flora <- occ_portalbio %>% 
    dplyr::filter(species %in% flora_list)
occ_portalbio_flora

# vector
occ_portalbio_fauna_v <- occ_portalbio_fauna %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    dplyr::filter(lon > -180 & lon < 180) %>% 
    dplyr::filter(lat > -90 & lat < 90) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_portalbio_fauna_v

occ_portalbio_flora_v <- occ_portalbio_flora %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    dplyr::filter(lon > -180 & lon < 180) %>% 
    dplyr::filter(lat > -90 & lat < 90) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_portalbio_flora_v

# maps
tm_shape(li[li$iso_a2 == "BR", ]) +
    tm_polygons() +
    tm_shape(occ_portalbio_fauna_v) +
    tm_dots()

tm_shape(li[li$iso_a2 == "BR", ]) +
    tm_polygons() +
    tm_shape(occ_portalbio_flora_v) +
    tm_dots()

# export
readr::write_csv(occ_portalbio_fauna, "02_data/01_occurrences/02_integrated/occ_raw_portalbio_fauna.csv")
readr::write_csv(occ_portalbio_flora, "02_data/01_occurrences/02_integrated/occ_raw_portalbio_flora.csv")

# end ---------------------------------------------------------------------