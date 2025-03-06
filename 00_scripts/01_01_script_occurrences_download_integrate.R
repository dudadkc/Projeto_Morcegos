#' ---
#' title: occurrence - download and integrate
#' author: madu
#' date: 06-02-2024
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(janitor)
library(jsonlite)
library(spocc)
library(sf)
library(tmap)

# import data -------------------------------------------------------------

## specieslink ----
occ_splink <- occ_splink <- jsonlite::fromJSON(
        paste0("https://specieslink.net/ws/1.0/search?scientificname=", 
               tolower(gsub(" ", "+", "Desmodus rotundus")), 
               "&apikey=aXGEJtnQW12sPuSyKMX7&offset=0&limit=5000"))$features$properties
occ_splink

## spocc ----
occ_spocc <- spocc::occ(query = "Desmodus rotundus", 
                        from = c("gbif", "vertnet", "idigbio", "ecoengine"),
                        has_coords = TRUE,
                        limit = 2e4,
                        throw_warnings = FALSE)
occ_spocc

## salve ----
occ_salve <- readr::read_csv("01_data/00_occurrences/00_raw/salve.csv")
occ_salve

## sibbr ----
occ_sibbr <- readr::read_csv("01_data/00_occurrences/00_raw/sibbr.csv") 
occ_sibbr

## portal biodiversidade ----
occ_portalbio <- readr::read_csv("01_data/00_occurrences/00_raw/portalbio.csv")
occ_portalbio

## atlantic bats ----
atlantic_bats <- readxl::read_excel("01_data/00_occurrences/00_raw/ATLANTIC_BATS_2020_comp.xlsx")
atlantic_bats

## datacides ----
datacides <- readr::read_csv("01_data/00_occurrences/00_raw/Dataset 2.csv") %>% 
    dplyr::mutate(Record.source = stringr::str_replace_all(Record.source, "\\.\\s(\\d{4})\\.", ". (\\1)."))
datacides

## vampire ----
vampire_2021 <- readr::read_csv("01_data/00_occurrences/00_raw/Desmodus_dataset_Dec_2021.csv")
vampire_2021

vampire_2022 <- readr::read_csv("01_data/00_occurrences/00_raw/Desmodus_dataset_Apr_2022.csv")
vampire_2022

# data -------------------------------------------------------------------

## specieslink ----
occ_splink_data <- occ_splink %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(species = "Desmodus rotundus",
                  longitude = as.numeric(decimallongitude),
                  latitude = as.numeric(decimallatitude),
                  source = "specieslink",
                  year = as.numeric(yearcollected)) %>% 
    dplyr::select(species, longitude, latitude, year, source)
occ_splink_data

## spocc ----
occ_spocc_data <- occ_spocc %>% 
    spocc::occ2df() %>% 
    dplyr::mutate(species = "Desmodus rotundus",
                  longitude = as.numeric(longitude),
                  latitude = as.numeric(latitude),
                  year = lubridate::year(date),
                  source = prov) %>% 
    dplyr::select(species, longitude, latitude, year, source)
occ_spocc_data

## salve ----
occ_salve_data <- occ_salve %>% 
    dplyr::mutate(species = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  year = as.numeric(year),
                  source = "salve") %>% 
    dplyr::select(species, longitude, latitude, year, source)
occ_salve_data

## sibbr ----
occ_sibbr_data <- occ_sibbr %>% 
    dplyr::mutate(species = scientificName,
                  longitude = as.numeric(decimalLongitude),
                  latitude = as.numeric(decimalLatitude),
                  year = as.numeric(year),
                  source = "salve") %>% 
    dplyr::select(species, longitude, latitude, year, source)
occ_salve_data

## portal biodiversidade ----
occ_portalbio_data <- occ_portalbio
occ_portalbio_data

## atlantic bats ----
atlantic_bats_data <- atlantic_bats %>% 
    dplyr::select(Desmodus.rotundus, Longitude, Latitude, Year_start) %>% 
    dplyr::filter(Desmodus.rotundus > 0) %>% 
    dplyr::mutate(species = "desmodus_rotundus", .before = 1) %>% 
    dplyr::mutate(source = "atlantic_bats") %>% 
    dplyr::select(-Desmodus.rotundus) %>% 
    dplyr::rename(longitude = Longitude,
                  latitude = Latitude,
                  year = Year_start) %>% 
    dplyr::mutate(longitude = as.numeric(longitude),
                  latitude = as.numeric(latitude),
                  year = as.numeric(year))
atlantic_bats_data

## datacides ----
datacides_year <- stringr::str_split(datacides$Record.source, "[(]", simplify = TRUE) %>%
    .[, 2] %>% 
    stringr::str_split("[)]", simplify = TRUE) %>%
    .[, 1] %>%
    as.numeric()
datacides_year

datacides_data <- datacides %>% 
    dplyr::select(Species.name, Longitude, Lattitude) %>% 
    dplyr::mutate(year = datacides_year,
                  source = "darkcides") %>% 
    dplyr::filter(Species.name == "Desmodus rotundus") %>% 
    dplyr::mutate(species = "desmodus_rotundus", .before = 1) %>% 
    dplyr::select(-Species.name) %>% 
    dplyr::rename(longitude = Longitude,
                  latitude = Lattitude) %>% 
    dplyr::mutate(longitude = as.numeric(longitude),
                  latitude = as.numeric(latitude),
                  year = as.numeric(year))
datacides_data    

## vampire ----
vampire_2021_data <- vampire_2021 %>% 
    dplyr::select(scientificName, decimalLongitude, decimalLatitude, year) %>% 
    dplyr::filter(scientificName == "Desmodus rotundus") %>% 
    dplyr::mutate(species = "desmodus_rotundus", .before = 1) %>% 
    dplyr::mutate(source = "vampires") %>% 
    dplyr::select(-scientificName) %>% 
    dplyr::rename(longitude = decimalLongitude,
                  latitude = decimalLatitude) %>% 
    dplyr::mutate(longitude = as.numeric(longitude),
                  latitude = as.numeric(latitude),
                  year = as.numeric(year))
vampire_2021_data

vampire_2022_data <- vampire_2022 %>% 
    dplyr::select(scientificName, decimalLongitude, decimalLatitude, year) %>% 
    dplyr::filter(scientificName == "Desmodus rotundus") %>% 
    dplyr::mutate(species = "desmodus_rotundus", .before = 1) %>% 
    dplyr::mutate(source = "vampires") %>% 
    dplyr::select(-scientificName) %>% 
    dplyr::rename(longitude = decimalLongitude,
                  latitude = decimalLatitude) %>% 
    dplyr::mutate(longitude = as.numeric(longitude),
                  latitude = as.numeric(latitude),
                  year = as.numeric(year))
vampire_2022_data

# combine -----------------------------------------------------------------

# combine
occs_combine <- dplyr::bind_rows(occ_splink_data,
                                    occ_spocc_data,
                                    occ_salve_data,
                                    occ_sibbr_data,
                                    occ_portalbio_data,
                                    atlantic_bats_data,
                                    datacides_data,
                                    vampire_2021_data, 
                                    vampire_2022_data) %>% 
    tidyr::drop_na(longitude, latitude)
occs_combine

occs_combine_sf <- sf::st_as_sf(occs_combine, coords = c("longitude", "latitude"), crs = 4326)
occs_combine_sf

# map ---------------------------------------------------------------------

# import limits
li <- spData::world
li

tm_shape(li) +
    tm_polygons() +
    tm_shape(occs_combine_sf) +
    tm_dots()

# export ------------------------------------------------------------------

# export
readr::write_csv(occs_combine, "01_data/00_occurrences/00_raw/00_occ_raw_bats.csv")
sf::st_write(occs_combine_sf, "01_data/00_occurrences/00_raw/00_occ_raw_bats.gpkg", delete_dsn = TRUE)

# end ---------------------------------------------------------------------
