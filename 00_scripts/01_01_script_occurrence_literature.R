#' ---
#' title: integrate occurrence from literature
#' author: madu
#' date: 06-02-2024
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(tmap)

# import data -------------------------------------------------------------

# atlantic bats
atlantic_bats_raw <- readxl::read_excel("01_data/00_occurrences/00_raw/ATLANTIC_BATS_2020_comp.xlsx")
atlantic_bats_raw

# datacides
datacides_raw <- readr::read_csv("01_data/00_occurrences/00_raw/Dataset 2.csv") %>% 
    dplyr::mutate(Record.source = stringr::str_replace_all(Record.source, "\\.\\s(\\d{4})\\.", ". (\\1)."))
datacides_raw

# vampire 
vampire_2021 <- readr::read_csv("01_data/00_occurrences/00_raw/Desmodus_dataset_Dec_2021.csv")
vampire_2021

vampire_2022 <- readr::read_csv("01_data/00_occurrences/00_raw/Desmodus_dataset_Apr_2022.csv")
vampire_2022

# clean -------------------------------------------------------------------

# atlantic bats
atlantic_bats_clean <- atlantic_bats_raw %>% 
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
atlantic_bats_clean

# datacides
datacides_year <- stringr::str_split(datacides_raw$Record.source, "[(]", simplify = TRUE) %>%
    .[, 2] %>% 
    stringr::str_split("[)]", simplify = TRUE) %>%
    .[, 1] %>%
    as.numeric()
datacides_year

datacides_clean <- datacides_raw %>% 
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
datacides_clean    

# vampire 
vampire_2021_clean <- vampire_2021 %>% 
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
vampire_2021_clean

vampire_2022_clean <- vampire_2022 %>% 
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
vampire_2022_clean

# combine -----------------------------------------------------------------

# literature
bats_literature <- dplyr::bind_rows(atlantic_bats_clean,
                                    datacides_clean,
                                    vampire_2021_clean, 
                                    vampire_2022_clean)
bats_literature

bats_literature_sf <- sf::st_as_sf(na.omit(bats_literature), 
                                   coords = c("longitude", "latitude"),
                                   crs = 4326)
bats_literature_sf

# map ---------------------------------------------------------------------

# import south america limite
li <- spData::world
li

tm_shape(li) +
    tm_polygons() +
    tm_shape(bats_literature_sf) +
    tm_dots()

# end ---------------------------------------------------------------------
