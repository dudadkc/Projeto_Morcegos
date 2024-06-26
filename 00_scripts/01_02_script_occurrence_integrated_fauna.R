#' ---
#' title: Plataforma Seleção
#' author: Paula Montagnana
#' date: 06 02 2024
#' aim: Join tabelas; summarize dados
#' ---

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(furrr)
library(future)
library(tmap)
library(viridis)

# import data -------------------------------------------------------------

# import south america limite
li <- spData::world
li

tm_shape(li) +
    tm_polygons()

## species lists ----
list_fauna <- readr::read_csv("02_data/00_species_list/fauna_mma_2024_fitered.csv")
list_fauna

## specieslink and spocc ----
occ_specieslink_spocc_fauna <- readr::read_csv("02_data/01_occurrences/02_integrated/occ_raw_splink_spocc_fauna.csv") %>% 
    dplyr::rename(species = species_searched,
                  year = date,
                  source = prov) %>% 
    dplyr::select(species, longitude, latitude, year, source)
occ_specieslink_spocc_fauna

## salve ----
occ_salve_fauna <- readr::read_csv("02_data/01_occurrences/02_integrated/occ_raw_salve_fauna.csv")
occ_salve_fauna

## sibbr ----
occ_sibbr_fauna <- readr::read_csv("02_data/01_occurrences/02_integrated/occ_raw_sibbr_fauna.csv")
occ_sibbr_fauna

## data papers ----
occ_data_papers_fauna <- readr::read_csv("02_data/01_occurrences/02_integrated/occ_raw_data_paper_fauna.csv")
occ_data_papers_fauna

## portal biodiversidade ----
occ_portalbio_fauna <- readr::read_csv("02_data/01_occurrences/02_integrated/occ_raw_portalbio_fauna.csv")
occ_portalbio_fauna

## combine ----
occ_fauna <- dplyr::bind_rows(occ_specieslink_spocc_fauna, occ_data_papers_fauna,
                              occ_salve_fauna, occ_sibbr_fauna, occ_portalbio_fauna) %>% 
    dplyr::filter(species %in% list_fauna$species) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::filter(longitude > -180 & longitude < 180) %>% 
    dplyr::filter(latitude > -90 & latitude < 90)
occ_fauna

occ_fauna_v <- sf::st_as_sf(occ_fauna, coords = c("longitude", "latitude"), crs = 4326)
occ_fauna_v

# código do Chat GPT porque o abaixo deu erro quando rodei
# tm_shape(li) +
#     tm_polygons() +
#     tm_shape(occ_fauna_v) +
#     tm_bubbles(size = .2, 
#                col = "species")
# 
# 
# tm_shape(li) +
#     tm_polygons() +
#     tm_shape(occ_fauna_v) +
#     tm_bubbles(size = .2, 
#                col = "species", 
#                col.scale = tm_scale_categorical(values = "viridis"),
#                col.legend = tm_legend_hide())
# 
# tm_shape(li) +
#     tm_polygons() +
#     tm_shape(occ_flora_v) +
#     tm_bubbles(size = .2, 
#                col = "species", 
#                col.scale = tm_scale_categorical(values = "viridis"),
#                col.legend = tm_legend_hide())


## join
occ_fauna_cat <- dplyr::left_join(occ_fauna, list_fauna[, c(3:6)]) %>% 
    dplyr::select(order, family, species, category, longitude, latitude, year, source)
occ_fauna_cat

## export ----
readr::write_csv(occ_fauna_cat, "02_data/01_occurrences/02_integrated/00_occ_raw_fauna.csv")

# end ---------------------------------------------------------------------
