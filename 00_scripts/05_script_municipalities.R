#' ----
#' aim: municipality data
#' author: mauricio vancine
#' date: 27/01/2025
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(tmap)

# import data -------------------------------------------------------------

# import
mun <- sf::st_read("01_data/04_vector/mun15_consolidacao_v00.shp")
mun

plot(mun$geometry)

# rabies_animals
rabies_animals <- readr::read_csv("01_data/02_rabies/02_summarized/rabie_animals.csv")
rabies_animals

# rabies_humans
rabies_humans <- readr::read_csv("")
rabies_humans

# rabies_natural_reservoir
rabies_natural_reservoir <- readr::read_csv("")
rabies_natural_reservoir

# rabies_rural
rabies_rural <- readr::read_csv("")
rabies_rural

# rabies_urban
rabies_urban <- readr::read_csv("")
rabies_urban

# rabies_wild 
rabies_wild <- readr::read_csv("")
rabies_wild

# join data ---------------------------------------------------------------

#
