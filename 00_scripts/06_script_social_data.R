# source
# https://cran.r-project.org/web/packages/sidrar/vignettes/Introduction_to_sidrar.html

# packages
library(tidyverse)
library(sidrar)
library(geobr)
library(raster)
library(geodata)
library(terra)
library(tmap)

# search
sidrar::search_sidra("tipo de parede")

# information
sidrar::info_sidra(x = 149) %>% names()

# classific category
sidrar::info_sidra(x = 149)$classific_category %>% names()

# get data
da_parede <- sidrar::get_sidra(x = 149, classific = c("c137"))
da_parede

# glimpse
dplyr::glimpse(da_parede)

# filter and selection
da_parede_fil <- da_parede
da_parede_fil

# vector
mun <- sf::st_read("01_data/03_municipalities/BR_Municipios_2023.shp")
mun

# join
mun_da <- dplyr::left_join(x = mun, y = da_parede_fil, by = c("CD_MUN" = "code_muni"))
mun_da

# end ---------------------------------------------------------------------