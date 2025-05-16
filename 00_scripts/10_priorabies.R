#' ----
#' aim: prioritization
#' author: re
#' date: May 2025
#' ----

library(tidyverse)
library(sf)
require(corrplot)
require(DataExplorer)
require(here)
library(DataExplorer)
library(GGally)
library(corrplot)


d <- st_read('D://OneDrive - Massey University//Supervisions//Madu//03_prioritization//00_mun_data.gpkg')

setwd(here())
setwd('01_data/03_eda/')

hazard_names <- scan("hazard_low_vif.txt", what = character())
vulnerability_names <- scan("vulnerability_low_vif.txt", what = character())
exposure_names <- scan("exposure_low_vif.txt", what = character())


d[hazard_names]

d[vulnerability_names]

d[exposure_names]

hazard_long <- d |>
    st_drop_geometry() |>
    select(all_of(hazard_names)) |>
    mutate(geometry = st_geometry(d)) |>
    pivot_longer(cols = -geometry, names_to = "variable", values_to = "value") |>
    st_as_sf()

ggplot(hazard_long) +
    geom_sf(aes(fill = value)) +
    facet_wrap(~variable) +
    scale_fill_viridis_c() +
    theme_minimal() +
    ggtitle("Rabies - Hazard Indicators")

# V

vulnerability_long <- d |>
    st_drop_geometry() |>
    select(all_of(vulnerability_names)) |>
    mutate(geometry = st_geometry(d)) |>
    pivot_longer(cols = -geometry, names_to = "variable", values_to = "value") |>
    st_as_sf()

