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
library(tmap)
library(corrplot)


d <- st_read('D://OneDrive - Massey University//Supervisions//Madu//03_prioritization//00_mun_data.gpkg')

d$PRural <- as.numeric(d$PRural)

df <- d %>% replace(is.na(.), 0) %>% as.data.frame()

setwd(here())
setwd('01_data/03_eda/')

hazard_names <- scan("hazard_low_vif.txt", what = character())
vulnerability_names <- scan("vulnerability_low_vif.txt", what = character())
exposure_names <- scan("exposure_low_vif.txt", what = character())

# plot

reshape_for_tmap <- function(data, variable_names) {
    data |>
        st_drop_geometry() |>
        select(all_of(variable_names)) |>
        mutate(geometry = st_geometry(d)) |>
        pivot_longer(cols = -geometry, names_to = "variable", values_to = "value") |>
        st_as_sf()
}

hazard_long <- reshape_for_tmap(df, hazard_names)
vulnerability_long <- reshape_for_tmap(df, vulnerability_names)
exposure_long <- reshape_for_tmap(df, exposure_names)

tmap_mode("plot")

# Hazard
tm_shape(hazard_long) +
    tm_fill(
        "value",
        fill.scale = tm_scale(values = "viridis"),
        fill.legend = tm_legend(title = "Hazard")
    ) +
    tm_borders() +
    tm_facets(by = "variable") +
    tm_title("Hazard Indicators")

# Vulnerability


# Exposure

#