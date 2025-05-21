#' ----
#' aim: prioritization
#' author: re
#' date: May 2025
#' ----

library(tidyverse)
library(sf)
require(here)
require(RColorBrewer)


d <- st_read('D://OneDrive - Massey University//Supervisions//Madu//03_prioritization//00_mun_data.gpkg')

d$PRural <- as.numeric(d$PRural)

df <- d %>% replace(is.na(.), 0) %>% as.data.frame()

setwd(here())

setwd('01_data/03_eda/')

hazard_names <- scan("hazard_low_vif.txt", what = character())
vulnerability_names <- scan("vulnerability_low_vif.txt", what = character())
exposure_names <- scan("exposure_low_vif.txt", what = character())

rescale_and_average <- function(data, var_names, prefix) {
    rescaled_vars <- paste0("rescaled_", var_names)
    
    data <- data %>%
        mutate(across(all_of(var_names), ~ scales::rescale(., to = c(0, 1)), .names = "rescaled_{.col}")) %>%
        rowwise() %>%
        mutate("{prefix}_average" := mean(c_across(all_of(rescaled_vars)), na.rm = TRUE)) %>%
        ungroup()
    
    return(data)
}


d <- rescale_and_average(d, hazard_names, "hazard")
d <- rescale_and_average(d, vulnerability_names, "vulnerability")
d <- rescale_and_average(d, exposure_names, "exposure")

# maps
map_hazard <- ggplot(d) +
    geom_sf(aes(fill = hazard_average),  color = 'gray20', size = 0.02) +
    #scale_fill_viridis_c(name = "Hazard", option = "inferno") +
    scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
    ggtitle("Rescaled Hazard Average") +
    theme_minimal()

map_hazard

map_vulnerability <- ggplot(d) +
    geom_sf(aes(fill = vulnerability_average),  color = 'gray20', size = 0.02) +
    scale_fill_distiller(name = "Vulnerability", palette = "Spectral") +
    #scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
    ggtitle("Rescaled Vulnerability Average") +
    theme_minimal()

map_exposure <- ggplot(d) +
    geom_sf(aes(fill = exposure_average), color = 'gray20', size = 0.02) +
    scale_fill_viridis_c(name = "Exposure", option = "viridis") +
    ggtitle("Rescaled Exposure Average") +
    theme_minimal()

# all
library(ggpubr)
setwd(here())
setwd('99_manuscript')

jpeg(filename = "rabies_risk_components.jpg", width = 22, height = 30, units = "cm", res = 400)
ggarrange(
    map_hazard,
    map_vulnerability,
    map_exposure,
    ncol = 1,
    nrow = 3,
    align = "v"
)
dev.off()

# historic risk 
top5_labels <- d %>%
    filter(!is.na(RABIES_CASES_HUMAN_INFECTION)) %>%
    top_n(5, RABIES_CASES_HUMAN_INFECTION)

top5_labels_coords <- st_centroid(top5_labels) %>%
    cbind(st_coordinates(.))  # adds X and Y columns

map_historic_risk <- ggplot(d) +
    geom_sf(aes(fill = RABIES_CASES_HUMAN_INFECTION)) +
    ggrepel::geom_text_repel(
        data = top5_labels_coords,
        aes(x = X, y = Y, label = paste0(NM_MUN, "\n(", RABIES_CASES_HUMAN_INFECTION, ")")),
        size = 3,
        color = "black",
        box.padding = 0.5,
        point.padding = 0.3,
        segment.color = "gray"
    ) +
    scale_fill_viridis_c(
        name = "Human rabies cases",
        option = "magma",
        na.value = "white"
    ) +
    labs(
        title = "Rabies Historical Risk",
        fill = "Human rabies cases"  # explicitly naming the fill to avoid defaults
    ) +
    ggtitle("Rabies Historic Risk") +
    theme_minimal()

jpeg(filename = "rabies_historic_risk.jpg", width = 22, height = 22, units = "cm", res = 400)
map_historic_risk
dev.off()
#------------------------------------------------------------------------------------------