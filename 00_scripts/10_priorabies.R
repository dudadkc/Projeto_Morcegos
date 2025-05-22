#' ----
#' aim: prioritization
#' author: re
#' date: May 2025
#' ----

library(tidyverse)
library(sf)
require(here)
require(RColorBrewer)


d <- st_read('C://Users//rdelaram//Downloads//00_mun_data.gpkg')

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

# Arranging the scale for positive expectations

summary(d$CANINE_VACCINATED)
d$CANINE_VACCINATED <- d$CANINE_VACCINATED*(-1)
summary(d$CANINE_VACCINATED)

d$RT_LITE <- d$RT_LITE*(-1)

plot(d$SPENDING_BASIC_CARE ~ d$PTotal)
plot(d$SPENDING_EPIDEMIO_SURV ~  d$PTota)
plot(d$SPENDING_HEALTH~  d$PTota)

d$HAB_KM2
ggplot(d, aes(x = HAB_KM2, y = SPENDING_HEALTH, 
              size = RABIES_CASES_HUMAN_INFECTION, 
              color = RABIES_CASES_HUMAN_INFECTION)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(2, 10)) +
    scale_color_gradient(low = "skyblue", high = "firebrick") +
    labs(x = "People / sq km", y = "Spending on Health", 
         size = "Human Rabies Cases", 
         color = "Human Rabies Cases") +
    theme_minimal()


ggplot(d, aes(x = HAB_KM2, y = SPENDING_BASIC_CARE, 
              size = RABIES_CASES_HUMAN_INFECTION, 
              color = RABIES_CASES_HUMAN_INFECTION)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(2, 10)) +
    scale_color_gradient(low = "skyblue", high = "firebrick") +
    labs(x = "People / sq km", y = "Spending on Basic Care", 
         size = "Human Rabies Cases", 
         color = "Human Rabies Cases") +
    theme_minimal()


ggplot(d, aes(x = HAB_KM2, y = SPENDING_EPIDEMIO_SURV, 
              size = RABIES_CASES_HUMAN_INFECTION, 
              color = RABIES_CASES_HUMAN_INFECTION)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(2, 10)) +
    scale_color_gradient(low = "skyblue", high = "firebrick") +
    labs(x = "People / sq km", y = "Spending on Epidemiological Surveillance", 
         size = "Human Rabies Cases", 
         color = "Human Rabies Cases") +
    theme_minimal()


d <- d %>%
    mutate(RabiesKnown = ifelse(!is.na(RABIES_CASES_HUMAN_INFECTION), "Rabies confirmed", "Unknown"))


d %>%   mutate(RabiesKnown = ifelse(!is.na(RABIES_CASES_HUMAN_INFECTION), "Rabies confirmed", "Unknown")) %>%
    group_by(RabiesKnown) %>%
    summarise(
        mean_spending = mean(SPENDING_HEALTH, na.rm = TRUE),
        sd_spending = sd(SPENDING_HEALTH, na.rm = TRUE),
        n = n()
    )


# Plot density histogram
ggplot(d, aes(x = RabiesKnown, y = SPENDING_EPIDEMIO_SURV, fill = RabiesKnown)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    #geom_boxplot(width = 0.1, outlier.shape = NA) +
    scale_fill_manual(values = c("Rabies confirmed" = "firebrick", "Unknown" = "grey")) +
    labs(title = "Spending on Epidemiological Surveillance by Rabies cases",
         x = "Rabies Infection Data",
         y = "Spending on Epidemiological Surveillance") +
    theme_minimal()


# rescaling
d <- rescale_and_average(d, hazard_names, "hazard")
d <- rescale_and_average(d, vulnerability_names, "vulnerability")
d <- rescale_and_average(d, exposure_names, "exposure")

# maps

# add  rabies -  
#geom_sf(aes(color = RABIES_CASES_HUMAN_INFECTION), color = 'snow3', fill = NA, size = 0.3)

d_rabies <- d %>%
    filter(!is.na(RABIES_CASES_HUMAN_INFECTION) & RABIES_CASES_HUMAN_INFECTION > 0)

map_hazard <- ggplot(d) +
    geom_sf(aes(fill = hazard_average), color = NA) + 
    scale_fill_gradient(name = "Hazard", high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
    geom_sf(data = d_rabies, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
    scale_color_gradient(name = "Rabies Cases", low = "black", high = "firebrick2") +
    ggtitle("Rescaled Hazard Average") +
    theme_minimal()

#map_hazard

map_vulnerability <- ggplot(d) +
    geom_sf(aes(fill = vulnerability_average),  color = NA ) + #'gray20', size = 0.02
    scale_fill_distiller(name = "Vulnerability", palette = "Spectral") +
    geom_sf(data = d_rabies, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
    scale_color_gradient(name = "Rabies Cases", low = "black", high = "firebrick2") +
    #scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
    ggtitle("Rescaled Vulnerability Average") +
    theme_minimal()

#map_vulnerability

map_exposure <- ggplot(d) +
    geom_sf(aes(fill = exposure_average), color = NA ) +
    scale_fill_viridis_c(name = "Exposure", option = "viridis") +
    geom_sf(data = d_rabies, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
    scale_color_gradient(name = "Rabies Cases", low = "black", high = "firebrick2") +
    ggtitle("Rescaled Exposure Average") +
    theme_minimal()

#map_exposure

# all
library(ggpubr)
setwd(here())
setwd('99_manuscript')
# width = 22, height = 30
jpeg(filename = "rabies_risk_components_wide.jpg", width = 33, height = 23, units = "cm", res = 400)
ggarrange(
    map_hazard,
    map_vulnerability,
    map_exposure,
    ncol = 3,
    nrow = 1,
    align = "v"
)
dev.off()

jpeg(filename = "rabies_risk_components_long.jpg", width = 23, height = 30, units = "cm", res = 400)
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

map_historic_risk <- ggplot(d) +  geom_sf(aes(fill = RABIES_CASES_HUMAN_INFECTION), color = NA ) +
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
    ) +     labs(      title = "Rabies Historical Risk",
        fill = "Human rabies cases" ) +
    ggtitle("Rabies Historic Risk") +
    theme_minimal()

jpeg(filename = "rabies_historic_risk.jpg", width = 22, height = 22, units = "cm", res = 400)
map_historic_risk
dev.off()
#------------------------------------------------------------------------------------------