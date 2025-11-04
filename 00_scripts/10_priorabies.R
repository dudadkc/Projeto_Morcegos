#' ----
#' aim: prioritization
#' author: re
#' date: Nov 2025
#' ----

library(tidyverse)
library(sf)
require(here)
library(spdep)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(sf)
library(RColorBrewer)
library(dplyr)
library(VennDiagram)

d <- st_read('C:/Users/rdel0062/Downloads/00_mun_data.gpkg')

d$PRural <- as.numeric(d$PRural)

df <- d %>% replace(is.na(.), 0) %>% as.data.frame()

setwd(here())

setwd('01_data/03_eda/')

hazard_names <- scan("hazard_low_vif.txt", what = character())
historical_hazard_names <- c('RABIES_CASES_HUMAN_INFECTION', 'RABIES_CASES_DOMESTIC_CANINE', 'RABIES_CASES_DOMESTIC_FELINE')
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
    scale_x_log10() +
    scale_y_log10() +
    scale_size_continuous(range = c(2, 10)) +
    scale_color_gradient(low = "skyblue", high = "firebrick") +
    labs(x = "People / sq km (log10)", 
         y = "Spending on Health (log10)", 
         size = "Human Rabies Cases", 
         color = "Human Rabies Cases") +
    theme_minimal()


ggplot(d, aes(x = HAB_KM2, y = SPENDING_BASIC_CARE, 
              size = RABIES_CASES_HUMAN_INFECTION, 
              color = RABIES_CASES_HUMAN_INFECTION)) +
    geom_point(alpha = 0.7) +
    scale_x_log10() +
    scale_size_continuous(range = c(2, 10)) +
        scale_color_gradient(low = "skyblue", high = "firebrick") +
    labs(x = "People / sq km (log10)", y = "Spending on Basic Care (%)", 
         size = "Human Rabies Cases", 
         color = "Human Rabies Cases") +
    theme_minimal()

max(d$SPENDING_EPIDEMIO_SURV, na.rm = T)

ggplot(d, aes(x = HAB_KM2, y = SPENDING_EPIDEMIO_SURV, 
              size = RABIES_CASES_HUMAN_INFECTION, 
              color = RABIES_CASES_HUMAN_INFECTION)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(2, 10)) +
    scale_x_log10() +
    scale_color_gradient(low = "skyblue", high = "firebrick") +
    labs(x = "People / sq km", y = "Spending on Epidemiological Surveillance (%)", 
         size = "Human Rabies Cases", 
         color = "Human Rabies Cases") +
    theme_minimal()

# comparison
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


# Updating historical risk

d %>%     filter(
        RABIES_CASES_HUMAN_INFECTION > 0,
        RABIES_CASES_DOMESTIC_FELINE > 0,
        RABIES_CASES_DOMESTIC_CANINE > 0
    ) %>%  select(name_muni, RABIES_CASES_HUMAN_INFECTION, 
           RABIES_CASES_DOMESTIC_FELINE, RABIES_CASES_DOMESTIC_CANINE)

# Mismatches
human_cases <- d$RABIES_CASES_HUMAN_INFECTION > 0
feline_cases <- d$RABIES_CASES_DOMESTIC_FELINE > 0
canine_cases <- d$RABIES_CASES_DOMESTIC_CANINE > 0

# Generate counts for Venn
venn_counts <- list(
    Human = which(human_cases),
    Feline = which(feline_cases),
    Canine = which(canine_cases)
)

# Plot Venn diagram
venn.plot <- venn.diagram(
    x = venn_counts,
    filename = NULL,   # plots to R device, not file
    fill = c("red", "green", "blue"),
    alpha = 0.5,
    cex = 1.5,
    cat.cex = 1.2,
    main = "Overlap of Rabies Cases"
)

grid.draw(venn.plot)


# rescaling

d <- rescale_and_average(d, hazard_names, "hazard")
d <- rescale_and_average(d, vulnerability_names, "vulnerability")
d <- rescale_and_average(d, exposure_names, "exposure")
d <- rescale_and_average(d, historical_hazard_names, "historical_hazard")

# maps

# add  rabies -  #geom_sf(aes(color = RABIES_CASES_HUMAN_INFECTION), color = 'snow3', fill = NA, size = 0.3)

d_rabies <- d %>%
    filter(!is.na(RABIES_CASES_HUMAN_INFECTION) & RABIES_CASES_HUMAN_INFECTION > 0)

#map_hazard

map_hazard <- ggplot(d) +
    geom_sf(aes(fill = hazard_average), color = NA) + 
    scale_fill_gradient(name = "Hazard", high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
    geom_sf(data = d_rabies, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
    scale_color_gradient(name = "Rabies Cases", low = "black", high = "firebrick2") +
    ggtitle("Rescaled Hazard Average") +
    theme_minimal()


#map_vulnerability

map_vulnerability <- ggplot(d) +
    geom_sf(aes(fill = vulnerability_average),  color = NA ) + #'gray20', size = 0.02
    scale_fill_distiller(name = "Vulnerability", palette = "Spectral") +
    geom_sf(data = d_rabies, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
    scale_color_gradient(name = "Rabies Cases", low = "black", high = "firebrick2") +
    #scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
    ggtitle("Rescaled Vulnerability Average") +
    theme_minimal()

#map_exposure
map_exposure <- ggplot(d) +
    geom_sf(aes(fill = exposure_average), color = NA ) +
    scale_fill_viridis_c(name = "Exposure", option = "viridis") +
    geom_sf(data = d_rabies, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
    scale_color_gradient(name = "Rabies Cases", low = "black", high = "firebrick2") +
    ggtitle("Rescaled Exposure Average") +
    theme_minimal()


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


# Spatial prioritization ---------------------------------------------------

colnames(d)



d_flagged <- d %>%
    mutate(
        RABIES_CASES_HUMAN_INFECTION = as.numeric(RABIES_CASES_HUMAN_INFECTION),
        RABIES_CASES_DOMESTIC_FELINE = as.numeric(RABIES_CASES_DOMESTIC_FELINE),
        RABIES_CASES_DOMESTIC_CANINE = as.numeric(RABIES_CASES_DOMESTIC_CANINE)
    ) %>%
    replace_na(list(
        RABIES_CASES_HUMAN_INFECTION = 0,
        RABIES_CASES_DOMESTIC_FELINE = 0,
        RABIES_CASES_DOMESTIC_CANINE = 0
    )) %>%
    mutate(
        historical_flag = ifelse(
            RABIES_CASES_HUMAN_INFECTION > 0 |
                RABIES_CASES_DOMESTIC_FELINE > 0 |
                RABIES_CASES_DOMESTIC_CANINE > 0,
            1, 0
        )
    )


# Check
head(d_flagged[, c("RABIES_CASES_HUMAN_INFECTION", "RABIES_CASES_DOMESTIC_FELINE",
                   "RABIES_CASES_DOMESTIC_CANINE", "historical_flag")])
table(d_flagged$historical_flag)


nb <- poly2nb(d)

no_neighbors <- which(card(nb) == 0) 
d[no_neighbors, 'RABIES_CASES_DOMESTIC_FELINE'] 
d[no_neighbors, 'RABIES_CASES_DOMESTIC_CANINE'] 
d[no_neighbors, 'RABIES_CASES_HUMAN_INFECTION'] 
d[no_neighbors,]


listw <- nb2listw(nb, style = "B", zero.policy = TRUE)  # binary weights, ignore islands

d$historical_flag <- d_flagged$historical_flag

d$neighbor_risk <- lag.listw(listw, d$historical_flag)

table(d$historical_flag)
table(d$neighbor_risk)

d <- d %>% mutate(  Spatial_prioritization_index = (exposure_average + hazard_average + vulnerability_average) / 3 + 
                 0.5 * historical_flag + 0.5 * neighbor_risk    )

hist(d$Spatial_prioritization_index)


d$scale_index <- scales::rescale(d$Spatial_prioritization_index)
summary(d$scale_index)


quantile(d$scale_index)[1]
#     0%        25%        50%        75%       100% 

q <- quantile(d$scale_index, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))

muni_sel <- d %>%
    mutate(
        index = scale_index,
        index_cat = case_when(
            index <= q[2] ~ "Very low",
            index > q[2] & index <= q[3] ~ "Low",
            index > q[3] & index <= q[4] ~ "Average",
            index > q[4] & index <= q[5] ~ "High",
            index > q[5] ~ "Very high"
        ),
        index_cat = fct_relevel(index_cat, c("Very low", "Low", "Average", "High", "Very high"))
    )

quantile(muni_sel$scale_index)

table(muni_sel$index_cat)

sum(d$scale_index > 0.04151593, na.rm = TRUE)


reg <- geobr::read_region(year = 2020) %>% 
    sf::st_crop(xmin = -74, ymin = -34, xmax = -35, ymax = 5)
reg


very_high_munis <- muni_sel %>%
    filter(index_cat == "Very high" & historical_flag == 1) %>%
    pull(name_muni)


length(very_high_munis)

# Sel

muni_cidades <- muni_sel %>% 
    dplyr::filter(name_muni %in% very_high_munis)

muni_cidades

muni_cidades_coords <- muni_cidades %>%
        mutate(centroid = st_centroid(.)) %>%
        mutate(
        X = st_coordinates(centroid)[,1],
        Y = st_coordinates(centroid)[,2]
    )

muni_sel$index_cat

map_prio <- ggplot() +   
    geom_sf(data = muni_sel, aes(fill = index_cat), colour = "white", linewidth = 0.01, alpha = 0.7) +
    # Region outline
    geom_sf(data = reg, fill = NA, colour = "black", linewidth = 0.8) +
    # City points
    #geom_sf(data = muni_sel, fill = "white", shape = 21, size = 2, colour = "gray40") +
    geom_text_repel(
        data = st_centroid(muni_cidades_coords) %>%
            cbind(st_coordinates(.)),
        aes(x = X, y = Y, label = name_muni),
        colour = "white",
        bg.color = "black",
        bg.r = 0.2,
        box.padding = 0.3,
        segment.colour = "grey60",
        size = 3
    ) +
     scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Index") +
     labs(
        title = "Rabies spatial prioritization index & Historical rabies risk",
        caption = "Font: IBGE"
    ) +
    annotation_north_arrow(
        location = "br",
        which_north = "true",
        style = north_arrow_fancy_orienteering
    ) +
    annotation_scale(
        location = "bl",
        bar_cols = c("grey80", "white"),
        text_cex = 0.8     ) +
    theme_minimal(base_family = "sans") +
    theme(  panel.grid.major = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position.inside = c(0.1, 0.3),
            legend.background = element_blank(),
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.caption = element_text(size = 9, hjust = 0, colour = "grey30"),
            plot.background = element_rect(fill = "white", colour = NA) )


map_prio

# Export
setwd(here())
setwd('99_manuscript')
ggsave( filename = "map_prio_AND_historical.jpg",
        plot = map_prio,
        width = 10,
        height = 8,
        units = "in",
        dpi = 300,
        bg = "white" )


# Just neighborhood

very_high_munis <- muni_sel %>%
    filter(index_cat == "Very high" & neighbor_risk== 1) %>%
    pull(name_muni)

length(very_high_munis)


muni_cidades <- muni_sel %>% 
    dplyr::filter(name_muni %in% very_high_munis)

muni_cidades

muni_cidades_coords <- muni_cidades %>%
    mutate(centroid = st_centroid(.)) %>%
    mutate(
        X = st_coordinates(centroid)[,1],
        Y = st_coordinates(centroid)[,2]
    )

muni_sel$index_cat

map_prio_neighbor <- ggplot() +   
    geom_sf(data = muni_sel, aes(fill = index_cat), colour = "white", linewidth = 0.01, alpha = 0.7) +
    # Region outline
    geom_sf(data = reg, fill = NA, colour = "black", linewidth = 0.8) +
    # City points
    #geom_sf(data = muni_sel, fill = "white", shape = 21, size = 2, colour = "gray40") +
    geom_text_repel(
        data = st_centroid(muni_cidades_coords) %>%
            cbind(st_coordinates(.)),
        aes(x = X, y = Y, label = name_muni),
        colour = "white",
        bg.color = "black",
        bg.r = 0.2,
        box.padding = 0.3,
        segment.colour = "grey60",
        size = 3
    ) +
    scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Index") +
    labs(
        title = "Rabies spatial prioritization index & Contiguity risk",
        caption = "Font: IBGE"
    ) +
    annotation_north_arrow(
        location = "br",
        which_north = "true",
        style = north_arrow_fancy_orienteering
    ) +
    annotation_scale(
        location = "bl",
        bar_cols = c("grey80", "white"),
        text_cex = 0.8     ) +
    theme_minimal(base_family = "sans") +
    theme(  panel.grid.major = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position.inside = c(0.1, 0.3),
            legend.background = element_blank(),
            plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
            plot.caption = element_text(size = 9, hjust = 0, colour = "grey30"),
            plot.background = element_rect(fill = "white", colour = NA) )


map_prio_neighbor

# Export
setwd(here())
setwd('99_manuscript')
ggsave( filename = "map_prio_AND_neihbor.jpg",
        plot = map_prio_neighbor,
        width = 10,
        height = 8,
        units = "in",
        dpi = 300,
        bg = "white" )


#--------------

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


# historic risk - old eval - Human only 
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