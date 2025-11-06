#' ----
#' aim: spatial prioritization rabies prevention
#' author: re
#' date: Nov 2025
#' ----

library(tidyverse)
library(plotly)
library(dplyr)
library(viridis)
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
library(pROC)
library(nngeo)
library(grid)
library(openxlsx)
library(dplyr)


d <- st_read('C:/Users/rdel0062/Downloads/00_mun_data.gpkg')

# Region data

reg <- geobr::read_region(year = 2020) %>% 
    sf::st_crop(xmin = -74, ymin = -34, xmax = -35, ymax = 5)
reg

# Data
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


# For Imbe city ---------- no information for sdm so we will fill in with its closest neighbor (Novo Barreiro, suitabilty = 0.7)

d %>% 
    filter(is.na(sdm_cont_mn)) %>% 
    select(name_muni) 

na_rows <- which(is.na(d$sdm_cont_mn))

for(i in na_rows){
    nearest <- st_nn(d[i,], d %>% filter(!is.na(sdm_cont_mn)), k = 1, returnDist = FALSE)[[1]]
    d$sdm_cont_mn[i] <- d$sdm_cont_mn[nearest]
}

d[nearest,'name_muni']
d[na_rows, 'sdm_cont_mn']


summary(d$CANINE_VACCINATED)
summary(d$sdm_cont_mn)

d$CANINE_VACCINATED <- d$CANINE_VACCINATED*(-1)
summary(d$CANINE_VACCINATED)

summary(d$CANINE_VACCINATED)
cor(d$CANINE_VACCINATED, d$sdm_cont_mn)

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


# Plot density histogram - Supplements
ggplot(d, aes(x = RabiesKnown, y = SPENDING_EPIDEMIO_SURV, fill = RabiesKnown)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    #geom_boxplot(width = 0.1, outlier.shape = NA) +
    scale_fill_manual(values = c("Rabies confirmed" = "firebrick", "Unknown" = "grey")) +
    labs(title = "Spending on Epidemiological Surveillance by Rabies cases",
         x = "Rabies Infection Data",
         y = "Spending on Epidemiological Surveillance") +
    theme_minimal()


# historical risk
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


png(filename = "venn_rabies_cases.png",
    width = 2000,   # pixels
    height = 2000,
    res = 300       )


venn_counts <- list( Human = which(human_cases), Feline = which(feline_cases), Canine = which(canine_cases) )


venn.plot <- venn.diagram(
    x = venn_counts,
    filename = NULL,
    fill = c("salmon",'lightyellow' ,"skyblue"),
    alpha = 0.5,
    cex = 1.8,
    fontface = "bold",
    cat.cex = 1.4,
    cat.fontface = "bold",
    cat.pos = c(-20, 20, 180),
    cat.dist = c(0.05, 0.05, 0.05),
    main = "Overlap of rabies cases in Brazilian cities",
    main.cex = 2,
    main.fontface = "bold",
    lwd = 1.2
)

grid.draw(venn.plot)
dev.off()

# rescaling risk components

d <- rescale_and_average(d, hazard_names, "hazard")
d <- rescale_and_average(d, vulnerability_names, "vulnerability")
d <- rescale_and_average(d, exposure_names, "exposure")

# maps

#map_hazard

map_hazard <- ggplot(d) +
    geom_sf(aes(fill = hazard_average), color = NA) + 
    scale_fill_gradient(name = "Hazard", high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
    geom_sf(data = d, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
    scale_color_gradient(name = "Rabies Cases", low = "black", high = "firebrick2") +
    ggtitle("Rescaled Hazard Average") +
    theme_minimal()


#map_vulnerability

map_vulnerability <- ggplot(d) +
    geom_sf(aes(fill = vulnerability_average),  color = NA ) + #'gray20', size = 0.02
    scale_fill_distiller(name = "Vulnerability", palette = "Spectral") +
    geom_sf(data = d, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
    scale_color_gradient(name = "Rabies Cases", low = "black", high = "firebrick2") +
    #scale_fill_gradient(high = "#e34a33", low = "#fee8c8", guide = "colorbar") +
    ggtitle("Rescaled Vulnerability Average") +
    theme_minimal()

#map_exposure
map_exposure <- ggplot(d) +
    geom_sf(aes(fill = exposure_average), color = NA ) +
    scale_fill_viridis_c(name = "Exposure", option = "viridis") +
    geom_sf(data = d, aes(color = RABIES_CASES_HUMAN_INFECTION), fill = NA, size = 0.4) +
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


# historic hazard

label_coords <- d %>%
    st_centroid() %>%   # centroids for labeling
    mutate(X = st_coordinates(.)[,1],
           Y = st_coordinates(.)[,2]) %>%
    select(NM_MUN,
           RABIES_CASES_HUMAN_INFECTION,
           RABIES_CASES_DOMESTIC_FELINE,
           RABIES_CASES_DOMESTIC_CANINE,
           X, Y) %>%
    pivot_longer(
        cols = c(RABIES_CASES_HUMAN_INFECTION,
                 RABIES_CASES_DOMESTIC_FELINE,
                 RABIES_CASES_DOMESTIC_CANINE),
        names_to = "host",
        values_to = "cases"
    ) %>%
    filter(!is.na(cases)) %>%
    mutate(
        host = recode(host,
                      "RABIES_CASES_HUMAN_INFECTION" = "Human",
                      "RABIES_CASES_DOMESTIC_FELINE" = "Feline",
                      "RABIES_CASES_DOMESTIC_CANINE" = "Canine")
    ) %>%
    group_by(host) %>%
    slice_max(order_by = cases, n = 5) %>%  # top 10
    ungroup()

# melt 
d_long <- d %>%
    pivot_longer(
        cols = c(RABIES_CASES_HUMAN_INFECTION,
                 RABIES_CASES_DOMESTIC_FELINE,
                 RABIES_CASES_DOMESTIC_CANINE),
        names_to = "host",
        values_to = "cases"
    ) %>%
    mutate(
        host = recode(host,
                      "RABIES_CASES_HUMAN_INFECTION" = "Human",
                      "RABIES_CASES_DOMESTIC_FELINE" = "Feline",
                      "RABIES_CASES_DOMESTIC_CANINE" = "Canine")
    )


# Fig rabies in dogs, cats, and humans

fig_facets1 <- ggplot(d_long) +
    geom_sf(aes(fill = cases), colour = "gray80", linewidth = 0.01, alpha = 0.7) +
    geom_sf(data = reg, fill = NA, colour = "black", linewidth = 0.8) +
    ggrepel::geom_text_repel(      data = label_coords,
        aes(x = X, y = Y, label = paste0(NM_MUN, "\n(", cases, ")")),
        size = 3,
        box.padding = 0.5,
        point.padding = 0.3,
        segment.color = "gray"
    ) +
    scale_fill_gradient(
        low = "darkgreen",
        high = "purple1",
        na.value = "white",
        name = "Rabies cases"    ) +
    facet_wrap(~host, ncol = 1) +
    labs(
        title = "",
        fill = "Rabies cases",    x = "Longitude",           y = "Latitude"   ) +
    annotation_north_arrow(pad_x = unit(1.2, "cm"), pad_y = unit(1.5, "cm"), which_north = "true", style = north_arrow_fancy_orienteering) +
    annotation_scale(pad_x = unit(1.5, "cm"), pad_y = unit(1, "cm"), bar_cols = c("grey80", "white"), text_cex = 0.8) +
    theme_minimal(base_family = "sans") +
    theme( legend.position = "bottom",        strip.text = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", hjust = 0.5)
    )

fig_facets1

setwd(here())
setwd('99_manuscript')
jpeg(filename = "rabies_historic_top_long.jpg", width =22, height = 32, units = "cm", res = 400)
fig_facets1
dev.off()

# Neighborhood -----------------------------------------------------------------

nb <- poly2nb(d) #slow

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

# Composite index

d <- d %>% mutate(  Spatial_prioritization_index = (exposure_average + hazard_average + vulnerability_average) / 3 + 
                 0.5 * historical_flag + 0.5 * neighbor_risk    )

d <- d %>% mutate(  Spatial_prioritization_index_1 = (exposure_average + hazard_average + vulnerability_average) / 3 + 
                       1 * historical_flag + 1 * neighbor_risk    )


preds <- (d$exposure_average + d$hazard_average + d$vulnerability_average) / 3

# Explo

plot(preds, d$historical_flag)
cor(d$exposure_average, d$historical_flag) # positive
cor(d$hazard_average, d$historical_flag) # negative
cor(d$vulnerability_average, d$historical_flag) # positive

cor(d$exposure_average, d$hazard_average)
cor(d$exposure_average, d$vulnerability_average)
cor(d$hazard_average, d$vulnerability_average)
summary(d$CANINE_VACCINATED)

cor(d$CANINE_VACCINATED, d$sdm_cont_mn)

#plot_ly(
 #   data = d,
   # x = ~exposure_average,
  #  y = ~hazard_average,
  #  z = ~vulnerability_average,
  #  color = ~historical_flag, #+ RABIES_CASES_DOMESTIC_CANINE + RABIES_CASES_DOMESTIC_FELINE,
  #  colors = viridis(100),
 #   type = "scatter3d",
 #   mode = "markers",
 #   marker = list(size = 4, opacity = 0.6)
#)

# Because spatial correlation occurs - historical rabies cases tend to cluster spatially

moran.test(d$historical_flag, listw)

#Moran I test under randomisation
#Moran I statistic standard deviate = 10.241, p-value < 2.2e-16
#alternative hypothesis: greater
#sample estimates:
#    Moran I statistic       Expectation          Variance 
#8.074974e-02     -1.795977e-04      6.245003e-05 

moran.test(d$Spatial_prioritization_index, listw)

moran.test(d$Spatial_prioritization_index_1, listw)

hist(d$Spatial_prioritization_index)

summary(d$Spatial_prioritization_index)

library(pROC)
d$historical_flag
roc_obj <- roc(d$historical_flag, d$Spatial_prioritization_index)

roc_obj1 <- roc(d$historical_flag, d$Spatial_prioritization_index_1)

auc(roc_obj)
auc(roc_obj1)
plot(roc_obj)
plot(roc_obj1)
# Classifying prioritization levels

min(d$Spatial_prioritization_index)
mean(d$Spatial_prioritization_index) + sd(d$Spatial_prioritization_index)
max(d$Spatial_prioritization_index)

d$scale_index <- scales::rescale(d$Spatial_prioritization_index)

# n = n()


library(classInt)
jenks_breaks <- classIntervals(d$Spatial_prioritization_index, n = 5, style = "jenks")$brks
#Jenks Natural Breaks
#groups similar values together while maximising the differences between groups

jenks_breaks


d %>%
    filter(Spatial_prioritization_index > jenks_breaks[5]) %>%
    select(name_muni, Spatial_prioritization_index)

d %>%
    filter(Spatial_prioritization_index > jenks_breaks[5]) %>%
    select(name_muni, historical_flag, name_state)

d_high_priority <- d %>%
    filter(Spatial_prioritization_index > jenks_breaks[4]) %>%
    select(name_muni, historical_flag, name_state)


muni_sel <- d %>%
    mutate(
        index = Spatial_prioritization_index,
        index_cat = case_when(
            index <= jenks_breaks[2] ~ "Very low",
            index > jenks_breaks[2] & index <= jenks_breaks[3] ~ "Low",
            index > jenks_breaks[3] & index <= jenks_breaks[4] ~ "Average",
            index > jenks_breaks[4] & index <= jenks_breaks[5] ~ "High",
            index > jenks_breaks[5] ~ "Very high"
        ),
        index_cat = fct_relevel(index_cat, c("Very low", "Low", "Average", "High", "Very high"))
    )

quantile(muni_sel$scale_index)

table(muni_sel$index_cat)

# Export table
# Prioriti index for all municipalities of Brazil
table(muni_sel$index_cat)
out_table <- muni_sel %>%
    arrange(factor(index_cat, levels = c("Very high", "High", "Average", "Low", "Very low"))) %>%
    select(Municipality = name_muni,
           State = name_state,
           `Rabies detection` = historical_flag,
           Priority = index_cat) %>%
    st_set_geometry(NULL)

#write.xlsx(out_table, "Table_S1_Rabies_Prioritization_Brazil_5571.xlsx", overwrite = TRUE)

# Table 1 - high risk

out_table_high <-  muni_sel %>%
    filter(index_cat %in% c("Very high", "High")) %>%
    arrange(factor(index_cat, levels = c("Very high", "High"))) %>%
    select(Municipality = name_muni,
           State = name_state,
           `Rabies detection` = historical_flag,
           Priority = index_cat) %>%
    st_set_geometry(NULL)

nrow(out_table_high)

# Export to Excel
#write.xlsx(out_table_high, "Table_1.xlsx", overwrite = TRUE)

# Selectiion

very_high_munis <- muni_sel %>%
    filter(index_cat == "Very high"| index_cat == "High" ) %>% #& historical_flag == 0
    pull(name_muni)

very_high_munis


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

map_prio <- ggplot() +   
    geom_sf(data = muni_sel, aes(fill = index_cat), colour = "white", linewidth = 0.01, alpha = 0.7) +
    # Region outline
    geom_sf(data = reg, fill = NA, colour = "black", linewidth = 0.9) +
    geom_text_repel(  data = st_centroid(muni_cidades_coords) %>%
            cbind(st_coordinates(.)),
        aes(x = X, y = Y, label = name_muni),
        colour = "white",
        bg.color = "black",
        bg.r = 0.2,
        box.padding = 0.3,
        segment.colour = "grey60",
        size = 3, 
        force = 2 ,
        max.overlaps = 100) +
    scale_fill_brewer(palette = "RdYlGn", direction = -1, name = "Index") +
    labs(title = "", fill = "Spatial\nPrioritization Index") +
    annotation_north_arrow(pad_x = unit(1.2, "cm"), pad_y = unit(1.5, "cm"), which_north = "true", style = north_arrow_fancy_orienteering) +
    annotation_scale(pad_x = unit(1.5, "cm"), pad_y = unit(1, "cm"), bar_cols = c("grey80", "white"), text_cex = 0.8) +
    theme_minimal(base_family = "sans") +
    theme(panel.grid.major = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = c(.15, .3),
          legend.background = element_blank(),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 10),
          plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
          plot.caption = element_text(size = 9, hjust = 0, colour = "grey30"),
          plot.background = element_rect(fill = "white", colour = NA) )


map_prio

# Export
setwd(here())
setwd('99_manuscript')
ggsave( filename = "Figure_5.jpg",
        plot = map_prio,
        width = 10,
        height = 8,
        units = "in",
        dpi = 300,
        bg = "white" )

#------------------------------------------------------------------------------------------