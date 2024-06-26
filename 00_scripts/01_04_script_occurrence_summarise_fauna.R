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
library(terra)
library(tmap)
library(viridis)
library(ggh4x)
library(parallelly)
library(doParallel)
library(foreach)
library(furrr)
library(foreach)
library(writexl)

# options
sf::sf_use_s2(FALSE)
tmap::tmap_options(check.and.fix = TRUE) 

# import data -------------------------------------------------------------

## species lists ----
list_fauna <- readr::read_csv("02_data/00_species_list/fauna_mma_2024_fitered.csv")
list_fauna

## occurrences ----
fauna_order_group <- readr::read_csv("02_data/00_species_list/fauna_orders.csv")
fauna_order_group

occ_fauna_raw <- readr::read_csv("02_data/01_occurrences/02_integrated/00_occ_raw_fauna.csv") %>% 
    dplyr::left_join(fauna_order_group)
occ_fauna_raw

occ_fauna_cleaned <- readr::read_csv("02_data/01_occurrences/03_cleaned/occ_cleaned_fauna_regional.csv")
occ_fauna_cleaned

# information
nrow(occ_fauna_cleaned)
length(unique(occ_fauna_cleaned$species))

# summarise raw data ---------------------------------------------------------------

## fauna ----
occ_fauna_raw_groups <- occ_fauna_raw %>% 
    dplyr::group_by(group, species) %>% 
    dplyr::summarise(nocc_total = n()) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(nsp_total = n(),
                     nocc_total = sum(nocc_total))
occ_fauna_raw_groups 

occ_fauna_raw_sources <- occ_fauna_raw %>% 
    dplyr::group_by(source, species) %>% 
    dplyr::summarise(nocc_total = n()) %>% 
    dplyr::group_by(source) %>% 
    dplyr::summarise(nsp_total = n(),
                     nocc_total = sum(nocc_total))
occ_fauna_raw_sources 

occ_fauna_raw_categories <- occ_fauna_raw %>% 
    dplyr::group_by(category, species) %>% 
    dplyr::summarise(nocc_total = n()) %>% 
    dplyr::group_by(category) %>% 
    dplyr::summarise(nsp_total = n(),
                     nocc_total = sum(nocc_total))
occ_fauna_raw_categories 

occ_fauna_raw_groups_categories <- occ_fauna_raw %>% 
    dplyr::group_by(group, category, species) %>% 
    dplyr::summarise(nocc_total = n()) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(nsp_total = n(),
                     nocc_total = sum(nocc_total))
occ_fauna_raw_groups_categories 

occ_fauna_raw_groups_categories_sources <- occ_fauna_raw %>% 
    dplyr::group_by(group, category, source, species) %>% 
    dplyr::summarise(nocc_total = n()) %>% 
    dplyr::group_by(group, category, source) %>% 
    dplyr::summarise(nsp_total = n(),
                     nocc_total = sum(nocc_total))
occ_fauna_raw_groups_categories_sources 

# summarise cleaned data ---------------------------------------------------------------

## fauna ----
occ_fauna_cleaned_groups <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(nsp_clean = n(),
                     nocc_clean = sum(nocc_clean)) 
occ_fauna_cleaned_groups 

occ_fauna_cleaned_sources <- occ_fauna_cleaned %>% 
    dplyr::group_by(source, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::group_by(source) %>% 
    dplyr::summarise(nsp_clean = n(),
                     nocc_clean = sum(nocc_clean))
occ_fauna_cleaned_sources 

occ_fauna_cleaned_categories <- occ_fauna_cleaned %>% 
    dplyr::group_by(category, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::group_by(category) %>% 
    dplyr::summarise(nsp_clean = n(),
                     nocc_clean = sum(nocc_clean))
occ_fauna_cleaned_categories 

occ_fauna_cleaned_groups_categories <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, category, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(nsp_clean = n(),
                     nocc_clean = sum(nocc_clean))
occ_fauna_cleaned_groups_categories 

occ_fauna_cleaned_groups_categories_sources <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, category, source, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::group_by(group, category, source) %>% 
    dplyr::summarise(nsp_clean = n(),
                     nocc_clean = sum(nocc_clean))
occ_fauna_cleaned_groups_categories_sources 

# summarise clean data with species >=25 e >=10 occurrences --------------------------------

## fauna -----

### grupos (invert, anfíbios, etc) ----
occ_fauna_cleaned_groups_over25 <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 25) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(nsp_clean_25 = n(),
                     nocc_clean_25 = sum(nocc_clean))
occ_fauna_cleaned_groups_over25 

occ_fauna_cleaned_groups_over10 <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 10) %>% 
    dplyr::group_by(group) %>% 
    dplyr::summarise(nsp_clean_10 = n(),
                     nocc_clean_10 = sum(nocc_clean))
occ_fauna_cleaned_groups_over10 

### fontes de dados (source) ----
occ_fauna_cleaned_sources_over25 <- occ_fauna_cleaned %>% 
    dplyr::group_by(source, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 25) %>% 
    dplyr::group_by(source) %>% 
    dplyr::summarise(nsp_clean_25 = n(),
                     nocc_clean_25 = sum(nocc_clean))
occ_fauna_cleaned_sources_over25

occ_fauna_cleaned_sources_over10 <- occ_fauna_cleaned %>% 
    dplyr::group_by(source, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 10) %>% 
    dplyr::group_by(source) %>% 
    dplyr::summarise(nsp_clean_10 = n(),
                     nocc_clean_10 = sum(nocc_clean))
occ_fauna_cleaned_sources_over10

### categorias (CR, EN, etc) ----
occ_fauna_cleaned_categories_over25 <- occ_fauna_cleaned %>% 
    dplyr::group_by(category, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 25) %>% 
    dplyr::group_by(category) %>% 
    dplyr::summarise(nsp_clean_25 = n(),
                     nocc_clean_25 = sum(nocc_clean))
occ_fauna_cleaned_categories_over25

occ_fauna_cleaned_categories_over10 <- occ_fauna_cleaned %>% 
    dplyr::group_by(category, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 10) %>% 
    dplyr::group_by(category) %>% 
    dplyr::summarise(nsp_clean_10 = n(),
                     nocc_clean_10 = sum(nocc_clean))
occ_fauna_cleaned_categories_over10

### grupos da fauna (invert, anfíbios) e categorias (CR, EN) ----
occ_fauna_cleaned_groups_categories_over25 <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, category, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 25) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(nsp_clean_25 = n(),
                     nocc_clean_25 = sum(nocc_clean))
occ_fauna_cleaned_groups_categories_over25

occ_fauna_cleaned_groups_categories_over10 <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, category, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 10) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(nsp_clean_10 = n(),
                     nocc_clean_10 = sum(nocc_clean))
occ_fauna_cleaned_groups_categories_over10

### grupos (invert, anfíbios), categorias (CR, EN) e fontes (specieslink, salve) ----
occ_fauna_cleaned_groups_categories_sources_over25 <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, category, source, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 25) %>% 
    dplyr::group_by(group, category, source) %>% 
    dplyr::summarise(nsp_clean_25 = n(),
                     nocc_clean_25 = sum(nocc_clean))
occ_fauna_cleaned_groups_categories_sources_over25

occ_fauna_cleaned_groups_categories_sources_over10 <- occ_fauna_cleaned %>% 
    dplyr::group_by(group, category, source, species) %>% 
    dplyr::summarise(nocc_clean = n()) %>% 
    dplyr::filter(nocc_clean >= 10) %>% 
    dplyr::group_by(group, category, source) %>% 
    dplyr::summarise(nsp_clean_10 = n(),
                     nocc_clean_10 = sum(nocc_clean))
occ_fauna_cleaned_groups_categories_sources_over10

# join  -------------------------------------------------------------------

## fauna ---------

### união das tabelas de resumo para grupos da fauna (invert, anfíbios, etc) ----
occ_fauna_groups_join_nsp <- occ_fauna_raw_groups %>% 
    dplyr::left_join(occ_fauna_cleaned_groups) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_over25) %>% 
    dplyr::select(group, nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25) %>% 
    replace(is.na(.), 0) %>%
    janitor::adorn_totals()
occ_fauna_groups_join_nsp

occ_fauna_groups_join_nocc <- occ_fauna_raw_groups %>% 
    dplyr::left_join(occ_fauna_cleaned_groups) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_over25) %>% 
    dplyr::select(-c(nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25)) %>% 
    replace(is.na(.), 0) %>%
    janitor::adorn_totals()
occ_fauna_groups_join_nocc

readr::write_csv(occ_fauna_groups_join_nsp, "99_reports/tables/occ_fauna_groups_join_nsp.csv")
readr::write_csv(occ_fauna_groups_join_nocc, "99_reports/tables/occ_fauna_groups_join_nocc.csv")

### fontes de dados (specieslink, salve, etc) ----
occ_fauna_sources_join_nsp <- occ_fauna_raw_sources %>% 
    dplyr::left_join(occ_fauna_cleaned_sources) %>% 
    dplyr::left_join(occ_fauna_cleaned_sources_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_sources_over25) %>% 
    dplyr::select(source, nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25) %>% 
    replace(is.na(.), 0) %>% 
    janitor::adorn_totals()
occ_fauna_sources_join_nsp

occ_fauna_sources_join_nocc <- occ_fauna_raw_sources %>% 
    dplyr::left_join(occ_fauna_cleaned_sources) %>% 
    dplyr::left_join(occ_fauna_cleaned_sources_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_sources_over25) %>% 
    dplyr::select(-c(nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25)) %>% 
    replace(is.na(.), 0) %>% 
    janitor::adorn_totals()
occ_fauna_sources_join_nocc

readr::write_csv(occ_fauna_sources_join_nsp, "99_reports/tables/occ_fauna_sources_join_nsp.csv")
readr::write_csv(occ_fauna_sources_join_nocc, "99_reports/tables/occ_fauna_sources_join_nocc.csv")

### categorias (CR, EN, etc) ----
occ_fauna_categories_join_nsp <- occ_fauna_raw_categories %>% 
    dplyr::left_join(occ_fauna_cleaned_categories) %>% 
    dplyr::left_join(occ_fauna_cleaned_categories_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_categories_over25) %>% 
    dplyr::select(category, nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25) %>% 
    replace(is.na(.), 0) %>% 
    janitor::adorn_totals()
occ_fauna_categories_join_nsp

occ_fauna_categories_join_nocc <- occ_fauna_raw_categories %>% 
    dplyr::left_join(occ_fauna_cleaned_categories) %>% 
    dplyr::left_join(occ_fauna_cleaned_categories_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_categories_over25) %>% 
    dplyr::select(-c(nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25)) %>% 
    replace(is.na(.), 0) %>% 
    janitor::adorn_totals()
occ_fauna_categories_join_nocc

readr::write_csv(occ_fauna_categories_join_nsp, "99_reports/tables/occ_fauna_categories_join_nsp.csv")
readr::write_csv(occ_fauna_categories_join_nocc, "99_reports/tables/occ_fauna_categories_join_nocc.csv")

### grupos da fauna (invert, anfíbios) e categorias (CR, EN) ----
occ_fauna_groups_categories_join_nsp <- occ_fauna_raw_groups_categories %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_over25) %>% 
    dplyr::select(group, category, nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25) %>% 
    replace(is.na(.), 0)
occ_fauna_groups_categories_join_nsp

occ_fauna_groups_categories_join_nocc <- occ_fauna_raw_groups_categories %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_over25) %>% 
    dplyr::select(-c(nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25)) %>% 
    replace(is.na(.), 0)
occ_fauna_groups_categories_join_nocc

readr::write_csv(occ_fauna_groups_categories_join_nsp, "99_reports/tables/occ_fauna_groups_categories_join_nsp.csv")
readr::write_csv(occ_fauna_groups_categories_join_nocc, "99_reports/tables/occ_fauna_groups_categories_join_nocc.csv")

### grupos (invert, anfíbios), categorias (CR, EN) e fontes (specieslink, salve) ----
occ_fauna_groups_categories_sources_join_nsp <- occ_fauna_raw_groups_categories_sources %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_sources) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_sources_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_sources_over25) %>% 
    dplyr::select(category, source, nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25) %>% 
    replace(is.na(.), 0)
occ_fauna_groups_categories_sources_join_nsp

occ_fauna_groups_categories_sources_join_nocc <- occ_fauna_raw_groups_categories_sources %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_sources) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_sources_over10) %>% 
    dplyr::left_join(occ_fauna_cleaned_groups_categories_sources_over25) %>% 
    dplyr::select(-c(nsp_total, nsp_clean, nsp_clean_10, nsp_clean_25)) %>% 
    replace(is.na(.), 0)
occ_fauna_groups_categories_sources_join_nocc

readr::write_csv(occ_fauna_groups_categories_sources_join_nsp, "99_reports/tables/occ_fauna_groups_categories_sources_join_nsp.csv")
readr::write_csv(occ_fauna_groups_categories_sources_join_nocc, "99_reports/tables/occ_fauna_groups_categories_sources_join_nocc.csv")

# figures -----------------------------------------------------------------

## summary cleaning ----
occ_fauna_raw_cleaned <- readr::read_csv("02_data/01_occurrences/03_cleaned/occ_raw_cleaned_fauna.csv") %>% 
    dplyr::left_join(fauna_order_group) %>% 
    dplyr::mutate(salve_filter = ifelse(is.na(salve_filter), 0, salve_filter)) %>% 
    dplyr::relocate(group, .after = category) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(all_filter = ifelse(sum(date_filter, precision_filter, bias_filter, spatial_filter, citizen_filter, neotropic_filter, landscape_filter, salve_filter) == 8, TRUE, FALSE))
occ_fauna_raw_cleaned

### nsp total ---- 
occ_fauna_raw_cleaned_summary_nsp <- occ_fauna_raw_cleaned %>% 
    dplyr::group_by(group, category, species) %>% 
    dplyr::mutate(total = 1) %>% 
    dplyr::summarise(total = sum(total),
                     date_nsp = sum(date_filter),
                     precision_nsp = sum(precision_filter),
                     bias_nsp = sum(bias_filter),
                     spatial_nsp = sum(spatial_filter),
                     citizen_nsp = sum(citizen_filter),
                     neotropic_nsp = sum(neotropic_filter),
                     salve_nsp = sum(salve_filter),
                     landscape_nsp = sum(landscape_filter),
                     all_nsp = sum(all_filter)) %>% 
    dplyr::mutate(date_nsp = ifelse(date_nsp > 0, 1, 0),
                  precision_nsp = ifelse(precision_nsp > 0, 1, 0),
                  bias_nsp = ifelse(bias_nsp > 0, 1, 0),
                  spatial_nsp = ifelse(spatial_nsp > 0, 1, 0),
                  citizen_nsp = ifelse(citizen_nsp > 0, 1, 0),
                  neotropic_nsp = ifelse(neotropic_nsp > 0, 1, 0),
                  salve_nsp = ifelse(salve_nsp > 0, 1, 0),
                  landscape_nsp = ifelse(landscape_nsp > 0, 1, 0),
                  all_nsp = ifelse(all_nsp > 0, 1, 0)) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(total = n(),
                     restante = sum(all_nsp),
                     date = sum(date_nsp)/n() * 100,
                     citizen = sum(citizen_nsp)/n() * 100,
                     precision = sum(precision_nsp)/n() * 100,
                     neotropic = sum(neotropic_nsp)/n() * 100,
                     spatial = sum(spatial_nsp)/n() * 100,
                     bias = sum(bias_nsp)/n() * 100,
                     salve = sum(salve_nsp)/n() * 100,
                     landscape = sum(landscape_nsp)/n() * 100,
                     all = sum(all_nsp)/n() * 100) %>% 
    tidyr::pivot_longer(cols = -c(group, category, total, restante), names_to = "filter", values_to = "percentage")
occ_fauna_raw_cleaned_summary_nsp 

occ_raw_cleaned_summary_nsp <- occ_fauna_raw_cleaned_summary_nsp %>% 
    dplyr::mutate(filter = forcats::as_factor(filter),
                  group = forcats::as_factor(group), 
                  category = forcats::as_factor(category)) %>% 
    dplyr::mutate(filter = forcats::fct_relevel(forcats::as_factor(filter),  c("all", "landscape", "salve", "bias", "spatial", "neotropic", "precision", "citizen", "date")),
                  group = forcats::fct_relevel(group,  c("anfibios", "repteis", "aves", "mamiferos", "invertebrados")),
                  category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU", "EW"))) %>% 
    dplyr::filter(category != "EW")
occ_raw_cleaned_summary_nsp

occ_raw_cleaned_summary_nsp_text <- occ_raw_cleaned_summary_nsp %>% 
    dplyr::distinct(group, category, total, restante) %>% 
    dplyr::mutate(total = as.character(total))
occ_raw_cleaned_summary_nsp_text

ggplot(data = occ_raw_cleaned_summary_nsp, aes(x = filter, y = percentage)) +
    geom_segment(aes(xend = filter, yend = 0), linewidth = .8, color = "#65737e") +
    geom_point(aes(color = group), size = 5) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    scale_color_manual(values = c("#c4c4df", "#bb3e03", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396")) +
    coord_flip() +
    facet_grid2(rows = vars(group), cols = vars(category),
                strip = strip_themed(background_x = list(element_rect(fill = "#4d012b"),
                                                         element_rect(fill = "#FF0000"),
                                                         element_rect(fill = "#FFA500"),
                                                         element_rect(fill = "#FFFF00")),
                                     background_y = list(element_rect(fill = "#c4c4df"),
                                                         element_rect(fill = "#bb3e03"),
                                                         element_rect(fill = "#ca6702"),
                                                         element_rect(fill = "#e9d8a6"),
                                                         element_rect(fill = "#94d2bd"),
                                                         element_rect(fill = "#0a9396")))) +
    labs(x = "Filtros", y = "Porcentagem do número de espécies restantes (%)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 12)) +
    geom_label(data = occ_raw_cleaned_summary_nsp_text, 
               mapping = aes(x = rep(8.5, 19), 
                             y = rep(20, 19), 
                             label = paste0("nsp= ", restante, "/", total)), size = 4)
ggsave(filename = "03_results/01_ocorrencias/figures/fig_nsp_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)
ggsave(filename = "99_reports/figures/fig_nsp_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)

### nsp >= 25 ocorrencias ----
occ_fauna_raw_cleaned_summary_nsp25 <- occ_fauna_raw_cleaned %>% 
    dplyr::group_by(group, category, species) %>%
    dplyr::mutate(total = 1) %>% 
    dplyr::summarise(total = sum(total),
                     date_nsp = sum(date_filter),
                     precision_nsp = sum(precision_filter),
                     bias_nsp = sum(bias_filter),
                     spatial_nsp = sum(spatial_filter),
                     citizen_nsp = sum(citizen_filter),
                     neotropic_nsp = sum(neotropic_filter),
                     salve_nsp = sum(salve_filter),
                     landscape_nsp = sum(landscape_filter),
                     all_nsp = sum(all_filter)) %>% 
    dplyr::mutate(date_nsp = ifelse(date_nsp >= 25, 1, 0),
                  precision_nsp = ifelse(precision_nsp >= 25, 1, 0),
                  bias_nsp = ifelse(bias_nsp >= 25, 1, 0),
                  spatial_nsp = ifelse(spatial_nsp >= 25, 1, 0),
                  citizen_nsp = ifelse(citizen_nsp >= 25, 1, 0),
                  neotropic_nsp = ifelse(neotropic_nsp >= 25, 1, 0),
                  salve_nsp = ifelse(salve_nsp >= 25, 1, 0),
                  landscape_nsp = ifelse(landscape_nsp >=25, 1, 0),
                  all_nsp = ifelse(all_nsp >= 25, 1, 0)) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(total = n(),
                     restante = sum(all_nsp),
                     date = sum(date_nsp)/n() * 100,
                     citizen = sum(citizen_nsp)/n() * 100,
                     precision = sum(precision_nsp)/n() * 100,
                     neotropic = sum(neotropic_nsp)/n() * 100,
                     spatial = sum(spatial_nsp)/n() * 100,
                     bias = sum(bias_nsp)/n() * 100,
                     salve = sum(salve_nsp)/n() * 100,
                     landscape = sum(landscape_nsp)/n() * 100,
                     all = sum(all_nsp)/n() * 100) %>% 
    tidyr::pivot_longer(cols = -c(group, category, total, restante), names_to = "filter", values_to = "percentage")
occ_fauna_raw_cleaned_summary_nsp25

occ_raw_cleaned_summary_nsp25 <- occ_fauna_raw_cleaned_summary_nsp25 %>% 
    dplyr::mutate(filter = forcats::as_factor(filter),
                  group = forcats::as_factor(group), 
                  category = forcats::as_factor(category)) %>% 
    dplyr::mutate(filter = forcats::fct_relevel(forcats::as_factor(filter),  c("all", "landscape", "salve", "bias", "spatial", "neotropic", "precision", "citizen", "date")),
                  group = forcats::fct_relevel(group,  c("anfibios", "repteis", "aves", "mamiferos", "invertebrados")),
                  category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU", "EW"))) %>% 
    dplyr::filter(category != "EW")
occ_raw_cleaned_summary_nsp25

occ_raw_cleaned_summary_nsp25_text <- occ_raw_cleaned_summary_nsp25 %>% 
    dplyr::distinct(group, category, total, restante) %>% 
    dplyr::mutate(total = as.character(total))
occ_raw_cleaned_summary_nsp25_text

ggplot(data = occ_raw_cleaned_summary_nsp25, aes(x = filter, y = percentage)) +
    geom_segment(aes(xend = filter, yend = 0), linewidth = .8, color = "#65737e") +
    geom_point(aes(color = group), size = 5) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    scale_color_manual(values = c("#c4c4df", "#bb3e03", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396")) +
    coord_flip() +
    facet_grid2(rows = vars(group), cols = vars(category), scales = "free",
                strip = strip_themed(background_x = list(element_rect(fill = "#4d012b"),
                                                         element_rect(fill = "#FF0000"),
                                                         element_rect(fill = "#FFA500"),
                                                         element_rect(fill = "#FFFF00")),
                                     background_y = list(element_rect(fill = "#c4c4df"),
                                                         element_rect(fill = "#bb3e03"),
                                                         element_rect(fill = "#ca6702"),
                                                         element_rect(fill = "#e9d8a6"),
                                                         element_rect(fill = "#94d2bd"),
                                                         element_rect(fill = "#0a9396")))) +
    labs(x = "Filtros", y = "Porcentagem do número de ocorrências restantes (%)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 10)) +
    geom_label(data = occ_raw_cleaned_summary_nsp25_text, 
               mapping = aes(x = rep(1.5, 1.5, 1.5, 1.5, 7.5, 7.5, 7.5, 7.5, 1.5, 1.5, 1.5, 1.5, 7.5, 1.5, 1.5, 7.5, 1.5, 1.5, 1.5), 
                             y = c(80, 70, 80, 85, 20, 20, 20, 20, 80, 70, 80, 80, 20, 70, 83, 20, 80, 80, 80), 
                             label = paste0("nsp25 = ", restante, "/", total)), size = 4)
ggsave(filename = "03_results/01_ocorrencias/figures/fig_nsp25_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)
ggsave(filename = "99_reports/figures/fig_nsp25_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)

### nsp >= 10 ocorrencias ----
occ_fauna_raw_cleaned_summary_nsp10 <- occ_fauna_raw_cleaned %>% 
    dplyr::group_by(group, category, species) %>%
    dplyr::mutate(total = 1) %>% 
    dplyr::summarise(total = sum(total),
                     date_nsp = sum(date_filter),
                     precision_nsp = sum(precision_filter),
                     bias_nsp = sum(bias_filter),
                     spatial_nsp = sum(spatial_filter),
                     citizen_nsp = sum(citizen_filter),
                     neotropic_nsp = sum(neotropic_filter),
                     salve_nsp = sum(salve_filter),
                     landscape_nsp = sum(landscape_filter),
                     all_nsp = sum(all_filter)) %>% 
    dplyr::mutate(date_nsp = ifelse(date_nsp >= 10, 1, 0),
                  precision_nsp = ifelse(precision_nsp >= 10, 1, 0),
                  bias_nsp = ifelse(bias_nsp >= 10, 1, 0),
                  spatial_nsp = ifelse(spatial_nsp >= 10, 1, 0),
                  citizen_nsp = ifelse(citizen_nsp >= 10, 1, 0),
                  neotropic_nsp = ifelse(neotropic_nsp >= 10, 1, 0),
                  salve_nsp = ifelse(salve_nsp >= 10, 1, 0),
                  landscape_nsp = ifelse(landscape_nsp >= 10, 1, 0),
                  all_nsp = ifelse(all_nsp >= 10, 1, 0)) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(total = n(),
                     restante = sum(all_nsp),
                     date = sum(date_nsp)/n() * 100,
                     citizen = sum(citizen_nsp)/n() * 100,
                     precision = sum(precision_nsp)/n() * 100,
                     neotropic = sum(neotropic_nsp)/n() * 100,
                     spatial = sum(spatial_nsp)/n() * 100,
                     bias = sum(bias_nsp)/n() * 100,
                     salve = sum(salve_nsp)/n() * 100,
                     landscape = sum(landscape_nsp)/n() * 100,
                     all = sum(all_nsp)/n() * 100) %>% 
    tidyr::pivot_longer(cols = -c(group, category, total, restante), names_to = "filter", values_to = "percentage")
occ_fauna_raw_cleaned_summary_nsp10

occ_raw_cleaned_summary_nsp10 <- occ_fauna_raw_cleaned_summary_nsp10 %>% 
    dplyr::mutate(filter = forcats::as_factor(filter),
                  group = forcats::as_factor(group), 
                  category = forcats::as_factor(category)) %>% 
    dplyr::mutate(filter = forcats::fct_relevel(forcats::as_factor(filter),  c("all", "landscape", "salve", "bias", "spatial", "neotropic", "precision", "citizen", "date")),
                  group = forcats::fct_relevel(group,  c("anfibios", "repteis", "aves", "mamiferos", "invertebrados")),
                  category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU", "EW"))) %>% 
    dplyr::filter(category != "EW")
occ_raw_cleaned_summary_nsp10

occ_raw_cleaned_summary_nsp10_text <- occ_raw_cleaned_summary_nsp10 %>% 
    dplyr::distinct(group, category, total, restante) %>% 
    dplyr::mutate(total = as.character(total))
occ_raw_cleaned_summary_nsp10_text

ggplot(data = occ_raw_cleaned_summary_nsp10, aes(x = filter, y = percentage)) +
    geom_segment(aes(xend = filter, yend = 0), linewidth = .8, color = "#65737e") +
    geom_point(aes(color = group), size = 5) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    scale_color_manual(values = c("#c4c4df", "#bb3e03", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396")) +
    coord_flip() +
    facet_grid2(rows = vars(group), cols = vars(category), scales = "free",
                strip = strip_themed(background_x = list(element_rect(fill = "#4d012b"),
                                                         element_rect(fill = "#FF0000"),
                                                         element_rect(fill = "#FFA500"),
                                                         element_rect(fill = "#FFFF00")),
                                     background_y = list(element_rect(fill = "#c4c4df"),
                                                         element_rect(fill = "#bb3e03"),
                                                         element_rect(fill = "#ca6702"),
                                                         element_rect(fill = "#e9d8a6"),
                                                         element_rect(fill = "#94d2bd"),
                                                         element_rect(fill = "#0a9396")))) +
    labs(x = "Filtros", y = "Porcentagem do número de espécies restantes (%)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 10)) +
    geom_label(data = occ_raw_cleaned_summary_nsp10_text, 
               mapping = aes(x = c(1.5, 7.5, 7.5, 1.5, 7.5, 7.5, 7.5, 7.5, 1.5, 1.5, 1.5, 1.5, 7.5, 1.5, 7.5, 7.5, 1.5, 1.5, 7.5), 
                             y = c(80, 20, 20, 85, 20, 15, 20, 20, 80, 70, 80, 80, 20, 70, 20, 20, 80, 85, 20), 
                             label = paste0("nsp10 = ", restante, "/", total)), size = 4)
ggsave(filename = "03_results/01_ocorrencias/figures/fig_nsp10_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)
ggsave(filename = "99_reports/figures/fig_nsp10_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)

### occ total ----
occ_fauna_raw_cleaned_summary_nocc <- occ_fauna_raw_cleaned %>% 
    dplyr::select(group, category, date_filter, precision_filter, bias_filter, spatial_filter, citizen_filter, neotropic_filter, salve_filter, landscape_filter, all_filter) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(total = n(),
                     restante = sum(all_filter),
                     date = round(sum(date_filter)/n(), 4) * 100,
                     precision = round(sum(precision_filter)/n(), 4) * 100,
                     bias = round(sum(bias_filter)/n(), 4) * 100,
                     spatial = round(sum(spatial_filter)/n(), 4) * 100,
                     citizen = round(sum(citizen_filter)/n(), 4) * 100, 
                     neotropic = round(sum(neotropic_filter)/n(), 4) * 100,
                     salve = round(sum(salve_filter)/n(), 4) * 100,
                     landscape = round(sum(landscape_filter)/n(), 4) * 100,
                     all = round(sum(all_filter)/n(), 4) * 100) %>% 
    tidyr::pivot_longer(cols = -c(group, category, total, restante), names_to = "filter", values_to = "percentage")
occ_fauna_raw_cleaned_summary_nocc

occ_raw_cleaned_summary_nocc <- occ_fauna_raw_cleaned_summary_nocc %>% 
    dplyr::mutate(filter = forcats::as_factor(filter),
                  group = forcats::as_factor(group), 
                  category = forcats::as_factor(category)) %>% 
    dplyr::mutate(filter = forcats::fct_relevel(forcats::as_factor(filter),  c("all", "landscape", "salve", "bias", "spatial", "neotropic", "precision", "citizen", "date")),
                  group = forcats::fct_relevel(group,  c("anfibios", "repteis", "aves", "mamiferos", "invertebrados")),
                  category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU", "EW"))) %>% 
    dplyr::filter(category != "EW")
occ_raw_cleaned_summary_nocc

occ_raw_cleaned_summary_nocc_text <- occ_raw_cleaned_summary_nocc %>% 
    dplyr::distinct(group, category, total, restante) %>% 
    dplyr::mutate(total = as.character(total))
occ_raw_cleaned_summary_nocc_text

ggplot(data = occ_raw_cleaned_summary_nocc, aes(x = filter, y = percentage)) +
    geom_segment(aes(xend = filter, yend = 0), linewidth = .8, color = "#65737e") +
    geom_point(aes(color = group), size = 5) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    scale_color_manual(values = c("#c4c4df", "#bb3e03", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396")) +
    coord_flip() +
    facet_grid2(rows = vars(group), cols = vars(category), scales = "free",
                strip = strip_themed(background_x = list(element_rect(fill = "#4d012b"),
                                                         element_rect(fill = "#FF0000"),
                                                         element_rect(fill = "#FFA500"),
                                                         element_rect(fill = "#FFFF00")),
                                     background_y = list(element_rect(fill = "#c4c4df"),
                                                         element_rect(fill = "#bb3e03"),
                                                         element_rect(fill = "#ca6702"),
                                                         element_rect(fill = "#e9d8a6"),
                                                         element_rect(fill = "#94d2bd"),
                                                         element_rect(fill = "#0a9396")))) +
    labs(x = "Filter", y = "Porcentagem do número de ocorrências restantes (%)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 12)) +
    geom_label(data = occ_raw_cleaned_summary_nocc_text, 
               mapping = aes(x = c(1.5, 1.5, 1.5, 1.5, 1.5, 7.5, 1.5, 1.5, 7.5, 7.5, 7.5, 7.5, 7.5, 7.5, 7.5, 7.5,1.5,1.5,1.5), 
                             y = c(90, 85, 85, 85, 85, 20, 80, 80, 20, 20, 20, 20, 20, 15, 25, 25, 83,83,83), 
                             label = paste0("nocc = ", restante, "/", total)), size = 4)
ggsave(filename = "03_results/01_ocorrencias/figures/fig_nocc_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)
ggsave(filename = "99_reports/figures/fig_nocc_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)

### occ >= 25 ----
occ_fauna_raw_cleaned_summary_nocc25 <- occ_fauna_raw_cleaned %>% 
    dplyr::group_by(group, category, species) %>%
    dplyr::mutate(total = 1) %>% 
    dplyr::summarise(total = sum(total),
                     date_nocc = sum(date_filter),
                     precision_nocc = sum(precision_filter),
                     bias_nocc = sum(bias_filter),
                     spatial_nocc = sum(spatial_filter),
                     citizen_nocc = sum(citizen_filter),
                     neotropic_nocc = sum(neotropic_filter),
                     salve_nocc = sum(salve_filter),
                     landscape_nocc = sum(landscape_filter),
                     all_nocc = sum(all_filter)) %>% 
    dplyr::filter(all_nocc >= 25) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(total = sum(total),
                     restante = sum(all_nocc),
                     date = round(sum(date_nocc)/total, 4) * 100,
                     precision = round(sum(precision_nocc)/total, 4) * 100,
                     bias = round(sum(bias_nocc)/total, 4) * 100,
                     spatial = round(sum(spatial_nocc)/total, 4) * 100,
                     citizen = round(sum(citizen_nocc)/total, 4) * 100, 
                     neotropic = round(sum(neotropic_nocc)/total, 4) * 100,
                     salve = round(sum(salve_nocc)/total, 4) * 100,
                     landscape = round(sum(landscape_nocc)/total, 4) * 100,
                     all = round(sum(all_nocc)/total, 4) * 100) %>% 
    tidyr::pivot_longer(cols = -c(group, category, total, restante), names_to = "filter", values_to = "percentage")
occ_fauna_raw_cleaned_summary_nocc25

occ_raw_cleaned_summary_nocc25 <- occ_fauna_raw_cleaned_summary_nocc25 %>% 
    dplyr::mutate(filter = forcats::as_factor(filter),
                  group = forcats::as_factor(group), 
                  category = forcats::as_factor(category)) %>% 
    dplyr::mutate(
        filter = forcats::fct_relevel(forcats::as_factor(filter),  c("all", "landscape", "salve", "bias", "spatial", "neotropic", "precision", "citizen", "date")),
        group = forcats::fct_relevel(group,  c("anfibios", "repteis", "aves", "mamiferos", "invertebrados")),
        category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU", "EW"))
    ) %>% 
    dplyr::filter(category != "EW")
occ_raw_cleaned_summary_nocc25

occ_raw_cleaned_summary_nocc25_join <- dplyr::left_join(occ_raw_cleaned_summary_nocc[, c(1:2, 5)], occ_raw_cleaned_summary_nocc25) %>% 
    replace(is.na(.), 0)
occ_raw_cleaned_summary_nocc25_join

occ_raw_cleaned_summary_nocc25_text <- occ_raw_cleaned_summary_nocc25_join %>% 
    dplyr::distinct(group, category, total, restante) %>% 
    dplyr::mutate(total = as.character(total))
occ_raw_cleaned_summary_nocc25_text

ggplot(data = occ_raw_cleaned_summary_nocc25_join, aes(x = filter, y = percentage)) +
    geom_segment(aes(xend = filter, yend = 0), linewidth = .8, color = "#65737e") +
    geom_point(aes(color = group), size = 5) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    scale_color_manual(values = c("#c4c4df", "#bb3e03", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396")) +
    coord_flip() +
    facet_grid2(rows = vars(group), cols = vars(category),
                strip = strip_themed(background_x = list(element_rect(fill = "#4d012b"),
                                                         element_rect(fill = "#FF0000"),
                                                         element_rect(fill = "#FFA500"),
                                                         element_rect(fill = "#FFFF00")),
                                     background_y = list(element_rect(fill = "#c4c4df"),
                                                         element_rect(fill = "#bb3e03"),
                                                         element_rect(fill = "#ca6702"),
                                                         element_rect(fill = "#e9d8a6"),
                                                         element_rect(fill = "#94d2bd"),
                                                         element_rect(fill = "#0a9396")))) +
    labs(x = "Filtros", y = "Porcentagem do número de ocorrências restantes (%)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 12)) +
    geom_label(data = occ_raw_cleaned_summary_nocc25_text, 
               mapping = aes(x = c(1.5,1.5,1.5,1.5, 7.5,1.5,7.5, 7.5,1.5,1.5,1.5,1.5, 7.5,1.5, 7.5, 7.5,1.5,1.5,1.5), 
                             y = c(86,86,86,82, 25,86, 25, 25,83,83, 82,80, 25,80, 25, 30, 80,80,80), 
                             label = paste0("nocc25 = ", restante, "/", total)), size = 4)
ggsave(filename = "03_results/01_ocorrencias/figures/fig_nocc25_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)
ggsave(filename = "99_reports/figures/fig_nocc25_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)

### occ >= 10 ----
occ_fauna_raw_cleaned_summary_nocc10 <- occ_fauna_raw_cleaned %>% 
    dplyr::group_by(group, category, species) %>%
    dplyr::mutate(total = 1) %>% 
    dplyr::summarise(total = sum(total),
                     date_nocc = sum(date_filter),
                     precision_nocc = sum(precision_filter),
                     bias_nocc = sum(bias_filter),
                     spatial_nocc = sum(spatial_filter),
                     citizen_nocc = sum(citizen_filter),
                     neotropic_nocc = sum(neotropic_filter),
                     salve_nocc = sum(salve_filter),
                     landscape_nocc = sum(landscape_filter),
                     all_nocc = sum(all_filter)) %>% 
    dplyr::filter(all_nocc >= 10) %>% 
    dplyr::group_by(group, category) %>% 
    dplyr::summarise(total = sum(total),
                     restante = sum(all_nocc),
                     date = round(sum(date_nocc)/total, 4) * 100,
                     precision = round(sum(precision_nocc)/total, 4) * 100,
                     bias = round(sum(bias_nocc)/total, 4) * 100,
                     spatial = round(sum(spatial_nocc)/total, 4) * 100,
                     citizen = round(sum(citizen_nocc)/total, 4) * 100, 
                     neotropic = round(sum(neotropic_nocc)/total, 4) * 100,
                     salve = round(sum(salve_nocc)/total, 4) * 100,
                     landscape = round(sum(landscape_nocc)/total, 4) * 100,
                     all = round(sum(all_nocc)/total, 4) * 100) %>% 
    tidyr::pivot_longer(cols = -c(group, category, total, restante), names_to = "filter", values_to = "percentage")
occ_fauna_raw_cleaned_summary_nocc10

occ_raw_cleaned_summary_nocc10 <- occ_fauna_raw_cleaned_summary_nocc10 %>% 
    dplyr::mutate(filter = forcats::as_factor(filter),
                  group = forcats::as_factor(group), 
                  category = forcats::as_factor(category)) %>% 
    dplyr::mutate(
        filter = forcats::fct_relevel(forcats::as_factor(filter),  c("all", "landscape", "salve", "bias", "spatial", "neotropic", "precision", "citizen", "date")),
        group = forcats::fct_relevel(group,  c("anfibios", "repteis", "aves", "mamiferos", "invertebrados")),
        category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU", "EW"))
    ) %>% 
    dplyr::filter(category != "EW")
occ_raw_cleaned_summary_nocc10

occ_raw_cleaned_summary_nocc10_join <- dplyr::left_join(occ_raw_cleaned_summary_nocc[, c(1:2, 5)], occ_raw_cleaned_summary_nocc10) %>% 
    replace(is.na(.), 0)
occ_raw_cleaned_summary_nocc10_join

occ_raw_cleaned_summary_nocc10_text <- occ_raw_cleaned_summary_nocc10_join %>% 
    dplyr::distinct(group, category, total, restante) %>% 
    dplyr::mutate(total = as.character(total))
occ_raw_cleaned_summary_nocc10_text

ggplot(data = occ_raw_cleaned_summary_nocc10_join, aes(x = filter, y = percentage)) +
    geom_segment(aes(xend = filter, yend = 0), linewidth = .8, color = "#65737e") +
    geom_point(aes(color = group), size = 5) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    geom_text(aes(label = as.character(round(percentage, 0))), size = 3.3, nudge_y = 8) +
    scale_color_manual(values = c("#c4c4df", "#bb3e03", "#ca6702", "#e9d8a6", "#94d2bd", "#0a9396")) +
    coord_flip() +
    facet_grid2(rows = vars(group), cols = vars(category),
                strip = strip_themed(background_x = list(element_rect(fill = "#4d012b"),
                                                         element_rect(fill = "#FF0000"),
                                                         element_rect(fill = "#FFA500"),
                                                         element_rect(fill = "#FFFF00")),
                                     background_y = list(element_rect(fill = "#c4c4df"),
                                                         element_rect(fill = "#bb3e03"),
                                                         element_rect(fill = "#ca6702"),
                                                         element_rect(fill = "#e9d8a6"),
                                                         element_rect(fill = "#94d2bd"),
                                                         element_rect(fill = "#0a9396")))) +
    labs(x = "Filtros", y = "Porcentagem do número de ocorrências restantes (%)") +
    theme_bw(base_size = 20) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 12)) +
    geom_label(data = occ_raw_cleaned_summary_nocc10_text, 
               mapping = aes(x = c(7.5, 7.5,1.5,1.5, 7.5,1.5,7.5, 7.5,1.5,1.5,1.5,1.5, 7.5,1.5, 7.5, 7.5,1.5,1.5,1.5), 
                             y = c(20, 20, 87, 85, 25, 88, 30, 30, 83, 85, 85, 80, 25, 80, 25, 30, 85, 85, 80), 
                             label = paste0("nocc10 = ", restante, "/", total)), size = 4)
ggsave(filename = "03_results/01_ocorrencias/figures/fig_nocc10_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)
ggsave(filename = "99_reports/figures/fig_nocc10_cleaned_filters.png", width = 40, height = 35, units = "cm", dpi = 300)

## summary by categories ----

### amphibians ----

# filtra pelo número de sp de anfíbios
occ_fauna_cleaned_amphibian_nsp_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "anfibios") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    length()
occ_fauna_cleaned_amphibian_nsp_total

# filtra pelo número de ocorrências de anfíbios
occ_fauna_cleaned_amphibian_nocc_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "anfibios") %>% 
    nrow()
occ_fauna_cleaned_amphibian_nocc_total

# tabela resumindo os dados limpos de anfíbios por categoria e fonte
occ_fauna_cleaned_groups_categories_sources_amphibian <- occ_fauna_cleaned_groups_categories_sources %>% 
    dplyr::filter(group == "anfibios") %>% 
    dplyr::mutate(category = forcats::as_factor(category),
                  category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU")))
occ_fauna_cleaned_groups_categories_sources_amphibian

# Gráfico de barras horizontais da porcentagem de ocorrências restante
# depois da aplicação dos filtros por fonte de dados (salve, specieslink, etc) e
# por categoria de ameaça.
# Anfíbios - número de espécies

ggplot(data = occ_fauna_cleaned_groups_categories_sources_amphibian,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nsp_clean)), 
           y = nsp_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_amphibian_nsp_total, 2) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#4d012b", "#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de espécies", title = "Anfíbios", fill = "") +
    ylim(0, 36) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_amphibian_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_amphibian_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)

# Anfíbios - número de ocorrências
ggplot(data = occ_fauna_cleaned_groups_categories_sources_amphibian,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nocc_clean)), 
           y = nocc_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_amphibian_nocc_total, 3) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#4d012b", "#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de ocorrências", title = "Anfíbios", fill = "") +
    ylim(0, 165) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_amphibian_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_amphibian_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)

### reptiles ----
occ_fauna_cleaned_reptiles_nsp_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "repteis") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    length()
occ_fauna_cleaned_reptiles_nsp_total

occ_fauna_cleaned_reptiles_nocc_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "repteis") %>% 
    nrow()
occ_fauna_cleaned_reptiles_nocc_total

# tabela resumindo os dados limpos de répteis por categoria e fonte
occ_fauna_cleaned_groups_categories_sources_reptiles <- occ_fauna_cleaned_groups_categories_sources %>% 
    dplyr::filter(group == "repteis") %>% 
    dplyr::mutate(category = forcats::as_factor(category),
                  category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU")))
occ_fauna_cleaned_groups_categories_sources_reptiles

# Gráfico de barras horizontais 
# Répteis - número de espécies
ggplot(data = occ_fauna_cleaned_groups_categories_sources_reptiles,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nsp_clean)), 
           y = nsp_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_reptiles_nsp_total, 2) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de espécies", title = "Répteis", fill = "") +
    ylim(0, 9) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_reptiles_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_reptiles_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)

# Répteis - número de ocorrências
ggplot(data = occ_fauna_cleaned_groups_categories_sources_reptiles,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nocc_clean)), 
           y = nocc_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_reptiles_nocc_total, 3) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de ocorrências", title = "Répteis", fill = "") +
    ylim(0, 65) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_reptiles_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_reptiles_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)

### birds ------

# filtra pelo número de sp de aves
occ_fauna_cleaned_birds_nsp_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "aves") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    length()
occ_fauna_cleaned_birds_nsp_total

# filtra pelo número de ocorrências de aves
occ_fauna_cleaned_birds_nocc_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "aves") %>% 
    nrow()
occ_fauna_cleaned_birds_nocc_total

# tabela resumindo os dados limpos de aves por categoria e fonte
occ_fauna_cleaned_groups_categories_sources_birds <- occ_fauna_cleaned_groups_categories_sources %>% 
    dplyr::filter(group == "aves") %>% 
    dplyr::mutate(category = forcats::as_factor(category),
                  category = forcats::fct_relevel(category, c("CR (PEX)", "CR", "EN", "VU"))) %>%
    dplyr::filter(category != "EW")
occ_fauna_cleaned_groups_categories_sources_birds

# Gráfico de barras horizontais da porcentagem de ocorrências restante
# depois da aplicação dos filtros por fonte de dados (salve, specieslink, etc) e
# por categoria de ameaça.
# Aves - número de espécies

ggplot(data = occ_fauna_cleaned_groups_categories_sources_birds,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nsp_clean)), 
           y = nsp_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_birds_nsp_total, 3) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#4d012b", "#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de espécies", title = "Aves", fill = "") +
    ylim(0, 130) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_birds_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_birds_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)

# Aves - número de ocorrências
ggplot(data = occ_fauna_cleaned_groups_categories_sources_birds,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nocc_clean)), 
           y = nocc_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_birds_nocc_total, 4) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#4d012b", "#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de ocorrências", title = "Aves", fill = "") +
    ylim(0, 41000) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_birds_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_birds_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)

# mammals ---

# filtra pelo número de sp de mamíferos
occ_fauna_cleaned_mammals_nsp_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "mamiferos") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    length()
occ_fauna_cleaned_mammals_nsp_total

# filtra pelo número de ocorrências de mamíferos
occ_fauna_cleaned_mammals_nocc_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "mamiferos") %>% 
    nrow()
occ_fauna_cleaned_mammals_nocc_total

# tabela resumindo os dados limpos de mamíferos por categoria e fonte
occ_fauna_cleaned_groups_categories_sources_mammals <- occ_fauna_cleaned_groups_categories_sources %>% 
    dplyr::filter(group == "mamiferos") %>% 
    dplyr::mutate(category = forcats::as_factor(category),
                  category = forcats::fct_relevel(category, c("CR", "EN", "VU"))) 
occ_fauna_cleaned_groups_categories_sources_mammals

# Gráfico de barras horizontais da porcentagem de ocorrências restante
# depois da aplicação dos filtros por fonte de dados (salve, specieslink, etc) e
# por categoria de ameaça.
# Mamíferos - número de espécies

ggplot(data = occ_fauna_cleaned_groups_categories_sources_mammals,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nsp_clean)), 
           y = nsp_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_mammals_nsp_total, 3) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de espécies", title = "Mamíferos", fill = "") +
    ylim(0, 65) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_mammals_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_mammals_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)

# Mamíferos - número de ocorrências
ggplot(data = occ_fauna_cleaned_groups_categories_sources_mammals,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nocc_clean)), 
           y = nocc_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_mammals_nocc_total, 4) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de ocorrências", title = "Mamíferos", fill = "") +
    ylim(0, 12000) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_mammals_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_mammals_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)

### invertebrates ----

# filtra pelo número de sp de invertebrados
occ_fauna_cleaned_invertebrate_nsp_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "invertebrados") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    length()
occ_fauna_cleaned_invertebrate_nsp_total

# filtra pelo número de ocorrências de invertebrados
occ_fauna_cleaned_invertebrate_nocc_total <- occ_fauna_cleaned %>% 
    dplyr::filter(group == "invertebrados") %>% 
    nrow()
occ_fauna_cleaned_invertebrate_nocc_total

# tabela resumindo os dados limpos de invertebrados por categoria e fonte
occ_fauna_cleaned_groups_categories_sources_invertebrate <- occ_fauna_cleaned_groups_categories_sources %>% 
    dplyr::filter(group == "invertebrados") %>% 
    dplyr::mutate(category = forcats::as_factor(category),
                  category = forcats::fct_relevel(category, c("CR", "EN", "VU")))
occ_fauna_cleaned_groups_categories_sources_invertebrate

# Gráfico de barras horizontais da porcentagem de ocorrências restante
# depois da aplicação dos filtros por fonte de dados (salve, specieslink, etc) e
# por categoria de ameaça.
# Invertebrados - número de espécies

ggplot(data = occ_fauna_cleaned_groups_categories_sources_invertebrate,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nsp_clean)), 
           y = nsp_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_invertebrate_nsp_total, 2) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de espécies", title = "Invertebrados", fill = "") +
    ylim(0, 240) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_invertebrate_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_invertebrate_nsp.png", width = 30, height = 20, units = "cm", dpi = 300)

# Invertebrados - número de ocorrências
ggplot(data = occ_fauna_cleaned_groups_categories_sources_invertebrate,
       aes(x = forcats::fct_rev(forcats::fct_infreq(source, nocc_clean)), 
           y = nocc_clean, fill = category)) +
    geom_col() +
    geom_text(aes(label = paste0(round(after_stat(y)/occ_fauna_cleaned_invertebrate_nocc_total, 3) * 100, "%"), 
                  group = source), stat = "summary", fun = sum, hjust = -.2, size = 7) +
    scale_fill_manual(values = c("#FF0000", "#FFA500", "#FFFF00"), na.translate = FALSE) +
    labs(x = "", y = "Número de ocorrências", title = "Invertebrados", fill = "") +
    ylim(0, 700) +
    coord_flip() +
    theme_minimal(base_size = 20) +
    theme(legend.position = "top",
          plot.background = element_rect(fill = "white"))
ggsave("03_results/01_ocorrencias/figures/fig_group_invertebrate_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)
ggsave("99_reports/figures/fig_group_invertebrate_nocc.png", width = 30, height = 20, units = "cm", dpi = 300)

# maps --------------------------------------------------------------------

# import brazil limit
br <- rnaturalearth::ne_states(country = "Brazil") %>% 
    sf::st_make_valid() %>% 
    sf::st_crop(xmin = -80, ymin = -35, xmax = -34, ymax = 6)
tm_shape(br) + tm_polygons()

### species list and number of occurrences ----
occ_fauna_cleaned_list_nocc <- occ_fauna_cleaned %>% 
    sf::st_drop_geometry() %>% 
    dplyr::group_by(group, species, category) %>% 
    dplyr::summarise(n_occ = n()) %>% 
    dplyr::filter(n_occ >= 25) %>%
    dplyr::mutate(link = writexl::xl_hyperlink(url = paste0("map_occ_clean_fauna_", gsub(" ", "_", tolower(species)), ".html"),
                                               name = gsub(" ", "_", tolower(species))))
occ_fauna_cleaned_list_nocc
writexl::write_xlsx(occ_fauna_cleaned_list_nocc, "03_results/01_ocorrencias/maps/fauna/00_species_list_fauna_over25.xlsx")

occ_fauna_cleaned_list_nocc_mammals <- occ_fauna_cleaned %>% 
    sf::st_drop_geometry() %>% 
    dplyr::group_by(group, species, category) %>% 
    dplyr::summarise(n_occ = n()) %>% 
    dplyr::filter(n_occ >= 25,
                  group == "mamiferos") %>%
    dplyr::mutate(link = writexl::xl_hyperlink(url = paste0("map_occ_clean_fauna_", gsub(" ", "_", tolower(species)), ".html"),
                                               name = gsub(" ", "_", tolower(species))))
occ_fauna_cleaned_list_nocc_mammals
writexl::write_xlsx(occ_fauna_cleaned_list_nocc_mammals, "03_results/01_ocorrencias/maps/fauna/mamiferos/00_species_list_fauna_over25_mamiferos.xlsx")

### vector ----

# tmap mode
tmap::tmap_mode("view")

#### fauna ----
doParallel::registerDoParallel(parallelly::availableCores(omit = 2))
foreach::foreach(i=sort(occ_fauna_cleaned_list_nocc$species)) %dopar% {
    
    occ_fauna_cleaned_i <- dplyr::filter(occ_fauna_cleaned, species == i)
    
    occ_fauna_cleaned_i_v <- occ_fauna_cleaned_i %>% 
        dplyr::select(id, longitude, latitude) %>% 
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    iucn_files <- grep(i, dir(path = "02_data/01_occurrences/tetrapods_vector/species", pattern = ".shp", full.names = TRUE), value = TRUE)
    salve_files <- grep(i, dir(path = "02_data/01_occurrences/buffer_salve/", pattern = ".shp", full.names = TRUE), value = TRUE)
    
    if(length(iucn_files) != 0 & length(salve_files) != 0){
        
        iucn <- sf::st_read(iucn_files)
        salve <- sf::st_read(salve_files)
        
        tmap_i <- tm_shape(occ_fauna_cleaned_i_v) +
            tm_bubbles(size = .5, fill = "black") +
            tm_shape(iucn) +
            tm_fill(fill = adjustcolor("red", .2)) +
            tm_borders(col = "red", lwd = 2) +
            tm_shape(salve) +
            tm_borders(col = "blue", lwd = 2) +
            tm_shape(br) +
            tm_borders()
        tmap_i
        tmap::tmap_save(tm = tmap_i, filename = paste0("03_results/01_ocorrencias/maps/fauna/map_occ_clean_fauna_", gsub(" ", "_", tolower(i)), ".html"))
        
    }else if(length(iucn_files) == 0 & length(salve_files) != 0){
        
        salve <- sf::st_read(salve_files)
        
        tmap_i <- tm_shape(occ_fauna_cleaned_i_v) +
            tm_bubbles(size = .5, fill = "black") +
            tm_shape(salve) +
            tm_borders(col = "blue", lwd = 2) +
            tm_shape(br) +
            tm_borders()
        tmap_i
        tmap::tmap_save(tm = tmap_i, filename = paste0("03_results/01_ocorrencias/maps/fauna/map_occ_clean_fauna_", gsub(" ", "_", tolower(i)), ".html"))
        
        
    } else if(length(iucn_files) != 0 & length(salve_files) == 0){
        
        iucn <- sf::st_read(iucn_files)
        
        tmap_i <- tm_shape(occ_fauna_cleaned_i_v) +
            tm_bubbles(size = .5, fill = "black") +
            tm_shape(iucn) +
            tm_fill(fill = adjustcolor("red", .2)) +
            tm_borders(col = "red", lwd = 2) +
            tm_shape(br) +
            tm_borders()
        tmap_i
        tmap::tmap_save(tm = tmap_i, filename = paste0("03_results/01_ocorrencias/maps/fauna/map_occ_clean_fauna_", gsub(" ", "_", tolower(i)), ".html"))
        
    }else{
        
        tmap_i <- tm_shape(occ_fauna_cleaned_i_v) +
            tm_bubbles(size = .5, fill = "black") +
            tm_shape(br) +
            tm_borders()
        tmap_i
        tmap::tmap_save(tm = tmap_i, filename = paste0("03_results/01_ocorrencias/maps/fauna/map_occ_clean_fauna_", gsub(" ", "_", tolower(i)), ".html"))
    }
    
}
doParallel::stopImplicitCluster()

#### mammals ----
doParallel::registerDoParallel(parallelly::availableCores(omit = 2))
foreach::foreach(i=sort(occ_fauna_cleaned_list_nocc_mammals$species)) %dopar% {
    
    occ_fauna_cleaned_i <- dplyr::filter(occ_fauna_cleaned, species == i)
    
    occ_fauna_cleaned_i_v <- occ_fauna_cleaned_i %>% 
        dplyr::select(id, longitude, latitude) %>% 
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    iucn_files <- grep(i, dir(path = "02_data/01_occurrences/tetrapods_vector/species", pattern = ".shp", full.names = TRUE), value = TRUE)
    salve_files <- grep(i, dir(path = "02_data/01_occurrences/buffer_salve/", pattern = ".shp", full.names = TRUE), value = TRUE)
    
    if(length(iucn_files) != 0 & length(salve_files) != 0){
        
        iucn <- sf::st_read(iucn_files)
        salve <- sf::st_read(salve_files)
        
        tmap_i <- tm_shape(occ_fauna_cleaned_i_v) +
            tm_bubbles(size = .5, fill = "black") +
            tm_shape(iucn) +
            tm_fill(fill = adjustcolor("red", .2)) +
            tm_borders(col = "red", lwd = 2) +
            tm_shape(salve) +
            tm_borders(col = "blue", lwd = 2) +
            tm_shape(br) +
            tm_borders()
        tmap_i
        tmap::tmap_save(tm = tmap_i, filename = paste0("03_results/01_ocorrencias/maps/fauna/mamiferos/map_occ_clean_fauna_", gsub(" ", "_", tolower(i)), ".html"))
    
        }else if(length(iucn_files) == 0 & length(salve_files) != 0){
            
            salve <- sf::st_read(salve_files)
            
            tmap_i <- tm_shape(occ_fauna_cleaned_i_v) +
                tm_bubbles(size = .5, fill = "black") +
                tm_shape(salve) +
                tm_borders(col = "blue", lwd = 2) +
                tm_shape(br) +
                tm_borders()
            tmap_i
            tmap::tmap_save(tm = tmap_i, filename = paste0("03_results/01_ocorrencias/maps/fauna/mamiferos/map_occ_clean_fauna_", gsub(" ", "_", tolower(i)), ".html"))
            
                    
        } else if(length(iucn_files) != 0 & length(salve_files) == 0){
            
            iucn <- sf::st_read(iucn_files)

            tmap_i <- tm_shape(occ_fauna_cleaned_i_v) +
                tm_bubbles(size = .5, fill = "black") +
                tm_shape(iucn) +
                tm_fill(fill = adjustcolor("red", .2)) +
                tm_borders(col = "red", lwd = 2) +
                tm_shape(br) +
                tm_borders()
            tmap_i
            tmap::tmap_save(tm = tmap_i, filename = paste0("03_results/01_ocorrencias/maps/fauna/mamiferos/map_occ_clean_fauna_", gsub(" ", "_", tolower(i)), ".html"))
            
    }else{
        
        tmap_i <- tm_shape(occ_fauna_cleaned_i_v) +
            tm_bubbles(size = .5, fill = "black") +
            tm_shape(br) +
            tm_borders()
        tmap_i
        tmap::tmap_save(tm = tmap_i, filename = paste0("03_results/01_ocorrencias/maps/fauna/mamiferos/map_occ_clean_fauna_", gsub(" ", "_", tolower(i)), ".html"))
    }
    
}
doParallel::stopImplicitCluster()


## raster ----
br_crop <- geodata::gadm(country = "BRA", level = 0, path = tempdir()) %>% 
    terra::buffer(4e4)
br_crop
plot(br_crop)

chelsa <- terra::rast("02_data/02_variables/01_climate/00_raw/CHELSA_bio01_1981-2010_V.2.1.tif") %>% 
    terra::subset(1) %>% 
    terra::crop(br_crop, mask = TRUE) %>% 
    terra::aggregate(fact = 60, core = parallelly::availableCores(omit = 2))
chelsa

plot(chelsa)
plot(br$geometry, add = TRUE)

##### rasters ----
occ_fauna_cleaned_splist <- sort(unique(occ_fauna_cleaned$species))
occ_fauna_cleaned_splist

doParallel::registerDoParallel(parallelly::availableCores(omit = 2))
foreach::foreach(i=occ_fauna_cleaned_splist) %dopar% {
    
    occ_fauna_cleaned_i <- occ_fauna_cleaned %>% 
        dplyr::filter(species == i) %>% 
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
        terra::vect()
    occ_fauna_cleaned_i
    
    occ_fauna_cleaned_r <- terra::rasterize(x = occ_fauna_cleaned_i, y = chelsa, background = 0) %>% 
        terra::crop(chelsa, mask = TRUE)
    
    terra::writeRaster(occ_fauna_cleaned_r, 
                       paste0("03_results/01_ocorrencias/rasters/fauna_", gsub(" ", "_", tolower(i)), ".tif"), overwrite = TRUE)
    
}
doParallel::stopImplicitCluster()

##### lists ----
fauna_cleaned_splist_cr_ex <- occ_fauna_cleaned %>% 
    dplyr::filter(category == "CR (PEX)") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" ", "_")
fauna_cleaned_splist_cr_ex

fauna_cleaned_splist_cr <- occ_fauna_cleaned %>% 
    dplyr::filter(category == "CR") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" ", "_")
fauna_cleaned_splist_cr

fauna_cleaned_splist_en <- occ_fauna_cleaned %>% 
    dplyr::filter(category == "EN") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" ", "_")
fauna_cleaned_splist_en

fauna_cleaned_splist_vu <- occ_fauna_cleaned %>% 
    dplyr::filter(category == "VU") %>% 
    dplyr::pull(species) %>% 
    unique() %>% 
    stringr::str_to_lower() %>% 
    stringr::str_replace_all(" ", "_")
fauna_cleaned_splist_vu

##### files ----
fauna_raster_files_total <- dir(path = "03_results/01_ocorrencias/rasters", pattern = "fauna", full.names = TRUE)
fauna_raster_files_total

fauna_raster_files_cr_pex <- stringr::str_subset(fauna_raster_files_total, paste0(fauna_cleaned_splist_cr_ex, collapse = "|"))
fauna_raster_files_cr_pex

fauna_raster_files_cr <- stringr::str_subset(fauna_raster_files_total, paste0(fauna_cleaned_splist_cr, collapse = "|"))
fauna_raster_files_cr

fauna_raster_files_en <- stringr::str_subset(fauna_raster_files_total, paste0(fauna_cleaned_splist_en, collapse = "|"))
fauna_raster_files_en

fauna_raster_files_vu <- stringr::str_subset(fauna_raster_files_total, paste0(fauna_cleaned_splist_vu, collapse = "|"))
fauna_raster_files_vu

##### raster filters ----
fauna_raster_total <- terra::rast(fauna_raster_files_total) %>% 
    terra::app(sum, cores = parallelly::availableCores(omit = 2))
fauna_raster_total_na <- terra::ifel(fauna_raster_total == 0, NA, fauna_raster_total)

fauna_raster_cr_pex <- terra::rast(fauna_raster_files_cr_pex) %>% 
    terra::app(sum, cores = parallelly::availableCores(omit = 2))
fauna_raster_cr_pex_na <- terra::ifel(fauna_raster_cr_pex == 0, NA, fauna_raster_cr_pex)

fauna_raster_cr <- terra::rast(fauna_raster_files_cr) %>% 
    terra::app(sum, cores = parallelly::availableCores(omit = 2))
fauna_raster_cr_na <- terra::ifel(fauna_raster_cr == 0, NA, fauna_raster_cr_pex)

fauna_raster_en <- terra::rast(fauna_raster_files_en) %>% 
    terra::app(sum, cores = parallelly::availableCores(omit = 2))
fauna_raster_en_na <- terra::ifel(fauna_raster_en == 0, NA, fauna_raster_en)

fauna_raster_vu <- terra::rast(fauna_raster_files_vu) %>% 
    terra::app(sum, cores = parallelly::availableCores(omit = 2))
fauna_raster_vu_na <- terra::ifel(fauna_raster_vu == 0, NA, fauna_raster_vu)

##### maps ----
tmap::tmap_mode("plot")

map_fauna_total <- tm_shape(br) +
    tm_fill() +
    tm_shape(fauna_raster_total_na) +
    tm_raster("sum", 
              col.scale = tm_scale_continuous(values = rev(RColorBrewer::brewer.pal(9, "Spectral")), n = 5),
              col.legend = tm_legend("Fauna total", position = tm_pos_in("left", "bottom"), 
                                     reverse = TRUE, title.size = 2, text.size = 1)) +
    tm_shape(br) +
    tm_borders()
map_fauna_total
tmap::tmap_save(map_fauna_total, "03_results/01_ocorrencias/figures/map_fauna_total.png", 
                width = 20, height = 20, units = "cm", dpi = 300)
tmap::tmap_save(map_fauna_total, "99_reports/figures/map_fauna_total.png", 
                width = 20, height = 20, units = "cm", dpi = 300)

map_fauna_cr_pex <- tm_shape(br) +
    tm_fill() +
    tm_shape(fauna_raster_cr_pex_na) +
    tm_raster("sum", 
              col.scale = tm_scale_continuous(values = rev(RColorBrewer::brewer.pal(9, "Spectral")), n = 3),
              col.legend = tm_legend("Fauna CR (PEX)", position = tm_pos_in("left", "bottom"), 
                                     reverse = TRUE, title.size = 2, text.size = 1)) +
    tm_shape(br) +
    tm_borders()
map_fauna_cr_pex
tmap::tmap_save(map_fauna_cr_pex, "03_results/01_ocorrencias/figures/map_fauna_cr_pex.png", 
                width = 20, height = 20, units = "cm", dpi = 300)
tmap::tmap_save(map_fauna_cr_pex, "99_reports/figures/map_fauna_cr_pex.png", 
                width = 20, height = 20, units = "cm", dpi = 300)

map_fauna_cr <- tm_shape(br) +
    tm_fill() +
    tm_shape(fauna_raster_cr_na) +
    tm_raster("sum", 
              col.scale = tm_scale_continuous(values = rev(RColorBrewer::brewer.pal(9, "Spectral")), n = 3),
              col.legend = tm_legend("Fauna CR", position = tm_pos_in("left", "bottom"), 
                                     reverse = TRUE, title.size = 2, text.size = 1)) +
    tm_shape(br) +
    tm_borders()
map_fauna_cr
tmap::tmap_save(map_fauna_cr, "03_results/01_ocorrencias/figures/map_fauna_cr.png", 
                width = 20, height = 20, units = "cm", dpi = 300)
tmap::tmap_save(map_fauna_cr, "99_reports/figures/map_fauna_cr.png", 
                width = 20, height = 20, units = "cm", dpi = 300)

map_fauna_en <- tm_shape(br) +
    tm_fill() +
    tm_shape(fauna_raster_en_na) +
    tm_raster("sum", 
              col.scale = tm_scale_continuous(values = rev(RColorBrewer::brewer.pal(9, "Spectral")), n = 5),
              col.legend = tm_legend("Fauna EN", position = tm_pos_in("left", "bottom"), 
                                     reverse = TRUE, title.size = 2, text.size = 1)) +
    tm_shape(br) +
    tm_borders()
map_fauna_en
tmap::tmap_save(map_fauna_en, "03_results/01_ocorrencias/figures/map_fauna_en.png", 
                width = 20, height = 20, units = "cm", dpi = 300)
tmap::tmap_save(map_fauna_en, "99_reports/figures/map_fauna_en.png", 
                width = 20, height = 20, units = "cm", dpi = 300)

map_fauna_vu <- tm_shape(br) +
    tm_fill() +
    tm_shape(fauna_raster_vu_na) +
    tm_raster("sum", 
              col.scale = tm_scale_continuous(values = rev(RColorBrewer::brewer.pal(9, "Spectral")), n = 5),
              col.legend = tm_legend("Fauna VU", position = tm_pos_in("left", "bottom"), 
                                     reverse = TRUE, title.size = 2, text.size = 1)) +
    tm_shape(br) +
    tm_borders()
map_fauna_vu
tmap::tmap_save(map_fauna_vu, "03_results/01_ocorrencias/figures/map_fauna_vu.png", 
                width = 20, height = 20, units = "cm", dpi = 300)
tmap::tmap_save(map_fauna_vu, "99_reports/figures/map_fauna_vu.png", 
                width = 20, height = 20, units = "cm", dpi = 300)

##### export ----
writeRaster(fauna_raster_total_na, "03_results/01_ocorrencias/rasters_richness/fauna_raster_total.tif", overwrite = TRUE)
writeRaster(fauna_raster_cr_pex_na, "03_results/01_ocorrencias/rasters_richness/fauna_raster_cr_pex.tif", overwrite = TRUE)
writeRaster(fauna_raster_cr_na, "03_results/01_ocorrencias/rasters_richness/fauna_raster_cr.tif", overwrite = TRUE)
writeRaster(fauna_raster_en_na, "03_results/01_ocorrencias/rasters_richness/fauna_raster_en.tif", overwrite = TRUE)
writeRaster(fauna_raster_vu_na, "03_results/01_ocorrencias/rasters_richness/fauna_raster_vu.tif", overwrite = TRUE)

# end ---------------------------------------------------------------------