#' ----
#' aim: spatial joint
#' author: mauricio vancine
#' date: 27/01/2025
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(sf)
library(tmap)

# import data -------------------------------------------------------------

## municipalities ----
mun <- sf::st_read("01_data/03_municipalities/BR_Municipios_2023.shp")
mun

plot(mun$geometry)

## data at the municipality level ----

### rabies domestic ----
rabies_domestic_canine <- readxl::read_excel("01_data/02_rabies/03_final/cases_cats_dogs.xlsx", sheet = 1)
rabies_domestic_canine

rabies_domestic_feline <- readxl::read_excel("01_data/02_rabies/03_final/cases_cats_dogs.xlsx", sheet = 2)
rabies_domestic_feline

### rabies humans ----
rabies_humans_msinf <- readxl::read_excel("01_data/02_rabies/03_final/cases_humans.xlsx", sheet = 1)
rabies_humans_msinf

rabies_humans_snot1 <- readxl::read_excel("01_data/02_rabies/03_final/cases_humans.xlsx", sheet = 2)
rabies_humans_snot1

rabies_humans_sinf1 <- readxl::read_excel("01_data/02_rabies/03_final/cases_humans.xlsx", sheet = 3)
rabies_humans_sinf1

rabies_humans_snot7 <- readxl::read_excel("01_data/02_rabies/03_final/cases_humans.xlsx", sheet = 4)
rabies_humans_snot7

rabies_humans_sinf7 <- readxl::read_excel("01_data/02_rabies/03_final/cases_humans.xlsx", sheet = 5)
rabies_humans_sinf7

## data at the states level ----

### uf_cases_all_animals ----
rabies_cases_all_animals <- NULL

year <- readxl::excel_sheets("01_data/02_rabies/03_final/uf_cases_all_animals.xlsx")[-11]
year

for(i in 1:10){
    
    rabies_cases_all_animals_i <- readxl::read_excel("01_data/02_rabies/03_final/uf_cases_all_animals.xlsx", sheet = i)
    rabies_cases_all_animals <- dplyr::bind_rows(rabies_cases_all_animals, rabies_cases_all_animals_i)
    
}

rabies_cases_all_animals

### cases map ----
rabies_cases_map <- NULL

animal <- readxl::excel_sheets("01_data/02_rabies/03_final/uf_cases_mapa.xlsx")[-6]
animal

for(i in 1:5){
    
    rabies_cases_map_i <- readxl::read_excel("01_data/02_rabies/03_final/uf_cases_mapa.xlsx", sheet = i)
    rabies_cases_map <- dplyr::bind_rows(rabies_cases_map, rabies_cases_map_i)
    
}

rabies_cases_map

# data summary ----------------------------------------------------------

# summary
rabies_domestic_canine_sum <- rabies_domestic_canine %>% 
    dplyr::group_by(CD_MUN, NM_MUN, CD_UF, NM_UF) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_MUN = as.character(CD_MUN),
                  CD_UF = as.character(CD_UF))
rabies_domestic_canine_sum

rabies_domestic_feline_sum <- rabies_domestic_feline %>% 
    dplyr::group_by(CD_MUN, NM_MUN, CD_UF, NM_UF) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_MUN = as.character(CD_MUN),
                  CD_UF = as.character(CD_UF))
rabies_domestic_feline_sum

rabies_humans_msinf_sum <- rabies_humans_msinf %>% 
    dplyr::group_by(CD_MUN, NM_MUN, CD_UF, NM_UF) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_MUN = as.character(CD_MUN),
                  CD_UF = as.character(CD_UF))
rabies_humans_msinf_sum

rabies_humans_snot1_sum <- rabies_humans_snot1 %>% 
    dplyr::group_by(CD_MUN, NM_MUN, CD_UF, NM_UF) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_MUN = as.character(CD_MUN),
                  CD_UF = as.character(CD_UF))
rabies_humans_snot1_sum

rabies_humans_sinf1_sum <- rabies_humans_sinf1 %>% 
    dplyr::group_by(CD_MUN, NM_MUN, CD_UF, NM_UF) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_MUN = as.character(CD_MUN),
                  CD_UF = as.character(CD_UF))
rabies_humans_sinf1_sum

rabies_humans_snot7_sum <- rabies_humans_snot7 %>% 
    dplyr::group_by(CD_MUN, NM_MUN, CD_UF, NM_UF) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_MUN = as.character(CD_MUN),
                  CD_UF = as.character(CD_UF))
rabies_humans_snot7_sum

rabies_humans_sinf7_sum <- rabies_humans_sinf7 %>% 
    dplyr::group_by(CD_MUN, NM_MUN, CD_UF, NM_UF) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_MUN = as.character(CD_MUN),
                  CD_UF = as.character(CD_UF))
rabies_humans_sinf7_sum

rabies_cases_all_animals_sum <- rabies_cases_all_animals %>% 
    dplyr::group_by(CD_UF, NM_UF, ANIMAL) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_UF = as.character(CD_UF)) %>% 
    tidyr::pivot_wider(id_cols = c(CD_UF, NM_UF), 
                       names_from = ANIMAL, values_from = "CASES") %>% 
    dplyr::mutate(across(everything(), ~replace_na(., 0)))
rabies_cases_all_animals_sum

rabies_cases_map_sum <- rabies_cases_map %>% 
    dplyr::group_by(CD_UF, NM_UF, ANIMAL) %>% 
    dplyr::summarise(CASES = n()) %>% 
    dplyr::mutate(CD_UF = as.character(CD_UF)) %>% 
    tidyr::pivot_wider(id_cols = c(CD_UF, NM_UF), 
                       names_from = ANIMAL, values_from = "CASES") %>% 
    dplyr::mutate(across(everything(), ~replace_na(., 0)))
rabies_cases_map_sum

# join data ---------------------------------------------------------------

# rabies_domestic_canine_sum
mun_rabies_domestic_canine_sum <- dplyr::left_join(mun, rabies_domestic_canine_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_domestic_canine_sum

tm_shape(mun_rabies_domestic_feline_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_domestic_feline_sum
mun_rabies_domestic_feline_sum <- dplyr::left_join(mun, rabies_domestic_feline_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_domestic_feline_sum

tm_shape(mun_rabies_domestic_feline_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_humans_msinf_sum_sum
mun_rabies_humans_msinf_sum <- dplyr::left_join(mun, rabies_humans_msinf_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_humans_msinf_sum

tm_shape(mun_rabies_humans_msinf_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_humans_snot1_sum
mun_rabies_humans_snot1_sum <- dplyr::left_join(mun, rabies_humans_snot1_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_humans_snot1_sum

tm_shape(mun_rabies_humans_snot1_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_humans_sinf1_sum
mun_rabies_humans_sinf1_sum <- dplyr::left_join(mun, rabies_humans_sinf1_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_humans_sinf1_sum

tm_shape(mun_rabies_humans_sinf1_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_humans_snot1_sum
mun_rabies_humans_snot1_sum <- dplyr::left_join(mun, rabies_humans_snot1_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_humans_snot1_sum

tm_shape(mun_rabies_humans_snot1_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_humans_sinf1_sum
mun_rabies_humans_sinf1_sum <- dplyr::left_join(mun, rabies_humans_sinf1_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_humans_sinf1_sum

tm_shape(mun_rabies_humans_sinf1_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_humans_snot1_sum
mun_rabies_humans_snot1_sum <- dplyr::left_join(mun, rabies_humans_snot1_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_humans_snot1_sum

tm_shape(mun_rabies_humans_snot1_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_humans_sinf1_sum
mun_rabies_humans_sinf1_sum <- dplyr::left_join(mun, rabies_humans_sinf1_sum, by = "CD_MUN") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_humans_sinf1_sum

tm_shape(mun_rabies_humans_sinf1_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))


# rabies_cases_all_animals_sum
mun_rabies_cases_all_animals_sum <- dplyr::left_join(mun, rabies_cases_all_animals_sum, by = "CD_UF") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_cases_all_animals_sum

tm_shape(mun_rabies_cases_all_animals_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# rabies_cases_map_sum
mun_rabies_cases_map_sum <- dplyr::left_join(mun, rabies_cases_map_sum, by = "CD_UF") %>% 
    dplyr::select(-ends_with(".x")) %>% 
    dplyr::select(-ends_with(".y"))
mun_rabies_cases_map_sum

tm_shape(mun_rabies_cases_map_sum) +
    tm_polygons(col = "gray",
                fill = "CASES", 
                fill.legend = tm_legend(
                    title = "Rabies domestic feline", 
                    position = tm_pos_in("right", "bottom"),
                    reverse = TRUE))

# end ---------------------------------------------------------------------
