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
mun <- sf::st_read("01_data/05_municipalities/BR_Municipios_2023.shp") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN))
mun

## sdm ----
data_mun_sdm <- readr::read_csv("03_prioritization/data_mun_sdm.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(3:5))
data_mun_sdm

## rabies ----
data_mun_rabies_domestic_canine <- readr::read_csv("03_prioritization/data_mun_rabies_domestic_canine_sum.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, CD_UF, NM_UF))
data_mun_rabies_domestic_canine

data_mun_rabies_domestic_feline <- readr::read_csv("03_prioritization/data_mun_rabies_domestic_feline_sum.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, CD_UF, NM_UF))
data_mun_rabies_domestic_feline

data_mun_rabies_humans_snot <- readr::read_csv("03_prioritization/data_mun_rabies_humans_snot_sum.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, CD_UF, NM_UF))
data_mun_rabies_humans_snot

data_mun_rabies_humans_sinf <- readr::read_csv("03_prioritization/data_mun_rabies_humans_sinf_sum.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, CD_UF, NM_UF))
data_mun_rabies_humans_sinf

## socioeconomic ----
data_mun_socioeconomic_basic_care <- readr::read_csv("03_prioritization/data_mun_socioeconomic_basic_care.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_basic_care

data_mun_socioeconomic_cattle_population <- readr::read_csv("03_prioritization/data_mun_socioeconomic_cattle_population.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_cattle_population

data_mun_socioeconomic_data_child <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_child.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_child

data_mun_socioeconomic_data_density <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_density.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_density

data_mun_socioeconomic_data_education <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_education.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN))  %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_education

data_mun_socioeconomic_data_gini <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_gini.csv")   %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_gini

data_mun_socioeconomic_data_idh <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_idh.csv")        %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_idh

data_mun_socioeconomic_data_indigenous <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_indigenous.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_indigenous

data_mun_socioeconomic_data_literacy <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_literacy.csv")   %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_literacy

data_mun_socioeconomic_data_poor <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_poor.csv")    %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_poor

data_mun_socioeconomic_data_population <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_population.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_population

data_mun_socioeconomic_data_years_study <- readr::read_csv("03_prioritization/data_mun_socioeconomic_data_years_study.csv")  %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_data_years_study

data_mun_socioeconomic_epi_surv <- readr::read_csv("03_prioritization/data_mun_socioeconomic_epi_surv.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_epi_surv

data_mun_socioeconomic_health_exp <- readr::read_csv("03_prioritization/data_mun_socioeconomic_health_exp.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN, NM_UF))
data_mun_socioeconomic_health_exp

data_mun_socioeconomic_vac_coverage_mean <- readr::read_csv("03_prioritization/data_mun_socioeconomic_vac_coverage_mean.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(NM_MUN))
data_mun_socioeconomic_vac_coverage_mean

## landscape ----
data_roads <- readr::read_csv("03_prioritization/data_roads.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
    dplyr::select(-c(2, 3))
data_roads

data_pasture <- readr::read_csv("03_prioritization/data_pasture.csv") %>% 
    dplyr::mutate(CD_MUN = as.numeric(CD_MUN))
data_pasture

# join data ---------------------------------------------------------------

# rabies_domestic_canine_sum
mun_data <- mun %>% 
    dplyr::left_join(data_mun_sdm, by = "CD_MUN") %>% 
    
    dplyr::left_join(data_mun_rabies_domestic_canine, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_rabies_domestic_feline, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_rabies_humans_snot, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_rabies_humans_sinf, by = "CD_MUN") %>% 
    
    dplyr::left_join(data_mun_socioeconomic_basic_care, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_cattle_population, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_child, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_density, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_education, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_gini, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_idh, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_indigenous, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_literacy, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_poor, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_population, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_data_years_study, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_epi_surv, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_health_exp, by = "CD_MUN") %>% 
    dplyr::left_join(data_mun_socioeconomic_vac_coverage_mean, by = "CD_MUN") %>% 

    dplyr::left_join(data_roads, by = "CD_MUN") %>% 
    dplyr::left_join(data_pasture, by = "CD_MUN")
mun_data

# export
sf::st_write(mun_data, "03_prioritization/00_mun_data.gpkg")

# end ---------------------------------------------------------------------
