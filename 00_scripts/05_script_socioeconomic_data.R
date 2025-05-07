#' ----
#' aim: socioeconomic data
#' author: mauricio vancine
#' date: 05/05/2025
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(stringi)
library(terra)
library(tidyterra)
library(furrr)

# import data -------------------------------------------------------------

## spending basic care ----
basic_care_states <- readxl::excel_sheets("01_data/03_socioeconomic/bas_care.xlsx")
basic_care_states <- sort(basic_care_states[-which(basic_care_states == last(basic_care_states))])
basic_care_states

basic_care <- NULL
for(i in 1:length(basic_care_states)){
    
    basic_care_i <- readxl::read_excel("01_data/03_socioeconomic/bas_care.xlsx", sheet = i) %>% 
        dplyr::select(1:2, Total) %>% 
        dplyr::mutate(NM_UF = basic_care_states[i], .after = 2) %>% 
        dplyr::mutate(CD_MUN = as.numeric(CD_MUN)) %>% 
        dplyr::rename(SPENDING_BASIC_CARE = Total)
    basic_care <- dplyr::bind_rows(basic_care, basic_care_i)
    
}
basic_care

## cattle population ----
cattle_population <- readxl::read_excel("01_data/03_socioeconomic/cattle_pop_2023.xlsx") %>% 
    dplyr::select(1, 2, `2023`) %>% 
    dplyr::rename(CATTLE_POP = 3,
                  local = NM_MUN) %>% 
    dplyr::mutate(CATTLE_POP = as.numeric(CATTLE_POP),
                  NM_MUN = str_remove(local, "\\s*\\(.*\\)"),
                  NM_UF = str_match(local, "\\(([^)]+)\\)")[, 2]) %>% 
    dplyr::select(-local) %>% 
    dplyr::select(CD_MUN, NM_MUN, NM_UF, CATTLE_POP)
cattle_population

## spending epidemiological surveillance ----
epi_surv_states <- readxl::excel_sheets("01_data/03_socioeconomic/epi_surv.xlsx")
epi_surv_states <- epi_surv_states[-which(epi_surv_states == last(epi_surv_states))]
epi_surv_states

epi_surv <- NULL
for(i in 1:length(epi_surv_states)){
    
    epi_surv_i <- readxl::read_excel("01_data/03_socioeconomic/epi_surv.xlsx", sheet = i) %>% 
        dplyr::select(1:2, Total) %>% 
        dplyr::mutate(CD_MUN = as.numeric(CD_MUN),
                      Total = as.numeric(Total)) %>% 
        dplyr::mutate(NM_UF = epi_surv_states[i], .after = 2) %>% 
        dplyr::rename(SPENDING_EPIDEMIO_SURV = Total)
    epi_surv <- dplyr::bind_rows(epi_surv, epi_surv_i)
    
}
epi_surv

## spending health ----
health_exp_states <- readxl::excel_sheets("01_data/03_socioeconomic/health_exp.xlsx")
health_exp_states <- health_exp_states[-which(health_exp_states == last(health_exp_states))]
health_exp_states

health_exp <- NULL
for(i in 1:length(health_exp_states)){
    health_exp_i <- readxl::read_excel("01_data/03_socioeconomic/health_exp.xlsx", sheet = i) %>% 
        dplyr::select(1:2, Total) %>% 
        dplyr::filter(NM_MUN != "Total") %>% 
        dplyr::mutate(CD_MUN = as.numeric(CD_MUN),
                      Total = as.numeric(Total)) %>% 
        dplyr::mutate(NM_UF = health_exp_states[i], .after = 2) %>% 
        dplyr::rename(SPENDING_HEALTH = Total)
    health_exp <- dplyr::bind_rows(health_exp, health_exp_i)
    
}
health_exp

## vaccine coverage ----
vac_coverage_years <- readxl::excel_sheets("01_data/03_socioeconomic/vac_coverage.xlsx")
vac_coverage_years <- vac_coverage_years[-which(vac_coverage_years == last(vac_coverage_years))]
vac_coverage_years

vac_coverage <- NULL
for(i in 1:length(vac_coverage_years)){
    
    vac_coverage_i <- readxl::read_excel("01_data/03_socioeconomic/vac_coverage.xlsx", sheet = i) %>% 
        dplyr::select(1:2, D_VACCINATED, C_VACCINATED) %>% 
        dplyr::mutate(CD_MUN = as.numeric(CD_MUN),
                      YEAR = vac_coverage_years[i]) %>% 
        dplyr::rename(CANINE_VACCINATED = D_VACCINATED,
                      FELINE_VACCINATED = C_VACCINATED)
    vac_coverage <- dplyr::bind_rows(vac_coverage, vac_coverage_i)
    
}
vac_coverage

# vac_coverage
vac_coverage <- vac_coverage %>% 
    dplyr::filter(YEAR == 2017)
vac_coverage

## socioeconomic data ----
socioeconomic_data_sheets <- readxl::excel_sheets("01_data/03_socioeconomic/socioeconomic_data.xlsx")
socioeconomic_data_sheets

### density ----
socioeconomic_data_density <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 1)
socioeconomic_data_density

### population ----
socioeconomic_data_population <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 3) %>% 
    separate(NM_MUN, into = c("NM_MUN", "NM_UF"), sep = " \\(", remove = TRUE) %>%
    mutate(NM_UF = gsub("\\)", "", NM_UF))
socioeconomic_data_population

mun_join <- socioeconomic_data_population %>% 
    dplyr::select(1:3) %>% 
    dplyr::mutate(NM_MUN_st = NM_MUN %>% 
                      stringr::str_to_lower() %>%
                      stringi::stri_trans_general("Latin-ASCII") %>%
                      stringr::str_replace_all("[[:punct:][:space:]]+", "")) %>% 
    dplyr::mutate(NM_MUN_st = paste0(NM_MUN_st, "_", NM_UF)) %>% 
    dplyr::select(1, 4)
mun_join

### poor ----
socioeconomic_data_poor <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 5) %>% 
    dplyr::mutate(NM_MUN = case_when(
        NM_MUN == "Amparo de São Francisco" ~ "Amparo do São Francisco",
        NM_MUN == "Augusto Severo" ~ "Campo Grande",
        NM_MUN == "Barão de Monte Alto" ~ "Barão do Monte Alto",
        NM_MUN == "Brasópolis" ~ "Brazópolis",
        NM_MUN == "Dona Eusébia" ~ "Dona Euzébia",
        NM_MUN == "Eldorado dos Carajás" ~ "Eldorado do Carajás",
        NM_MUN == "Embu" ~ "Embu das Artes",
        NM_MUN == "Florínia" ~ "Florínea",
        NM_MUN == "Fortaleza do Tabocão" ~ "Tabocão",
        NM_MUN == "Gracho Cardoso" ~ "Graccho Cardoso",
        NM_MUN == "Iguaraci" ~ "Iguaracy",
        NM_MUN == "Itapagé" ~ "Itapajé",
        NM_MUN == "Muquém de São Francisco" ~ "Muquém do São Francisco",
        NM_MUN == "Poxoréo" ~ "Poxoréu",
        NM_MUN == "Presidente Juscelino" & NM_UF == "RN" ~ "Serra Caiada",
        NM_MUN == "Santa Isabel do Pará" ~ "Santa Izabel do Pará",
        NM_MUN == "Santarém" & NM_UF == "PB" ~ "Joca Claudino",
        NM_MUN == "Santa Teresinha" & NM_UF == "BA" ~ "Santa Terezinha",
        NM_MUN == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
        NM_MUN == "São Luís do Paraitinga" ~ "São Luíz do Paraitinga",
        NM_MUN == "São Thomé das Letras" ~ "São Tomé das Letras",
        NM_MUN == "Seridó" ~ "São Vicente do Seridó",
        .default = NM_MUN)) %>% 
    dplyr::mutate(NM_MUN_st = NM_MUN %>% 
                      stringr::str_to_lower() %>%
                      stringi::stri_trans_general("Latin-ASCII") %>%
                      stringr::str_replace_all("[[:punct:][:space:]]+", "")) %>% 
    dplyr::mutate(NM_MUN_st = paste0(NM_MUN_st, "_", NM_UF)) %>% 
    dplyr::left_join(mun_join, by = "NM_MUN_st") %>% 
    dplyr::relocate(CD_MUN, .before = 1) %>% 
    dplyr::select(-NM_MUN_st)
socioeconomic_data_poor

### gini ----
socioeconomic_data_gini <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 7) %>% 
    dplyr::mutate(NM_MUN = case_when(
        NM_MUN == "Amparo de São Francisco" ~ "Amparo do São Francisco",
        NM_MUN == "Augusto Severo" ~ "Campo Grande",
        NM_MUN == "Barão de Monte Alto" ~ "Barão do Monte Alto",
        NM_MUN == "Brasópolis" ~ "Brazópolis",
        NM_MUN == "Dona Eusébia" ~ "Dona Euzébia",
        NM_MUN == "Eldorado dos Carajás" ~ "Eldorado do Carajás",
        NM_MUN == "Embu" ~ "Embu das Artes",
        NM_MUN == "Florínia" ~ "Florínea",
        NM_MUN == "Fortaleza do Tabocão" ~ "Tabocão",
        NM_MUN == "Gracho Cardoso" ~ "Graccho Cardoso",
        NM_MUN == "Iguaraci" ~ "Iguaracy",
        NM_MUN == "Itapagé" ~ "Itapajé",
        NM_MUN == "Muquém de São Francisco" ~ "Muquém do São Francisco",
        NM_MUN == "Poxoréo" ~ "Poxoréu",
        NM_MUN == "Presidente Juscelino" & NM_UF == "RN" ~ "Serra Caiada",
        NM_MUN == "Santa Isabel do Pará" ~ "Santa Izabel do Pará",
        NM_MUN == "Santarém" & NM_UF == "PB" ~ "Joca Claudino",
        NM_MUN == "Santa Teresinha" & NM_UF == "BA" ~ "Santa Terezinha",
        NM_MUN == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
        NM_MUN == "São Luís do Paraitinga" ~ "São Luíz do Paraitinga",
        NM_MUN == "São Thomé das Letras" ~ "São Tomé das Letras",
        NM_MUN == "Seridó" ~ "São Vicente do Seridó",
        .default = NM_MUN)) %>%
    dplyr::mutate(NM_MUN_st = NM_MUN %>% 
                      stringr::str_to_lower() %>%
                      stringi::stri_trans_general("Latin-ASCII") %>%
                      stringr::str_replace_all("[[:punct:][:space:]]+", "")) %>% 
    dplyr::mutate(NM_MUN_st = paste0(NM_MUN_st, "_", NM_UF)) %>% 
    dplyr::left_join(mun_join, by = "NM_MUN_st") %>% 
    dplyr::relocate(CD_MUN, .before = 1) %>% 
    dplyr::select(-NM_MUN_st)
socioeconomic_data_gini

### indigenous ----
socioeconomic_data_indigenous <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 9) %>% 
    dplyr::left_join(socioeconomic_data_population[, 1:3]) %>% 
    dplyr::relocate(NM_UF, .after = 2)
socioeconomic_data_indigenous

### child ----
socioeconomic_data_child <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 11) %>% 
    dplyr::mutate(NM_MUN = case_when(
        NM_MUN == "Amparo de São Francisco" ~ "Amparo do São Francisco",
        NM_MUN == "Augusto Severo" ~ "Campo Grande",
        NM_MUN == "Barão de Monte Alto" ~ "Barão do Monte Alto",
        NM_MUN == "Brasópolis" ~ "Brazópolis",
        NM_MUN == "Dona Eusébia" ~ "Dona Euzébia",
        NM_MUN == "Eldorado dos Carajás" ~ "Eldorado do Carajás",
        NM_MUN == "Embu" ~ "Embu das Artes",
        NM_MUN == "Florínia" ~ "Florínea",
        NM_MUN == "Fortaleza do Tabocão" ~ "Tabocão",
        NM_MUN == "Gracho Cardoso" ~ "Graccho Cardoso",
        NM_MUN == "Iguaraci" ~ "Iguaracy",
        NM_MUN == "Itapagé" ~ "Itapajé",
        NM_MUN == "Muquém de São Francisco" ~ "Muquém do São Francisco",
        NM_MUN == "Poxoréo" ~ "Poxoréu",
        NM_MUN == "Presidente Juscelino" & NM_UF == "RN" ~ "Serra Caiada",
        NM_MUN == "Santa Isabel do Pará" ~ "Santa Izabel do Pará",
        NM_MUN == "Santarém" & NM_UF == "PB" ~ "Joca Claudino",
        NM_MUN == "Santa Teresinha" & NM_UF == "BA" ~ "Santa Terezinha",
        NM_MUN == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
        NM_MUN == "São Luís do Paraitinga" ~ "São Luíz do Paraitinga",
        NM_MUN == "São Thomé das Letras" ~ "São Tomé das Letras",
        NM_MUN == "Seridó" ~ "São Vicente do Seridó",
        .default = NM_MUN)) %>%
    dplyr::mutate(NM_MUN_st = NM_MUN %>% 
                      stringr::str_to_lower() %>%
                      stringi::stri_trans_general("Latin-ASCII") %>%
                      stringr::str_replace_all("[[:punct:][:space:]]+", "")) %>% 
    dplyr::mutate(NM_MUN_st = paste0(NM_MUN_st, "_", NM_UF)) %>% 
    dplyr::left_join(mun_join, by = "NM_MUN_st") %>% 
    dplyr::relocate(CD_MUN, .before = 1) %>% 
    dplyr::select(-NM_MUN_st)
socioeconomic_data_child

### idh ----
socioeconomic_data_idh <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 13) %>% 
    dplyr::mutate(NM_MUN = case_when(
        NM_MUN == "Amparo de São Francisco" ~ "Amparo do São Francisco",
        NM_MUN == "Augusto Severo" ~ "Campo Grande",
        NM_MUN == "Barão de Monte Alto" ~ "Barão do Monte Alto",
        NM_MUN == "Brasópolis" ~ "Brazópolis",
        NM_MUN == "Dona Eusébia" ~ "Dona Euzébia",
        NM_MUN == "Eldorado dos Carajás" ~ "Eldorado do Carajás",
        NM_MUN == "Embu" ~ "Embu das Artes",
        NM_MUN == "Florínia" ~ "Florínea",
        NM_MUN == "Fortaleza do Tabocão" ~ "Tabocão",
        NM_MUN == "Gracho Cardoso" ~ "Graccho Cardoso",
        NM_MUN == "Iguaraci" ~ "Iguaracy",
        NM_MUN == "Itapagé" ~ "Itapajé",
        NM_MUN == "Muquém de São Francisco" ~ "Muquém do São Francisco",
        NM_MUN == "Poxoréo" ~ "Poxoréu",
        NM_MUN == "Presidente Juscelino" & NM_UF == "RN" ~ "Serra Caiada",
        NM_MUN == "Santa Isabel do Pará" ~ "Santa Izabel do Pará",
        NM_MUN == "Santarém" & NM_UF == "PB" ~ "Joca Claudino",
        NM_MUN == "Santa Teresinha" & NM_UF == "BA" ~ "Santa Terezinha",
        NM_MUN == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
        NM_MUN == "São Luís do Paraitinga" ~ "São Luíz do Paraitinga",
        NM_MUN == "São Thomé das Letras" ~ "São Tomé das Letras",
        NM_MUN == "Seridó" ~ "São Vicente do Seridó",
        .default = NM_MUN)) %>%
    dplyr::mutate(NM_MUN_st = NM_MUN %>% 
                      stringr::str_to_lower() %>%
                      stringi::stri_trans_general("Latin-ASCII") %>%
                      stringr::str_replace_all("[[:punct:][:space:]]+", "")) %>% 
    dplyr::mutate(NM_MUN_st = paste0(NM_MUN_st, "_", NM_UF)) %>% 
    dplyr::left_join(mun_join, by = "NM_MUN_st") %>% 
    dplyr::relocate(CD_MUN, .before = 1) %>% 
    dplyr::select(-NM_MUN_st)
socioeconomic_data_idh

### education ----
socioeconomic_data_education <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 15) %>% 
    dplyr::mutate(NM_MUN = case_when(
        NM_MUN == "Amparo de São Francisco" ~ "Amparo do São Francisco",
        NM_MUN == "Augusto Severo" ~ "Campo Grande",
        NM_MUN == "Barão de Monte Alto" ~ "Barão do Monte Alto",
        NM_MUN == "Brasópolis" ~ "Brazópolis",
        NM_MUN == "Dona Eusébia" ~ "Dona Euzébia",
        NM_MUN == "Eldorado dos Carajás" ~ "Eldorado do Carajás",
        NM_MUN == "Embu" ~ "Embu das Artes",
        NM_MUN == "Florínia" ~ "Florínea",
        NM_MUN == "Fortaleza do Tabocão" ~ "Tabocão",
        NM_MUN == "Gracho Cardoso" ~ "Graccho Cardoso",
        NM_MUN == "Iguaraci" ~ "Iguaracy",
        NM_MUN == "Itapagé" ~ "Itapajé",
        NM_MUN == "Muquém de São Francisco" ~ "Muquém do São Francisco",
        NM_MUN == "Poxoréo" ~ "Poxoréu",
        NM_MUN == "Presidente Juscelino" & NM_UF == "RN" ~ "Serra Caiada",
        NM_MUN == "Santa Isabel do Pará" ~ "Santa Izabel do Pará",
        NM_MUN == "Santarém" & NM_UF == "PB" ~ "Joca Claudino",
        NM_MUN == "Santa Teresinha" & NM_UF == "BA" ~ "Santa Terezinha",
        NM_MUN == "Santo Antônio do Leverger" ~ "Santo Antônio de Leverger",
        NM_MUN == "São Luís do Paraitinga" ~ "São Luíz do Paraitinga",
        NM_MUN == "São Thomé das Letras" ~ "São Tomé das Letras",
        NM_MUN == "Seridó" ~ "São Vicente do Seridó",
        .default = NM_MUN)) %>%
    dplyr::mutate(NM_MUN_st = NM_MUN %>% 
                      stringr::str_to_lower() %>%
                      stringi::stri_trans_general("Latin-ASCII") %>%
                      stringr::str_replace_all("[[:punct:][:space:]]+", "")) %>% 
    dplyr::mutate(NM_MUN_st = paste0(NM_MUN_st, "_", NM_UF)) %>% 
    dplyr::left_join(mun_join, by = "NM_MUN_st") %>% 
    dplyr::relocate(CD_MUN, .before = 1) %>% 
    dplyr::select(-NM_MUN_st)
socioeconomic_data_education

### literacy ----
socioeconomic_data_literacy <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 17) %>% 
    dplyr::mutate(RT_LITE = as.numeric(stringr::str_replace_all(RT_LITE, ",", "."))) %>% 
    dplyr::left_join(socioeconomic_data_population[, 1:3]) %>% 
    dplyr::relocate(NM_UF, .after = 2)
socioeconomic_data_literacy

### years_study ----
socioeconomic_data_years_study <- readxl::read_excel("01_data/03_socioeconomic/socioeconomic_data.xlsx", sheet = 19) %>% 
    dplyr::mutate(YEARS = as.numeric(stringr::str_replace_all(YEARS, ",", "."))) %>% 
    dplyr::left_join(socioeconomic_data_population[, 1:3]) %>% 
    dplyr::relocate(NM_UF, .after = 2)
socioeconomic_data_years_study

# motorized travel time healthcare -------------------------------------

# import
travel_time_healthcare <- terra::rast("01_data/03_socioeconomic/travel_time_healthcare/2020_motorized_travel_time_to_healthcare.geotiff")
travel_time_healthcare

mun <- terra::vect("01_data/05_municipalities/BR_Municipios_2023.shp") %>% 
    terra::project(travel_time_healthcare)
mun

mun_calculated <- dir("01_data/03_socioeconomic/travel_time_healthcare/", pattern = ".csv") %>% 
    stringr::str_replace_all("travel_time_healthcare_mun_mn_data_dcmun", "") %>% 
    stringr::str_replace_all(".csv", "") %>% 
    as.numeric()
mun_calculated

mun_filtered <- mun %>% 
    tidyterra::filter(!CD_MUN %in% mun_calculated)
mun_filtered

# calculate
for(i in 1:nrow(mun_filtered)){
    
    print(i)
    mun_i <- mun_filtered[i,]
    
    travel_time_healthcare_mun <- terra::crop(travel_time_healthcare, mun_i, mask= TRUE)
    terra::writeRaster(travel_time_healthcare_mun, paste0("01_data/03_socioeconomic/travel_time_healthcare/2020_motorized_travel_time_to_healthcare.geotifftravel_time_healthcare_cdmun", mun_i$CD_MUN, ".tif"), overwrite = TRUE)
    
    travel_time_healthcare_mun_mn <- zonal(travel_time_healthcare_mun, mun_i)
    
    travel_time_healthcare_mun_mn_data_i <- tibble::tibble(
        CD_MUN = mun_i$CD_MUN, 
        travel_time_healthcare = as.numeric(travel_time_healthcare_mun_mn))
    readr::write_csv(
        travel_time_healthcare_mun_mn_data_i, 
        paste0("01_data/03_socioeconomic/travel_time_healthcare/travel_time_healthcare_mun_mn_data_dcmun", mun_i$CD_MUN, ".csv"))
    
}

plan(multisession, workers = 10)
travel_time_healthcare_mun_data <- dir("01_data/03_socioeconomic/travel_time_healthcare/", pattern = ".csv", full.names = TRUE) %>% 
    furrr::future_map_dfr(read_csv)
travel_time_healthcare_mun_data

# export data -------------------------------------------------------------

readr::write_csv(basic_care, "03_prioritization/data_mun_socioeconomic_basic_care.csv")
readr::write_csv(cattle_population, "03_prioritization/data_mun_socioeconomic_cattle_population.csv")
readr::write_csv(epi_surv, "03_prioritization/data_mun_socioeconomic_epi_surv.csv")
readr::write_csv(health_exp, "03_prioritization/data_mun_socioeconomic_health_exp.csv")
readr::write_csv(vac_coverage_mean, "03_prioritization/data_mun_socioeconomic_vac_coverage_mean.csv")

readr::write_csv(socioeconomic_data_density, "03_prioritization/data_mun_socioeconomic_data_density.csv")
readr::write_csv(socioeconomic_data_population, "03_prioritization/data_mun_socioeconomic_data_population.csv")
readr::write_csv(socioeconomic_data_poor, "03_prioritization/data_mun_socioeconomic_data_poor.csv")
readr::write_csv(socioeconomic_data_gini, "03_prioritization/data_mun_socioeconomic_data_gini.csv")
readr::write_csv(socioeconomic_data_indigenous, "03_prioritization/data_mun_socioeconomic_data_indigenous.csv")
readr::write_csv(socioeconomic_data_child, "03_prioritization/data_mun_socioeconomic_data_child.csv")
readr::write_csv(socioeconomic_data_idh, "03_prioritization/data_mun_socioeconomic_data_idh.csv")
readr::write_csv(socioeconomic_data_education, "03_prioritization/data_mun_socioeconomic_data_education.csv")
readr::write_csv(socioeconomic_data_literacy, "03_prioritization/data_mun_socioeconomic_data_literacy.csv")
readr::write_csv(socioeconomic_data_years_study, "03_prioritization/data_mun_socioeconomic_data_years_study.csv")

readr::write_csv(travel_time_healthcare_mun_data, "03_prioritization/data_mun_socioeconomic__travel_time_healthcare.csv")

# end ---------------------------------------------------------------------