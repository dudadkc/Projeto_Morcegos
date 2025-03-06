#' ----
#' aim: rabies data
#' author: mauricio vancine
#' date: 2025-03-06
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(janitor)

# download pdf tables ----------------------------------------------------

# download tables
for(i in 1:16){
    
    download.file(url = paste0("https://www.gov.br/saude/pt-br/assuntos/saude-de-a-a-z/r/raiva/arquivos/tabela-", i, ".pdf"),
                  destfile = paste0("01_data/02_rabies/00_raw/tabela_", i, ".pdf"), mode = "wb")
}

# rabies_animals ----------------------------------------------------------

uf_animais_2015 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2015.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(uf_animais_2015)[7] <- "Quiroptéros_Hematófagos"
colnames(uf_animais_2015)[8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2015 <- uf_animais_2015 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2015) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric))
uf_animais_2015

uf_animais_2016 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2016.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(uf_animais_2016)[7] <- "Quiroptéros_Hematófagos"
colnames(uf_animais_2016)[8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2016 <- uf_animais_2016 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2016) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric))
uf_animais_2016

uf_animais_2017 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2017.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(uf_animais_2017)[7] <- "Quiroptéros_Hematófagos"
colnames(uf_animais_2017)[8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2017 <- uf_animais_2017 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2017) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric))
uf_animais_2017

uf_animais_2018 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2018.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(uf_animais_2018)[7] <- "Quiroptéros_Hematófagos"
colnames(uf_animais_2018)[8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2018 <- uf_animais_2018 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2018) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric))
uf_animais_2018

uf_animais_2019 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2019.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(uf_animais_2019)[7] <- "Quiroptéros_Hematófagos"
colnames(uf_animais_2019)[8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2019 <- uf_animais_2019 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2019) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric))
uf_animais_2019

uf_animais_2020 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2020.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(uf_animais_2020)[7] <- "Quiroptéros_Hematófagos"
colnames(uf_animais_2020)[8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2020 <- uf_animais_2020 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2020) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric))
uf_animais_2020

uf_animais_2021 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2021.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(uf_animais_2021)[7] <- "Quiroptéros_Hematófagos"
colnames(uf_animais_2021)[8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2021 <- uf_animais_2021 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2021) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric))
uf_animais_2021

uf_animais_2022 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2022.xlsx") %>%
    dplyr::slice(-c(1, 36:37)) %>% 
    janitor::clean_names()
uf_animais_2022[1, 7] <- "Quiroptéros_Hematófagos"
uf_animais_2022[1, 8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2022 <- uf_animais_2022 %>% 
    setNames(uf_animais_2022[1, ]) %>% 
    dplyr::slice(-c(1:2)) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2022) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric))
uf_animais_2022

uf_animais_2023 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2023.xlsx") %>%
    dplyr::slice(-c(1, 36:37)) %>% 
    janitor::clean_names()
uf_animais_2023[1, 7] <- "Quiroptéros_Hematófagos"
uf_animais_2023[1, 8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2023 <- uf_animais_2023 %>% 
    setNames(uf_animais_2023[1, ]) %>% 
    dplyr::slice(-c(1:2)) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2023) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric)) %>% 
    dplyr::rename(bovina = bovino,
                  equina = equino,
                  suina_caprina_ovina_e_outros_herbivoros = suino_caprino_ovino_e_outros_herb)
uf_animais_2023

uf_animais_2024 <- readxl::read_excel(path = "01_data/02_rabies/01_cleaned/rabies_animals/UF_x_animais_2024.xlsx") %>%
    dplyr::slice(-c(1, 36:37)) %>% 
    janitor::clean_names()
uf_animais_2024[1, 7] <- "Quiroptéros_Hematófagos"
uf_animais_2024[1, 8] <- "Quiroptéros_Não Hematófagos"
uf_animais_2024 <- uf_animais_2024 %>% 
    setNames(uf_animais_2024[1, ]) %>% 
    dplyr::slice(-c(1:2)) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(year = 2024) %>% 
    dplyr::mutate(across(-regiao_uf, as.numeric)) %>% 
    dplyr::rename(bovina = bovino,
                  equina = equino,
                  suina_caprina_ovina_e_outros_herbivoros = suino_caprino_ovino_e_outros_herb)
uf_animais_2024

uf_animais <- dplyr::bind_rows(
    uf_animais_2015, uf_animais_2016, uf_animais_2017, uf_animais_2018, 
    uf_animais_2019, uf_animais_2020, uf_animais_2021, uf_animais_2022,
    uf_animais_2023, uf_animais_2024) %>% 
    dplyr::select(-year) %>% 
    dplyr::group_by(regiao_uf) %>% 
    dplyr::summarise(across(
        .cols = where(is.numeric),
        .fns = list(sum = sum),
        .names = "{.col}"))
uf_animais

readr::write_csv(uf_animais, "01_data/02_rabies/02_summarized/uf_animais.csv")

# rabies_humans -----------------------------------------------------------

rabies_humans_atend <- NULL
rabies_humans_atend_files <- dir("01_data/02_rabies/01_cleaned/rabies_humans/atend_antir", full.names = TRUE)
for(i in 1:length(rabies_humans_atend_files)){
    
    if(i == 1){
        rabies_humans_atend_i <- readr::read_tsv(rabies_humans_atend_files[i]) %>% 
            janitor::clean_names() %>% 
            dplyr::mutate(cs_escol_n = as.numeric(cs_escol_n),
                          dt_notific = as.character(dt_notific))
    }else{
        rabies_humans_atend_i <- readr::read_csv(rabies_humans_atend_files[i]) %>% 
            janitor::clean_names() %>% 
            dplyr::mutate(cs_escol_n = as.numeric(cs_escol_n),
                          dt_notific = as.character(dt_notific))
    }
    rabies_humans_atend <- dplyr::bind_rows(rabies_humans_atend, rabies_humans_atend_i)
}
rabies_humans_atend

# end ---------------------------------------------------------------------
