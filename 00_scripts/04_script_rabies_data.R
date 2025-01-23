#' ----
#' aim: rabies data
#' author: mauricio vancine
#' date: 22/01/2025
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(tidyxl)
library(read.dbc)
library(janitor)

# download pdf tables ----------------------------------------------------

# download tables
for(i in 1:16){
    
    download.file(url = paste0("https://www.gov.br/saude/pt-br/assuntos/saude-de-a-a-z/r/raiva/arquivos/tabela-", i, ".pdf"),
                  destfile = paste0("01_data/03_rabies/00_raw/tabela_", i, ".pdf"), mode = "wb")
}

# read tables
table1 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_1.xlsx", sheet = 2) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(regiao_uf = str_replace_all(regiao_uf, "\\d+", ""))
table1

table2 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_2.xlsx", sheet = 2) %>% 
    janitor::clean_names()
table2

table3_2 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_3.xlsx", sheet = 2) %>% 
    janitor::clean_names()
table3_4 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_3.xlsx", sheet = 4, col_names = FALSE) 
colnames(table3_4) <- colnames(table3_2)
table3 <- rbind(table3_2, table3_4)    
table3

table4 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_4.xlsx", sheet = 2) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(regiao_uf = str_replace_all(regiao_uf, "\\d+", ""))
table4

table5 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_5.xlsx") %>% 
    dplyr::slice(-c(1:2))
table5[1, 2] <- "municipality"
table5 <- table5 %>% 
    dplyr::slice(-1) %>% 
    setNames(table5[1, ]) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(regiao_uf = 1) %>% 
    dplyr::slice(-1) %>% 
    dplyr::select(-starts_with("na")) %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
                  municipality != "-") %>% 
    tidyr::fill(regiao_uf, .direction = "down") %>% 
    dplyr::mutate(across(everything(), ~ str_replace_all(., "-", "0"))) %>% 
    dplyr::mutate(across(starts_with("x"), ~ replace_na(., "0"))) %>% 
    dplyr::mutate(across(starts_with("x"), ~ as.numeric(.)))
table5

table6 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_6.xlsx", sheet = 3) %>% 
    janitor::clean_names() %>% 
    dplyr::rename(regiao_uf = 1) %>% 
    dplyr::slice(-1)
colnames(table6)[2] <- "municipality"
table6 <- table6 %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"),
                  municipality != "-") %>% 
    dplyr::select(1:2, paste0("x", 2015:2024)) %>% 
    tidyr::fill(regiao_uf, .direction = "down") %>% 
    dplyr::mutate(across(everything(), ~ str_replace_all(., "-", "0"))) %>% 
    dplyr::mutate(across(starts_with("x"), ~ replace_na(., "0"))) %>% 
    dplyr::mutate(across(starts_with("x"), ~ as.numeric(.)))
table6

table7 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_7.xlsx") %>%
    dplyr::slice(-c(1, 36:37)) %>% 
    janitor::clean_names()
table7[1, 7] <- "Quiroptéros_Hematófagos"
table7[1, 8] <- "Quiroptéros_Não Hematófagos"
table7 <- table7 %>% 
    setNames(table7[1, ]) %>% 
    dplyr::slice(-c(1:2)) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table7

table8 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_8.xlsx") %>%
    dplyr::slice(-c(1, 36:37)) %>% 
    janitor::clean_names()
table8[1, 7] <- "Quiroptéros_Hematófagos"
table8[1, 8] <- "Quiroptéros_Não Hematófagos"
table8 <- table8 %>% 
    setNames(table8[1, ]) %>% 
    dplyr::slice(-c(1:2)) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table8

table9 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_9.xlsx") %>%
    dplyr::slice(-c(1, 36:37)) %>% 
    janitor::clean_names()
table9[1, 7] <- "Quiroptéros_Hematófagos"
table9[1, 8] <- "Quiroptéros_Não Hematófagos"
table9 <- table9 %>% 
    setNames(table9[1, ]) %>% 
    dplyr::slice(-c(1:2)) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table9

table10 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_10.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(table10)[7] <- "Quiroptéros_Hematófagos"
colnames(table10)[8] <- "Quiroptéros_Não Hematófagos"
table10 <- table10 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table10

table11 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_11.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(table11)[7] <- "Quiroptéros_Hematófagos"
colnames(table11)[8] <- "Quiroptéros_Não Hematófagos"
table11 <- table11 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table11

table12 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_12.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(table12)[7] <- "Quiroptéros_Hematófagos"
colnames(table12)[8] <- "Quiroptéros_Não Hematófagos"
table12 <- table12 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table12

table13 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_13.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(table13)[7] <- "Quiroptéros_Hematófagos"
colnames(table13)[8] <- "Quiroptéros_Não Hematófagos"
table13 <- table13 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table13

table14 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_14.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(table14)[7] <- "Quiroptéros_Hematófagos"
colnames(table14)[8] <- "Quiroptéros_Não Hematófagos"
table14 <- table14 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table14

table15 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_15.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(table15)[7] <- "Quiroptéros_Hematófagos"
colnames(table15)[8] <- "Quiroptéros_Não Hematófagos"
table15 <- table15 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table15

table16 <- readxl::read_excel(path = "01_data/03_rabies/00_raw/tabela_16.xlsx", sheet = 2) %>%
    dplyr::slice(-c(1, 34))
colnames(table16)[7] <- "Quiroptéros_Hematófagos"
colnames(table16)[8] <- "Quiroptéros_Não Hematófagos"
table16 <- table16 %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste"))
table16

# dbc ---------------------------------------------------------------------

read.dbc::read.dbc("01_data/03_rabies/00_raw/dbc/atendimento_antirrabico/ANTRBR06.dbc")