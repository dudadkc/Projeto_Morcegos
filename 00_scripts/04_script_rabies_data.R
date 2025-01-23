#' ----
#' aim: rabies data
#' author: mauricio vancine
#' date: 22/01/2025
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(readxl)
library(janitor)

# download tables ---------------------------------------------------------

# download table
for(i in 1:16){
    
    download.file(url = paste0("https://www.gov.br/saude/pt-br/assuntos/saude-de-a-a-z/r/raiva/arquivos/tabela-", i, ".pdf"),
                    destfile = paste0("01_data/03_rabies/00_raw/tabela_", i, ".pdf"), mode = "wb")
}

# read tables
table_path <- dir(path = "01_data/03_rabies/00_raw", pattern = ".xlsx", full.names = TRUE)
table_path

table_1 <- readxl::read_excel(table_path[1], sheet = 2) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!regiao_uf %in% c("Brasil", "Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")) %>% 
    dplyr::mutate(regiao_uf = str_replace_all(regiao_uf, "\\d+", ""))
table_1
