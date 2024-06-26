#' ---
#' title: plataform sn - ocorrencias - data papers
#' author: mauricio vancine
#' date: 2024-01-30
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(janitor)
library(tmap)

# options
options(timeout = 3e5)

# import data -------------------------------------------------------------

# import south america limite
li <- spData::world
li

tm_shape(li) +
    tm_polygons()

# species
fauna_list <- readr::read_csv("02_data/00_species_list/fauna_mma_2024_fitered.csv") %>% 
    dplyr::pull(species) %>% 
    sort()
fauna_list

flora_list <- readr::read_csv("02_data/00_species_list/flora_mma_2024_fitered.csv") %>% 
    dplyr::pull(species) %>% 
    sort()
flora_list

# download ----------------------------------------------------------------

## atlantic amphibians ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2392&file=ecy2392-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2392-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
amp_sites <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_AMPHIBIANS_sites.csv") %>% 
    dplyr::select(id, longitude, latitude, year_finish)
amp_sites

amp_species <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_AMPHIBIANS_species.csv") %>% 
    tidyr::drop_na(valid_name) %>% 
    dplyr::select(id, valid_name)
amp_species

amp_occ <- amp_species %>% 
    dplyr::left_join(amp_sites) %>% 
    dplyr::mutate(species = valid_name,
                  source = "atlantic_amphibians",
                  year = year_finish) %>%  
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
amp_occ

## atlantic bats ----

# download data
download.file(url = "https://github.com/LEEClab/Atlantic_series/blob/master/ATLANTIC_BATS/DATASET/2018_02_d21/ATLANTIC_BATS_2020_comp.xlsx?raw=true",
              destfile = "02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_BATS_2019_comp.xlsx", mode = "wb")

# download data
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2007-sup-0002-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import data
ba_occ <- readxl::read_xlsx("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_BATS_2019_comp.xlsx") %>% 
    dplyr::select(1, 6, 7, 22, Precision, Anoura.caudifer:Micronycteris.sp.) %>% 
    dplyr::rename(id = ID...1) %>% 
    tidyr::pivot_longer(cols = Anoura.caudifer:Micronycteris.sp.,
                        names_to = "name",
                        values_to = "abundance") %>% 
    dplyr::filter(abundance > 0,
                  Precision == "Precise") %>% 
    dplyr::mutate(species = str_replace(name, "[.]", " "),
                  longitude = as.numeric(sub(",", ".", Longitude)),
                  latitude = as.numeric(sub(",", ".", Latitude)),
                  year = as.numeric(Year_finish),
                  source = "atlantic_bats") %>%
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_replace(species, "cf..", "cf. ")) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
ba_occ

## atlantic camtrap ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1998&file=ecy1998-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy1998-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import sites
ca_si <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_CAMTRAPS_1-0_LOCATION.csv") %>% 
    dplyr::select(location_id, X, Y)
ca_si

# import sites names
ca_si_na <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_CAMTRAPS_1-0_SURVEY.csv") %>% 
    dplyr::left_join(., ca_si, by = "location_id") %>% 
    dplyr::select(location_id, survey_id, yearfinish,  X, Y)
ca_si_na

# import species names
ca_sp_na <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_CAMTRAPS_1-0_SPECIES.csv") %>% 
    dplyr::select(order:genus, species_name, species_code)
ca_sp_na

# import species
ca_sp <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_CAMTRAPS_1-0_RECORDS.csv") %>% 
    dplyr::select(survey_id, species_code, presence_absence) %>% 
    dplyr::filter(presence_absence == 1) %>% 
    dplyr::mutate(species_code = ifelse(species_code == "Potu_flav", "Poto_flav", species_code)) %>% 
    dplyr::left_join(., ca_sp_na, by = "species_code") %>% 
    dplyr::select(-c(presence_absence,  species_code))
ca_sp

# join data
ca_occ <- ca_sp %>% 
    dplyr::left_join(ca_si_na, by = "survey_id") %>% 
    dplyr::mutate(species = species_name,
                  longitude = as.numeric(X), 
                  latitude = as.numeric(Y), 
                  year = as.numeric(yearfinish),
                  source = "atlantic_camtrap") %>%
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
ca_occ

## atlantic mammals ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2785&file=ecy2785-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2785-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import data
lm_occ <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_MAMMAL_MID_LARGE _assemblages_and_sites.csv") %>% 
    dplyr::mutate(species = Actual_species_Name,
                  longitude = as.numeric(Longitude), 
                  latitude = as.numeric(Latitude), 
                  year = as.numeric(Year_finish),
                  source = "atlantic_large_mammals") %>%
    dplyr::select(species, longitude, latitude, year, source, Precision) %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list,
                  Precision == "precise") %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(-Precision)
lm_occ

## atlantic primates ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2525&file=ecy2525-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2525-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import communities
pr_co <- read.csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC-PR_Community.csv", sep = ";") %>%
    tibble::as_tibble() %>% 
    dplyr::mutate(source = "atlantic_primates") %>% 
    dplyr::filter(PRECISION == "Precise") %>% 
    dplyr::select(SPECIES, LONGITUDE_X, LATITUDE_Y, COL_END_YR, source)
pr_co

# import quantitative
pr_qu <- read.csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC-PR_Quantitative.csv", sep = ";") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(source = "atlantic_primates") %>% 
    dplyr::filter(PRECISION == "Precise") %>% 
    dplyr::select(SPECIES, LONGITUDE_X, LATITUDE_Y, COL_END_YR, source)
pr_qu

# import occurrences
pr_oc <- read.csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC-PR_Occurrence.csv", sep = ";") %>%
    tibble::as_tibble() %>% 
    dplyr::mutate(source = "atlantic_primates") %>% 
    dplyr::filter(PRECISION == "Precise") %>% 
    dplyr::select(SPECIES, LONGITUDE_X, LATITUDE_Y, COL_END_YR, source)
pr_oc

# bind data
pr_occ <- dplyr::bind_rows(pr_co, pr_qu, pr_oc) %>% 
    dplyr::mutate(species = SPECIES,
                  genus = str_split(SPECIES, " ", simplify = TRUE)[, 1],
                  name = SPECIES, 
                  longitude = as.numeric(LONGITUDE_X), 
                  latitude = as.numeric(LATITUDE_Y), 
                  year = as.numeric(COL_END_YR)) %>%
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
pr_occ

## atlantic small mammals ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1893&file=ecy1893-sup-0002-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy1893-sup-0002-datas1.zip",
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import sites
sm_occ_si <- data.table::fread("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_SM_Study_Site.csv") %>% 
    dplyr::rename(ID = `ID\xa0`) %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(Precision == "Precise") %>% 
    dplyr::select(ID, Reference_number, Latitude, Longitude) %>% 
    dplyr::mutate(id = paste(ID, sub(" / ", "_", Reference_number), sep = "_")) %>% 
    dplyr::select(id, Latitude, Longitude)
sm_occ_si

# import species
sm_occ_sp <- data.table::fread("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_SM_Capture.csv") %>%
    dplyr::rename(ID = `ID\xa0`) %>% 
    tibble::as_tibble() %>%
    dplyr::select(ID, Reference_number, Order, Genus, Actual_species_name, Year_finish) %>% 
    dplyr::mutate(id = paste(ID, sub(" / ", "_", Reference_number), sep = "_")) %>% 
    dplyr::select(id, Order, Genus, Actual_species_name, Year_finish)
sm_occ_sp

# join data
sm_occ <- sm_occ_sp %>% 
    dplyr::left_join(sm_occ_si, by = "id") %>%
    dplyr::mutate(order = Order, 
                  genus = str_split(Actual_species_name, " ", simplify = TRUE)[, 1],
                  species = Actual_species_name,
                  longitude = as.numeric(Longitude), 
                  latitude = as.numeric(Latitude), 
                  year = as.numeric(Year_finish),
                  source = "atlantic_small_mammals") %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
sm_occ

## atlantic small mammals abundance ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2005&file=ecy2005-sup-0001-datas1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2005-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import sites
sm_ab_si <- data.table::fread("02_data/01_occurrences/01_raw/04_data_papers/Localities.csv") %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(Study_year = as.numeric(ifelse(str_length(Study_year) > 4, 
                                                 stringr::str_sub(Study_year, -4, -1),
                                                 Study_year))) %>% 
    dplyr::select(SampleID, Latitude, Longitude, Study_year)
sm_ab_si

# import species
sm_ab_sp <- data.table::fread("02_data/01_occurrences/01_raw/04_data_papers/Mammal_Communities.csv") %>% 
    tibble::as_tibble() %>% 
    dplyr::select(SampleID, Valid_Species) %>% 
    dplyr::filter(Valid_Species != "")
sm_ab_sp

# join data
sm_ab_occ <- dplyr::left_join(sm_ab_sp, sm_ab_si, by = "SampleID") %>% 
    dplyr::mutate(species = Valid_Species, 
                  genus = str_split(Valid_Species, " ", simplify = TRUE)[, 1],
                  name = Valid_Species, 
                  longitude = Longitude, 
                  latitude = Latitude, 
                  year = as.numeric(Study_year),
                  source = "atlantic_small_mammals_abu") %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
sm_ab_occ

## atlantic mammals uprb ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2107&file=ecy2107-sup-0002-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2107-sup-0002-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import sites
muprb_si <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/Mammals_UPRB_study_sites.csv")
muprb_si

muprb_sp <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/Mammals_UPRB_species.csv") %>% 
    dplyr::select(-c(order:genus, frequency_of_occurrence:Brazilian_status)) %>% 
    t() %>% 
    as.data.frame()
colnames(muprb_sp) <- muprb_sp[1, ]
muprb_sp <- muprb_sp[-1, ]
muprb_sp$site <- rownames(muprb_sp)
muprb_sp <- tidyr::pivot_longer(muprb_sp, cols = -site) %>% 
    dplyr::filter(value > 0) %>% 
    dplyr::select(-value) %>% 
    dplyr::mutate(site = as.numeric(stringr::str_replace_all(site, "site_", "")))
muprb_sp

# join data
muprb_occ <- dplyr::left_join(muprb_sp, muprb_si) %>% 
    dplyr::mutate(species = name, 
                  year = as.numeric(year_finish),
                  source = "atlantic_mammal_uprb") %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
muprb_occ

## atlantic mammal traits ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2106&file=ecy2106-sup-0002-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2106-sup-0002-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
mt_occ <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_TR_all_data.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(species = binomial,
                  source = "atlantic_mammal_traits") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
mt_occ

## atlantic birds ----

# download
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2119&file=ecy2119-sup-0002-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2119-sup-0002-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
bird_qual <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_BIRDS_qualitative.csv") %>%
    janitor::clean_names() %>% 
    dplyr::filter(category_coord %in% c("PTCENTRAL", "AREAq", "MOS", "PRK", "RES", "STM")) %>% 
    dplyr::rename(longitude = longitude_x, latitude = latitude_y) %>% 
    dplyr::select(species, longitude, latitude, year) %>% 
    dplyr::mutate(source = "atlantic_birds")
bird_qual

bird_quan <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_BIRDS_quantitative.csv") %>%
    janitor::clean_names() %>% 
    dplyr::rename(longitude = longitude_x, latitude = latitude_y, year = year_finish) %>% 
    dplyr::select(species, longitude, latitude, year) %>%
    dplyr::mutate(source = "atlantic_birds")
bird_quan

# bind data
bird_occ <- dplyr::bind_rows(bird_qual, bird_quan) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
bird_occ

## atlantic bird traits ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2647&file=ecy2647-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2647-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
bt_occ <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_BIRD_TRAITS_completed_2018_11_d05.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(species = binomial,
                  longitude = longitude_decimal_degrees,
                  latitude = latitude_decimal_degrees, 
                  source = "atlantic_bird_traits") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
bt_occ

## atlantic pollination ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.3595&file=ecy3595-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy3595-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
po_fauna_occ <- readr::read_delim("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC-floverint_int.csv", delim = ";") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(species = paste0(genera_floralvisitor_ver, " ", sp_floralvisitor_ver),
                  year = NA,
                  source = "atlantic_pollination") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
po_fauna_occ

po_flora_occ <- readr::read_delim("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC-floverint_int.csv", delim = ";") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate(species = paste0(genera_plan_ver, " ", sp_plant_ver),
                  year = NA,
                  source = "atlantic_pollination") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% flora_list) %>% 
    tidyr::drop_na(longitude, latitude)
po_flora_occ

## atlantic frugivory ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.1818&file=ecy1818-sup-0002-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy1818-sup-0002-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
fru_fauna_occ <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_frugivory.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::filter(precision == "preciso") %>% 
    dplyr::mutate(species = frugivore_species,
                  year = NA,
                  source = "atlantic_frugivory") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
fru_fauna_occ

fru_flora_occ <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_frugivory.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::filter(precision == "preciso") %>% 
    dplyr::mutate(species = plant_species,
                  year = NA,
                  source = "atlantic_frugivory") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% flora_list) %>% 
    tidyr::drop_na(longitude, latitude)
fru_flora_occ

## atlantic ants ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.3580&file=ecy3580-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy3580-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
ant_occ <- readr::read_delim("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_ANTS_dataset.txt", delim = "\t") %>%
    janitor::clean_names() %>% 
    dplyr::filter(precision_meters %in% c("precise", as.character(1:100))) %>% 
    dplyr::mutate(species = paste0(genus, " ", species),
                  longitude = longitude_x, 
                  latitude = latitude_y,
                  year = end_year,
                  source = "atlantic_ants") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
ant_occ

## atlantic flower–invertebrate interactions ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.3900&file=ecy3900-sup-0001-Data_S1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy3900-sup-0001-data_s1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
flo_inv_occ_fauna <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/AtlanticForestInvertFloInteractionData_2022-07.csv") %>%
    janitor::clean_names() %>% 
    dplyr::filter(precision %in% c(c("10 m (GPS coordinate)", 
                                     "11 m (GPS coordinate)",
                                     "25 (around the coordinate point)",
                                     "forest patch", "forest trail",
                                     "GPS coordinate", "grassland patch",
                                     "university campus"), 
                                   as.character(1:100))) %>% 
    dplyr::mutate(species = invertebrate_species,
                  longitude = longitude_x, 
                  latitude = latitude_y,
                  year = campain_year_finish,
                  source = "atlantic_flower_invertebrate") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
flo_inv_occ_fauna

flo_inv_occ_flora <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/AtlanticForestInvertFloInteractionData_2022-07.csv") %>%
    janitor::clean_names() %>% 
    dplyr::filter(precision %in% c(c("10 m (GPS coordinate)", 
                                     "11 m (GPS coordinate)",
                                     "25 (around the coordinate point)",
                                     "forest patch", "forest trail",
                                     "GPS coordinate", "grassland patch",
                                     "university campus"), 
                                   as.character(1:100))) %>% 
    dplyr::mutate(species = plant_species,
                  longitude = longitude_x, 
                  latitude = latitude_y,
                  year = campain_year_finish,
                  source = "atlantic_flower_invertebrate") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% flora_list) %>% 
    tidyr::drop_na(longitude, latitude)
flo_inv_occ_flora

## atlantic butterflies ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2507&file=ecy2507-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2507-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
atlantic_butterflies_sites <- readr::read_delim("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_BUTTERFLIES_sites.csv", delim = ";") %>%
    janitor::clean_names() %>% 
    dplyr::filter(precision %in% c(as.character(1:100)))
atlantic_butterflies_sites

atlantic_butterflies_species <- readr::read_csv2("02_data/01_occurrences/01_raw/04_data_papers/ATLANTIC_BUTTERFLIES_species.csv") %>%
    janitor::clean_names() %>% 
    dplyr::mutate(species = stringr::str_replace_all(species, "_", " "))
atlantic_butterflies_species

but_occ <- atlantic_butterflies_species %>%
    dplyr::left_join(atlantic_butterflies_sites) %>% 
    dplyr::mutate(year = NA,
                  source = "atlantic_butterflies") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
but_occ

## atlantic epiphytes ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2541&file=ecy2541-sup-0002-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2541-sup-0002-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
epi_occ <- readr::read_delim("02_data/01_occurrences/01_raw/04_data_papers/DataS1_Occurrence.txt", delim = "\t") %>%
    dplyr::filter(PRECISION %in% c("Central point in study site", 
                                   "Forest fragment", "Forest fragment edge",
                                   "Google Earth", "Plot", "Sampled tree", "Study site",
                                   as.character(0:100))) %>% 
    dplyr::mutate(species = EPIPHYTE_SPECIES,
                  longitude = LONGITUDE_X, 
                  latitude = LATITUDE_Y,
                  year = as.numeric(YEAR_FINISH),
                  source = "atlantic_epiphytes") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% flora_list) %>% 
    tidyr::drop_na(longitude, latitude)
epi_occ

epi_abu <- readr::read_delim("02_data/01_occurrences/01_raw/04_data_papers/DataS1_Abundance.txt", delim = "\t") %>%
    dplyr::filter(PRECISION %in% c("Central point in study site", 
                                   "Forest fragment", "Forest fragment edge",
                                   "Google Earth", "Plot", "Sampled tree", "Study site",
                                   as.character(0:100))) %>% 
    dplyr::mutate(species = EPIPHYTE_SPECIES,
                  longitude = LONGITUDE_X, 
                  latitude = LATITUDE_Y,
                  year = as.numeric(YEAR_FINISH),
                  source = "atlantic_epiphytes") %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% flora_list) %>% 
    tidyr::drop_na(longitude, latitude)
epi_abu

epi_occ <- dplyr::bind_rows(epi_occ, epi_abu)
epi_occ

## brazil isotope ----

# download data
"https://zenodo.org/api/records/5929839/files-archive"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/5929839.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
bi_occ <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/SIA_BRA_1.0.csv") %>% 
    dplyr::mutate(longitude = long, 
                  latitude = lat,
                  year = as.numeric(year),
                  source = "brasil_isotopes") %>%
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
bi_occ

## brazil roadkill ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2464&file=ecy2464-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy2464-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

br_occ <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/Brazil_Roadkill_20180527.csv") %>% 
    dplyr::mutate(species = Scientific_name, 
                  longitude = Long, 
                  latitude = Lat,
                  year = Year,
                  source = "brasil_roadkill") %>%
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
br_occ

## neotropical carnivores ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.3128&file=ecy3128-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy3128-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
neo_car_occ <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/NEOTROPICAL_CARNIVORES_DATASET_2020-04.csv") %>% 
    dplyr::filter(PRECISION_m %in% c(as.character(0:100))) %>% 
    dplyr::mutate(species = SPECIES,
                  name = SPECIES, 
                  longitude = LONG_X, 
                  latitude = LAT_Y,
                  year = as.numeric(COL_END_YR),
                  source = "neotropical_carnivores") %>%
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
neo_car_occ

## neotropical xenarthrans ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2663&file=ecy2663-sup-0001-DataS1.rar"

# unrar
# system("unrar e 02_data/01_occurrences/01_raw/04_data_papers/ecy2663-sup-0001-datas1.rar 02_data/01_occurrences/01_raw/04_data_papers/")

# import
neo_xen_qual <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/NEOTROPICAL_XENARTHRANS_QUALITATIVE.csv") %>%
    dplyr::filter(PRECISION %in% c(as.character(0:100))) %>%
    dplyr::select(ORDER, FAMILY, GENUS, SPECIES, LONG_X, LAT_Y, COL_END_YR) %>% 
    dplyr::mutate(source = "neotropical_xenarthrans")
neo_xen_qual

neo_xen_quan <- readr::read_csv("02_data/01_occurrences/01_raw/04_data_papers/NEOTROPICAL_XENARTHRANS_QUANTITATIVE.csv") %>%
    dplyr::filter(PRECISION %in% c(as.character(0:100))) %>%
    dplyr::select(ORDER, FAMILY, GENUS, SPECIES, LONG_X, LAT_Y, COL_END_YR) %>% 
    dplyr::mutate(source = "neotropical_xenarthrans")
neo_xen_quan

# bind data
neo_xen_occ <- dplyr::bind_rows(neo_xen_qual, neo_xen_quan) %>% 
    dplyr::mutate(order = ORDER, 
                  family = FAMILY, 
                  genus = GENUS, 
                  species = SPECIES,
                  name = SPECIES, 
                  longitude = LONG_X, 
                  latitude = LAT_Y,
                  year = as.numeric(ifelse(COL_END_YR == 0, NA, COL_END_YR))) %>%
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
neo_xen_occ

## neotropical alien mammals ----

# download data
"https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.3115&file=ecy3115-sup-0001-DataS1.zip"

# unzip
unzip(zipfile = "02_data/01_occurrences/01_raw/04_data_papers/ecy3115-sup-0001-datas1.zip", 
      exdir = "02_data/01_occurrences/01_raw/04_data_papers")

# import
neo_alien_occ <- readr::read_delim("02_data/01_occurrences/01_raw/04_data_papers/DataS1/NEOTROPICAL_ALIEN_MAMMALS_OCCURENCE.csv", delim = ";") %>% 
    dplyr::filter(PRECISION %in% c(as.character(0:100))) %>%
    dplyr::mutate(species = SPECIES,
                  longitude = LONG_X, 
                  latitude = LAT_Y,
                  year = as.numeric(RECORD_YEAR),
                  source = "neotropical_alien_mammals") %>%
    dplyr::select(species, longitude, latitude, year, source) %>% 
    dplyr::mutate(species = str_trim(species)) %>% 
    dplyr::filter(species %in% fauna_list) %>% 
    tidyr::drop_na(longitude, latitude)
neo_alien_occ

## combine ----

### fauna ----
occ_data_papers_fauna <- dplyr::bind_rows(amp_occ, ba_occ, ca_occ, lm_occ,
                                          pr_occ, sm_occ, sm_ab_occ, muprb_occ, 
                                          mt_occ, bird_occ, bt_occ, po_fauna_occ,
                                          fru_fauna_occ, ant_occ, but_occ, bi_occ, 
                                          br_occ, neo_car_occ, neo_xen_occ, 
                                          neo_alien_occ, flo_inv_occ_fauna)
occ_data_papers_fauna

occ_data_papers_fauna %>% 
    count(species, sort = TRUE)

occ_data_papers_fauna_v <- sf::st_as_sf(occ_data_papers_fauna, coords = c("longitude", "latitude"), crs = 4326)
occ_data_papers_fauna_v

tm_shape(li[li$iso_a2 == "BR", ]) +
    tm_polygons() +
    tm_shape(occ_data_papers_fauna_v) +
    tm_dots()

### flora ----   
occ_data_papers_flora <- dplyr::bind_rows(po_flora_occ, fru_flora_occ, epi_occ, flo_inv_occ_flora)
occ_data_papers_flora

occ_data_papers_flora_v <- sf::st_as_sf(occ_data_papers_flora, coords = c("longitude", "latitude"), crs = 4326)
occ_data_papers_flora_v

tm_shape(li[li$iso_a2 == "BR", ]) +
    tm_polygons() +
    tm_shape(occ_data_papers_flora_v) +
    tm_dots()

## export ----
readr::write_csv(occ_data_papers_fauna, "02_data/01_occurrences/02_integrated/occ_raw_data_paper_fauna.csv")
readr::write_csv(occ_data_papers_flora, "02_data/01_occurrences/02_integrated/occ_raw_data_paper_flora.csv")

# end ---------------------------------------------------------------------