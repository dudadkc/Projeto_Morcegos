#' ---
#' title: plataform sn - download and integration ocorrencias
#' author: mauricio vancine
#' date: 2024-01-30
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(janitor)
library(jsonlite)
library(lubridate)
library(parallelly)
library(doParallel)
library(foreach)
library(furrr)
library(future)
library(spocc)
library(BIEN)
library(rnaturalearth)
library(spData)
library(sf)
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
    janitor::clean_names() %>% 
    dplyr::pull(species) %>% 
    sort()
fauna_list

flora_list <- readr::read_csv("02_data/00_species_list/flora_mma_2024_fitered.csv") %>% 
    janitor::clean_names() %>% 
    dplyr::pull(species) %>% 
    sort()
flora_list

# download fauna --------------------------------------------------------

# occ
doParallel::registerDoParallel(parallelly::availableCores(omit = 2))
foreach::foreach(i=fauna_list) %dopar% {
    
    ## specieslink ----
    # information
    print("splink")
    
    # download
    occ_splink <- jsonlite::fromJSON(
        paste0("https://specieslink.net/ws/1.0/search?scientificname=", 
               tolower(gsub(" ", "+", i)), 
               "&apikey=aXGEJtnQW12sPuSyKMX7&offset=0&limit=5000"))$features$properties
    
    # conditional without data
    if (length(occ_splink) == 0){
        
        occ_splink_data <- tibble::tibble(species_searched = i,
                                          name = NA,
                                          longitude = NA,
                                          latitude = NA,
                                          prov = "specieslink",
                                          date = NA,
                                          key = NA)
        
        # conditional with data and year
    } else{
        if ("yearcollected" %in% colnames(occ_splink)) {
            occ_splink_data <- occ_splink %>% 
                tidyr::drop_na(decimallongitude, decimallatitude) %>% 
                dplyr::mutate(species_searched = i,
                              name = scientificname,
                              longitude = as.numeric(decimallongitude),
                              latitude = as.numeric(decimallatitude),
                              prov = "specieslink",
                              date = as.numeric(yearcollected),
                              key = as.character(catalognumber)) %>% 
                dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
            
        } else {
            # Se 'yearcollected' não estiver presente, faça algo apropriado (crie uma coluna de data vazia, por exemplo)
            occ_splink_data <- occ_splink %>% 
                tidyr::drop_na(decimallongitude, decimallatitude) %>% 
                dplyr::mutate(species_searched = i,
                              name = scientificname,
                              longitude = as.numeric(decimallongitude),
                              latitude = as.numeric(decimallatitude),
                              prov = "specieslink",
                              date = NA,
                              key = as.character(catalognumber)) %>% 
                dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        }
    }
    
    
    # spocc ----
    # information
    print("spocc")
    
    # download
    occ_spocc <- spocc::occ(query = i, 
                            from = c("gbif", "inat", "vertnet", "idigbio", "ecoengine"),
                            has_coords = TRUE,
                            limit = 1e6,
                            throw_warnings = FALSE)
    
    # data
    occ_spocc_data <- spocc::occ2df(occ_spocc)
    
    # conditional without data
    if(nrow(occ_spocc_data) == 0){
        
        occ_spocc_data <- tibble::tibble(species_searched = i,
                                         name = NA,
                                         longitude = NA,
                                         latitude = NA,
                                         prov = "spocc",
                                         date = NA,
                                         key = NA)
        
        # conditional without year  
    } else if(!"date" %in% colnames(occ_spocc_data)){
        
        occ_spocc_data <- occ_spocc_data %>% 
            dplyr::mutate(species_searched = i, .before = 1) %>% 
            dplyr::mutate(longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          date = NA,
                          key = as.character(key)) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
        # conditional with data and year
    } else{
        
        occ_spocc_data <- occ_spocc_data %>% 
            dplyr::mutate(species_searched = i, .before = 1) %>% 
            dplyr::mutate(longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          date = lubridate::year(occ_spocc_data$date),
                          key = as.character(key)) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
    }
    
    # combine data ----
    occ_data <- dplyr::bind_rows(occ_splink_data, occ_spocc_data)
    
    # export 
    readr::write_csv(occ_data, 
                     paste0("02_data/01_occurrences/01_raw/01_spocc_specieslink/01_fauna/occ_raw_splink_spocc_fauna_", sub(" ", "_", tolower(i)), ".csv"))
    
}
doParallel::stopImplicitCluster()

# download flora --------------------------------------------------------

# occ
doParallel::registerDoParallel(parallelly::availableCores(omit = 2))
foreach::foreach(i=flora_list) %dopar% {
    
    ## specieslink ----
    # information
    print("splink")
    
    # download
    occ_splink <- jsonlite::fromJSON(
        paste0("https://specieslink.net/ws/1.0/search?scientificname=", 
               tolower(gsub(" ", "+", paste0(stringr::str_split(i, " ", simplify = TRUE)[, 1:2], collapse = " "))), 
               "&apikey=aXGEJtnQW12sPuSyKMX7&offset=0&limit=5000"))$features$properties
    
    # conditional without data
    if(length(occ_splink) == 0){
        
        occ_splink_data <- tibble::tibble(species_searched = i,
                                          name = NA,
                                          longitude = NA,
                                          latitude = NA,
                                          prov = "specieslink",
                                          date = NA,
                                          key = NA)
        
        
        
        # conditional with data and year and key
    } else if("yearcollected" %in% colnames(occ_splink) & "catalognumber" %in% colnames(occ_splink)) {
        occ_splink_data <- occ_splink %>% 
            tidyr::drop_na(decimallongitude, decimallatitude) %>% 
            dplyr::mutate(species_searched = i,
                          name = scientificname,
                          longitude = as.numeric(decimallongitude),
                          latitude = as.numeric(decimallatitude),
                          prov = "specieslink",
                          date = as.numeric(yearcollected),
                          key = as.character(catalognumber)) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
        
        # conditional with data and year and not key
    } else if("yearcollected" %in% colnames(occ_splink) & !"catalognumber" %in% colnames(occ_splink)) {
        occ_splink_data <- occ_splink %>% 
            tidyr::drop_na(decimallongitude, decimallatitude) %>% 
            dplyr::mutate(species_searched = i,
                          name = scientificname,
                          longitude = as.numeric(decimallongitude),
                          latitude = as.numeric(decimallatitude),
                          prov = "specieslink",
                          date = as.numeric(yearcollected),
                          key = NA) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
        
        # conditional with data and not year and key
    } else if(!"yearcollected" %in% colnames(occ_splink) & "catalognumber" %in% colnames(occ_splink)) {
        occ_splink_data <- occ_splink %>% 
            tidyr::drop_na(decimallongitude, decimallatitude) %>% 
            dplyr::mutate(species_searched = i,
                          name = scientificname,
                          longitude = as.numeric(decimallongitude),
                          latitude = as.numeric(decimallatitude),
                          prov = "specieslink",
                          date = NA,
                          key = as.character(catalognumber)) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
        
        # conditional with data and not year and not key
    } else if(!"yearcollected" %in% colnames(occ_splink) & !"catalognumber" %in% colnames(occ_splink)) {
        occ_splink_data <- occ_splink %>% 
            tidyr::drop_na(decimallongitude, decimallatitude) %>% 
            dplyr::mutate(species_searched = i,
                          name = scientificname,
                          longitude = as.numeric(decimallongitude),
                          latitude = as.numeric(decimallatitude),
                          prov = "specieslink",
                          date = NA,
                          key = NA) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
    }
    
    
    
    # spocc ----
    # information
    print("spocc")
    
    # download
    occ_spocc <- spocc::occ(query = i, 
                            from = c("gbif", "inat", "vertnet", "idigbio", "ecoengine"),
                            has_coords = TRUE,
                            limit = 1e6,
                            throw_warnings = FALSE)
    
    # data
    occ_spocc_data <- spocc::occ2df(occ_spocc)
    
    # conditional without data
    if(nrow(occ_spocc_data) == 0){
        
        occ_spocc_data <- tibble::tibble(species_searched = i,
                                         name = NA,
                                         longitude = NA,
                                         latitude = NA,
                                         prov = "spocc",
                                         date = NA,
                                         key = NA)
        
        # conditional without year  
    } else if(!"date" %in% colnames(occ_spocc_data)){
        
        occ_spocc_data <- occ_spocc_data %>% 
            dplyr::mutate(species_searched = i, .before = 1) %>% 
            dplyr::mutate(longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          date = NA,
                          key = as.character(key)) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
        # conditional with data and year
    } else{
        
        occ_spocc_data <- occ_spocc_data %>% 
            dplyr::mutate(species_searched = i, .before = 1) %>% 
            dplyr::mutate(longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          date = lubridate::year(occ_spocc_data$date),
                          key = as.character(key)) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
    }
    
    ## bien ----
    
    # download
    occ_bien_data <- BIEN::BIEN_occurrence_species(species = i)
    
    # conditional without data
    if(nrow(occ_bien_data) == 0){
        
        occ_bien_data <- tibble::tibble(species_searched = i,
                                        name = NA,
                                        longitude = NA,
                                        latitude = NA,
                                        prov = paste0("bien"),
                                        date = NA,
                                        key = NA)
        
        # conditional without year  
    } else if(!"date_collected" %in% colnames(occ_bien_data)){
        
        occ_bien_data <- occ_bien_data %>% 
            dplyr::mutate(species_searched = i, .before = 1) %>% 
            dplyr::mutate(name = scrubbed_species_binomial,
                          longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          prov = paste0("bien_", datasource),
                          date = NA,
                          key = as.character(datasource_id)) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
        # conditional with data and year
    } else{
        
        occ_bien_data <- occ_bien_data %>% 
            dplyr::mutate(species_searched = i, .before = 1) %>% 
            dplyr::mutate(name = scrubbed_species_binomial, 
                          longitude = as.numeric(longitude),
                          latitude = as.numeric(latitude),
                          prov = paste0("bien_", datasource),
                          date = lubridate::year(occ_bien_data$date_collected),
                          key = as.character(datasource_id)) %>% 
            dplyr::select(species_searched, name, longitude, latitude, prov, date, key)
        
    }
    
    # combine data ----
    occ_data <- dplyr::bind_rows(occ_splink_data, occ_spocc_data, occ_bien_data)
    
    # export 
    readr::write_csv(occ_data, 
                     paste0("02_data/01_occurrences/01_raw/01_spocc_specieslink/02_flora/occ_raw_splink_spocc_bien_flora_", sub(" ", "_", tolower(i)), ".csv"))
    
}
doParallel::stopImplicitCluster()

# integrated --------------------------------------------------------------

# import
plan(multisession, workers = parallelly::availableCores(omit = 2))
occ_data_fauna <- dir(path = "02_data/01_occurrences/01_raw/01_spocc_specieslink/01_fauna", 
                pattern = ".csv", full.names = TRUE) %>% 
    furrr::future_map_dfr(readr::read_csv, col_types = "ccddcdc")
occ_data_fauna

# vector
occ_data_fauna_v <- occ_data_fauna %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    dplyr::filter(lon > -180 & lon < 180) %>% 
    dplyr::filter(lat > -90 & lat < 90) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_fauna_v

# map
tm_shape(li) +
    tm_polygons() +
    tm_shape(occ_data_fauna_v) +
    tm_bubbles(size = .2, 
               col = "species_searched", 
               col.scale = tm_scale_categorical(values = "viridis"),
               col.legend = tm_legend_hide())

# export
readr::write_csv(occ_data_fauna, "02_data/01_occurrences/02_integrated/occ_raw_splink_spocc_fauna.csv")

# import
plan(multisession, workers = parallelly::availableCores(omit = 2))
occ_data_flora <- dir(path = "02_data/01_occurrences/01_raw/01_spocc_specieslink/02_flora", 
                pattern = ".csv", full.names = TRUE) %>% 
    furrr::future_map_dfr(readr::read_csv, col_types = "ccddcdc")
occ_data_flora

# vector
occ_data_flora_v <- occ_data_flora %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::mutate(lon = longitude,
                  lat = latitude) %>% 
    dplyr::filter(lon > -180 & lon < 180) %>% 
    dplyr::filter(lat > -90 & lat < 90) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_data_flora_v

# map
tm_shape(li) +
    tm_polygons() +
    tm_shape(occ_data_flora_v) +
    tm_bubbles(size = .2, 
               col = "species_searched", 
               col.scale = tm_scale_categorical(values = "viridis"),
               col.legend = tm_legend_hide())

# export
readr::write_csv(occ_data_flora, "02_data/01_occurrences/02_integrated/occ_raw_splink_spocc_bien_flora.csv")

# end ---------------------------------------------------------------------