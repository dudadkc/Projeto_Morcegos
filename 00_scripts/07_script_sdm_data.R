#' ----
#' aim: spatial joint
#' author: mauricio vancine
#' date: 27/01/2025
#' ----

# prepare r ---------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(sf)
library(tmap)

# import data -------------------------------------------------------------

## municipalities ----
mun <- terra::vect("01_data/05_municipalities/BR_Municipios_2023.shp")
mun

plot(mun)

## sdms ----
sdm_cont <- terra::rast("02_results/03_01_sdms_v12/03_05_pred.tif")
sdm_cont

sdm_bin <- terra::rast("02_results/03_01_sdms_v12/03_05_pred_thr_spec_sens.tif")
sdm_bin

## zonal ----
sdm_data <- NULL
for(i in 1:nrow(mun)){

    print(i)
    mun_i <- mun[i,]
    sdm_data_i <- tibble::tibble(
        CD_MUN = mun_i$CD_MUN,
        sdm_cont_mn = as.vector(round(terra::zonal(sdm_cont, mun_i, fun = "mean", na.rm = TRUE), 2))$maxent,
        sdm_cont_md = as.vector(round(terra::zonal(sdm_cont, mun_i, fun = "median", na.rm = TRUE), 2))$maxent,
        sdm_cont_sd = as.vector(round(terra::zonal(sdm_cont, mun_i, fun = "sd", na.rm = TRUE), 2))$maxent,
        sdm_bin_mn = as.vector(round(terra::zonal(sdm_bin, mun_i, na.rm = TRUE), 2))$maxent)
    
    sdm_data <- rbind(sdm_data, sdm_data_i)
    
}
sdm_data

# export ------------------------------------------------------------------

readr::write_csv(sdm_data, "03_priorizarion/data_mun_sdm.csv")

# end ---------------------------------------------------------------------
