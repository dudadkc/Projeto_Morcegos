#' ---
#' title: sdm - evaluation
#' author: mauricio vancine
#' date: 2024-04-08
#' ---

# prepare r -------------------------------------------------------------

# packages
library(tidyverse)
library(terra)
library(dismo)
library(ecospat)

# import ------------------------------------------------------------------

data_ev <- NULL

for(i in c(2:12)){
    
    print(i)
    
    # occ
    occ <- readxl::read_excel(paste0("02_results/03_01_sdms_v", ifelse(i < 10, paste0("0", i), i), "/01_occs.xlsx")) %>% 
        dplyr::select(longitude, latitude)
    occ
    
    # bg
    bg <- readxl::read_excel(paste0("02_results/03_01_sdms_v", ifelse(i < 10, paste0("0", i), i), "/01_bg.xlsx")) %>% 
        dplyr::select(longitude, latitude)
    bg
    
    # sdm
    sdm <- terra::rast(paste0("02_results/03_01_sdms_v", ifelse(i < 10, paste0("0", i), i), "/03_05_pred.tif"))
    sdm
    
    # evaluation
    ev <- dismo::evaluate(p = terra::extract(sdm, occ, ID = FALSE)[, 1], 
                          a = terra::extract(sdm, bg, ID = FALSE)[, 1])
    ev
    
    thr <- threshold(ev)
    thr
    
    thr_id <- NULL
    for(j in names(threshold(ev))) thr_id <- c(thr_id, which(ev@t == dismo::threshold(ev, j)))
    tss <- ev@TPR[thr_id] + ev@TNR[thr_id] - 1
    
    thr_tss <- rbind(thr, tss)
    thr_tss <- cbind(data.frame(metrics = c("thresholds", "tss")), thr_tss)
    thr_tss
    
    auc <- ev@auc
    auc
    
    boyce <- ecospat::ecospat.boyce(fit = sdm, obs = occ)
    boyce
    
    data_ev_i <- tibble::tibble(
        version = i,
        auc = auc,
        tss_equal_sens_spec = thr_tss[2, ]$equal_sens_spec,
        tss_max_sens_spec = thr_tss[2, ]$spec_sens,
        boyce = boyce$cor)
    
    data_ev <- rbind(data_ev, data_ev_i)
    
}
data_ev

# export
readr::write_csv(data_ev, "02_results/03_02_evaluation_table/data_ev.csv")

# end --------------------------------------------------------------------