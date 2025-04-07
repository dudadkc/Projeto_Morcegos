#' ---
#' title: modelos - regional - buffer
#' author: mauricio vancine
#' date: 2024-04-08
#' ---

# prepare r -------------------------------------------------------------

# options
options(scipen = 2, java.parameters = "-Xmx6g")
gc()

# packages
library(tidyverse)
library(ggsci)
library(sf)
library(raster)
library(terra)
library(dismo)
library(ecospat)
library(rJava)
library(ENMeval)
library(tmap)

# tmap options
tmap_options(show.messages = FALSE, show.warnings = FALSE)

# maxent
download.file(url = "https://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download",
              destfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"), mode = "wb")
unzip(zipfile = paste0(system.file("java", package = "dismo"), "/maxent.zip"),
      exdir = system.file("java", package = "dismo"), junkpaths = TRUE)
dir(system.file("java", package = "dismo"))

# import data -------------------------------------------------------------

# occ
occ <- readr::read_csv("01_data/00_occurrences/01_clean/01_occ_cleaned.csv") %>% 
    dplyr::mutate(species = "Desmodus rotundus") %>% 
    dplyr::slice_sample(n = 50)
occ

# vector
occ_sf <- occ %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::mutate(lon = longitude, lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_sf

# covar
covar <- terra::rast("test/covar.tif")
covar

# map
tm_shape(covar[[1]]) +
    tm_raster() +
    tm_shape(occ_sf) +
    tm_dots(fill = "gray", fill_alpha.scale = .5) +
    tm_layout(legend.show = FALSE)

# sdm ---------------------------------------------------------------------

# tune args
tune_args <- list(fc = c("L", "LQ", "H", "LQH", "LQHP", "LQHPT"), rm = seq(.5, 4, .5))
tune_args

# enmeval

## covariates selection ----
covar_vif <- usdm::vifstep(covar, th = 2)
covar_sel <- usdm::exclude(covar, covar_vif)
names(covar_sel)

## sdm ----

## species ----
occ_i <- occ %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::select(3:4)
occ_i

### data ----
bg <- terra::spatSample(x = covar_sel[[1]], size = 1e4, "random", as.df = TRUE, xy = TRUE, values = FALSE, na.rm = TRUE)
colnames(bg) <- colnames(occ_i)

occ_z <- cbind(occ_i, terra::extract(covar_sel, occ_i, ID = FALSE))
bg_z <- cbind(bg, terra::extract(covar_sel, bg))

plot(bg_z[, 1:2], col = "gray", pch = 20)
points(occs_z[, 1:2], pch = 20, col = "red")

### fit ----
eval_fit <- ENMeval::ENMevaluate(
    occs = occ_z, 
    bg = bg_z,
    tune.args = tune_args, 
    algorithm = "maxent.jar", 
    partitions = "block",
    parallel = TRUE, 
    numCores = 2)
eval_fit

### occs, bg and partitions ----
eval_occs <- cbind(eval_fit@occs.grp, eval_fit@occs)
eval_bg <- cbind(eval_fit@bg.grp, eval_fit@bg)

### results ----  
eval_resul <- eval_fit@results

### selection ----
eval_resul_aic <- dplyr::filter(eval_resul, delta.AICc == 0)[1, ]
eval_fit_aic <- eval.models(eval_fit)[[as.character(eval_resul_aic$tune.args)]]

### covariate importance ----
eval_resul_aic_varimp <- dplyr::arrange(eval_fit@variable.importance[[eval_resul_aic$tune.args]], -percent.contribution)

plot_covar_imp <- ggplot(eval_resul_aic_varimp,
                         aes(x = percent.contribution, 
                             y = reorder(variable, percent.contribution))) +
    geom_bar(stat = "identity", fill = "gray") +
    geom_text(aes(label = round(percent.contribution, 1)), size = 7) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +
    labs(x = "Percent contribution", y = "") +
    theme_bw(base_size = 20) 
plot_covar_imp

### covariate response ----
for(j in covar_vif@results$Variables){
    
    eval_resul_aic_response_i <- dismo::response(
        x = eval_fit@models[[eval_resul_aic$tune.args]], 
        var = j) %>% 
        tibble::as_tibble() %>% 
        dplyr::rename(value = 1, predict = 2) %>% 
        dplyr::mutate(covar = j)
    eval_resul_aic_response <- rbind(eval_resul_aic_response, eval_resul_aic_response_i) 
}
eval_resul_aic_response

plot_covar_res <- ggplot(eval_resul_aic_response, aes(x = value, y = predict)) +
    geom_line(color = "red", lwd = 1) +
    facet_wrap(~covar, scales = "free") +
    labs(x = "Values", y = "Cloglog") +
    theme_bw(base_size = 20)
plot_covar_res

### prediction ----
eval_fit_aic_predict <- enm.maxent.jar@predict(
    mod = eval_fit@models[[as.character(eval_resul_aic$tune.args)]], 
    envs = covar_sel, 
    other.settings = list(pred.type = "cloglog"))
eval_fit_aic_predict

plot(eval_fit_aic_predict)

## threshold ----
ev <- dismo::evaluate(p = terra::extract(eval_fit_aic_predict, eval_fit@occs[, 1:2], ID = FALSE)[, 1], 
                      a = terra::extract(eval_fit_aic_predict, eval_fit@bg[, 1:2], ID = FALSE)[, 1])
thr <- threshold(ev)

eval_fit_aic_predict_thr_equal_sens_spec <- eval_fit_aic_predict >= thr$equal_sens_spec
eval_fit_aic_predict_thr_sensitivity <- eval_fit_aic_predict >= thr$sensitivity
eval_fit_aic_predict_thr_spec_sens <- eval_fit_aic_predict >= thr$spec_sens

plot(eval_fit_aic_predict_thr_equal_sens_spec)
plot(eval_fit_aic_predict_thr_spec_sens)
plot(eval_fit_aic_predict_thr_sensitivity)

### tss ---
thr_id <- NULL
for(j in names(threshold(ev))) thr_id <- c(thr_id, which(ev@t == dismo::threshold(ev, j)))
tss <- ev@TPR[thr_id] + ev@TNR[thr_id] - 1

thr_tss <- rbind(thr, tss)
thr_tss <- cbind(data.frame(metrics = c("thresholds", "tss")), thr_tss)

## export ----
path_sp <- paste0("02_results/03_01_sdms_v05")

dir.create(path = path_sp)

writexl::write_xlsx(covar_vif@results, paste0(path_sp, "/02_vif.xlsx"))

writexl::write_xlsx(eval_occs, paste0(path_sp, "/01_occs.xlsx"))
writexl::write_xlsx(eval_bg, paste0(path_sp, "/01_bg.xlsx"))
# ggsave(paste0(path_sp, "/01_map_occs_block.png"), plot_spat_block_part_occ, wi = 25, he = 20, un = "cm", dpi = 300)
# ggsave(paste0(path_sp, "/01_map_bg_block.png"), plot_spat_block_part_bg, wi = 25, he = 20, un = "cm", dpi = 300)
# ggsave(paste0(path_sp, "/01_map_occs_envsim.png"), plot_spat_block_part_envsim_occ, wi = 25, he = 20, un = "cm", dpi = 300)
# ggsave(paste0(path_sp, "/01_map_bg_envsim.png"), plot_spat_block_part_envsim_bg, wi = 25, he = 20, un = "cm", dpi = 300)

writexl::write_xlsx(eval_resul, paste0(path_sp, "/03_01_eval_results.xlsx"))
writexl::write_xlsx(eval_resul_partitions, paste0(path_sp, "/03_01_eval_resul_partitions.xlsx"))
writexl::write_xlsx(eval_resul_aic, paste0(path_sp, "/03_01_eval_resul_aic.xlsx"))

# ggsave(paste0(path_sp, "/03_01_delta_aicc_tune_args.png"), plot_delta_aicc_tune_args, wi = 30, he = 20, un = "cm", dpi = 300)
# ggsave(paste0(path_sp, "/03_01_ormtp_tune_args.png"), plot_ormtp_tune_args, wi = 30, he = 20, un = "cm", dpi = 300)
# ggsave(paste0(path_sp, "/03_01_auc_tune_args.png"), plot_auc_tune_args, wi = 30, he = 20, un = "cm", dpi = 300)

# writexl::write_xlsx(mod_null_results, paste0(path_sp, "/03_02_mod_null_results.xlsx"))
# ggsave(paste0(path_sp, "/03_02_mod_null_hist.png"), plot_mod_null_hist, wi = 30, he = 20, un = "cm", dpi = 300)

ggsave(paste0(path_sp, "/03_03_covarimp.png"), plot_covar_imp, wi = 30, he = 20, un = "cm", dpi = 300)
ggsave(paste0(path_sp, "/03_03_covarrep.png"), plot_covar_res, wi = 30, he = 20, un = "cm", dpi = 300)

writexl::write_xlsx(thr_tss, paste0(path_sp, "/03_04_thresholds_tss.xlsx"))

terra::writeRaster(eval_fit_aic_predict, paste0(path_sp, "/03_05_pred.tif"), overwrite = TRUE)

terra::writeRaster(eval_fit_aic_predict_thr_equal_sens_spec, paste0(path_sp, "/03_05_pred_thr_equal_sens_spec.tif"), overwrite = TRUE)
terra::writeRaster(eval_fit_aic_predict_thr_sensitivity, paste0(path_sp, "/03_05_pred_thr_sensitivity.tif"), overwrite = TRUE)
terra::writeRaster(eval_fit_aic_predict_thr_spec_sens, paste0(path_sp, "/03_05_pred_thr_spec_sens.tif"), overwrite = TRUE)

# end ---------------------------------------------------------------------