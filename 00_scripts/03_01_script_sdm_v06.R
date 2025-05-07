#' ---
#' title: sdm - fit
#' author: mauricio vancine
#' date: 2024-04-08
#' ---

# prepare r -------------------------------------------------------------

# options
options(scipen = 2, java.parameters = "-Xmx32g")
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
    dplyr::mutate(species = "Desmodus rotundus")
occ

# vector
occ_sf <- occ %>% 
    tidyr::drop_na(longitude, latitude) %>% 
    dplyr::mutate(lon = longitude, lat = latitude) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
occ_sf

# limit
neo <- sf::st_read("01_data/01_variables/00_limits/neotropic_dissolved_fill_holes.shp")
neo

# map
tm_shape(neo) +
    tm_polygons() +
    tm_shape(occ_sf) +
    tm_bubbles(fill = "species", size = .3,
               fill.legend = tm_legend(title = "Espécies",
                                       position = tm_pos_in("left", "bottom")))

# covar
covar_clim <- terra::rast("01_data/01_variables/01_climate/01_adjusted/climate_neo.tif")
covar_topo <- terra::rast("01_data/01_variables/02_topography/01_adjusted/topography_neo.tif")[[-1]]
covar_hydro <- log10(terra::rast("01_data/01_variables/03_hidrology/01_adjusted/hydrology_neo.tif") + 1)
covar_cave <- log10(terra::rast("01_data/01_variables/04_caves/01_adjusted/caves_neo.tif") + 1)

range(values(covar_hydro), na.rm = TRUE)
range(values(covar_cave), na.rm = TRUE)

covar <- c(covar_clim, covar_topo, covar_hydro, covar_cave)
covar

names(covar)
plot(covar[[1]])

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

occs_z <- cbind(occ_i, terra::extract(covar_sel, occ_i, ID = FALSE))
bg_z <- cbind(bg, terra::extract(covar_sel, bg))

plot(bg_z[, 1:2])
points(occs_z[, 1:2], pch = 20, col = "red")

### fit ----
eval_fit <- ENMeval::ENMevaluate(occs = occs_z, 
                                 bg = bg_z,
                                 tune.args = tune_args, 
                                 algorithm = "maxent.jar", 
                                 partitions = "block",
                                 parallel = TRUE, 
                                 numCores = 10)
eval_fit

### occs, bg and partitions ----
eval_occs <- cbind(eval_fit@occs.grp, eval_fit@occs)
eval_bg <- cbind(eval_fit@bg.grp, eval_fit@bg)

# plot_spat_block_part_occ <- evalplot.grps(pts = eval_fit@occs, pts.grp = eval_fit@occs.grp, 
#                                           envs = raster::raster(covar[[1]])) + 
#     ggplot2::ggtitle("Spatial block partitions: occurrences")
# 
# plot_spat_block_part_bg <- evalplot.grps(pts = eval_fit@bg, pts.grp = eval_fit@bg.grp, 
#                                          envs = raster::raster(covar[[1]])) + 
#     ggplot2::ggtitle("Spatial block partitions: background")

# block <- get.block(occ_i, bg)
# plot_spat_block_part_envsim_occ <- evalplot.envSim.hist(sim.type = "mess", ref.data = "occs", 
#                                                         occs.z = occs_z, bg.z = bg_z, 
#                                                         occs.grp = block$occs.grp, 
#                                                         bg.grp = block$bg.grp)
# 
# plot_spat_block_part_envsim_bg <- evalplot.envSim.hist(sim.type = "mess", ref.data = "bg", 
#                                                        occs.z = occs_z, bg.z = bg_z, 
#                                                        occs.grp = block$occs.grp, 
#                                                        bg.grp = block$bg.grp)


### results ----  
eval_resul <- eval_fit@results
eval_resul_partitions <- eval_fit@results.partitions

### selection ----
eval_resul_aic <- dplyr::filter(eval_resul, delta.AICc == 0)[1, ]
eval_fit_aic <- eval.models(eval_fit)[[as.character(eval_resul_aic$tune.args)]]

# plot_delta_aicc_tune_args <- ggplot(eval_resul, aes(x = rm, y = delta.AICc, color = fc, group = fc)) +
#     geom_line() +
#     geom_point(color = "black", shape = 1) +
#     geom_point(data = eval_resul_aic, aes(x = rm, y = delta.AICc), color = "red", size = 5) +
#     geom_hline(yintercept = 2, lty = 2) +
#     scale_color_lancet() +
#     labs(x = "Regularization multiplier", y = "Delta AICc", color = "Feature classes") +
#     theme_bw(base_size = 15)
# 
# plot_ormtp_tune_args <- ggplot(eval_resul, aes(x = rm, y = or.mtp.avg, color = fc, group = fc)) +
#     geom_line() +
#     geom_point(color = "black", shape = 1) +
#     geom_point(data = eval_resul_aic, aes(x = rm, y = or.mtp.avg), color = "red", size = 5) +
#     scale_color_lancet() +
#     labs(x = "Regularization multiplier", y = "Average ORMTP ('Minimum Training Presence' omission rate)", color = "Feature classes") +
#     theme_bw(base_size = 15)
# 
# plot_auc_tune_args <- ggplot(eval_resul, aes(x = rm, y = auc.val.avg, color = fc, group = fc)) +
#     geom_line() +
#     geom_point(color = "black", shape = 1) +
#     geom_point(data = eval_resul_aic, aes(x = rm, y = auc.val.avg), color = "red", size = 5) +
#     geom_hline(yintercept = .75, lty = 2) +
#     scale_color_lancet() +
#     ylim(min(eval_resul$auc.val.avg) - .15, max(eval_resul$auc.val.avg) + .1) +
#     labs(x = "Regularization multiplier", y = "Average validation Area Under Curve (AUC)", color = "Feature classes") +
#     theme_bw(base_size = 15)
# 
# plot_cbi_tune_args <- ggplot(eval_resul, aes(x = rm, y = cbi.val.avg, color = fc, group = fc)) +
#     geom_line() +
#     geom_point(color = "black", shape = 1) +
#     geom_point(data = eval_resul_aic, aes(x = rm, y = cbi.val.avg), color = "red", size = 5) +
#     scale_color_lancet() +
#     labs(x = "Regularization multiplier", y = "Average validation Continuous Boyce Index (CBI)", color = "Feature classes") +
#     theme_bw(base_size = 15)

### null models ----
# mod_null <- ENMeval::ENMnulls(e = eval_fit, no.iter = 100, parallel = FALSE, numCores = 10,
#                               mod.settings = list(fc = as.character(eval_resul_aic$fc),
#                                                   rm = as.numeric(as.character(eval_resul_aic$rm))))
# mod_null_results <- null.emp.results(mod_null)
# plot_mod_null_hist <- evalplot.nulls(mod_null, stats = c("or.mtp", "auc.val"), plot.type = "histogram")

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
eval_resul_aic_response <- NULL

dismo::response(eval.models(eval_fit)[[eval_resul_aic$tune.args]])

for(j in covar_vif@results$Variables){
    
    eval_resul_aic_response_i <- tibble::as_tibble(dismo::response(eval.models(eval_fit)[[eval_resul_aic$tune.args]], var = j)) %>% 
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
path_sp <- paste0("02_results/03_01_sdms_v06")

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
