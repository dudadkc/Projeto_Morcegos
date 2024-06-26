#' ---
#' title: plataform sn - variavies - download
#' author: mauricio vancine
#' date: 2024-03-23
#' ---

# prepare r -------------------------------------------------------------

# options
options(timeout = 3e5)

# download ----------------------------------------------------------------

## regional ----

# climate
for(i in 1:19){
    
    download.file(url = paste0("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio", i, "_1981-2010_V.2.1.tif"), 
                  destfile = paste0("02_data/02_variables/01_regional/00_raw/CHELSA_bio", i, "_1981-2010_V.2.1.tif"), 
                  mode = "wb")
}

# topography
download.file(url = paste0("https://data.earthenv.org/topography/elevation_1KMmd_GMTEDmd.tif"), 
              destfile = paste0("02_data/02_variables/01_regional/00_raw/elevation_1KMmd_GMTEDmd.tif"), 
              mode = "wb")

download.file(url = paste0("https://data.earthenv.org/topography/slope_1KMmd_GMTEDmd.tif"), 
              destfile = paste0("02_data/02_variables/01_regional/00_raw/slope_1KMmd_GMTEDmd.tif"), 
              mode = "wb")

## local ----

# mapbiomas
download.file(url = "https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_8/lclu/coverage/brasil_coverage_2022.tif", 
              destfile = "02_data/02_variables/02_local/00_raw/brasil_coverage_2022.tif", 
              mode = "wb")

# footprint
download.file(url = "https://storage.googleapis.com/hii-export/2020-01-01/hii_2020-01-01.tif", 
              destfile = "02_data/02_variables/02_local/00_raw/hii_2020-01-01.tif", 
              mode = "wb")

# end ---------------------------------------------------------------------