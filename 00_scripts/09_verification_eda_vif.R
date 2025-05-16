#' ----
#' aim: verification, eda and variable selection
#' author: re
#' date: May 2025
#' ----

library(tidyverse)
library(sf)
require(corrplot)
require(DataExplorer)
require(here)
library(DataExplorer)
library(GGally)
library(corrplot)


d <- st_read('D://OneDrive - Massey University//Supervisions//Madu//03_prioritization//00_mun_data.gpkg')

want <- colnames(d[13:ncol(d)])

want <- want[1:26]

want

head(d)

# Data verification

# check children 
sum(na.omit(d$TOTAL))

# check basic care
unique(d$SPENDING_BASIC_CARE)

# check dogs
unique(d$CANINE_VACCINATED)

# check cats
unique(d$FELINE_VACCINATED)

# check health 
table(d$SPENDING_EPIDEMIO_SURV)

d$PRural <- as.numeric(d$PRural)

str(d)

#------- exploratory data analysis

list.files()

setwd(here())

setwd('01_data')

dir.create('03_eda')

setwd('03_eda')

missing_data <- plot_missing(d)

# export 

jpeg(filename = 'missing_data.jpg', units='cm', height = 30, width=34, res=300)
missing_data
dev.off()

# check data completeness for areas with rabies infection

df %>% filter(RABIES_CASES_HUMAN_INFECTION > 0) %>%
    summarise(any_na = any(is.na(SPENDING_BASIC_CARE)))

vars_to_check <- c(
    "SPENDING_BASIC_CARE",
    "SPENDING_EPIDEMIO_SURV",
    "HOSPITAL_TRAVEL_TIME",
    "RABIES_CASES_DOMESTIC_CANINE",
    "CD_CONCURB"
)

map_dfr(vars_to_check, ~ { 
        df %>% filter(RABIES_CASES_HUMAN_INFECTION > 0) %>%
        summarise(var = .x, any_na = any(is.na(.data[[.x]])))
                        })

# transform nas to zero to check variation

df <- d %>% replace(is.na(.), 0) %>% as.data.frame()

# remove latlong and nas

df <- df[-ncol(df)]


# create report

create_report(df[14:ncol(df)])

# keep nas

dna <- d %>% as.data.frame()

dna <- dna[-ncol(dna)]

dna

# correlation
p.mat <- cor.mtest(df[14:ncol(df)])$p 

warnings()

p.mat

M <- cor(df[14:ncol(df)])

M


high_cor_vars <- as.data.frame(as.table(M)) %>%
    filter(Var1 != Var2, abs(Freq) > 0.7) %>%
    select(Var1, Var2) %>%
    unlist() %>%
    unique()

cor_subset <- M[high_cor_vars, high_cor_vars]

jpeg(filename = 'cor_continuous_all.jpg', units='cm', height = 30, width=34, res=300)

corrplot(M,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black", # show correlation values
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200))

dev.off()

jpeg(filename = 'cor_continuous_correlated_only.jpg', units='cm', height = 30, width=34, res=300)

corrplot(cor_subset,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200))
dev.off()

# ----------

# vif
covar <- df[14:ncol(df)]

vif_result <-  usdm::vifstep(covar)

vif_result

covar_vif <- usdm::vifstep(covar, th = 2)

covar_vif

covar_sel <- usdm::exclude(covar, covar_vif)
colnames(df)
names(covar_sel)

# variables out

setdiff(colnames(covar), names(covar_sel))

names(covar_sel)

setdiff(row.names(cor_subset), names(covar_sel))

cor(df$SPENDING_HEALTH, df$CANINE_VACCINATED)


# Dividing the data into risk components------------------------------------------
exposure <- c('WALL', 'HAB_KM2','PRural', 'PUrban', 'roads_density_km', 'CHILDREN',
              'PIndig', 'pasture_pct' )

Me <- cor(df[exposure])


jpeg(filename = 'cor_exposure.jpg', units='cm', height = 30, width=34, res=300)
corrplot(Me,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200))
dev.off()


vulnerability <- c('HOSPITAL_TRAVEL_TIME', 'GINI_INDEX', "RT_LITE" ,
                   'PCT_POOR', 'IDHM', 'IDHM_EDU',
                   'STUDY_YEARS', 'SPENDING_HEALTH', 'SPENDING_EPIDEMIO_SURV',
                   'SPENDING_BASIC_CARE')

Mv <- cor(df[vulnerability])

jpeg(filename = 'cor_vulnerability.jpg', units='cm', height = 30, width=34, res=300)
corrplot(Mv,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200))
dev.off()

hazard <- c('sdm_cont_mn', 'CATTLE_POP', 'CANINE_VACCINATED', 'FELINE_VACCINATED'  )

Mh <- cor(df[hazard])

jpeg(filename = 'cor_hazard.jpg', units='cm', height = 30, width=34, res=300)
corrplot(Mh,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         col = colorRampPalette(c("blue", "white", "red"))(200))
dev.off()

# Exposure
# we remove 'PUrban' to keep children option
cor(covar[exposure]) > 0.7
plot(covar$CHILDREN, covar$PUrban)
plot(covar$CHILDREN, covar$PRural)
summary(covar$PUrban)
sum(covar$CHILDREN)

exposure <- c('WALL', 'HAB_KM2','PRural',  'roads_density_km', 'CHILDREN',
             'PIndig', 'pasture_pct' )

exposure_vif <- usdm::vifstep(covar[exposure], th = 2)

exposure_vif

exposure_sel <- usdm::exclude(covar[exposure], exposure_vif)

names(exposure_sel) #7

setdiff(colnames(covar[exposure]), names(exposure_sel))

# Children and Purban
cor(covar[exposure]) > 0.7
cor(covar[exposure]) < -0.7

# Hazard
# feline and canine 
cor(covar[hazard]) > 0.7

# rm 'FELINE_VACCINATED'  to keep canine option
hazard <- c('sdm_cont_mn', 'CATTLE_POP', 'CANINE_VACCINATED' )
    
hazard_vif <- usdm::vifstep(covar[hazard], th = 2)

hazard_vif

hazard_sel <- usdm::exclude(covar[hazard], hazard_vif)

names(hazard_sel)

setdiff(colnames(covar[hazard]), names(hazard_sel))

# Vulnerability
# gini closer to zero -- more equality
#  'PCT_POOR' is inflating the var so it is coming out

vulnerability_vif <- usdm::vifstep(covar[vulnerability ], th = 2)

vulnerability_vif

vulnerability_sel <- usdm::exclude(covar[vulnerability], vulnerability_vif)

names(vulnerability_sel)

setdiff(colnames(covar[vulnerability]), names(vulnerability_sel))

# IDHM_EDU IDHM PCT_POOR STUDY_YEARS 
cor(covar[vulnerability]) > 0.7

# final selection

names(hazard_sel) # 3
names(vulnerability_sel) # 6
names(exposure_sel) # 7

write.table(names(hazard_sel), file = "hazard_low_vif.txt", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

write.table(names(vulnerability_sel), file = "vulnerability_low_vif.txt", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

write.table(names(exposure_sel), file = "exposure_low_vif.txt", 
            row.names = FALSE, col.names = FALSE, quote = FALSE)

#-------------------------------------------------------------------------------------------------------------