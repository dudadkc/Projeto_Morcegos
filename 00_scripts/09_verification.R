#' ----
#' aim: spatial joint
#' author: re
#' date: 09/05/2025
#' ----

library(tidyverse)
library(sf)
require(corrplot)
require(DataExplorer)

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

# correlation

df <- d %>% replace(is.na(.), 0) %>% as.data.frame()

p.mat <- cor.mtest(df[want])$p 

warnings()

p.mat

M <- cor(df[want])

M
