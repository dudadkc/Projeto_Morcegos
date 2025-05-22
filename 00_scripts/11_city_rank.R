#' ----
#' aim: city rank
#' author: re
#' date: May 2025
#' ----

library(tidyverse)
library(sf)
require(here)
require(RColorBrewer)

d <- st_read('C://Users//rdelaram//Downloads//00_mun_data.gpkg')

d$PRural <- as.numeric(d$PRural)

df <- d %>% replace(is.na(.), 0) %>% as.data.frame()

setwd(here())

setwd('01_data/03_eda/')


hazard_names <- scan("hazard_low_vif.txt", what = character())
vulnerability_names <- scan("vulnerability_low_vif.txt", what = character())
exposure_names <- scan("exposure_low_vif.txt", what = character())

rescale_and_average <- function(data, var_names, prefix) {
    rescaled_vars <- paste0("rescaled_", var_names)
    
    data <- data %>%
        mutate(across(all_of(var_names), ~ scales::rescale(., to = c(0, 1)), .names = "rescaled_{.col}")) %>%
        rowwise() %>%
        mutate("{prefix}_average" := mean(c_across(all_of(rescaled_vars)), na.rm = TRUE)) %>%
        ungroup()
    
    return(data)
}

# Arranging the scale for positive expectations

summary(d$CANINE_VACCINATED)
d$CANINE_VACCINATED <- d$CANINE_VACCINATED*(-1)
summary(d$CANINE_VACCINATED)

d$RT_LITE <- d$RT_LITE*(-1)


# City rank
d <- rescale_and_average(d, hazard_names, "hazard")
d <- rescale_and_average(d, vulnerability_names, "vulnerability")
d <- rescale_and_average(d, exposure_names, "exposure")

# maps
d$NM_MUN

d$hazard_average

d$vulnerability_average

d$exposure_average

top50_by_var <- function(df, var) {
    df %>%
        select(NM_MUN, all_of(var)) %>%
        arrange(desc(.data[[var]])) %>%
        slice_head(n = 50) %>%
        mutate(variable = var)
}

# Get top 50 lists for each variable
top_hazard <- top50_by_var(d, "hazard_average")
top_vulnerability <- top50_by_var(d, "vulnerability_average")
top_exposure <- top50_by_var(d, "exposure_average")

# Combine all top 50s into one dataframe
top_combined <- bind_rows(top_hazard, top_vulnerability, top_exposure)

# Count how many times each NM_MUN appears (across variables)
flagged <- top_combined %>%
    group_by(NM_MUN) %>%
    summarise(
        count = n(),  # number of variables where it appears in top 50
        variables = paste(sort(unique(variable)), collapse = ", ")
    ) %>%
    arrange(desc(count))


flagged$count

# flagged 100
top100_by_var <- function(df, var) {
    df %>%
        select(NM_MUN, all_of(var)) %>%
        arrange(desc(.data[[var]])) %>%
        slice_head(n = 100) %>%
        mutate(variable = var)
}

# Get top 100 lists for each variable
top_hazard <- top100_by_var(d, "hazard_average")
top_vulnerability <- top100_by_var(d, "vulnerability_average")
top_exposure <- top100_by_var(d, "exposure_average")

top_combined <- bind_rows(top_hazard, top_vulnerability, top_exposure)

flagged <- top_combined %>%
    group_by(NM_MUN) %>%
    summarise(
        count = n(),  # in how many top 100 lists it appears
        variables = paste(sort(unique(variable)), collapse = ", ")
    ) %>%
    arrange(desc(count))


flagged

table(d$NM_MUN) > 1

# Check flags with historical risk rank

#---------------