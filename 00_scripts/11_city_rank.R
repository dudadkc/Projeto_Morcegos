#' ----
#' aim: city rank
#' author: re
#' date: May 2025
#' ----

library(tidyverse)
library(sf)
require(here)
require(RColorBrewer)
library(ggvenn)
library(eulerr)
library(dplyr)
library(ggplot2)

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

table(table(d$CD_MUN) > 1)

cd_mun_freq <- table(d$CD_MUN)

duplicated_cd_mun <- names(which(cd_mun_freq > 1))
d[d$CD_MUN == duplicated_cd_mun,'NM_MUN']

# ----------------------------
# flagged 200

top200_by_var <- function(df, var) {
    df %>%
        select(CD_MUN, all_of(var)) %>%
        arrange(desc(.data[[var]])) %>%
        slice_head(n = 200) %>%
        mutate(variable = var)
}

top_hazard <- top200_by_var(d, "hazard_average")
top_vulnerability <- top200_by_var(d, "vulnerability_average")
top_exposure <- top200_by_var(d, "exposure_average")

top_combined <- bind_rows(top_hazard, top_vulnerability, top_exposure)

flagged200 <- top_combined %>%
    group_by(CD_MUN) %>%
    summarise(
        count = n(),  # in how many top 200 lists it appears
        variables = paste(sort(unique(variable)), collapse = ", ")
    ) %>%
    arrange(desc(count))

flagged200

# Check flags with historical risk rank

rabies_200_cities <- d[order(-d$RABIES_CASES_HUMAN_INFECTION), ][1:sum(table(d$RABIES_CASES_HUMAN_INFECTION)), "CD_MUN"]

length(rabies_200_cities$CD_MUN)

length(flagged200$CD_MUN)

common_cities <- intersect(rabies_200_cities$CD_MUN, flagged200$CD_MUN)

length(common_cities)

unique_to_rabies <- setdiff(rabies_200_cities$CD_MUN, flagged200$CD_MUN)

length(unique_to_rabies)

unique_to_flagged <- setdiff(flagged200$CD_MUN, rabies_200_cities$CD_MUN)

length(unique_to_flagged)

d1 <- data.frame(city = common_cities, type = 'Rabies-positive and highly ranked in Naive ranking')
d2 <- data.frame(city = unique_to_rabies, type = 'Rabies-positive and not ranked in Naive ranking')
d3 <- data.frame(city = unique_to_flagged, type = 'Rabies-unknown and ranked in Naive ranking')

table_naive_rank <- rbind(d1,d2,d3)

length(unique(table_naive_rank$city))

table_naive_rank <- table_naive_rank %>% left_join(d %>% 
                                                       select(CD_MUN, NM_MUN, NM_UF) %>% 
                                                       distinct(), by = c("city" = "CD_MUN"))

nrow(table_naive_rank)

table_naive_rank
# export table
table_naive_rankdf <- as.data.frame(table_naive_rank)

head(table_naive_rankdf[c(3,4,2)])

setwd(here())
setwd('99_manuscript')
write.table(table_naive_rankdf[c(3,4,2)], file = "Table_naive_rank.txt",  sep = "\t",
            row.names = FALSE, col.names = TRUE, quote = FALSE)

rabies <- data.frame(city = rabies_200_cities$CD_MUN, source = "rabies_200_cities")

# all flagged, too many
flagged <- data.frame(city = flagged200$CD_MUN, source = "flagged200")

length(flagged$city)

# flagged where 2 or more components rank high (200 rank)
flaggedtop200comps <- flagged200 %>% filter(count >1)

# Municipalities that rank high in 2 or more risk components
nrow(top200comps)

all_cities <- bind_rows(rabies, flagged) %>%
    group_by(city) %>%
    summarise(count = n()) %>%
    mutate(category = case_when(
        count == 2 ~ "Both",
        city %in% rabies_200_cities$CD_MUN ~ "Rabies only",
        TRUE ~ "Flagged only"   ))

nrow(all_cities)

# plot

ggplot(all_cities, aes(x = category)) +
    geom_bar(fill = "steelblue") +
    theme_minimal() +
    labs(title = "Overlap of Top 200 Rabies Cities and Flagged Cities",
         x = "Category", y = "Number of Cities")

# Create a named list of the two sets
city_sets <- list(
    Rabies_Top_200 = rabies_200_cities$CD_MUN,
    Flagged_200 = flagged200$CD_MUN
)

# Draw the Venn diagram
ggvenn(city_sets, 
       fill_color = c("skyblue", "salmon"),
       stroke_size = 0.5, 
       set_name_size = 4,
       text_size = 5)

# Make circles proportional

# Create the sets
rabies_cities <- rabies_200_cities$CD_MUN
flagged_cities <- flagged200$CD_MUN

# Generate the Euler diagram input
fit <- euler(c(
    "Rabies" = length(setdiff(rabies_cities, flagged_cities)),
    "Flagged" = length(setdiff(flagged_cities, rabies_cities)),
    "Rabies&Flagged" = length(intersect(rabies_cities, flagged_cities))
))

# Plot
plot(fit,  fills = c("azure1", "salmon"),
     labels = list(font = 4),
     quantities = TRUE,
     main = "Top-ranking municipalities in terms of rabies risk components")

#---------------
