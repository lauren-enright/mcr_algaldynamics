rm(list = ls())
library(here)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(codyn)

#source("01b_data_prep.R")

#want to have appropriate functions and plot themes
source("00_functions_and_aes.R")

#read in data created in 
macro_functional_groups_long <- read.csv(here::here("data", "macroalgalfunctionalgroups_long_09162025.csv"), stringsAsFactors = F)
alpha_diversity_quad_macro <- read.csv(here::here("data", "alpha_diversity_quad_macro_09182025.csv"))
site_macro_wide <- read.csv(here::here("data", "site_macro_alpha_wide_09182025.csv"))
alpha_diversity_site_macro <- read.csv(here::here("data", "alpha_diversity_site_macro_09182025.csv"))

#### SPECIES SYNCHRONY ####
lm_synchrony <- codyn::synchrony(df = macro_functional_groups_long,
                                 time.var = "year",
                                 species.var = "taxa",
                                 abundance.var = "prop_cover",
                                 metric = "Loreau",
                                 replicate.var = "location")


diversity_stability <- alpha_diversity_quad_macro %>%
  dplyr::group_by(location, habitat, site) %>%
  dplyr::summarise(
    richness_mean = mean(richness), 
    shannon_mean = mean(shannon), 
    cover_mean = mean(macroalgal_prop_cover),
    functional_richness_mean = mean(functional_richness),
    cover_cv = CV(macroalgal_prop_cover),
    cover_stability = 1/CV(macroalgal_prop_cover)) 

diversity_stability_synchrony <- left_join(diversity_stability, lm_synchrony, by = join_by(location))
diversity_stability_synchrony$synchrony_trans <- sv_trans(diversity_stability_synchrony$synchrony)

write.csv(file = here::here("data", "full_plot_level_dss_09172025.csv"), diversity_stability_synchrony, row.names = FALSE)


#### SITE LEVEL SPECIES SYNCHRONY ####
macro_long_data_site <- pivot_longer(
  data = site_macro_wide,
  cols = grep("acanthophora_spicifera", colnames(site_macro_wide)):grep("valonia_ventricosa", colnames(site_macro_wide)),
  names_to = "taxa",
  values_to = "percent_cover") %>% 
  mutate(prop_cover = percent_cover/100) %>% 
  dplyr::select(-percent_cover)

##### do we need to write this out?? $$$
# write_csv(macro_long_data, here("Data", "macro_long_data.csv"))

# CALCULATE SYNCHRONY CODE
lm_synchrony_site <- codyn::synchrony(df = macro_long_data_site,
                                      time.var = "year",
                                      species.var = "taxa",
                                      abundance.var = "prop_cover",
                                      metric = "Loreau",
                                      replicate.var = "site_habitat")


diversity_stability_site <- alpha_diversity_site_macro %>%
  dplyr::group_by(site_habitat, habitat, site) %>%
  dplyr::summarise(
    richness_mean = mean(richness), 
    functional_richness_mean = mean(functional_richness), 
    cover_mean = mean(prop_cover),
    cover_cv = CV(prop_cover),
    cover_stability = 1/CV(prop_cover)) 

diversity_stability_synchrony_site <- left_join(diversity_stability_site, lm_synchrony_site, by = join_by(site_habitat))
diversity_stability_synchrony_site$synchrony_trans <- sv_trans(diversity_stability_synchrony_site$synchrony)

write.csv(file = here::here("data", "diversity_stability_synchrony_site_09172025.csv"), diversity_stability_synchrony_site, row.names = FALSE)

