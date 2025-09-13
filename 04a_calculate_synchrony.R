rm(list = ls())
library(here)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(codyn)

source("01b_data_prep.R")

#### SPECIES SYNCHRONY ####
lm_synchrony <- codyn::synchrony(df = macro_long_data,
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
#write.csv(file = here::here("data", "full_plot_level_dss.csv"), diversity_stability_synchrony)

# EXTRACT RANGES
dss_ranges <- extract_ranges(diversity_stability_synchrony, "habitat",
                             c("richness_mean", "functional_richness_mean", "cover_stability", "synchrony"))

#### SITE LEVEL SPECIES SYNCHRONY ####
macro_long_data_site <- pivot_longer(
  data = site_macro,
  cols = grep("acanthophora_spicifera", colnames(site_macro)):grep("valonia_ventricosa", colnames(site_macro)),
  names_to = "taxa",
  values_to = "percent_cover") %>% 
  mutate(prop_cover = percent_cover/100) %>% 
  dplyr::select(-percent_cover)

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

#write_csv(diversity_stability_synchrony_site, here::here("data", "diversity_stability_synchrony_site.csv"))

dss_ranges_site <- extract_ranges(diversity_stability_synchrony_site, "habitat",
                                  c("richness_mean", "functional_richness_mean", "cover_stability", "synchrony"))
