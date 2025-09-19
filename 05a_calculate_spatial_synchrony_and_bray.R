rm(list = ls())
#source("04a_calculate_synchrony.R")
library(tidyverse)
library(vegan)
library(codyn)
library(here)

source("00_functions_and_aes.R")

#read in from script 04a
diversity_stability_synchrony <- read.csv(here::here("data", "full_plot_level_dss_09172025.csv")) 
alpha_diversity_quad_macro <- read.csv(here::here("data", "alpha_diversity_quad_macro_09182025.csv"))
diversity_stability_synchrony_site <- read.csv(here::here("data", "diversity_stability_synchrony_site_09172025.csv"))
macro_functional_groups_long <- read.csv(here::here("data", "macroalgalfunctionalgroups_long_09162025.csv"), stringsAsFactors = F)




#### SPATIAL SYNCHRONY ####
# calculate mean synchrony and stability within a site
mean_synchrony_stability <- diversity_stability_synchrony %>% 
  dplyr::group_by(habitat, site) %>% 
  dplyr::summarise(richness_mean = mean(richness_mean, na.rm = T),
    functional_richness_mean = mean(functional_richness_mean, na.rm = T),
    synchrony_mean = mean(synchrony, na.rm = T),
    stability_mean = mean(cover_stability, na.rm = T),
    cover_mean = mean(cover_mean, na.rm = T))

# calculate spatial synchrony - degree to which quadrats vary together within a site
spatial_synchrony <- codyn::synchrony(df = alpha_diversity_quad_macro,
                                      time.var = "year",
                                      species.var = "location",
                                      abundance.var = "macroalgal_prop_cover",
                                      metric = "Loreau",
                                      replicate.var = "site_habitat")
colnames(spatial_synchrony) <- c("site_habitat", "spatial_synchrony") # rename to avoid overwrite 

#spatial synch matches

# merge with site level dss metrics
dss_spatial <- merge(diversity_stability_synchrony_site, spatial_synchrony, by = "site_habitat") #this matches
dss_spatial_2 <- merge(dss_spatial, mean_synchrony_stability[, which(names(mean_synchrony_stability) %in%
                                                                       c("habitat", "site", "synchrony_mean", "stability_mean"))],
                       by = c("habitat", "site"))


dss_spatial_2$ratio <- dss_spatial_2$cover_stability/dss_spatial_2$stability_mean

#write.csv(file = here::here("data", "spatial_synchrony_09172025.csv"), dss_spatial_2)

# calculate ranges
dss_spatial_ranges <- extract_ranges(dss_spatial_2, "habitat", c("spatial_synchrony", "synchrony_mean", "stability_mean"))

#### BRAY ####
##### setup #####
# SUM the data across years for each quad 
macro_long_data_summed <- macro_functional_groups_long %>%
  group_by(location, site, habitat, site_habitat, transect, quadrat, taxa) %>%
  dplyr::summarise(sum_taxa = sum(prop_cover)) %>%
  ungroup()

# pivot wide
#from long to wide
macro_wide_data_summed <- macro_long_data_summed %>%
  pivot_wider(names_from = taxa, values_from = sum_taxa, values_fill = 0)


##### calculate bray #####
# Initialize a results data frame
results_beta <- data.frame(
  site_habitat = character(),
  median_bray = numeric(),
  mean_bray = numeric(),
  mean_betadisp = numeric(),
  stringsAsFactors = FALSE
)

betadisp_list <- list()


# Get unique site_habitat combinations
sites <- unique(macro_wide_data_summed$site_habitat)

for (sh in sites) {
  # Filter for one site/habitat
  df_sub <- macro_wide_data_summed %>%
    filter(site_habitat == sh) %>%
    filter(rowSums(.[, 7:69]) != 0) #updated to 69, there are 63 macroalgal taxa and 6 meta columns
  
  # Skip if no rows left
  if (nrow(df_sub) < 2) {
    warning(paste("Skipping", sh, "- not enough data"))
    next
  }
  
  # Community and metadata
  sp_community <- as.matrix(df_sub[, 7:69])
  meta <- df_sub[, 1:6]
  
  # Bray-Curtis distances
  bray <- vegdist(sp_community, method = "bray")
  median_bray <- median(bray) # i used median not mean and I am not sure why? should it be mean? 
  mean_bray <- mean(bray)
  
  # Beta dispersion
  betadisp <- betadisper(bray, meta$site_habitat)
  mean_disp <- mean(betadisp$distances)
  
  betadisp_list[[sh]] <- betadisp
  
  # Store result
  results_beta <- rbind(results_beta, data.frame(
    site_habitat = sh,
    #betadisp_results = betadisp_results,
    median_bray = median_bray,
    mean_bray = mean_bray,
    mean_betadisp = mean_disp,
    stringsAsFactors = FALSE
  ))
}

# View results
# print(results_beta)

# merge together the beta div with the spatial synchrony output
beta_spatialsync <- merge(dss_spatial_2, results_beta, by = "site_habitat")

# calculate ranges
beta_ranges <- extract_ranges(beta_spatialsync, "habitat", c("spatial_synchrony", "synchrony_mean", "stability_mean", "mean_bray"))

##### BRAY WISCONSIN TRANSFORMED ####

# Initialize a results data frame
results_beta_transformed <- data.frame(
  site_habitat = character(),
  median_bray = numeric(),
  median_bray_trans = numeric(),
  mean_bray = numeric(),
  mean_bray_trans = numeric(),
  mean_betadisp = numeric(),
  mean_disp_transformed = numeric(),
  stringsAsFactors = FALSE
)

betadisp_list_transformed <- list()

# Get unique site_habitat combinations
sites <- unique(macro_wide_data_summed$site_habitat)

for (sh in sites) {
  # Filter for one site/habitat
  df_sub <- macro_wide_data_summed %>%
    filter(site_habitat == sh) %>%
    filter(rowSums(.[, 7:68]) != 0)  # Remove rows with all zeros
  
  # Community and metadata
  sp_community <- as.matrix(df_sub[, 7:68])
  meta <- df_sub[, 1:6]
  
  # Arc-sine square root transformation for the community matrix (required before vegdist)
  sp_community_transformed <- wisconsin(sp_community)
  
  # Confirm it's working
  print(paste("Range after transform for", sh, ":", paste(range(sp_community_transformed, na.rm = TRUE), collapse = " - ")))
  
  
  # Bray-Curtis distances
  bray_raw <- vegdist(sp_community, method = "bray")
  bray_transformed <- vegdist(sp_community_transformed, method = "bray")
  
  correlation <- cor(as.numeric(bray_raw), as.numeric(bray_transformed))
  print(paste("Correlation between raw and transformed Bray-Curtis for", sh, ":", round(correlation, 4)))
  
  
  # Calculate median and mean Bray-Curtis distance
  median_bray <- median(bray_raw)
  median_bray_trans <- median(bray_transformed)
  mean_bray <- mean(bray_raw)
  mean_bray_trans <- mean(bray_transformed)
  
  # Beta dispersion
  betadisp <- betadisper(bray_raw, meta$site_habitat)
  betadisp_transformed <- betadisper(bray_transformed, meta$site_habitat)
  mean_disp <- mean(betadisp$distances)
  mean_disp_transformed <- mean(betadisp_transformed$distances)
  
  betadisp_list[[sh]] <- betadisp
  betadisp_list_transformed[[sh]] <- betadisp_transformed
  
  # Store results
  results_beta_transformed <- bind_rows(results_beta_transformed, data.frame(
    site_habitat = sh,
    median_bray = median_bray,
    median_bray_trans = median_bray_trans,
    mean_bray = mean_bray,
    mean_bray_trans = mean_bray_trans,
    mean_betadisp = mean_disp,
    mean_disp_transformed = mean_disp_transformed,
    stringsAsFactors = FALSE
  ))
}

# View results
# print(results_beta_transformed)
# betadisp_list_transformed

beta_spatialsync_transformed <- merge(dss_spatial_2, results_beta_transformed, by = "site_habitat")