## Callie Stephenson
# Made 08/19/24
#Requested Fish Figures

#packages, u kno the drill
library(dplyr)
library(readr)
library(here)
library(fishualize)
library(ggplot2)
library(vegan)
library(mclust)
library(gridExtra)
library(glue)
library(patchwork) 
library(scales)
library(lme4)
library(car)

#data toime
new_diversity <- read.csv(here("data", "algae_diversity_site.csv"))
fish <- read.csv(here("data", "biomass_herb_summary.csv"))

#merge em
#truly i will not forgive y'all for making all them things lowercase
fish$site <- fish$Site
fish$year <- fish$Year
fish$habitat <- tolower(fish$Habitat)
fish$habitat <- gsub("outer 10", "outer_10", fish$habitat)
fish_join_div <- fish %>% 
  mutate(site = paste0("lter_", Site))
alg_div_fish <- left_join(new_diversity, fish_join_div[,c(4:8)], by = join_by(year, site, habitat))
merged_div <- alg_div_fish[alg_div_fish$habitat != "outer_17", ]


#STARTIN WITH THE HERB
#max(merged_div$Herbivore.Biomass,na.rm = TRUE) / max(merged_div$algae_cover_hard_mean,na.rm = TRUE)
herb_cover <- ggplot(merged_div, aes(x = year)) +
  geom_line(aes(y = algae_cover_hard_mean, color = "Algae Cover of Hard Substrate")) +
  geom_line(aes(y = Herbivore.Biomass / 817.1225, color = "Herbivore Biomass")) + # Rescale Herbivore.Biomass
  facet_grid(site ~ habitat) +
  scale_y_continuous(
    name = "Mean Algae Cover of Hard Substrate (%)",
    sec.axis = sec_axis(~ . * 817.1225, name = "Herbivore Biomass") # Rescale back for labeling
  ) +
  scale_color_manual(values = c("Algae Cover of Hard Substrate" = "darkgreen", "Herbivore Biomass" = "darkblue")) +
  labs(x = "Year", color = "Legend", title = "Trends in Algal Cover and Herbivore Biomass" # Add the plot title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom")


#max(merged_div$Herbivore.Biomass,na.rm = TRUE) / max(merged_div$algae_richness_mean,na.rm = TRUE)
herb_richness <- ggplot(merged_div, aes(x = year)) +
  geom_line(aes(y = algae_richness_mean, color = "Algal Richness")) +
  geom_line(aes(y = Herbivore.Biomass /18734.85, color = "Herbivore Biomass")) + # Rescale Herbivore.Biomass
  facet_grid(site ~ habitat) +
  scale_y_continuous(
    name = "Algal Species Richness",
    sec.axis = sec_axis(~ . * 18734.85, name = "Herbivore Biomass") # Rescale back for labeling
  ) +
  scale_color_manual(values = c("Algal Richness" = "darkgreen", "Herbivore Biomass" = "darkblue")) +
  labs(x = "Year", color = "Legend", title = "Trends in Algal Richness and Herbivore Biomass" # Add the plot title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#max(merged_div$Herbivore.Biomass,na.rm = TRUE) / max(merged_div$algae_shannon_mean,na.rm = TRUE)
herb_diversity <- ggplot(merged_div, aes(x = year)) +
  geom_line(aes(y = algae_shannon_mean, color = "Algal Diversity")) +
  geom_line(aes(y = Herbivore.Biomass /67604.8, color = "Herbivore Biomass")) + # Rescale Herbivore.Biomass
  facet_grid(site ~ habitat) +
  scale_y_continuous(
    name = "Algal Diversity (Shannon Index)",
    sec.axis = sec_axis(~ . * 67604.8, name = "Herbivore Biomass") # Rescale back for labeling
  ) +
  scale_color_manual(values = c("Algal Diversity" = "darkgreen", "Herbivore Biomass" = "darkblue")) +
  labs(x = "Year", color = "Legend", title = "Trends in Algal Diversity and Herbivore Biomass" # Add the plot title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

#max(merged_div$Herbivore.Biomass,na.rm = TRUE) / max(merged_div$centroid_dist_habyear_mean,na.rm = TRUE)
herb_beta <- ggplot(merged_div, aes(x = year)) +
  geom_line(aes(y = centroid_dist_habyear_mean, color = "Centroid Distance")) +
  geom_line(aes(y = Herbivore.Biomass /103388.3, color = "Herbivore Biomass")) + # Rescale Herbivore.Biomass
  facet_grid(site ~ habitat) +
  scale_y_continuous(
    name = "Mean Centroid Distance",
    sec.axis = sec_axis(~ . * 103388.3, name = "Herbivore Biomass") # Rescale back for labeling
  ) +
  scale_color_manual(values = c("Centroid Distance" = "darkgreen", "Herbivore Biomass" = "darkblue")) +
  labs(x = "Year", color = "Legend", title = "Trends in Algal Beta Dispersion and Herbivore Biomass" # Add the plot title here
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

print(herb_cover)
print(herb_richness)
print(herb_diversity)
print(herb_beta)

#ggsave(filename = file.path(here("output"), "cover_vs_herb_biomass.png"), plot = herb_cover, height = 12, width = 10)
#ggsave(filename = file.path(here("output"), "richness_vs_herb_biomass.png"), plot = herb_richness, height = 12, width = 10)
#ggsave(filename = file.path(here("output"), "diversity_vs_herb_biomass.png"), plot = herb_diversity, height = 12, width = 10)


## Relationships between metrics and biomass not over time
herb_beta_overall <- ggplot(merged_div, aes(x=centroid_dist_habyear_mean, y = log10(Herbivore.Biomass)))+
  geom_point()+
  #geom_smooth()+
  facet_grid(site ~ habitat, scales = "free_x")
herb_beta_overall

merged_div$site <- as.factor(merged_div$site)
merged_div$habitat<- as.factor(merged_div$habitat)
merged_div$year<- as.factor(merged_div$year)

herb_beta_model <- lmer(centroid_dist_habyear_mean ~ log10(Herbivore.Biomass) + 
                          (1 | year) + (1 | site) + (1 | habitat), 
                        data = merged_div)

Anova(herb_beta_model)

#YOU THOUGHT I WAS DONE?
#no

new_diversity_stab <- read.csv(here("data", "algae_diversity_stability_metrics_alltime.csv"))

#jk I don't really know what 'vibe' metric to use for fish to contrast this with

