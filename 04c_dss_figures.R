library(ggplot2)
library(ggpubr)
library(patchwork)

#require this for model output
source("04b_dss_models.R")


#### STABILITY ~ RICHNESS ####
# SETUP
rich_stab_emm <- emmip(rich_stab_mod,
                       habitat ~ richness_mean,
                       at = list(richness_mean = seq(0,10,0.01)), plotit = F, CIs = T)

filtered_rich_stab_effects <- filter_ranges(rich_stab_emm, dss_ranges, "habitat", "richness_mean")

any(is.na(diversity_stability_synchrony$richness_mean))  #false
sum(is.na(diversity_stability_synchrony$cover_stability)) #16
any(is.infinite(diversity_stability_synchrony$richness_mean)) #false
any(is.infinite(diversity_stability_synchrony$cover_stability)) #false

diversity_stability_synchrony %>% filter(is.na(cover_stability))

## NOAM --> is it right that there were only 16 quads through time that don't have any algae cover?
#Warrning message:
#Removed 16 rows containing missing values or values outside the scale range (`geom_point()`) --> these are NAs in the stability column

# PLOT
(figure_3a <- 
    ggplot() +
    geom_point(data = diversity_stability_synchrony,
               aes(x = richness_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_rich_stab_effects, aes(x = richness_mean, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_rich_stab_effects, aes(x = richness_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
    scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
    labs(y = "Stability\n", x = "Taxonomic richness", title = "") +
    model_themes
)

# ggsave(filename = "output/figure_4a.png", figure_4a, height = 10, width = 14)

#### STABILITY ~ FUNCTIONAL RICHNESS ####
# SETUP
rich_stab_emm_fg <- emmip(rich_stab_mod_fg,
                          habitat ~ functional_richness_mean,
                          at = list(functional_richness_mean = seq(0,10,0.01)), plotit = F, CIs = T)

filtered_rich_stab_effects_fg <- filter_ranges(rich_stab_emm_fg, dss_ranges, "habitat", "functional_richness_mean")


## NOAM --> is it right that there were only 16 quads through time that don't have any algae cover? 
#Warrning message:
#Removed 16 rows containing missing values or values outside the scale range (`geom_point()`). -->  these are NAs in the stability column

# PLOT
(figure_3b <- 
    ggplot() +
    geom_point(data = diversity_stability_synchrony,
               aes(x = functional_richness_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_rich_stab_effects_fg, aes(x = functional_richness_mean, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_rich_stab_effects_fg,
                aes(x = functional_richness_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
    scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
    scale_x_continuous(breaks = c(0,1,2)) +
    labs(y = "", x = "Functional richness", title = "") +
    model_themes
)

#ggsave(filename = "output/figure_4b_v2.png", figure_4b, height = 10, width = 14)

#### STABILITY ~ SYNCHRONY ####
# SETUP
synch_stab_emm <- emmip(synch_stab_mod,
                        habitat ~ synchrony,
                        at = list(synchrony = seq(0,1,0.01)), plotit = F, CIs = T)

filtered_synch_stab_effects <- filter_ranges(synch_stab_emm, dss_ranges, "habitat", "synchrony")

# PLOT
(figure_3c <- 
    ggplot() +
    geom_point(data = diversity_stability_synchrony,
               aes(x = synchrony, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_synch_stab_effects, aes(x = synchrony, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_synch_stab_effects, aes(x = synchrony, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
    scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
    labs(x = "Synchrony", y = "", title = "") +
    model_themes
)
# ggsave(filename = "output/figure_4c.png", figure_4c, height = 10, width = 14)

#same 16 rows removal warning... 

#### PANEL GRAPH ####
(figure_3 <- ggarrange(figure_3a, figure_3b, figure_3c, 
                       ncol = 3, nrow = 1, 
                       common.legend = TRUE,
                       heights = c(1,1),
                       widths = c(1,1,1),
                       legend = "bottom", 
                       labels = c("a.", "b.", "c.", "d.", "e.", "f."),
                       font.label = list(size = 26, color = "black", face = "plain")))

#ggsave(filename = "output/figure3_v5_09252025.jpg", figure_3, height = 6, width = 18)

# Supplemental figure S4

#taxonomic
supplemental_tables_tableS4_plot %>% 
  filter(Table == "S4") %>% 
  filter(Predictor == "Taxonomic richness") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  #facet_wrap(~ Predictor) +
  scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(a) Taxonomic richness") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.01 , y = Habitat, 
                label = .group), colour = "black", size = 10) +
  theme(legend.position = "none") +
  guides(color = guide_legend(reverse = TRUE)) -> s4.taxon

#functional
supplemental_tables_tableS4_plot %>% 
  filter(Table == "S4") %>% 
  filter(Predictor == "Functional richness") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(b) Functional richness") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.01 , y = Habitat, 
                label = .group), colour = "black", size = 10) +
  theme(legend.position = "none") +
  guides(color = guide_legend(reverse = TRUE)) -> s4.functional

#Synchrony
supplemental_tables_tableS4_plot %>% 
  filter(Table == "S4") %>% 
  filter(Predictor == "Synchrony") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours, label = habitat_labels) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(c) Synchrony") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.10 , y = Habitat, 
                label = .group), colour = "black", size = 10) +
  guides(color = guide_legend(reverse = TRUE)) -> s4.synch

s4_figure <- s4.taxon / s4.functional / s4.synch


#ggsave(filename = "output/figure_s4_plotlevel_v2_09172025.png", height = 17, width = 13)
#if you make it narrower than 13 it cuts off the legend



