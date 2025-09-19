rm(list = ls())
source("03a_diversity_cover_models.R")
library(here)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(ggplot2)
library(patchwork)
library(ggpubr)


#### COVER ~ RICHNESS ####
# SETUP

diversity_ranges_quad <- extract_ranges(alpha_diversity_quad_macro, "habitat", c("richness", "cover_trans", "functional_richness"))

cover_emm <- emmip(cover_mod,
                   habitat ~ richness,
                   at = list(richness = seq(0,10,1)), plotit = F, CIs = T)



filtered_cover_effects <- filter_ranges(trend = cover_emm, range_obj = diversity_ranges_quad, group = "habitat", value = "richness")

# PLOT
(figure_2a <- 
    ggplot() +
#    geom_point(data = alpha_diversity_quad_macro,
#               aes(x = richness, y = cover_trans, colour = habitat), size = 1, alpha = 0.75) + 
    geom_line(data = filtered_cover_effects, aes(x = richness, y = inv_logit(yvar), colour = habitat)) +
    geom_ribbon(data = filtered_cover_effects, aes(x = richness, ymin = inv_logit(LCL), ymax = inv_logit(UCL), fill = habitat),
                alpha = 0.5) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(y = "Cover\n", x = "Taxonomic richness", title = "") +
    model_themes 
)

#### COVER ~ FUNCTIONAL RICHNESS ####
# SETUP
cover_emm_fg <- emmip(cover_mod_fg,
                      habitat ~ functional_richness,
                      at = list(functional_richness = seq(0,10,1)), plotit = F, CIs = T)

filtered_cover_effects_fg <- filter_ranges(cover_emm_fg, diversity_ranges_quad, "habitat", "functional_richness")

# PLOT
(figure_2b <- 
    ggplot() +
    #geom_jitter(data = alpha_diversity_quad_macro,
             #  aes(x = functional_richness, y = cover_trans, colour = habitat), width = 0.2, size = 1, alpha = 0.1) + 
    geom_line(data = filtered_cover_effects_fg, aes(x = functional_richness, y = inv_logit(yvar), colour = habitat)) +
    geom_ribbon(data = filtered_cover_effects_fg, aes(x = functional_richness, ymin = inv_logit(LCL), ymax = inv_logit(UCL), fill = habitat),
                alpha = 0.5) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(y = "", x = "Functional richness", title = "") +
    model_themes
)
#ggsave(filename = "output/figure_3b_quad_v2.png", figure_3b, height = 10, width = 14)

#### PANEL GRAPH ####

(figure_2 <- ggarrange(figure_2a, figure_2b,  
                       ncol = 2, nrow = 1, 
                       common.legend = TRUE,
                       labels = c("a.", "b."),
                       legend = "bottom", 
                       font.label = list(size = 26, color = "black", face = "plain")) )

#ggsave(filename = "output/figure_2_09182025.png", figure_2, height = 8, width = 16)

#adding supplemental figures

#want taxonomic richness as first panel to match

supplemental_tables_tableS2_plot <- supplemental_tables_tableS2_plot %>%
  mutate(Predictor = factor(Predictor, 
                          levels = c("Taxonomic richness", "Functional richness")))

supplemental_tables_tableS2_plot %>% 
  filter(Table == "S2_plotlevel") %>% 
  filter(Predictor == "Taxonomic richness") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours, label = habitat_labels) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(a) Taxonomic Richness") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.10 , y = Habitat, 
                label = .group), colour = "black", size = 10) +
  guides(color = guide_legend(reverse = TRUE)) -> s2.taxo

supplemental_tables_tableS2_plot %>% 
  filter(Table == "S2_plotlevel") %>% 
  filter(Predictor == "Functional richness") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours, label = habitat_labels) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(a) Functional Richness") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.10 , y = Habitat, 
                label = .group), colour = "black", size = 10) +
  guides(color = guide_legend(reverse = TRUE)) -> s2.functional

s2_figure <- s2.taxo + s2.functional +  plot_layout(guides = "collect") &          # collect legends into one
  theme(legend.position = "bottom")  


supplemental_tables_tableS2_plot %>% 
  #filter(Table == "S2") %>% 
  #filter(`Spatial scale` == "Plot-level") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  facet_wrap(~ Predictor) +
  scale_colour_manual(values = habitat_colours) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(a) Plot-level") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.03 , y = Habitat, 
                label = .group), colour = "black", size = 10) +
  theme(legend.position = "none",
          strip.text = element_text(size = 16, face = "bold")  # change size/style
        ) -> s2.plt.level

ggsave(filename = "output/Supp_FigS2.png", s2_figure, height = 10, width = 22)


