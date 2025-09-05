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
library(ggpubr)



#### COVER ~ RICHNESS ####
# SETUP

diversity_ranges_quad <- extract_ranges(alpha_diversity_quad_macro, "habitat", c("richness", "cover_trans", "functional_richness"))

cover_emm <- emmip(cover_mod,
                   habitat ~ richness,
                   at = list(richness = seq(0,10,1)), plotit = F, CIs = T)



filtered_cover_effects <- filter_ranges(trend = cover_emm, range_obj = diversity_ranges_quad, group = "habitat", value = "richness")

# PLOT
(figure_3a <- 
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
# ggsave(filename = "output/figure_3a.png", figure_3a, height = 10, width = 14)

#### COVER ~ FUNCTIONAL RICHNESS ####
# SETUP
cover_emm_fg <- emmip(cover_mod_fg,
                      habitat ~ functional_richness,
                      at = list(functional_richness = seq(0,10,1)), plotit = F, CIs = T)

filtered_cover_effects_fg <- filter_ranges(cover_emm_fg, diversity_ranges_quad, "habitat", "functional_richness")

# PLOT
(figure_3b <- 
    ggplot() +
    geom_jitter(data = alpha_diversity_quad_macro,
               aes(x = functional_richness, y = cover_trans, colour = habitat), width = 0.2, size = 1, alpha = 0.1) + 
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

(figure_3 <- ggarrange(figure_3a, figure_3b,  
                       ncol = 2, nrow = 1, 
                       common.legend = TRUE,
                       labels = c("a.", "b."),
                       legend = "bottom", 
                       font.label = list(size = 26, color = "black", face = "plain")) )

# ggsave(filename = "output/figure_3v3.png", figure_3, height = 8, width = 16)
