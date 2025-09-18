
rm(list = ls())
source("03c_diversity_cover_site_models.R")
library(here)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(ggplot2)
library(ggpubr)

unique(alpha_diversity_site_macro$habitat)


#### Taxonomic Richness Setup ####

diversity_ranges_site <- extract_ranges(alpha_diversity_site_macro, "habitat", c("richness", "functional_richness", "cover_trans"))

site_cover_emm <- emmip(site_cover_mod2,
                        habitat ~ richness,
                        at = list(richness = seq(0,17,1)), plotit = F, CIs = T)

filtered_site_cover_effects <- filter_ranges(site_cover_emm, diversity_ranges_site, "habitat", "richness")

#since we are removing raw data, need to remind R what order we want the legend in

filtered_site_cover_effects$habitat <- factor(filtered_site_cover_effects$habitat,
                                             levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))


### Taxonomic Richness Figure - Figure S3a ####

(figure_S3a <- 
   ggplot() +
  # geom_point(data = alpha_diversity_site_macro,
      #        aes(x = richness, y = cover_trans, colour = habitat), size = 1, alpha = 0.3) + 
   geom_line(data = filtered_site_cover_effects, aes(x = richness, y = inv_logit(yvar), colour = habitat), size = 1.5) +
   geom_ribbon(data = filtered_site_cover_effects, aes(x = richness, ymin = inv_logit(LCL), ymax = inv_logit(UCL), fill = habitat),
               alpha = 0.3) +
   scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
   scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
   labs(y = "Proportional Cover\n", x = "Taxonomic Richness", title = "") +
   scale_y_continuous(
     limits = c(0, 0.4),               # force axis range
     breaks = c(0, 0.2, 0.4)) + 
   trends_theme +
   theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
)
# ggsave(filename = "output/figure_3d.png", figure_3d, height = 10, width = 14)


#### Functional Richness Setup ####

site_cover_fg_emm <- emmip(site_cover_mod_fg2,
                           habitat ~ functional_richness,
                           at = list(functional_richness = seq(0,17,1)), plotit = F, CIs = T)


filtered_site_fg_cover_effects <- filter_ranges(site_cover_fg_emm, diversity_ranges_site, "habitat", "functional_richness")

#since we are removing raw data, need to remind R what order we want the legend in

filtered_site_fg_cover_effects$habitat <- factor(filtered_site_fg_cover_effects$habitat,
                                              levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))


### Functional Richness Figure -  Figure S3b ####

(figure_S3b <- 
    ggplot() +
    #geom_point(data = alpha_diversity_site_macro,
           #    aes(x = functional_richness, y = cover_trans, colour = habitat), size = 1, alpha = 0.3) + 
    geom_line(data = filtered_site_fg_cover_effects, aes(x = functional_richness, y = inv_logit(yvar), colour = habitat),
              linewidth = 1.5) +
    geom_ribbon(data = filtered_site_fg_cover_effects, aes(x = functional_richness, ymin = inv_logit(LCL), ymax = inv_logit(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(y = "", x = "Functional Richness", title = "") +
   scale_y_continuous(
     limits = c(0, 0.4),               # force axis range
     breaks = c(0, 0.2, 0.4)) + 
    trends_theme +
   theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
)
#ggsave(filename = "output/figure_3e_v2.png", figure_3e, height = 10, width = 14)

(figure_S3 <- ggarrange(figure_S3a, figure_S3b,  
                       ncol = 2, nrow = 1, 
                       common.legend = TRUE,
                       labels = c("a.", "b."),
                       legend = "bottom", 
                       font.label = list(size = 26, color = "black", face = "plain")))

#ggsave(filename = "output/figure_S3_v2_09182025.png", figure_S3, height = 10, width = 18)


