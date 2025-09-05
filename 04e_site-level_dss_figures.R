source("04d_dss_models.R")
library(ggplot2)
library(ggpubr)

#### STABILITY ~ RICHNESS ####
# SETUP
rich_stab_emm_site <- emmip(rich_stab_mod_site,
                            habitat ~ richness_mean,
                            at = list(richness_mean = seq(0,10,0.01)), plotit = F, CIs = T)

filtered_rich_stab_effects_site <- filter_ranges(rich_stab_emm_site, dss_ranges_site, "habitat", "richness_mean")

# PLOT
(figure_4d <- 
   ggplot() +
   geom_point(data = diversity_stability_synchrony_site,
              aes(x = richness_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
   geom_line(data = filtered_rich_stab_effects_site, aes(x = richness_mean, y = exp(yvar), colour = habitat), size = 1.5) +
   geom_ribbon(data = filtered_rich_stab_effects_site, aes(x = richness_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
               alpha = 0.3) +
   scale_colour_manual(values = habitat_colours) +
   scale_fill_manual(values = habitat_colours) +
   scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
   labs(y = "Site-level Stability\n", x = "Taxonomic richness", title = "") +
   model_themes
)

# ggsave(filename = "output/figure_4d.png", figure_4d, height = 10, width = 14)

#### STABILITY ~ FUNCTIONAL RICHNESS ####
# SETUP
rich_stab_emm_site_fg <- emmip(rich_stab_mod_site_fg,
                               habitat ~ functional_richness_mean,
                               at = list(functional_richness_mean = seq(0,10,0.01)), plotit = F, CIs = T)

filtered_rich_stab_effects_site <- filter_ranges(rich_stab_emm_site_fg, dss_ranges_site, "habitat", "functional_richness_mean")

# PLOT
(figure_4e <- 
    ggplot() +
    geom_point(data = diversity_stability_site,
               aes(x = functional_richness_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_rich_stab_effects_site, aes(x = functional_richness_mean, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_rich_stab_effects_site, 
                aes(x = functional_richness_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    scale_x_continuous(limits = c(2, 4.2), breaks = c(1,2,3,4)) +
    labs(y = "", x = "Functional richness", title = "") +
    model_themes
)

#ggsave(filename = "output/figure_4e_v2.png", figure_4e, height = 10, width = 14)

#### STABILITY ~ SYNCHRONY ####
# SETUP
synch_stab_emm_site <- emmip(synch_stab_mod_site,
                             habitat ~ synchrony,
                             at = list(synchrony = seq(0,1,0.01)), plotit = F, CIs = T)

filtered_synch_stab_effects_site <- filter_ranges(synch_stab_emm_site, dss_ranges_site, "habitat", "synchrony")

# PLOT
(figure_4f <- 
    ggplot() +
    geom_point(data = diversity_stability_synchrony_site,
               aes(x = synchrony, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_synch_stab_effects_site, aes(x = synchrony, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_synch_stab_effects_site, aes(x = synchrony, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(x = "Synchrony", y = "", title = "") +
    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    model_themes
)
# ggsave(filename = "output/figure_4f.png", figure_4f, height = 10, width = 14)

#### PANEL GRAPH ####
(site_dss <- ggarrange(figure_4d, figure_4e, figure_4f, 
                       ncol = 3, nrow = 1, 
                       common.legend = TRUE,
                       heights = c(1,1),
                       widths = c(1,1,1),
                       legend = "bottom", 
                       labels = c("a.", "b.", "c.", "d.", "e.", "f."),
                       font.label = list(size = 26, color = "black", face = "plain")))

# ggsave(filename = "output/site_dss.png", site_dss, height = 6, width = 18)
