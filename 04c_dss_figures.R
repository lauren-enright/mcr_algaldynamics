source("04b_dss_models.R")
library(ggplot2)
library(ggpubr)

#### STABILITY ~ RICHNESS ####
# SETUP
rich_stab_emm <- emmip(rich_stab_mod,
                       habitat ~ richness_mean,
                       at = list(richness_mean = seq(0,10,0.01)), plotit = F, CIs = T)

filtered_rich_stab_effects <- filter_ranges(rich_stab_emm, dss_ranges, "habitat", "richness_mean")

# PLOT
(figure_4a <- 
    ggplot() +
    geom_point(data = diversity_stability_synchrony,
               aes(x = richness_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_rich_stab_effects, aes(x = richness_mean, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_rich_stab_effects, aes(x = richness_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
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

# PLOT
(figure_4b <- 
    ggplot() +
    geom_point(data = diversity_stability_synchrony,
               aes(x = functional_richness_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_rich_stab_effects_fg, aes(x = functional_richness_mean, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_rich_stab_effects_fg,
                aes(x = functional_richness_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
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
(figure_4c <- 
    ggplot() +
    geom_point(data = diversity_stability_synchrony,
               aes(x = synchrony, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_synch_stab_effects, aes(x = synchrony, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_synch_stab_effects, aes(x = synchrony, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(x = "Synchrony", y = "", title = "") +
    model_themes
)
# ggsave(filename = "output/figure_4c.png", figure_4c, height = 10, width = 14)

#### PANEL GRAPH ####
(figure_4 <- ggarrange(figure_4a, figure_4b, figure_4c, 
                       ncol = 3, nrow = 1, 
                       common.legend = TRUE,
                       heights = c(1,1),
                       widths = c(1,1,1),
                       legend = "bottom", 
                       labels = c("a.", "b.", "c.", "d.", "e.", "f."),
                       font.label = list(size = 26, color = "black", face = "plain")))

# ggsave(filename = "output/figure_4v3.png", figure_4, height = 6, width = 18)
