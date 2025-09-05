rm(list = ls())
source("05b_spatial_synchrony_and_bray_models.R")
library(ggplot2)
library(ggpubr)

#### REGIONAL STABILITY ~ LOCAL STABILITY ####
# SETUP
alpha_gamma_emm <- emmip(alpha_gamma_stab_mod,
                         habitat ~ stability_mean,
                         at = list(stability_mean = seq(0,10,0.01)), plotit = F, CIs = T)  

filtered_ag_effects <- filter_ranges(alpha_gamma_emm, dss_spatial_ranges, "habitat", "stability_mean")

# PLOT
(alpha_gamma_stability_plot <- 
    ggplot() +
    geom_point(data = dss_spatial_2,
               aes(x = stability_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_ag_effects, aes(x = stability_mean, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_ag_effects, aes(x = stability_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(y = "Site-level stability", x = "Mean plot-level stability", title = "") +
#    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    model_themes
)

#### REGIONAL STABILITY ~ SPATIAL SYNCHRONY ####
# SETUP
spat_stab_emm <- emmip(spatial_synchrony_mod,
                       habitat ~ spatial_synchrony,
                       at = list(spatial_synchrony = seq(0,1,0.01)), plotit = F, CIs = T)  

filtered_spat_stab_effects <- filter_ranges(spat_stab_emm, dss_spatial_ranges, "habitat", "spatial_synchrony")

# PLOT
(spatial_synchrony_plot <- 
    ggplot() +
    geom_point(data = dss_spatial_2,
               aes(x = spatial_synchrony, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_spat_stab_effects, aes(x = spatial_synchrony, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_spat_stab_effects, aes(x = spatial_synchrony, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(y = "Site-level stability", x = "Spatial synchrony", title = "") +
#    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    model_themes
)

#### REGIONAL STABILITY : LOCAL STABILITY ~ SPATIAL SYNCHRONY ####
# SETUP
ratio_emm <- emmip(ratio_mod,
                       habitat ~ spatial_synchrony,
                       at = list(spatial_synchrony = seq(0,1,0.01)), plotit = F, CIs = T)  

filtered_ratio_effects <- filter_ranges(ratio_emm, dss_spatial_ranges, "habitat", "spatial_synchrony")

# PLOT
(ratio_plot <- 
    ggplot() +
    geom_point(data = dss_spatial_2,
               aes(x = spatial_synchrony, y = ratio, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_ratio_effects, aes(x = spatial_synchrony, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_ratio_effects, aes(x = spatial_synchrony, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(y = "", x = "Spatial synchrony", title = "") +
    ylab(expression(frac(Site-level~stability, Mean~plot-level~stability))) +
#    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    model_themes
)

#### PANEL GRAPH ####
(figure_4 <- ggarrange(alpha_gamma_stability_plot, spatial_synchrony_plot, ratio_plot,
                       ncol = 3, nrow = 1, 
                       common.legend = TRUE,
                       legend = "bottom", 
                       labels = c("a.", "b.", "c."),
                       font.label = list(size = 26, color = "black", face = "plain")))

# ggsave(filename = "output/region_stability_figure.png", figure_4, height = 6, width = 18)

#### SPATIAL SYNCHRONY ~ BRAY ####
# SETUP
bray_emm <- emmip(bray_mod,
                   habitat ~ mean_bray,
                   at = list(mean_bray = seq(0,1,0.01)), plotit = F, CIs = T)  

filtered_bray_effects <- filter_ranges(bray_emm, beta_ranges, "habitat", "mean_bray")

# PLOT
(bray_plot <- 
    ggplot() +
    geom_point(data = beta_spatialsync,
               aes(x = mean_bray, y = spatial_synchrony, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_bray_effects, aes(x = mean_bray, y = inv_logit(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_bray_effects, aes(x = mean_bray, ymin = inv_logit(LCL), ymax = inv_logit(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours) +
    scale_fill_manual(values = habitat_colours) +
    labs(y = "Spatial synchrony", x = "Mean Bray dissimilarity", title = "") +
 #   scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    model_themes
)
