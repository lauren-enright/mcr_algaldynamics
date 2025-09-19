rm(list = ls())
source("05b_spatial_synchrony_and_bray_models.R")
library(ggplot2)
library(ggpubr)
library(patchwork)


#want habitats to be in order.
dss_spatial_2$habitat <- factor(dss_spatial_2$habitat,
                                levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

#### REGIONAL STABILITY ~ LOCAL STABILITY ####
# SETUP
alpha_gamma_emm <- emmip(alpha_gamma_stab_mod,
                         habitat ~ stability_mean,
                         at = list(stability_mean = seq(0,10,0.01)), plotit = F, CIs = T)  

filtered_ag_effects <- filter_ranges(alpha_gamma_emm, dss_spatial_ranges, "habitat", "stability_mean")

#remind R about correct factor order, otherwise legends won't merge...
filtered_ag_effects$habitat <- factor(filtered_ag_effects$habitat,
                                levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

# PLOT
(alpha_gamma_stability_plot <- 
    ggplot() +
    geom_point(data = dss_spatial_2,
               aes(x = stability_mean, y = cover_stability, colour = habitat), size = 3, alpha = 0.5) + 
    geom_line(data = filtered_ag_effects, aes(x = stability_mean, y = exp(yvar), colour = habitat), linewidth = 1.5) +
    geom_ribbon(data = filtered_ag_effects, aes(x = stability_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours,labels = habitat_labels) +
    scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
    labs(y = "Site-level stability", x = "Mean plot-level stability", title = "") +
#    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    model_themes +
    ylim(c(0.5,5.5))
)

#### REGIONAL STABILITY ~ SPATIAL SYNCHRONY ####
# SETUP
spat_stab_emm <- emmip(spatial_synchrony_mod2,
                       habitat ~ spatial_synchrony,
                       at = list(spatial_synchrony = seq(0,1,0.01)), plotit = F, CIs = T)  

filtered_spat_stab_effects <- filter_ranges(spat_stab_emm, dss_spatial_ranges, "habitat", "spatial_synchrony")

#remind R about correct factor order, otherwise legends won't merge...
filtered_spat_stab_effects$habitat <- factor(filtered_spat_stab_effects$habitat,
                                      levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

# PLOT
(spatial_synchrony_plot <- 
    ggplot() +
    geom_point(data = dss_spatial_2,
               aes(x = spatial_synchrony, y = cover_stability, colour = habitat), size = 3, alpha = 0.5) + 
    geom_line(data = filtered_spat_stab_effects, aes(x = spatial_synchrony, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_spat_stab_effects, aes(x = spatial_synchrony, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
    scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
    labs(y = "Site-level stability", x = "Spatial synchrony", title = "") +
    #    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    model_themes +
    ylim(c(0.5,5.5))
)

#LOOKS DIFFERENT THAN OLD VERISON BC NO INTERACTION 


#### REGIONAL STABILITY : LOCAL STABILITY ~ SPATIAL SYNCHRONY ####
# SETUP

#using if model is by habitat
#ratio_emm <- emmip(ratio_mod,
                   #    habitat ~ spatial_synchrony,
                    #   at = list(spatial_synchrony = seq(0,1,0.01)), plotit = F, CIs = T) 

#filtered_ratio_effects <- filter_ranges(ratio_emm, dss_spatial_ranges, "habitat", "spatial_synchrony")

# using for model not by habitat
ratio_emm2 <- emmip(ratio_mod2,
                   ~ spatial_synchrony,
                   at = list(spatial_synchrony = seq(0,1,0.01)), plotit = F, CIs = T)  

rng_ratio <- range(dss_spatial_2$spatial_synchrony, na.rm = TRUE)
ratio_emm2_filterered <- ratio_emm2 %>% filter(spatial_synchrony >= rng_ratio [1], spatial_synchrony <= rng_ratio [2])



# PLOT - With habitat model
#(ratio_plot <- 
 #   ggplot() +
  #  geom_point(data = dss_spatial_2,
  #             aes(x = spatial_synchrony, y = ratio, colour = habitat), size = 1, alpha = 0.5) + 
  #  geom_line(data = filtered_ratio_effects, aes(x = spatial_synchrony, y = exp(yvar), colour = habitat), size = 1.5) +
  #  geom_ribbon(data = filtered_ratio_effects, aes(x = spatial_synchrony, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
  #              alpha = 0.3) +
  #  scale_colour_manual(values = habitat_colours) +
  #  scale_fill_manual(values = habitat_colours) +
  #  labs(y = "", x = "Spatial synchrony", title = "") +
  #  ylab(expression(frac(Site-level~stability, Mean~plot-level~stability))) +
#    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
  #  model_themes
#)

# PLOT - without habitat model
(ratio_plot2 <- 
    ggplot() +
    geom_point(data = dss_spatial_2,
               aes(x = spatial_synchrony, y = ratio), size = 3, alpha = 0.5) + 
    geom_smooth(method = "loess") +
    geom_line(data = ratio_emm2_filterered, aes(x = spatial_synchrony, y = exp(yvar)), size = 1.5) +
    geom_ribbon(data = ratio_emm2_filterered, aes(x = spatial_synchrony, ymin = exp(LCL), ymax = exp(UCL)),
                alpha = 0.3) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
   #scale_colour_manual(values = habitat_colours) +
    #scale_fill_manual(values = habitat_colours) +
    labs(y = "", x = "Spatial synchrony", title = "") +
    ylab("Site-level stability\n────────────\nMean plot-level stability") +
    #ylab(expression(frac(Site-level~stability, Mean~plot-level~stability))) +
     #   scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    model_themes
)


#### SPATIAL SYNCHRONY ~ BRAY ####
# SETUP
#if using model with habitats
#bray_emm <- emmip(bray_mod,
 #                  habitat ~ mean_bray,
#                   at = list(mean_bray = seq(0,1,0.01)), plotit = F, CIs = T)  


#filtered_bray_effects <- filter_ranges(bray_emm, beta_ranges, "habitat", "mean_bray")

#if using model w/o habitat
bray_emm2 <- emmip(bray_mod2,
                   ~ mean_bray,
                   at = list(mean_bray = seq(0,1,0.01)), plotit = F, CIs = T)   


#Noam, I think this does the same thing as your fancy function, but just for 1 range of values... :) 
rng_bray <- range(beta_spatialsync$mean_bray, na.rm = TRUE)
bray_emm2_filterered <- bray_emm2 %>% filter(mean_bray >= rng_bray[1], mean_bray <= rng_bray[2])



# PLOT - w/ habitat model 
#(bray_plot <- 
  #  ggplot() +
  #  geom_point(data = beta_spatialsync,
  #             aes(x = mean_bray, y = spatial_synchrony, colour = habitat), size = 1, alpha = 0.5) + 
  #  geom_line(data = filtered_bray_effects, aes(x = mean_bray, y = inv_logit(yvar), colour = habitat), size = 1.5) +
  #  geom_ribbon(data = filtered_bray_effects, aes(x = mean_bray, ymin = inv_logit(LCL), ymax = inv_logit(UCL), fill = habitat),
  #               alpha = 0.3) +
  #  scale_colour_manual(values = habitat_colours) +
  #  scale_fill_manual(values = habitat_colours) +
  #  labs(y = "Spatial synchrony", x = "Mean Bray dissimilarity", title = "") +
 #   scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
 #   model_themes
#)

# plot without habitat model

(bray_plot <- 
    ggplot() +
    geom_point(data = beta_spatialsync,
               aes(x = mean_bray, y = spatial_synchrony), size = 3, alpha = 0.5) + 
    geom_line(data = bray_emm2_filterered, aes(x = mean_bray, y = inv_logit(yvar)), size = 1.5) +
    geom_ribbon(data = bray_emm2_filterered, aes(x = mean_bray, ymin = inv_logit(LCL), ymax = inv_logit(UCL)),
                alpha = 0.3) +
   # scale_colour_manual(values = habitat_colours) +
    #scale_fill_manual(values = habitat_colours) +
    labs(y = "Spatial synchrony", x = "Mean Bray–Curtis dissimilarity", title = "") +
    #   scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    xlim(c(0.2,0.9)) +
    model_themes
)


#### PANEL GRAPH ####
(figure_4 <- ggarrange(alpha_gamma_stability_plot, spatial_synchrony_plot, ratio_plot2, bray_plot,
                       ncol = 2, nrow = 2, 
                       common.legend = TRUE,
                       legend = "bottom", 
                       labels = c("a.", "b.", "c.", "d."),
                       font.label = list(size = 26, color = "black", face = "plain")))

ggsave(filename = "output/figure4_09192025.png", figure_4, height = 18, width = 18)


#### Figure S6: #####

supplemental_tableS6 %>% 
  filter(Table == "S6") %>% 
  filter(Predictor == "Mean plot-level stability") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours, label = habitat_labels) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(a) Mean plot-level stability") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.10 , y = Habitat, 
                label = .group), colour = "black", size = 10) +
  guides(color = guide_legend(reverse = TRUE)) +
  #theme(legend.position = "none") +
  scale_y_discrete(labels = c(
    "Forereef 17m" = "Fore reef 17 m",
    "Forereef 10m" = "Fore reef 10 m",
    "Backreef"     = "Back reef",
    "Fringing"     = "Fringing reef")) -> s6.a

supplemental_tableS6 %>% 
  filter(Table == "S6") %>% 
  filter(Predictor == "Spatial synchrony") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours, label = habitat_labels) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(b) Spatial synchrony") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.10 , y = Habitat, 
                label = .group), colour = "black", size = 10) +
  guides(color = guide_legend(reverse = TRUE)) +
  scale_y_discrete(labels = c(
    "Forereef 17m" = "Fore reef 17 m",
    "Forereef 10m" = "Fore reef 10 m",
    "Backreef"     = "Back reef",
    "Fringing"     = "Fringing reef")) -> s6.b
    
supplement_S6 <- s6.a + s6.b +   
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

#ggsave(filename = "output/Supp_FigS6.png", supplement_S6, height = 10, width = 20)

#### Figure S7: Box Plots of Spatial Synchrony by Habitat ####

#clean up data for plotting purposes and plot

supplement_S7 <- dss_spatial_2 %>%
  ggplot(aes(x = habitat, y = spatial_synchrony, color = habitat)) +
  geom_boxplot() +
  geom_point(shape = 1) +
  scale_colour_manual(values = habitat_colours,labels = habitat_labels) +
  #scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
  labs(y = "Spatial synchrony", x = "", title = "") +
  #    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
  model_themes

#ggsave(filename = "output/Supp_FigS7.png", supplement_S7, height = 10, width = 15)
