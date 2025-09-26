source("04d_site-level_dss_models.R")
library(ggplot2)
library(ggpubr)


#### STABILITY ~ RICHNESS ####
# SETUP
rich_stab_emm_site <- emmip(rich_stab_mod_site,
                            habitat ~ richness_mean,
                            at = list(richness_mean = seq(0,10,0.01)), plotit = F, CIs = T)

filtered_rich_stab_effects_site <- filter_ranges(rich_stab_emm_site, dss_ranges_site, "habitat", "richness_mean")

# PLOT
(figure_s5a <- 
   ggplot() +
   geom_point(data = diversity_stability_synchrony_site,
              aes(x = richness_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
   geom_line(data = filtered_rich_stab_effects_site, aes(x = richness_mean, y = exp(yvar), colour = habitat), size = 1.5) +
   geom_ribbon(data = filtered_rich_stab_effects_site, aes(x = richness_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
               alpha = 0.3) +
   scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
   scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
   scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
   labs(y = "Site-level Stability", x = "Taxonomic richness", title = "") +
   model_themes
  #theme(axis.title.y = element_text(size = 30))
)

# ggsave(filename = "output/figure_4d.png", figure_4d, height = 10, width = 14)

#### STABILITY ~ FUNCTIONAL RICHNESS ####
# SETUP
rich_stab_emm_site_fg <- emmip(rich_stab_mod_site_fg,
                               habitat ~ functional_richness_mean,
                               at = list(functional_richness_mean = seq(0,10,0.01)), plotit = F, CIs = T)

filtered_rich_stab_effects_site <- filter_ranges(rich_stab_emm_site_fg, dss_ranges_site, "habitat", "functional_richness_mean")

# PLOT
(figure_s5b <- 
    ggplot() +
    geom_point(data = diversity_stability_synchrony_site,
               aes(x = functional_richness_mean, y = cover_stability, colour = habitat), size = 1, alpha = 0.5) + 
    geom_line(data = filtered_rich_stab_effects_site, aes(x = functional_richness_mean, y = exp(yvar), colour = habitat), size = 1.5) +
    geom_ribbon(data = filtered_rich_stab_effects_site, 
                aes(x = functional_richness_mean, ymin = exp(LCL), ymax = exp(UCL), fill = habitat),
                alpha = 0.3) +
    scale_colour_manual(values = habitat_colours, labels = habitat_labels) +
    scale_fill_manual(values = habitat_colours, labels = habitat_labels) +
    scale_y_continuous(limits = c(0.5, 4.2), breaks = c(1,2,3,4)) +
    scale_x_continuous(limits = c(1.75, 4.2), breaks = c(1,2,3,4)) +
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
(figure_s5c <- 
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



# ggsave(filename = "output/site_dss.png", site_dss, height = 6, width = 18)

#supplemental tables -- emtrends output 

#Site-level taxonomic richness 

supplemental_tables_tableS5_plot %>% 
  filter(Table == "S5") %>% 
  filter(Predictor == "Site-level taxonomic-richness") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours) +
  # if you want the 0 reference:
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "gray") +
  xlab("Coefficients of site-level\n taxonomic richness") +
  ylab("") +
  ggtitle("") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.05 , y = Habitat, 
                label = .group), colour = "black", size =8) +
  scale_y_discrete(labels = c(
    "Fringing"     = "Fringing reef",
    "Backreef"     = "Back reef",
    "Forereef 10m" = "Fore reef 10 m",
    "Forereef 17m" = "Forereef 17 m"
  )) +
  theme(legend.position = "none",
        #axis.text.y = element_text(size = 27, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(size = 20)) -> s5.taxon.d

#Site-level functional richness

supplemental_tables_tableS5_plot %>% 
  filter(Table == "S5") %>% 
  filter(Predictor == "Site-level functional richness") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours) +
  # if you want the 0 reference:
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "gray") +
  xlab("Coefficients of site-level\n functional richness") +
  ylab("") +
  ggtitle("") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.1 , y = Habitat, 
                label = .group), colour = "black", size = 8) +
  scale_y_discrete(labels = c(
    "Fringing"     = "Fringing reef",
    "Backreef"     = "Back reef",
    "Forereef 10m" = "Fore reef 10 m",
    "Forereef 17m" = "Forereef 17 m"
  )) +
  theme(legend.position = "none",
      # axis.text.y = element_blank(),
      #  axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 20)) -> s5.functional.e

#Site-level species synchrony

supplemental_tables_tableS5_plot  %>% 
  filter(Table == "S5") %>% 
  filter(Predictor == "Site-level species synchrony") %>% 
  # order the habitats by distance from shore
  mutate(Habitat = factor(Habitat, levels = c("Forereef 17m", "Forereef 10m",  "Backreef", "Fringing"))) %>% 
  ggplot(aes(x = Mean, y = Habitat, color = Habitat)) +
  geom_point(size = 6) +
  # add confidence intervals:
  geom_linerange(aes(xmin = Lower_CI, xmax = Upper_CI), linewidth = 2.5) +
  scale_colour_manual(values = habitat_colours) +
  # if you want the 0 reference:
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, color = "gray") +
  xlab("Coefficients of site-level\n species synchrony") +
  ylab("") +
  ggtitle("") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.4 , y = Habitat, 
                label = .group), colour = "black", size = 8) +
  scale_y_discrete(labels = c(
    "Fringing"     = "Fringing reef",
    "Backreef"     = "Back reef",
    "Forereef 10m" = "Fore reef 10 m",
    "Forereef 17m" = "Forereef 17 m"
  )) +
  theme(legend.position = "none",
      # axis.text.y = element_blank(),
       # axis.ticks.y = element_blank(),
        axis.text.y = element_text(size = 20)) -> s5.synch.f

# pull all 6 panels together...

(supplement5_site_dss <- ggarrange(figure_s5a, figure_s5b, figure_s5c, 
                                    s5.taxon.d, s5.functional.e, s5.synch.f,
                       ncol = 3, nrow = 2, 
                       common.legend = TRUE,
                       heights = c(1.5,1.5),
                       widths = c(2.5,2.5,2.5),
                       legend = "bottom", 
                       labels = c("a.", "b.", "c.", "d.", "e.", "f."),
                       font.label = list(size = 26, color = "black", face = "plain")))


#ggsave(filename = "output/Supp_FigS5.png", supplement5_site_dss, height = 22, width = 18)

