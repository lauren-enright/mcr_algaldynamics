source("04b_dss_models.R")
library(ggplot2)
library(ggpubr)
library(patchwork)

supplemental_tables_tableS4_plot <- read.csv(here::here("data", "supplemental_tables_tableS4_plotlevel.csv"))

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
  scale_colour_manual(values = habitat_colours) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(a) Taxonomic richness") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.01 , y = Habitat, 
                label = Letter), colour = "black", size = 10) +
  theme(legend.position = "none") -> s4.taxon

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
  scale_colour_manual(values = habitat_colours) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(b) Functional richness") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.01 , y = Habitat, 
                label = Letter), colour = "black", size = 10) +
  theme(legend.position = "none") -> s4.functional

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
  scale_colour_manual(values = habitat_colours) +
  xlab("Coefficient") +
  ylab("") +
  ggtitle("(c) Synchrony") +
  model_themes + 
  # add letters denoting significance
  geom_text(aes(x = Upper_CI + 0.10 , y = Habitat, 
                label = Letter), colour = "black", size = 10) -> s4.synch

s4_figure <- s4.taxon / s4.functional / s4.synch


ggsave(filename = "output/figure_s4_plotlevel.png", s4_figure, height = 17, width = 13)
#if you make it narrower than 13 it cuts off the legend



