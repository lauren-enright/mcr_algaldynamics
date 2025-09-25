rm(list = ls())
library(here)
library(tidyverse)
library(ggplot2)
library(ggpubr)

#want to have appropriate functions and plot themes
source("00_functions_and_aes.R")

#source("01b_data_prep.R")

#read in cover_df from script 01b 

cover_df <- read.csv(here::here("data", "cover_df_09162025.csv"))


#make habitat a factor
cover_df$habitat <- as.factor(cover_df$habitat)
levels(cover_df$habitat)
#change order

cover_df$habitat <- factor(cover_df$habitat,
                           levels = c("Fringing", "Backreef",
                                      "Forereef 10m", "Forereef 17m"))

#### CREATE MEAN AND SE DATAFRAMES ####


cover_means <- cover_df %>%
  dplyr::group_by(year, habitat) %>%
  dplyr::summarise(macroalgae = mean(macroalgae),
                   coral = mean(coral),
                   turf = mean(turf),
                   cca = mean(cca),
                   sand = mean(sand),
                   other = mean(other)
  )

cover_se <- cover_df %>%
  dplyr::group_by(year, habitat) %>%
  dplyr::summarise(macroalgae = se(macroalgae),
                   coral = se(coral),
                   turf = se(turf),
                   cca = se(cca),
                   sand = se(sand),
                   other = se(other)
  )

# pivot long
cover_long_means <- pivot_longer(
  data = cover_means,
  cols = 3:8,
  names_to = "functional_group",
  values_to = "percent_cover"
)

cover_long_se <- pivot_longer(
  data = cover_se,
  cols = 3:8,
  names_to = "functional_group",
  values_to = "se"
)

cover_long <- left_join(cover_long_means,cover_long_se, by = join_by(year, habitat, functional_group))

# re-label benthic groupings for plotting
cover_long$functional_group <- factor(cover_long$functional_group, 
                                      levels = c("macroalgae", "coral", "turf","cca", "sand", "other"),
                                      labels = c("Macroalgae", "Coral", "Turf", "CCA", "Sand", "Other"))

# subset relevant data
figure_1_data <- subset(cover_long, functional_group != "Sand" & functional_group != "Other")


#### PLOT FIGURE 1 ####
plot_list <- lapply(levels(figure_1_data$habitat), function(hab) {
  ggplot(data = subset(figure_1_data, habitat == hab)) +
    geom_point(aes(x = year, y = percent_cover, colour = functional_group), size = 3) +
    geom_errorbar(aes(x = year, ymin = percent_cover - se, ymax = percent_cover + se, colour = functional_group), 
                  width = 0.1, linewidth = 2) +
    geom_line(aes(x = year, y = percent_cover, colour = functional_group), linewidth = 1) +
    annotate("segment", x = 2006, xend = 2010, y = 75, yend = 75, linewidth = 2, colour = "grey") +
    geom_vline(xintercept = 2010, linetype = 2, colour = "black", size = 1) +
    geom_vline(xintercept = 2019, linetype = 2, colour = "red", size = 1) +
    scale_colour_manual(values = functional_group_colours) +
    scale_x_continuous(breaks = seq(2007, 2023, 2)) +
    labs(y = "Percent Cover",
         x = "Year", title = "") +  
    trends_theme
})

(figure_1 <- ggarrange(plotlist = plot_list, 
                       ncol = 2, nrow = 2, 
                       labels = c("a. Fringing reef", "b. Back reef", "c. Fore reef 10 m", "d. Fore reef 17 m"),
                       common.legend = TRUE,
                       legend = "bottom", 
                       font.label = list(size = 24, color = "black", face = "plain")))


#ggsave(filename = "output/figure1_v6_092252025.jpg", figure_1, height = 10, width = 14)
