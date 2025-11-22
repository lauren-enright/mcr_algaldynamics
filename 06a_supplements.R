
#### Making Supplemental Figures S1 ####

# other supplemental figures have been made throughout the main scripts
## S1 --> in this script
## S2 --> in script 03b
## S3 --> in script 03d
## S4 --> in script 04c
## S5 --> in script 04e
## S6 --> in script 05c
## S7 --> in script 05c 

library(tidyverse)
library(codyn)
library(here)

macro_functional_groups_long <- read.csv(here::here("data", "macroalgalfunctionalgroups_long_09262025.csv"), stringsAsFactors = F)

#### Figure S1: Clock Circle Absolute Abundance Figure ####
# set colors and labels. these will not be used in any other figures

colors_abundance <- c(
  "sargassum_pacificum" = "#543005",
  #"turbinaria_ornata" =  "#7B3F00",
  "turbinaria_ornata" = "#8C510A",
  # "lobophora_variegata" = "#8C510A",
  "lobophora_variegata" = "#BF812D",
  #"dictyota_bartayresiana" =  "#BF812D",
  "dictyota_bartayresiana" = "#DFC27D",
  "peyssonnelia_inamoena" = "#E6B88A",
  "peyssonnelia_sp" = "#F6E8C3",
  "halimeda_sp" = "#C7EAE5",
  "halimeda_minima" = "#80CDC1" ,
  "halimeda_opuntia" = "#35978F",
  "amansia_rhodantha" = "#01665E", 
  "asparagopsis_taxiformis" = "#003C30",
  "Other" = "grey70")

#other colors I considered
#"#5AA6D1"
#"#7BAFD4"
#"#2B83BA"
#"#F1D6A7"
#"#D2C48D"
#"#E6B88A"
#"#E9A972"
#"#704214"
#"#8B4513"
#"#6C3A08"
#"#75420B"
#"#64330A"

# Use a named LIST of expressions for italics
labs_fill <- list(
  "Other" = "Other",
  "sargassum_pacificum" = expression(italic("Sargassum pacificum")),
  "turbinaria_ornata"   = expression(italic("Turbinaria ornata")),
  "lobophora_variegata" = expression(italic("Lobophora variegata")),
  "dictyota_bartayresiana" = expression(italic("Dictyota bartayresiana")),
  "peyssonnelia_inamoena"  = expression(italic("Peyssonnelia inamoena")),
  "peyssonnelia_sp" = expression(italic("Peyssonnelia") ~ "sp."),
  # "peyssonnelia_sp"        = expression(italic("Peyssonnelia sp.")),
  "halimeda_sp" = expression(italic("Halimeda") ~ "sp."),
  "halimeda_minima"        = expression(italic("Halimeda minima")),
  "halimeda_opuntia"       = expression(italic("Halimeda opuntia")),
  "amansia_rhodantha"      = expression(italic("Amansia rhodantha")),
  "asparagopsis_taxiformis"= expression(italic("Asparagopsis taxiformis"))
)

#pull out top 10 abundance, otherwise is messy with all ofthem 
aggdat_hab <- aggregate(prop_cover ~ taxa * year * habitat,  data = subset(macro_functional_groups_long,
                                                                           taxa == "sargassum_pacificum" |
                                                                             taxa == "turbinaria_ornata" | 
                                                                             taxa == "dictyota_bartayresiana"| 
                                                                             taxa == "peyssonnelia_inamoena"| 
                                                                             taxa == "peyssonnelia_sp"| 
                                                                             taxa == "halimeda_minima"| 
                                                                             taxa == "halimeda_sp"| 
                                                                             taxa == "lobophora_variegata"| 
                                                                             taxa == "halimeda_opuntia"| 
                                                                             taxa == "amansia_rhodantha"| 
                                                                             taxa == "asparagopsis_taxiformis"), FUN = mean)

# order the habitats so figure is correct
aggdat_hab$habitat <- factor(aggdat_hab$habitat, levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

aggdat_hab$habitat

(aggdat_hab_supp <- ggplot(aggdat_hab, aes(year, prop_cover, color = taxa)) + 
  # plot species lines
  geom_line(size = 3) + # 3 or 4 works well. 
  # faceted by species
  facet_wrap(~habitat) +
  # on polor coordinates
  coord_polar()  +
  scale_color_manual(values = colors_abundance, labels = labs_fill, drop = FALSE) +
  theme_classic() +
  labs(x = "", y = "Cover", color = "Taxa") +
  theme(
    panel.grid.major = element_line(color = "grey40", linewidth = 0.6),
    panel.grid.minor = element_line(color = "grey40", linewidth = 0.4),
    strip.text   = element_text(size = 25, face = "bold"),   # facet labels
    axis.title   = element_text(size = 25),                  # axis titles
    axis.text    = element_text(size = 20),                  # axis tick labels
    legend.title = element_text(size = 25, face = "bold"),   # legend title
    legend.text  = element_text(size = 20)                   # legend items
  ))


# ggsave(filename = "output/Supp_FigS1_11212025.jpg", aggdat_hab_supp, height = 15, width = 20)





