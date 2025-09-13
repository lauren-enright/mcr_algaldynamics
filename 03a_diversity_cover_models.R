rm(list = ls())
library(here)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)

source("01b_data_prep.R")

#alpha_diversity_quad_macro <- read.csv(here::here("data", "alpha_diversity_quad_macro.csv"))
#alpha_diversity_site_macro <- read.csv(here::here("data", "alpha_diversity_site_macro.csv"))

alpha_diversity_quad_macro$habitat <- factor(alpha_diversity_quad_macro$habitat,
                     levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

#### COVER ~ RICHNESS ####
cover_mod <- glmmTMB(cover_trans ~ richness*habitat + (1|site/location) + (1|year), family = beta_family(), data = alpha_diversity_quad_macro)
summary(cover_mod)
car::Anova(cover_mod)
#                     Chisq Df Pr(>Chisq)    
# richness         30918.96  1  < 2.2e-16 ***
# habitat            141.68  3  < 2.2e-16 ***
# richness:habitat   294.44  3  < 2.2e-16 ***
hist(residuals(cover_mod)) # looks fine
plot(residuals(cover_mod) ~ fitted(cover_mod)) # negative trend but it can't be less than 0 so not too concerned. some wave pattern
performance::r2(cover_mod) # marginal: 0.635, conditional: 0.654
emtrends(cover_mod, pairwise ~ habitat, var = "richness") # backreef not different from forereef, forereef also not different
# contrast                    estimate     SE  df z.ratio p.value
# Fringing - Backreef          0.34715 0.0241 Inf  14.407  <.0001
# Fringing - Forereef 10m      0.34862 0.0240 Inf  14.513  <.0001
# Fringing - Forereef 17m      0.38937 0.0250 Inf  15.593  <.0001
# Backreef - Forereef 10m      0.00147 0.0205 Inf   0.072  0.9999
# Backreef - Forereef 17m      0.04223 0.0214 Inf   1.975  0.1974
# Forereef 10m - Forereef 17m  0.04076 0.0209 Inf   1.947  0.2087

#### COVER ~ FUNCTIONAL RICHNESS ####

cover_mod_fg <- glmmTMB(cover_trans ~ functional_richness*habitat + (1|site/location) + (1|year), family = beta_family(), data = alpha_diversity_quad_macro)


summary(cover_mod_fg)
car::Anova(cover_mod_fg)
#                                 Chisq Df Pr(>Chisq)    
# functional_richness         31565.55  1  < 2.2e-16 ***
# habitat                        49.36  3  1.093e-10 ***
# functional_richness:habitat   444.61  3  < 2.2e-16 ***
hist(residuals(cover_mod_fg)) # good
performance::r2(cover_mod_fg) # margingal = 0.63, conditional = 0.65
emtrends(cover_mod_fg, pairwise ~ habitat, var = "functional_richness") 
# contrast                    estimate     SE  df z.ratio p.value
# Fringing - Backreef            0.220 0.0266 Inf   8.287  <.0001
# Fringing - Forereef 10m        0.352 0.0261 Inf  13.453  <.0001
# Fringing - Forereef 17m        0.531 0.0263 Inf  20.211  <.0001
# Backreef - Forereef 10m        0.131 0.0231 Inf   5.683  <.0001
# Backreef - Forereef 17m        0.310 0.0232 Inf  13.396  <.0001
# Forereef 10m - Forereef 17m    0.179 0.0223 Inf   8.035  <.0001

#pull out data from models to make supplemental figures:

#richness by cover
t.s2.a <- as_tibble(emtrends(cover_mod, pairwise ~ habitat, var = "richness")$emtrends) %>% 
  mutate(Predictor = "Taxonomic richness",
         `Spatial scale` = "Plot-level") %>% 
  # rename this so it matches for joining purposes
  rename_if(str_detect(names(.), ".trend"), ~"Mean")

#functional richness by cover

t.s2.b <- as_tibble(emtrends(cover_mod_fg, pairwise ~ habitat, var = "functional_richness")$emtrends) %>% 
  mutate(Predictor = "Functional richness",
         `Spatial scale` = "Plot-level") %>% 
  rename_if(str_detect(names(.), ".trend"), ~"Mean")

#joining together

rbind(t.s2.a, t.s2.b) %>% 
  mutate(Table = "S2_plotlevel") %>% 
  select(-SE, -df) -> table_s2_plot

table_s2_plot %>% 
  # rename to match previous version/code
  dplyr::rename(Habitat = habitat,
         Lower_CI = asymp.LCL,
         Upper_CI = asymp.UCL) %>%
  # make a blank column to fill in the letter designations manually
  mutate(Letter = NA) %>%
  # save as CSV
  write_csv(here::here("data/supplemental_tables_tableS2_plot.csv"))


