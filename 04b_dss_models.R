rm(list = ls())
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(codyn)
library(multcomp)

source("04a_calculate_synchrony.R")

#read in data produced in 04a_calculate_synchrony script
#diversity_stability_synchrony <- read.csv(here::here("data", "full_plot_level_dss.csv")) 
#diversity_stability_synchrony_site <- read.csv(here::here("data", "diversity_stability_synchrony_site.csv"))

#want habitats to be in order.
diversity_stability_synchrony$habitat <- factor(diversity_stability_synchrony$habitat,
                                             levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

#### STABILITY ~ RICHNESS ####
rich_stab_mod <- glmmTMB(cover_stability~ richness_mean*habitat + (1|site), family = Gamma(link = "log"), data = diversity_stability_synchrony)
summary(rich_stab_mod)
car::Anova(rich_stab_mod)
#                          Chisq Df Pr(>Chisq)    
# richness_mean         2720.311  1  < 2.2e-16 ***
# habitat                325.657  3  < 2.2e-16 ***
# richness_mean:habitat   33.857  3  2.124e-07 ***
hist(residuals(rich_stab_mod)) # looks good
plot(residuals(rich_stab_mod) ~ fitted(rich_stab_mod)) # heteroskedastic - bring to group
r.squaredGLMM(rich_stab_mod) # 0.803, 0.808
performance::r2(rich_stab_mod) # marginal: 0.812, conditional: 0.816
em_rich_stab_mod <- emtrends(rich_stab_mod, pairwise ~ habitat, var = "richness_mean") # fringing not diff from outer17, backreef not different from outer10
# $contrasts
#  contrast                     estimate     SE  df z.ratio p.value
#  Fringing - Backreef          0.186640 0.0414 Inf   4.511  <.0001
#  Fringing - Forereef 10m      0.187248 0.0455 Inf   4.114  0.0002
#  Fringing - Forereef 17m      0.027482 0.0463 Inf   0.594  0.9340
#  Backreef - Forereef 10m      0.000608 0.0405 Inf   0.015  1.0000
#  Backreef - Forereef 17m     -0.159158 0.0411 Inf  -3.875  0.0006
#  Forereef 10m - Forereef 17m -0.159766 0.0439 Inf  -3.640  0.0016

cld_rich_stab_mod <- multcomp::cld(em_rich_stab_mod, Letters = letters, sort = FALSE)

#### STABILITY ~ FUNCTIONAL RICHNESS ####
rich_stab_mod_fg <- glmmTMB(cover_stability ~ functional_richness_mean*habitat + (1|site), family = Gamma(link = "log"), data = diversity_stability_synchrony)

summary(rich_stab_mod_fg)
car::Anova(rich_stab_mod_fg)

#                                    Chisq Df Pr(>Chisq)    
# functional_richness_mean         3077.168  1  < 2.2e-16 ***
# habitat                           120.732  3  < 2.2e-16 ***
# functional_richness_mean:habitat   22.582  3  4.935e-05 ***

hist(residuals(rich_stab_mod_fg)) # looks fine
plot(residuals(rich_stab_mod_fg) ~ fitted(rich_stab_mod_fg)) # same as usual 
performance::r2(rich_stab_mod_fg) # marginal: 0.823, conditional: 0.832
em_rich_stab_mod_fg <- emtrends(rich_stab_mod_fg, pairwise ~ habitat, var = "functional_richness_mean") 
#  fringing different from all others
# contrast                    estimate     SE  df z.ratio p.value
#  Fringing - Backreef           0.1723 0.0466 Inf   3.698  0.0012
#  Fringing - Forereef 10m       0.2224 0.0517 Inf   4.304  0.0001
#  Fringing - Forereef 17m       0.1866 0.0488 Inf   3.826  0.0008
#  Backreef - Forereef 10m       0.0501 0.0472 Inf   1.061  0.7131
#  Backreef - Forereef 17m       0.0142 0.0440 Inf   0.324  0.9883
#  Forereef 10m - Forereef 17m  -0.0358 0.0476 Inf  -0.753  0.8756

cld_rich_stab_mod_fg <- multcomp::cld(em_rich_stab_mod_fg, Letters = letters, sort = FALSE)

#### STABILITY ~ SYNCHRONY ####
synch_stab_mod <- glmmTMB(cover_stability ~ synchrony*habitat + (1|site), family = Gamma("log"), data = diversity_stability_synchrony)
summary(synch_stab_mod)
car::Anova(synch_stab_mod)
#                     Chisq Df Pr(>Chisq)    
# synchrony         1308.34  1  < 2.2e-16 ***
# habitat            103.13  3  < 2.2e-16 ***
# synchrony:habitat  327.96  3  < 2.2e-16 ***
hist(residuals(synch_stab_mod)) # looks good
plot(residuals(synch_stab_mod) ~ fitted(synch_stab_mod)) # same issue as always
r.squaredGLMM(synch_stab_mod) # 0.63, 0.71
performance::r2(synch_stab_mod) # marginal: 0.66, conditional: 0.74
em_synch_stab_mod <- emtrends(synch_stab_mod, pairwise ~ habitat, var = "synchrony") # fringing and backreef not different
# contrast                    estimate    SE  df z.ratio p.value
# Fringing - Backreef            0.181 0.107 Inf   1.695  0.3262
# Fringing - Forereef 10m        1.492 0.142 Inf  10.487  <.0001
# Fringing - Forereef 17m        2.436 0.149 Inf  16.336  <.0001
# Backreef - Forereef 10m        1.311 0.157 Inf   8.346  <.0001
# Backreef - Forereef 17m        2.255 0.161 Inf  14.008  <.0001
# Forereef 10m - Forereef 17m    0.944 0.181 Inf   5.222  <.0001

cld_synch_stab_mod <- multcomp::cld(em_synch_stab_mod, Letters = letters, sort = FALSE)

#pull out data from models to make supplemental figures:

# get estimates for taxonomic richness
t.s4.a <- as.tibble(cld_rich_stab_mod) %>%
  mutate(Predictor = "Taxonomic richness") %>% 
  rename_if(str_detect(names(.), ".trend"), ~"Mean")

# get estimates for fucntional richness
t.s4.b <- as.tibble(cld_rich_stab_mod_fg) %>%
  mutate(Predictor = "Functional richness") %>% 
  rename_if(str_detect(names(.), ".trend"), ~"Mean")

# get estimates for synchrony
t.s4.c <- as.tibble(cld_synch_stab_mod) %>% 
  mutate(Predictor = "Synchrony") %>% 
  rename_if(str_detect(names(.), ".trend"), ~"Mean")

rbind(t.s4.a, t.s4.b, t.s4.c) %>% 
  mutate(`Spatial scale` = "Plot-level", 
         Table = "S4") %>% 
  dplyr::select(-SE, -df) -> table_s4_plotlevel

table_s4_plotlevel %>% 
  # rename to match previous version/code
  dplyr::rename(Habitat = habitat,
                Lower_CI = asymp.LCL,
                Upper_CI = asymp.UCL) %>%
  # make a blank column to fill in the letter designations manually
  # save as CSV
  write_csv(here::here("data/supplemental_tables_tableS4_plotlevel.csv"))



