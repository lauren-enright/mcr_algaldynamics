rm(list = ls())
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(codyn)
library(multcomp)
library(here)

source("00_functions_and_aes.R")
#source("04a_calculate_synchrony.R")

#read in data produced in 04a_calculate_synchrony script
diversity_stability_synchrony <- read.csv(here::here("data", "full_plot_level_dss_09262025.csv")) 
diversity_stability_synchrony_site <- read.csv(here::here("data", "diversity_stability_synchrony_site_09262025.csv"))

#want habitats to be in order.
diversity_stability_synchrony$habitat <- factor(diversity_stability_synchrony$habitat,
                                             levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

# EXTRACT RANGES
dss_ranges <- extract_ranges(diversity_stability_synchrony, "habitat",
                             c("richness_mean", "functional_richness_mean", "cover_stability", "synchrony"))


#### STABILITY ~ RICHNESS ####
rich_stab_mod <- glmmTMB(cover_stability~ richness_mean*habitat + (1|site), family = Gamma(link = "log"), data = diversity_stability_synchrony)
summary(rich_stab_mod)
car::Anova(rich_stab_mod)

#great, matches
#                        Chisq Df Pr(>Chisq)    
#richness_mean         2721.440  1  < 2.2e-16 ***
#habitat                325.876  3  < 2.2e-16 ***
#richness_mean:habitat   33.926  3  2.054e-07 ***

hist(residuals(rich_stab_mod)) # looks good
plot(residuals(rich_stab_mod) ~ fitted(rich_stab_mod)) # heteroskedastic - bring to group
#r.squaredGLMM(rich_stab_mod) # 0.803, 0.808
performance::r2(rich_stab_mod) # marginal: 0.812, conditional: 0.816 - yes, matches
em_rich_stab_mod <- emtrends(rich_stab_mod, pairwise ~ habitat, var = "richness_mean") # fringing not diff from outer17, backreef not different from outer10

#contrasts
#contrast                     estimate     SE  df z.ratio p.value
#Fringing - Backreef          0.186671 0.0414 Inf   4.512  <.0001
#Fringing - Forereef 10m      0.187196 0.0455 Inf   4.114  0.0002
#Fringing - Forereef 17m      0.027111 0.0463 Inf   0.586  0.9364
#Backreef - Forereef 10m      0.000525 0.0405 Inf   0.013  1.0000
#Backreef - Forereef 17m     -0.159559 0.0411 Inf  -3.885  0.0006
#Forereef 10m - Forereef 17m -0.160084 0.0439 Inf  -3.647  0.0015

cld_rich_stab_mod <- multcomp::cld(em_rich_stab_mod, Letters = letters, sort = FALSE)

#### STABILITY ~ FUNCTIONAL RICHNESS ####
rich_stab_mod_fg <- glmmTMB(cover_stability ~ functional_richness_mean*habitat + (1|site), family = Gamma(link = "log"), data = diversity_stability_synchrony)

summary(rich_stab_mod_fg)
car::Anova(rich_stab_mod_fg)

#                                    Chisq Df Pr(>Chisq)    
  
#functional_richness_mean         3099.815  1  < 2.2e-16 ***
#habitat                           120.580  3  < 2.2e-16 ***
#functional_richness_mean:habitat   25.078  3  1.487e-05 ***

#new, with 8 functional groups

#Chisq Df Pr(>Chisq)    
#functional_richness_mean         3104.718  1  < 2.2e-16 ***
#habitat                           119.466  3  < 2.2e-16 ***
# functional_richness_mean:habitat   24.858  3  1.653e-05 ***

hist(residuals(rich_stab_mod_fg)) # looks fine
plot(residuals(rich_stab_mod_fg) ~ fitted(rich_stab_mod_fg)) # same as usual 
performance::r2(rich_stab_mod_fg) # marginal: 0.823, conditional: 0.832, matches 
#new for 8 functional groups: #Conditional R2: 0.833, Marginal R2: 0.824
em_rich_stab_mod_fg <- emtrends(rich_stab_mod_fg, pairwise ~ habitat, var = "functional_richness_mean") 
#  fringing different from all others

#contrast                    estimate     SE  df z.ratio p.value
#Fringing - Backreef           0.1829 0.0466 Inf   3.921  0.0005
#Fringing - Forereef 10m       0.2337 0.0517 Inf   4.520  <.0001
#Fringing - Forereef 17m       0.1981 0.0488 Inf   4.058  0.0003
#Backreef - Forereef 10m       0.0508 0.0471 Inf   1.079  0.7024
#Backreef - Forereef 17m       0.0152 0.0439 Inf   0.346  0.9858
#Forereef 10m - Forereef 17m  -0.0356 0.0475 Inf  -0.750  0.8767

#new, with 8 functional groups
#$contrasts
#contrast                    estimate     SE  df z.ratio p.value
#Fringing - Backreef           0.1791 0.0467 Inf   3.836  0.0007
#Fringing - Forereef 10m       0.2337 0.0517 Inf   4.521  <.0001
#Fringing - Forereef 17m       0.1979 0.0488 Inf   4.056  0.0003
#Backreef - Forereef 10m       0.0546 0.0471 Inf   1.159  0.6525
#Backreef - Forereef 17m       0.0189 0.0439 Inf   0.429  0.9735
#Forereef 10m - Forereef 17m  -0.0357 0.0475 Inf  -0.753  0.8755

cld_rich_stab_mod_fg <- multcomp::cld(em_rich_stab_mod_fg, Letters = letters, sort = FALSE)

#### STABILITY ~ SYNCHRONY ####
synch_stab_mod <- glmmTMB(cover_stability ~ synchrony*habitat + (1|site), family = Gamma("log"), data = diversity_stability_synchrony)
summary(synch_stab_mod)
car::Anova(synch_stab_mod)
  
#great, matches.  
#synchrony         1308.33  1  < 2.2e-16 ***
#  habitat            103.13  3  < 2.2e-16 ***
#  synchrony:habitat  327.97  3  < 2.2e-16 ***
hist(residuals(synch_stab_mod)) # looks good
plot(residuals(synch_stab_mod) ~ fitted(synch_stab_mod)) # same issue as always
#r.squaredGLMM(synch_stab_mod) # 0.63, 0.71
performance::r2(synch_stab_mod) # marginal: 0.66, conditional: 0.74 - matches
em_synch_stab_mod <- emtrends(synch_stab_mod, pairwise ~ habitat, var = "synchrony") # fringing and backreef not different
# contrast                    estimate    SE  df z.ratio p.value
# Fringing - Backreef            0.181 0.107 Inf   1.695  0.3262
# Fringing - Forereef 10m        1.492 0.142 Inf  10.487  <.0001
# Fringing - Forereef 17m        2.436 0.149 Inf  16.336  <.0001
# Backreef - Forereef 10m        1.311 0.157 Inf   8.346  <.0001
# Backreef - Forereef 17m        2.255 0.161 Inf  14.008  <.0001
# Forereef 10m - Forereef 17m    0.944 0.181 Inf   5.222  <.0001
#matches

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

supplemental_tables_tableS4_plot <- table_s4_plotlevel %>% 
  # rename to match previous version/code
  dplyr::rename(Habitat = habitat,
                Lower_CI = asymp.LCL,
                Upper_CI = asymp.UCL)


