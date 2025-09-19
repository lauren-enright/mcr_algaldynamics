rm(list = ls())
library(here)
library(tidyverse)
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(multcomp)

source("01b_data_prep.R")
source("00_functions_and_aes.R")

alpha_diversity_quad_macro <- read.csv(here::here("data", "alpha_diversity_quad_macro_09182025.csv"))
#alpha_diversity_site_macro <- read.csv(here::here("data", "alpha_diversity_site_macro.csv"))

alpha_diversity_quad_macro$habitat <- factor(alpha_diversity_quad_macro$habitat,
                     levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

#### COVER ~ RICHNESS ####
cover_mod <- glmmTMB(cover_trans ~ richness*habitat + (1|site/location) + (1|year), family = beta_family(), data = alpha_diversity_quad_macro)
summary(cover_mod)
car::Anova(cover_mod)

#Response: cover_trans
#Chisq Df Pr(>Chisq)    
#richness         30916.15  1  < 2.2e-16 ***
#  habitat            141.77  3  < 2.2e-16 ***
#  richness:habitat   294.57  3  < 2.2e-16 ***

hist(residuals(cover_mod)) # looks fine
plot(residuals(cover_mod) ~ fitted(cover_mod)) # negative trend but it can't be less than 0 so not too concerned. some wave pattern
performance::r2(cover_mod) # marginal: 0.635, conditional: 0.654
em_cover_mod <- emtrends(cover_mod, pairwise ~ habitat, var = "richness") # backreef not different from forereef, forereef also not different

#contrast                    estimate     SE  df z.ratio p.value
#Fringing - Backreef          0.34720 0.0241 Inf  14.409  <.0001
#Fringing - Forereef 10m      0.34865 0.0240 Inf  14.514  <.0001
#Fringing - Forereef 17m      0.38953 0.0250 Inf  15.599  <.0001
#Backreef - Forereef 10m      0.00144 0.0205 Inf   0.071  0.9999
#Backreef - Forereef 17m      0.04232 0.0214 Inf   1.979  0.1957
#Forereef 10m - Forereef 17m  0.04088 0.0209 Inf   1.952  0.2063

cld_cover_mod <- multcomp::cld(em_cover_mod, Letters = letters, sort = FALSE)

#### COVER ~ FUNCTIONAL RICHNESS ####

cover_mod_fg <- glmmTMB(cover_trans ~ functional_richness*habitat + (1|site/location) + (1|year), family = beta_family(), data = alpha_diversity_quad_macro)


summary(cover_mod_fg)
car::Anova(cover_mod_fg)

#Lauren
#Chisq Df Pr(>Chisq)    
#functional_richness         31722.867  1  < 2.2e-16 ***
#habitat                        48.585  3  1.598e-10 ***
#functional_richness:habitat   455.808  3  < 2.2e-16 ***

hist(residuals(cover_mod_fg)) # good
performance::r2(cover_mod_fg) # Noam: margingal = 0.63, conditional = 0.65 #Lauren = same 
em_cover_mod_fg <- emtrends(cover_mod_fg, pairwise ~ habitat, var = "functional_richness") 

#contrast                    estimate     SE  df z.ratio p.value
#Fringing - Backreef            0.225 0.0265 Inf   8.500  <.0001
#Fringing - Forereef 10m        0.357 0.0261 Inf  13.701  <.0001
#Fringing - Forereef 17m        0.536 0.0262 Inf  20.499  <.0001
#Backreef - Forereef 10m        0.132 0.0231 Inf   5.697  <.0001
#Backreef - Forereef 17m        0.311 0.0231 Inf  13.435  <.0001
#Forereef 10m - Forereef 17m    0.179 0.0223 Inf   8.061  <.0001

cld_cover_mod_fg <- multcomp::cld(em_cover_mod_fg, Letters = letters, sort = FALSE)

#pull out data from models to make supplemental figures:

#richness by cover
t.s2.a <- as.tibble(cld_cover_mod) %>%
  mutate(Predictor = "Taxonomic richness",
         `Spatial scale` = "Plot-level") %>% 
  # rename this so it matches for joining purposes
  rename_if(str_detect(names(.), ".trend"), ~"Mean")
#functional richness by cover

t.s2.b <-  as.tibble(cld_cover_mod_fg) %>%
  mutate(Predictor = "Functional richness",
         `Spatial scale` = "Plot-level") %>% 
  rename_if(str_detect(names(.), ".trend"), ~"Mean")

#joining together

rbind(t.s2.a, t.s2.b) %>% 
  mutate(Table = "S2_plotlevel") %>% 
  dplyr::select(-SE, -df) -> table_s2_plot

supplemental_tables_tableS2_plot <- table_s2_plot %>% 
  # rename to match previous version/code
  dplyr::rename(Habitat = habitat,
         Lower_CI = asymp.LCL,
         Upper_CI = asymp.UCL)



