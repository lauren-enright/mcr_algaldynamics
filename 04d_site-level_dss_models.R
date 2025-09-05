rm(list = ls())
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(codyn)

source("04a_calculate_synchrony.R")

#### STABILITY ~ RICHNESS ####
rich_stab_mod_site <- glmmTMB(cover_stability~ richness_mean*habitat, family = Gamma(link = "log"), data = diversity_stability_synchrony_site)
summary(rich_stab_mod_site)
Anova(rich_stab_mod_site)

#                        Chisq Df Pr(>Chisq)    
# richness_mean         17.645  1  2.662e-05 ***
# habitat               32.229  3  4.682e-07 ***
# richness_mean:habitat  8.075  3    0.04449 *  
hist(residuals(rich_stab_mod_site)) # blocky but fine
plot(residuals(rich_stab_mod_site) ~ fitted(rich_stab_mod_site)) # heteroskedastic - bring to group
performance::r2(rich_stab_mod_site)
emtrends(rich_stab_mod_site, pairwise ~ habitat, var = "richness_mean") # no differences across habitats
# contrast                    estimate    SE  df z.ratio p.value
# Fringing - Backreef           0.1854 0.120 Inf   1.539  0.4138
# Fringing - Forereef 10m      -0.2155 0.145 Inf  -1.484  0.4471
# Fringing - Forereef 17m      -0.2876 0.214 Inf  -1.344  0.5349
# Backreef - Forereef 10m      -0.4009 0.165 Inf  -2.432  0.0711
# Backreef - Forereef 17m      -0.4730 0.228 Inf  -2.077  0.1607
# Forereef 10m - Forereef 17m  -0.0721 0.242 Inf  -0.298  0.9908

#### STABILITY ~ FUNCTIONAL RICHNESS ####

rich_stab_mod_site_fg <- glmmTMB(cover_stability~ functional_richness_mean*habitat, family = Gamma(link = "log"), data = diversity_stability_site)
summary(rich_stab_mod_site_fg)
Anova(rich_stab_mod_site_fg)

# 
#                                    Chisq Df Pr(>Chisq)   
# functional_richness_mean          2.2098  1   0.137136   
# habitat                          14.5000  3   0.002298 **
# functional_richness_mean:habitat 12.6102  3   0.005560 **

hist(residuals(rich_stab_mod_site_fg)) # blocky but fine
plot(residuals(rich_stab_mod_site_fg) ~ fitted(rich_stab_mod_site_fg)) # fine
performance::r2(rich_stab_mod_site_fg) # 0.59
emtrends(rich_stab_mod_site_fg, pairwise ~ habitat, var = "functional_richness_mean") 
# Fringing different from outer 10
#$contrasts
# contrast                    estimate    SE  df z.ratio p.value
#  Fringing - Backreef            0.255 0.304 Inf   0.840  0.8356
#  Fringing - Forereef 10m        1.237 0.407 Inf   3.038  0.0127
#  Fringing - Forereef 17m        0.786 0.374 Inf   2.102  0.1525
#  Backreef - Forereef 10m        0.982 0.479 Inf   2.051  0.1695
#  Backreef - Forereef 17m        0.530 0.451 Inf   1.177  0.6416
#  Forereef 10m - Forereef 17m   -0.452 0.526 Inf  -0.859  0.8259

#### STABILITY ~ SYNCHRONY ####
synch_stab_mod_site <- glmmTMB(cover_stability ~ synchrony*habitat, family = Gamma("log"), data = diversity_stability_synchrony_site)
summary(synch_stab_mod_site)
Anova(synch_stab_mod_site)
#                    Chisq Df Pr(>Chisq)    
# synchrony         22.117  1  2.565e-06 ***
# habitat            9.345  3    0.02504 *  
# synchrony:habitat 10.534  3    0.01453 *   
hist(residuals(synch_stab_mod_site)) # almost uniform
plot(residuals(synch_stab_mod_site) ~ fitted(synch_stab_mod_site)) # same issue as always
r.squaredGLMM(synch_stab_mod_site) # 0.678
performance::r2(synch_stab_mod_site) # 0.702
emtrends(synch_stab_mod_site, pairwise ~ habitat, var = "synchrony") # backreef is different from forereef 17, fringing is a p = 0.05
#
# contrast                    estimate    SE  df z.ratio p.value
# Fringing - Backreef           -0.262 0.972 Inf  -0.269  0.9932
# Fringing - Forereef 10m        1.318 0.918 Inf   1.437  0.4764
# Fringing - Forereef 17m        2.924 1.143 Inf   2.559  0.0513
# Backreef - Forereef 10m        1.580 0.857 Inf   1.843  0.2533
# Backreef - Forereef 17m        3.186 1.095 Inf   2.910  0.0189
# Forereef 10m - Forereef 17m    1.606 1.047 Inf   1.534  0.4170
