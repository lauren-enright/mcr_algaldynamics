rm(list = ls())
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(codyn)

source("05a_calculate_spatial_synchrony_and_bray.R")

#### REGIONAL STABILITY ~ LOCAL STABILITY ####
alpha_gamma_stab_mod <- glmmTMB(cover_stability ~ stability_mean*habitat, family = Gamma("log"), data = dss_spatial_2)
summary(alpha_gamma_stab_mod)
Anova(alpha_gamma_stab_mod)
#                          Chisq Df Pr(>Chisq)    
# stability_mean         61.2742  1  4.965e-15 ***
# habitat                12.9328  3   0.004784 ** 
# stability_mean:habitat  9.4834  3   0.023508 * 
performance::r2(alpha_gamma_stab_mod) # 0.838

emtrends(alpha_gamma_stab_mod, pairwise ~ habitat, var = "stability_mean") # backreef different from fringe and forereef 10
# $contrasts
#  contrast                    estimate    SE  df z.ratio p.value
#  Fringing - Backreef            1.078 0.381 Inf   2.828  0.0242
#  Fringing - Forereef 10m        0.175 0.345 Inf   0.509  0.9571
#  Fringing - Forereef 17m        0.380 0.305 Inf   1.248  0.5965
#  Backreef - Forereef 10m       -0.903 0.349 Inf  -2.589  0.0474
#  Backreef - Forereef 17m       -0.698 0.309 Inf  -2.256  0.1085
#  Forereef 10m - Forereef 17m    0.205 0.263 Inf   0.779  0.8639


#### REGIONAL STABILITY ~ SPATIAL SYNCHRONY ####
spatial_synchrony_mod <- glmmTMB(cover_stability ~ spatial_synchrony*habitat, family = Gamma("log"), data = dss_spatial_2)
hist(residuals(spatial_synchrony_mod)) # a little skew
plot(residuals(spatial_synchrony_mod) ~ predict(spatial_synchrony_mod))
summary(spatial_synchrony_mod)
Anova(spatial_synchrony_mod)
#                             Chisq Df Pr(>Chisq)    
# spatial_synchrony         16.2942  1  5.423e-05 ***
# habitat                   19.5302  3  0.0002124 ***
# spatial_synchrony:habitat  6.7253  3  0.0811882 .  
performance::r2(spatial_synchrony_mod) # 0.64
emtrends(spatial_synchrony_mod, pairwise ~ habitat, var = "spatial_synchrony") # no sig differences but close
# contrast                    estimate    SE  df z.ratio p.value
# Fringing - Backreef            0.404 1.764 Inf   0.229  0.9958
# Fringing - Forereef 10m        1.365 1.754 Inf   0.778  0.8643
# Fringing - Forereef 17m        3.112 1.882 Inf   1.654  0.3484
# Backreef - Forereef 10m        0.961 0.857 Inf   1.121  0.6766
# Backreef - Forereef 17m        2.708 1.095 Inf   2.472  0.0643
# Forereef 10m - Forereef 17m    1.747 1.078 Inf   1.621  0.3667

#### REGIONAL STABILITY : LOCAL STABILITY ~ SPATIAL SYNCHRONY ####
ratio_mod <- glmmTMB(ratio ~ spatial_synchrony*habitat, family = Gamma("log"), data = dss_spatial_2)
hist(residuals(ratio_mod)) # looks good
plot(residuals(ratio_mod) ~ predict(ratio_mod)) # looks good
summary(ratio_mod)
Anova(ratio_mod)
   
#                             Chisq Df Pr(>Chisq)    
# spatial_synchrony         258.466  1  < 2.2e-16 ***
# habitat                    25.584  3  1.166e-05 ***
# spatial_synchrony:habitat  35.752  3  8.451e-08 ***

performance::r2(ratio_mod) # 0.955

emtrends(ratio_mod, pairwise ~ habitat, var = "spatial_synchrony") 
# Fringing - Backreef            0.205 0.445 Inf   0.461  0.9674
# Fringing - Forereef 10m       -0.967 0.440 Inf  -2.195  0.1246
# Fringing - Forereef 17m       -0.857 0.473 Inf  -1.812  0.2677
# Backreef - Forereef 10m       -1.172 0.209 Inf  -5.599  <.0001
# Backreef - Forereef 17m       -1.062 0.272 Inf  -3.912  0.0005
# Forereef 10m - Forereef 17m    0.109 0.265 Inf   0.413  0.9762

#### SPATIAL SYNCHRONY ~ BRAY ####
bray_mod <- glmmTMB(spatial_synchrony ~ mean_bray*habitat, family = beta_family(), data = beta_spatialsync)
hist(residuals(bray_mod)) # looks good
plot(residuals(bray_mod) ~ predict(bray_mod)) # looks good
summary(bray_mod)
Anova(bray_mod)

#                             Chisq Df Pr(>Chisq)    
# mean_bray         14.004  1  0.0001824 ***
# habitat           11.796  3  0.0081172 ** 
# mean_bray:habitat 16.732  3  0.0008023 ***

performance::r2(bray_mod) # 0.743

emtrends(bray_mod, pairwise ~ habitat, var = "mean_bray") 
#  Fringing - Backreef            4.380 1.98 Inf   2.210  0.1205
#  Fringing - Forereef 10m        4.086 1.94 Inf   2.111  0.1495
#  Fringing - Forereef 17m       -4.568 2.33 Inf  -1.957  0.2045
#  Backreef - Forereef 10m       -0.294 2.21 Inf  -0.133  0.9992
#  Backreef - Forereef 17m       -8.948 2.57 Inf  -3.486  0.0028
#  Forereef 10m - Forereef 17m   -8.654 2.53 Inf  -3.420  0.0035

