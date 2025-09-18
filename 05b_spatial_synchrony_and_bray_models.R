rm(list = ls())
library(glmmTMB)
library(emmeans)
library(car)
library(effects)
library(DHARMa)
library(codyn)

source("05a_calculate_spatial_synchrony_and_bray.R")

#read in data 
#dss_spatial_2 <- read.csv(here::here("data", "spatial_synchrony_09172025.csv")) 

#remind r of the correct order 
#want habitats to be in order.
dss_spatial_2$habitat <- factor(dss_spatial_2$habitat,
                                                levels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

#### REGIONAL STABILITY ~ LOCAL STABILITY ####
alpha_gamma_stab_mod <- glmmTMB(cover_stability ~ stability_mean*habitat, family = Gamma("log"), data = dss_spatial_2)
summary(alpha_gamma_stab_mod)
car::Anova(alpha_gamma_stab_mod)
#NOAM
#                          Chisq Df Pr(>Chisq)    
# stability_mean         61.2742  1  4.965e-15 ***
# habitat                12.9328  3   0.004784 ** 
# stability_mean:habitat  9.4834  3   0.023508 * 
performance::r2(alpha_gamma_stab_mod) # 0.838 #matches
#LAUREN
#Response: cover_stability
#                        Chisq Df Pr(>Chisq)    
#stability_mean         61.2725  1   4.97e-15 ***
#habitat                12.9323  3   0.004785 ** 
#stability_mean:habitat  9.4832  3   0.023511 *  
em_alpha_gamma_stab_mod <- emtrends(alpha_gamma_stab_mod, pairwise ~ habitat, var = "stability_mean") # backreef different from fringe and forereef 10

#NOAM
# $contrasts
#  contrast                    estimate    SE  df z.ratio p.value
#  Fringing - Backreef            1.078 0.381 Inf   2.828  0.0242
#  Fringing - Forereef 10m        0.175 0.345 Inf   0.509  0.9571
#  Fringing - Forereef 17m        0.380 0.305 Inf   1.248  0.5965
#  Backreef - Forereef 10m       -0.903 0.349 Inf  -2.589  0.0474
#  Backreef - Forereef 17m       -0.698 0.309 Inf  -2.256  0.1085
#  Forereef 10m - Forereef 17m    0.205 0.263 Inf   0.779  0.8639

#LAUREN --> very slight differences in p-value for these 2 
# Backreef - Forereef 10m       -0.903 0.349 Inf  -2.589  0.0475
#Forereef 10m - Forereef 17m    0.205 0.263 Inf   0.779  0.8638

cld_em_alpha_gamma_stab_mod <- multcomp::cld(em_alpha_gamma_stab_mod, Letters = letters, sort = FALSE)

#### REGIONAL STABILITY ~ SPATIAL SYNCHRONY ####
spatial_synchrony_mod <- glmmTMB(cover_stability ~ spatial_synchrony*habitat, family = Gamma("log"), data = dss_spatial_2)
hist(residuals(spatial_synchrony_mod)) # a little skew
plot(residuals(spatial_synchrony_mod) ~ predict(spatial_synchrony_mod))
summary(spatial_synchrony_mod)
car::Anova(spatial_synchrony_mod)
#NOAM
#                             Chisq Df Pr(>Chisq)    
# spatial_synchrony         16.2942  1  5.423e-05 ***
# habitat                   19.5302  3  0.0002124 ***
# spatial_synchrony:habitat  6.7253  3  0.0811882 .  

#LAUREN
#Response: cover_stability
#                         Chisq Df Pr(>Chisq)    
#spatial_synchrony         16.2914  1  5.431e-05 ***
#habitat                   19.5303  3  0.0002124 ***
#spatial_synchrony:habitat  6.7239  3  0.0812368 .  
performance::r2(spatial_synchrony_mod) # 0.64 #this matches

## interaction is not signficant, re run 

spatial_synchrony_mod2 <- glmmTMB(cover_stability ~ spatial_synchrony + habitat, family = Gamma("log"), data = dss_spatial_2)
hist(residuals(spatial_synchrony_mod2)) # a little skew
plot(residuals(spatial_synchrony_mod2) ~ predict(spatial_synchrony_mod2))
summary(spatial_synchrony_mod2)
car::Anova(spatial_synchrony_mod2)

#Response: cover_stability
#Chisq Df Pr(>Chisq)    
#spatial_synchrony 11.483  1  0.0007024 ***
#habitat           16.655  3  0.0008321 ***

#NOAM OG CODE
emtrends(spatial_synchrony_mod2, pairwise ~ habitat, var = "spatial_synchrony") # no sig differences but close
# contrast                    estimate    SE  df z.ratio p.value
# Fringing - Backreef            0.404 1.764 Inf   0.229  0.9958
# Fringing - Forereef 10m        1.365 1.754 Inf   0.778  0.8643
# Fringing - Forereef 17m        3.112 1.882 Inf   1.654  0.3484
# Backreef - Forereef 10m        0.961 0.857 Inf   1.121  0.6766
# Backreef - Forereef 17m        2.708 1.095 Inf   2.472  0.0643
# Forereef 10m - Forereef 17m    1.747 1.078 Inf   1.621  0.3667

#BUT SHOULDN'T IT BE SINCE NO SIGN INTERACTION?
em_spatial_synchrony_mod2 <- emmeans(spatial_synchrony_mod2, pairwise ~ habitat, type = "response")

#$contrasts
#contrast                    ratio     SE  df null z.ratio p.value
#Fringing / Backreef         0.827 0.1294 Inf    1  -1.215  0.6172
#Fringing / Forereef 10m     0.655 0.1197 Inf    1  -2.314  0.0949
#Fringing / Forereef 17m     0.540 0.0849 Inf    1  -3.921  0.0005
#Backreef / Forereef 10m     0.792 0.1441 Inf    1  -1.279  0.5762
#Backreef / Forereef 17m     0.653 0.1026 Inf    1  -2.716  0.0335
#Forereef 10m / Forereef 17m 0.823 0.1441 Inf    1  -1.110  0.6833

cld_spatial_synchrony_mod2 <- multcomp::cld(em_spatial_synchrony_mod2, Letters = letters, sort = FALSE)

#### REGIONAL STABILITY : LOCAL STABILITY ~ SPATIAL SYNCHRONY ####

#IF YOU LOG (spatial_synchrony), NOT A SIGNIFICANT INTERACTION w/ habitat..
ratio_mod1a <- glmmTMB(ratio ~ spatial_synchrony*habitat, family = Gamma("log"), data = dss_spatial_2)
ratio_mod1b <- glmmTMB(ratio ~ log(spatial_synchrony)*habitat, family = Gamma("log"), data = dss_spatial_2)
hist(residuals(ratio_mod1a)) # looks good
plot(residuals(ratio_mod1a) ~ predict(ratio_mod1a)) # looks good
summary(ratio_mod1a)
car::Anova(ratio_mod1a)
   
#NOAM                             Chisq Df Pr(>Chisq)    
# spatial_synchrony         258.466  1  < 2.2e-16 ***
# habitat                    25.584  3  1.166e-05 ***
# spatial_synchrony:habitat  35.752  3  8.451e-08 ***

#LAUREN
#Response: ratio
#Chisq Df Pr(>Chisq)    
#spatial_synchrony         258.529  1  < 2.2e-16 ***
#habitat                    25.583  3  1.166e-05 ***
#spatial_synchrony:habitat  35.756  3  8.433e-08 ***

performance::r2(ratio_mod1a) # 0.955 #matches, also DAMN

#no significant interaction..
car::Anova(ratio_mod1b)
#Chisq Df Pr(>Chisq)    
#log(spatial_synchrony)         983.8985  1  < 2.2e-16 ***
#habitat                         19.7533  3  0.0001909 ***
#log(spatial_synchrony):habitat   2.9257  3  0.4032213    

emtrends(ratio_mod1a, pairwise ~ habitat, var = "spatial_synchrony") 
#NOAM
# Fringing - Backreef            0.205 0.445 Inf   0.461  0.9674
# Fringing - Forereef 10m       -0.967 0.440 Inf  -2.195  0.1246
# Fringing - Forereef 17m       -0.857 0.473 Inf  -1.812  0.2677
# Backreef - Forereef 10m       -1.172 0.209 Inf  -5.599  <.0001
# Backreef - Forereef 17m       -1.062 0.272 Inf  -3.912  0.0005
# Forereef 10m - Forereef 17m    0.109 0.265 Inf   0.413  0.9762

#LAUREN -- 
#$contrasts
#contrast                    estimate    SE  df z.ratio p.value
#Fringing - Backreef            0.205 0.445 Inf   0.461  0.9674
#Fringing - Forereef 10m       -0.967 0.440 Inf  -2.195  0.1246
#Fringing - Forereef 17m       -0.857 0.473 Inf  -1.811  0.2679
#Backreef - Forereef 10m       -1.172 0.209 Inf  -5.599  <.0001
#Backreef - Forereef 17m       -1.062 0.272 Inf  -3.912  0.0005
#Forereef 10m - Forereef 17m    0.110 0.265 Inf   0.414  0.9761

# actually, we don't want to consider habitat, so rerunning model... 

#this is not logged
ratio_mod2 <- glmmTMB(ratio ~ spatial_synchrony, family = Gamma("log"), data = dss_spatial_2)

hist(residuals(ratio_mod2)) # THIS LOOKS WEIRD?
plot(residuals(ratio_mod2) ~ predict(ratio_mod2)) # looks fine I think?
summary(ratio_mod2)
car::Anova(ratio_mod2)

#Response: ratio
#Chisq Df Pr(>Chisq)    
#spatial_synchrony   126  1  < 2.2e-16 ***

#this is logged - GOING TO USE THIS MODEL
ratio_mod3 <- glmmTMB(ratio ~ log(spatial_synchrony), family = Gamma("log"), data = dss_spatial_2)

hist(residuals(ratio_mod3)) # this looks better
plot(residuals(ratio_mod3) ~ predict(ratio_mod3)) # looks fine I think?
summary(ratio_mod3)
car::Anova(ratio_mod3)

#Response: ratio
#                       Chisq Df Pr(>Chisq)    
#log(spatial_synchrony) 774.21  1  < 2.2e-16 ***

performance::r2(ratio_mod3)  #0.97

#### SPATIAL SYNCHRONY ~ BRAY ####
bray_mod <- glmmTMB(spatial_synchrony ~ mean_bray*habitat, family = beta_family(), data = beta_spatialsync)
hist(residuals(bray_mod)) # looks good
plot(residuals(bray_mod) ~ predict(bray_mod)) # looks good
summary(bray_mod)
car::Anova(bray_mod) 
#Noam
#                             Chisq Df Pr(>Chisq)    
# mean_bray         14.004  1  0.0001824 ***
# habitat           11.796  3  0.0081172 ** 
# mean_bray:habitat 16.732  3  0.0008023 ***

#Lauren
#Response: spatial_synchrony
#Chisq Df Pr(>Chisq)    
#mean_bray         14.020  1  0.0001809 ***
#habitat           11.792  3  0.0081300 ** 
#mean_bray:habitat 16.737  3  0.0008003 ***

performance::r2(bray_mod) # 0.743 #this matches

#NOAM
emtrends(bray_mod, pairwise ~ habitat, var = "mean_bray") 
#  Fringing - Backreef            4.380 1.98 Inf   2.210  0.1205
#  Fringing - Forereef 10m        4.086 1.94 Inf   2.111  0.1495
#  Fringing - Forereef 17m       -4.568 2.33 Inf  -1.957  0.2045
#  Backreef - Forereef 10m       -0.294 2.21 Inf  -0.133  0.9992
#  Backreef - Forereef 17m       -8.948 2.57 Inf  -3.486  0.0028
#  Forereef 10m - Forereef 17m   -8.654 2.53 Inf  -3.420  0.0035
#LAUREN
#$contrasts
#contrast                    estimate   SE  df z.ratio p.value
#Fringing - Backreef            4.376 1.98 Inf   2.211  0.1201
#Fringing - Forereef 10m        4.088 1.93 Inf   2.114  0.1485
#Fringing - Forereef 17m       -4.569 2.33 Inf  -1.957  0.2043
#Backreef - Forereef 10m       -0.288 2.21 Inf  -0.130  0.9992
#Backreef - Forereef 17m       -8.945 2.57 Inf  -3.486  0.0028
#Forereef 10m - Forereef 17m   -8.657 2.53 Inf  -3.420  0.0035

# but actually we are going to show without habitat so..
bray_mod2 <- glmmTMB(spatial_synchrony ~ mean_bray, family = beta_family(), data = beta_spatialsync)
hist(residuals(bray_mod2)) # looks fine
plot(residuals(bray_mod2) ~ predict(bray_mod2)) # looks good
summary(bray_mod2)
car::Anova(bray_mod2)
#Response: spatial_synchrony
#Chisq Df Pr(>Chisq)    
#mean_bray 13.832  1  0.0001999 ***

performance::r2(bray_mod2) # 0.40 --> this got way worse.. 



#pull out data from models to make supplemental figures:

# get estimates for mean plot level stability
t.s6.a <- as_tibble(cld_em_alpha_gamma_stab_mod) %>%
  mutate(Predictor = "Mean plot-level stability") %>% 
  rename_if(str_detect(names(.), ".trend"), ~"Mean")

# get estimates for spatial synchrony
t.s6.b <- as_tibble(cld_spatial_synchrony_mod2) %>%
  mutate(Predictor = "Spatial synchrony") %>% 
  rename_if(str_detect(names(.), ".trend"), ~"Mean") %>%
  rename(Mean = response) #note these are different things, but just want for plotting.. 


rbind(t.s6.a, t.s6.b) %>% 
  mutate(Table = "S6") %>% 
  dplyr::select(-SE, -df) -> table_s6

supplemental_tableS6 <- table_s6 %>% 
  # rename to match previous version/code
  dplyr::rename(Habitat = habitat,
                Lower_CI = asymp.LCL,
                Upper_CI = asymp.UCL)
