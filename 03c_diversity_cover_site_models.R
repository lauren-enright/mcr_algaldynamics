#### Diversity Cover Site Models ####

source("00_functions_and_aes.R")

# read in data

alpha_diversity_site_macro <- read.csv(here::here("data", "alpha_diversity_site_macro_09182025.csv"))

#### Taxonomic Diversity ####

site_cover_mod <- glmmTMB(cover_trans ~ richness*habitat + (1|site_habitat) + (1|year), family = beta_family(), data = alpha_diversity_site_macro)
summary(site_cover_mod)
car::Anova(site_cover_mod)
#Noam
#                    Chisq Df Pr(>Chisq)    
# richness         58.9196  1  1.642e-14 ***
# habitat           6.6248  3    0.08487 .  
# richness:habitat  3.2966  3    0.34811  

#Lauren
#                   Chisq Df Pr(>Chisq)    
#richness         58.9195  1  1.643e-14 ***
#  habitat           6.6247  3    0.08487 .  
#richness:habitat  3.2967  3    0.34811  

#this matches
hist(residuals(site_cover_mod)) # some positive skew, not the worst thing ever
plot(residuals(site_cover_mod) ~ fitted(site_cover_mod)) # a little heteroscedastic but can't be less than 0
#r.squaredGLMM(site_cover_mod) # 0.2212534, 0.5990825
performance::r2(site_cover_mod) # marginal: 0.22, conditional: 0.59; matches

# do not need to do a posthoc test here because no interaction and habitat was also not signficant. 

#running again without the interaction, this is what we will report 
site_cover_mod2 <- glmmTMB(cover_trans ~ richness + habitat + (1|site_habitat) + (1|year), family = beta_family(), data = alpha_diversity_site_macro)
summary(site_cover_mod2)
car::Anova(site_cover_mod2)
#           Chisq Df Pr(>Chisq)    
#richness 58.0746  1  2.524e-14 ***
# habitat   6.7036  3    0.08197 .  

hist(residuals(site_cover_mod2)) # some positive skew, not the worst thing ever
plot(residuals(site_cover_mod2) ~ fitted(site_cover_mod2)) # a little heteroscedastic but can't be less than 0
performance::r2(site_cover_mod2) # marginal: 0.21, conditional: 0.58

#### Functional Diversity ####

site_cover_mod_fg <- glmmTMB(cover_trans ~ functional_richness*habitat + (1|site_habitat) + (1|year), family = beta_family(), data = alpha_diversity_site_macro)

summary(site_cover_mod_fg)
car::Anova(site_cover_mod_fg)

#NOAM
#                               Chisq Df Pr(>Chisq)    
# functional_richness         30.3776  1  3.556e-08 ***
# habitat                      4.9314  3     0.1769    
# functional_richness:habitat  4.1429  3     0.2464    

#LAUREN

#                             Chisq Df Pr(>Chisq)    
#functional_richness         26.1066  1  3.231e-07 ***
#habitat                      4.2211  3    0.23855    
#functional_richness:habitat  6.6844  3    0.08267 . 

hist(residuals(site_cover_mod_fg)) # positive skew
plot(residuals(site_cover_mod_fg) ~ fitted(site_cover_mod_fg)) # a little heteroscedastic but can't be less than 0
performance::r2(site_cover_mod_fg) # NOAM: 0.180, 0.563 LAUREN: Conditional R2: 0.567, Marginal: 0.173

#don't need a post hoc test here because no interaction and habitat was also not signficant. 

#running again without the interaction, this is what we will report 
site_cover_mod_fg2 <- glmmTMB(cover_trans ~ functional_richness + habitat + (1|site_habitat) + (1|year), family = beta_family(), data = alpha_diversity_site_macro)

summary(site_cover_mod_fg2)
car::Anova(site_cover_mod_fg2)
#                      Chisq Df Pr(>Chisq)    
#functional_richness 25.0504  1  5.585e-07 ***
#habitat              3.9032  3     0.2721   

hist(residuals(site_cover_mod_fg2)) # positive skew
plot(residuals(site_cover_mod_fg2) ~ fitted(site_cover_mod_fg2)) # a little heteroscedastic but can't be less than 0
performance::r2(site_cover_mod_fg2) #Conditional R2: 0.563, Marginal: 0.150
