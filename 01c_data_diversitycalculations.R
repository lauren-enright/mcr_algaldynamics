
#This is the first script that should be run for the paper and starts with the cleaned macroalgal data that will be uploaded to EDI. 


#want to have appropriate functions and plot themes
source("00_functions_and_aes.R")

library(here)
library(tidyverse)
library(vegan)


## read in data from script 01b
macro_functional_groups_long <- read.csv(here::here("data", "macroalgalfunctionalgroups_long_09162025.csv"), stringsAsFactors = F)
cover_df <- read.csv(here::here("data", "cover_df_09162025.csv"))

colnames(macro_functional_groups_long)
unique(macro_functional_groups_long$taxa)




# Summarize to check against functional group sums later..
taxa_sum_check <- macro_functional_groups_long %>%
  dplyr::group_by(location, year) %>%
  dplyr::summarise(sum_macrocover = sum(prop_cover))

# how many quads never have macroaglae in them?
taxa_sum_check  %>%
  dplyr::group_by(location,) %>%
  dplyr::summarise(sum_macrocover_allyears = sum(sum_macrocover)) %>% 
  dplyr::filter(sum_macrocover_allyears == 0)

#okay, there are 16 quads that never have macroalgal in them across all years. 
# this means 16 quads are dropped in our models with stability, as you cannot model the stability of 0.
# quads with 0s are included in the cover data.
  

#wide data at taxonomic level
macro_taxa_groups_wide_taxonomic <- macro_functional_groups_long %>%
  dplyr::select(-functional_group) %>%
  pivot_wider(names_from = taxa, values_from = prop_cover)


# define meta data columns
meta_cols <- c("year", "location", "site", "habitat", "site_habitat", "transect", "quadrat")

meta_df <- macro_taxa_groups_wide_taxonomic[,which(names(macro_taxa_groups_wide_taxonomic) %in% meta_cols)]


### aggregate for site-level analyses
# average at site level 

site_macro <- macro_taxa_groups_wide_taxonomic %>%
  dplyr::group_by(year, site, habitat,site_habitat) %>%
  dplyr::summarize(across(acanthophora_spicifera:valonia_ventricosa, \(x) mean(x, na.rm = TRUE)))  #taking the average here

#384 observations/24 (6 sites * 4 habitats = 24)

site_meta_cols <- c("year", "site", "habitat", "site_habitat")
site_meta_df <- site_macro[,which(names(site_macro) %in% meta_cols)]



#### CALCULATE DIVERSITY METRICS ####
alpha_diversity_quad_macro <-
  data.frame(
    year = macro_taxa_groups_wide_taxonomic$year,
    location = macro_taxa_groups_wide_taxonomic$location,
    shannon = vegan::diversity(macro_taxa_groups_wide_taxonomic[, -which(names(macro_taxa_groups_wide_taxonomic) %in% meta_cols)], "shannon"),
    richness = rowSums(macro_taxa_groups_wide_taxonomic[, -which(names(macro_taxa_groups_wide_taxonomic) %in% meta_cols)] > 0)
  )

alpha_diversity_quad_macro <- merge(meta_df, alpha_diversity_quad_macro, by = c("location", "year"))

macroalgal_cover_quad <- cover_df[,c("year", "habitat", "site", "location", "macroalgae")]

alpha_diversity_quad_macro <-
  left_join(alpha_diversity_quad_macro, macroalgal_cover_quad,by = join_by(location, year, site, habitat)) %>% 
  mutate(macroalgal_prop_cover = macroalgae / 100) %>% 
  dplyr::select(-macroalgae)

alpha_diversity_quad_macro$cover_trans <- sv_trans(alpha_diversity_quad_macro$macroalgal_prop_cover)



##### REPEAT FOR SITES ######
alpha_diversity_site_macro <-
  data.frame(
    year = site_macro$year,
    site_habitat = site_macro$site_habitat,
    shannon = diversity(site_macro[ , -which(names(site_macro) %in% meta_cols)], "shannon"),
    richness = rowSums(site_macro[ , -which(names(site_macro) %in% meta_cols)] > 0)
  ) %>% 
  left_join(site_meta_df, alpha_diversity_site_macro, by = c("site_habitat", "year"))

# add cover
alpha_diversity_site_macro$prop_cover <- rowSums(site_macro[,-which(names(site_macro)  %in% colnames(site_meta_df))]) 

#alpha_diversity_site_macro$prop_cover <- alpha_diversity_site_macro$percent_cover/100

alpha_diversity_site_macro$cover_trans <- sv_trans(alpha_diversity_site_macro$prop_cover)


#### Functional Groups ####

#summarizing functional groups at the quad level
macro_taxa_groups_long_functionalgroup_summarized <- macro_functional_groups_long %>%
  dplyr::select(-taxa) %>%
  dplyr::group_by(location, year, functional_group, habitat, site) %>%
  dplyr::summarise(sum_fg_cover = sum(prop_cover))
  
max(macro_taxa_groups_long_functionalgroup_summarized$sum_fg_cover)
#max 1, good. 
  

#summarizing functional groups at the site level
#NOTE that this is ADDED and not averaged. This data is only used to find richness at the site level. LE did this as adding and as a sum, both end up the same from a richness perspective. 
fg_summary_site_SUM <- macro_functional_groups_long %>%
  dplyr::select(-taxa) %>%
  dplyr::group_by(site, year, habitat, functional_group) %>%
  dplyr::summarise(sum_fg_cover = sum(prop_cover))

#sum can be over 1... 

#### QAQC ####
fg_qaqc <- macro_taxa_groups_long_functionalgroup_summarized %>%
  dplyr::group_by(location, year) %>%
  dplyr::summarise(sum_fg_check = sum(sum_fg_cover))

check_them <- merge(taxa_sum_check, fg_qaqc)

check_them %>%
  filter(all.equal(sum_macrocover, sum_fg_check) != TRUE)

# perfect, should be 0 that do not match. 

#pivot wide at quad level
macro_taxa_groups_wide_functionalgroup <- macro_taxa_groups_long_functionalgroup_summarized %>%
  tidyr::pivot_wider(names_from = "functional_group", values_from = "sum_fg_cover")

#richness at every quad/year combination 
macro_taxa_groups_wide_functionalgroup$functional_richness <- vegan::specnumber(macro_taxa_groups_wide_functionalgroup[5:13])

unique(macro_taxa_groups_wide_functionalgroup$functional_richness)
#0 to 4

##### REPEAT AT SITE LEVEL ####
#pivot wide at site level
fg_summary_site_wide_SUM <- fg_summary_site_SUM %>%
  pivot_wider(names_from = "functional_group", values_from = "sum_fg_cover")

#richness at every site/year combination ---> NOT AVERAGED,
fg_summary_site_wide_SUM$functional_richness <- vegan::specnumber(fg_summary_site_wide_SUM[4:12])


#merge taxonomic data with functional group summary 
alpha_diversity_all_quad_macro <- merge(alpha_diversity_quad_macro,
                                        macro_taxa_groups_wide_functionalgroup[,c("location", "year", "site",  "habitat", "functional_richness")],
                                    by = c("location", "year", "site", "habitat"))


#merge site level cover with site functional group richness
alpha_diversity_all_site_macro<-merge(alpha_diversity_site_macro,
                                  fg_summary_site_wide_SUM[,c("year", "site",  "habitat", "functional_richness")],
                                  by = c("year", "site",  "habitat"))



#write out csvs

write.csv(alpha_diversity_all_quad_macro, "data/alpha_diversity_quad_macro_09182025.csv", row.names = FALSE)
write.csv(alpha_diversity_all_site_macro, "data/alpha_diversity_site_macro_09182025.csv", row.names = FALSE)
write.csv(site_macro, "data/site_macro_alpha_wide_09182025.csv", row.names = FALSE)



