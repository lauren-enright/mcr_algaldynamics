rm(list = ls())
library(here)
library(tidyverse)
library(vegan)

source("00_functions_and_aes.R")

#### MACROALGAE DATA ####

## read in data from script 01a
wide_alldata_cleaned <- read.csv(here::here("data", "allbenthicdata_cleaned_colnames_09162025.csv"), stringsAsFactors = F) 

### if we want to make a missing or faulty data table
# length(unique(data$location))
# expected_data <- expand.grid(year = seq(2007,2023,1), location = unique(data$location))
# missing_quadrats <- expected_data %>%
#   anti_join(data, by = c("year", "location"))
###

# remove remaining backreef and fringing 2020 data because of covid extrapolation
filtered_data <- wide_alldata_cleaned %>%
  filter(!(year == 2020 & habitat %in% c("fringing", "backreef")))
#19173

# re-factor for figures
filtered_data$habitat <- factor(filtered_data$habitat, 
                                levels = c("fringing", "backreef", "outer_10","outer_17"),
                                labels = c("Fringing", "Backreef", "Forereef 10m", "Forereef 17m"))

colnames(filtered_data)
#okay - here is the deal:
# filtered_data has 93 columns. 7 of them are meta data ("year", "location", "site", "habitat", "site_habitat", "transect", "quadrat")
  # and one is "row_sums_final" --> so, 93 - 7 - 1 = 85.
  # there were 86 taxa in the original EDI dataset, but we changed "Boodlea kaeneana" to Cladophoropsis membranacea, 
  #and Cladophoropsis membranacea was already in the original EDI data, they combined down, so 85 total taxa. Yay!




# note non-algae species
not_algae <- c("ascidian", "bare_space", "coral", "coral_rubble", "corallimorpharia", "heteractis_sp", "millepora_platyphylla", "no_data", "sand", "sarcophyton_sp", "shell_debris", "soft_coral", "sponge", "tridacna_sp", "zooxanthellae", "row_sums_final")

#15 taxa in non algae (16 values in not_algae vector, but one is "row_sums_final)

# remove
algae <- filtered_data[ , -which(names(filtered_data) %in% not_algae)]

# did the filter work? 93 -16 = 77 = good. 
#algae dataframe has 77 variables. 7 of these are meta data, which means 70 of them are algae. We had
# 85 taxa, and removed 15, so, there should be 70 other columns. 70+7 = 77 = YAY! 


#macroalgae
#meta = 7 --> 70 - 7 = 63
#was 62. 


macroalgae <- algae %>% 
  dplyr::select(-algal_turf, -crustose_corallines, -cyanophyta, -damselfish_turf, -lithophyllum_kotschyanum,
         -neogoniolithon_brassica_florida, -sporolithon_sp)

# did the filter work? 77-7 = 70 = good. 
#macroalgae dataframe has 70 variables. 7 of these are meta data, which means 63 of them are algae. We had
# 85 taxa, and removed 15, and now removed 7 more, so, there should be 63 other columns. 63+7 = 70 = YAY! 

meta_cols <- c("year", "location", "site", "habitat", "site_habitat", "transect", "quadrat")

meta_df <- algae[,which(names(algae) %in% meta_cols)]

algae_species_cols <- algae[,-which(names(algae) %in% meta_cols)]

macro_species_cols <- macroalgae[,-which(names(macroalgae) %in% meta_cols)]

# make long dataset
macro_long_data <- pivot_longer(
  data = macroalgae,
  cols = grep("acanthophora_spicifera", colnames(macroalgae)):grep("valonia_ventricosa", colnames(macroalgae)),
  names_to = "taxa",
  values_to = "percent_cover") %>% 
  mutate(prop_cover = percent_cover/100) %>% 
  dplyr::select(-percent_cover)

#new:1207899
#old:1188726
#difference is #19173
#cool, I think that checks out bc 50 quads * 6 sites * 4 habitats = 1200 * 16 years = 19200 & we know we are missing a few... 

#NOW SWITCHING THE SCRIPT ORDER, NEED TO ADD FG species to get a dataframe to give to Hillary, then ALL analyses need to come from this dataframe.. 

algae_fg_raw <- read_csv(here::here("data","MCR_Algal_Functional_Groups_09132025_updatedtax_v4.csv"))

#clean column names
algae_fg <- algae_fg_raw %>%
  janitor::clean_names()

#change species names to be lowercase & have underscore 
algae_fg$species <- gsub(" ", "_", tolower(algae_fg$species))
algae_fg$morphology <- trimws(algae_fg$functional_grouping_updated_08202025)
algae_fg$morphology <- gsub(" ", "_", algae_fg$morphology)

#rename species column to taxa

algae_fg <- algae_fg %>%
  dplyr::select(species, morphology) %>%
  dplyr::rename(taxa = species) %>%
  dplyr::rename(functional_group = morphology)

unique(algae_fg$functional_group) # 9 functional groups
unique(algae_fg$taxa) #63 taxa

#merge functional groups  
macro_functional_groups <- merge(macro_long_data, algae_fg, by = "taxa", all.x = TRUE)
#yay, same amount of observations + 1 variable 


unique(macro_functional_groups$taxa) #63
unique(macro_functional_groups$functional_group) #9

fg_NAs_QAQC <- macro_functional_groups %>%
  filter(is.na(functional_group))

#yay, 0 

#THIS IS THE DATA WE WANT TO WRITE OUT - THIS WILL GO ON EDI

write.csv(macro_functional_groups, "data/macroalgalfunctionalgroups_long_09162025.csv", row.names = FALSE)

#### GENERAL COVER DATAFRAME ####
# For figures 1 and merging with figure 2

### We also want to make a nice, summarized "cover_df" dataframe, using our filtered data from above.

cover_df <- data.frame(
  year = filtered_data$year,
  habitat = filtered_data$habitat,
  site = filtered_data$site,
  location = filtered_data$location,
  coral = filtered_data$coral,
  cca = filtered_data$crustose_corallines +
    filtered_data$lithophyllum_kotschyanum +
    filtered_data$neogoniolithon_brassica_florida +
    filtered_data$sporolithon_sp,
  macroalgae = rowSums(macro_species_cols),
  turf = filtered_data$algal_turf  + filtered_data$damselfish_turf, 
  sand = filtered_data$sand,
  coral_rubble = filtered_data$coral_rubble,
  cyanophyta = filtered_data$cyanophyta,
  other = filtered_data$ascidian + 
    filtered_data$corallimorpharia +
    filtered_data$heteractis_sp + 
    filtered_data$millepora_platyphylla +
    filtered_data$sarcophyton_sp + filtered_data$shell_debris +
    filtered_data$soft_coral + filtered_data$sponge + filtered_data$tridacna_sp +
    filtered_data$zooxanthellae + filtered_data$no_data + filtered_data$bare_space
)

cover_df$total <- rowSums(cover_df[,-which(names(cover_df) %in% meta_cols)])

cover_df %>%
  filter(total != 100) #nice 


#write this out... 
write.csv(cover_df, "data/cover_df_09162025.csv", row.names = FALSE)



#now, i think we need to flip long data to wide


taxa_sum_check <- macro_functional_groups %>%
  dplyr::group_by(location, year) %>%
  dplyr::summarise(sum_macrocover = sum(prop_cover))

macro_functional_groups %>%
  filter(location == "lter_2_backreef_algae_transect_1_quad_1") %>%
  filter(year == "2022")

# aggregate for site-level analyses

site_macro <- macroalgae %>%
  dplyr::group_by(year, site, habitat,site_habitat) %>%
  dplyr::summarize(across(acanthophora_spicifera:valonia_ventricosa, \(x) mean(x, na.rm = TRUE))) 

site_meta_cols <- c("year", "site", "habitat", "site_habitat")
site_meta_df <- site_macro[,which(names(site_macro) %in% meta_cols)]


#### CALCULATE DIVERSITY METRICS ####
alpha_diversity_quad_macro <-
  data.frame(
    year = macroalgae$year,
    location = macroalgae$location,
    shannon = diversity(macroalgae[, -which(names(macroalgae) %in% meta_cols)], "shannon"),
    richness = rowSums(macroalgae[, -which(names(macroalgae) %in% meta_cols)] > 0)
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
alpha_diversity_site_macro$percent_cover <- rowSums(site_macro[,-which(names(site_macro)  %in% colnames(site_meta_df))]) 

alpha_diversity_site_macro$prop_cover <- alpha_diversity_site_macro$percent_cover/100

alpha_diversity_site_macro$cover_trans <- sv_trans(alpha_diversity_site_macro$prop_cover)



#summarizing functional groups at the quad level
fg_summary <- macro_functional_groups %>%
  dplyr::group_by(location, year, functional_group, habitat, site) %>%
  dplyr::summarise(sum_fg_cover = sum(prop_cover)) 

max(fg_summary$sum_fg_cover)


#summarizing functional groups at the site level
#NOTE that this is ADDED and not averaged. This data is only used to find richness at the site level. LE did this as adding and as a sum, both end up the same from a richness perspective. 
fg_summary_site_SUM <- macro_functional_groups %>%
  dplyr::group_by(site, year, habitat, functional_group) %>%
  dplyr::summarise(sum_fg_cover = sum(prop_cover))



#QAQC
fg_summary_qaqc <- macro_functional_groups %>%
  dplyr::group_by(location, year, functional_group) %>%
  dplyr::summarise(sum_fg_cover = sum(prop_cover))

fg_qaqc <- fg_summary_qaqc %>%
  dplyr::group_by(location, year) %>%
  dplyr::summarise(sum_fg_check = sum(sum_fg_cover))

check_them <- merge(taxa_sum_check, fg_qaqc)

check_them %>%
  filter(all.equal(sum_macrocover, sum_fg_check) != TRUE)

#pivot wide at quad level
fg_summary_wide <- fg_summary %>%
  tidyr::pivot_wider(names_from = "functional_group", values_from = "sum_fg_cover")

colnames(fg_summary_wide)
#richness at every quad/year combination 
fg_summary_wide$functional_richness <- vegan::specnumber(fg_summary_wide[5:13])

unique(fg_summary_wide$functional_richness)
#0 to 4

##### REPEAT AT SITE LEVEL 
#pivot wide at site level
fg_summary_site_wide_SUM <- fg_summary_site_SUM %>%
  pivot_wider(names_from = "functional_group", values_from = "sum_fg_cover")

#richness at every site/year combination ---> NOT AVERAGED,
fg_summary_site_wide_SUM$functional_richness <- vegan::specnumber(fg_summary_site_wide_SUM[4:12])


#merge Noam's data with functional group summary 
alpha_diversity_quad_macro <- merge(alpha_diversity_quad_macro,
                                    fg_summary_wide[,c("location", "year", "site",  "habitat", "functional_richness")],
                                    by = c("location", "year", "site", "habitat"))

alpha_diversity_quad_macro %>%
  dplyr::select(location, year, richness, functional_richness) %>%
  dplyr::filter(location == "lter_1_backreef_algae_transect_1_quad_1")


#merge site level cover with site functional group richness
alpha_diversity_site_macro<-merge(alpha_diversity_site_macro,
                                  fg_summary_site_wide_SUM[,c("year", "site",  "habitat", "functional_richness")],
                                  by = c("year", "site",  "habitat"))

write.csv(alpha_diversity_quad_macro, "data/alpha_diversity_quad_macro.csv", row.names = FALSE)
write.csv(alpha_diversity_site_macro, "data/alpha_diversity_site_macro.csv", row.names = FALSE)


