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

#algae_fg_raw <- read_csv(here::here("data","MCR_Algal_Functional_Groups_09132025_updatedtax_v4.csv"))
algae_fg_raw <- read_csv(here::here("data","MCR_Algal_Functional_Groups_09262025_updatedtax_v5.csv"))


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

unique(algae_fg$functional_group) # 9 functional groups, 8 after removing foliose 
unique(algae_fg$taxa) #63 taxa

#how many taxa in each functional group?
algae_fg %>%
  group_by(functional_group) %>%
  tally()

#merge functional groups  
macro_functional_groups <- merge(macro_long_data, algae_fg, by = "taxa", all.x = TRUE)
#yay, same amount of observations + 1 variable 


unique(macro_functional_groups$taxa) #63
unique(macro_functional_groups$functional_group) #9, now 8

fg_NAs_QAQC <- macro_functional_groups %>%
  filter(is.na(functional_group))


#yay, 0 

#THIS IS THE DATA WE WANT TO WRITE OUT - THIS WILL GO ON EDI

#write.csv(macro_functional_groups, "data/macroalgalfunctionalgroups_long_09262025.csv", row.names = FALSE)

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
#write.csv(cover_df, "data/cover_df_09162025.csv", row.names = FALSE)

