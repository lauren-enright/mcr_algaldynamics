#### Tidy fish data
### Created by: Callie Stephenson
### Created on: 6/11/24

library(tidyverse)
library(here)
library(fishualize)
library(ggplot2)
library(dplyr)
library(stringr) #to make functional fish groups match taxonomy
library(tidyr)

fish <- read.csv("data/MCR_LTER_Annual_Fish_Survey_20230615.csv") %>% 
  filter(!Year == "(100840 rows)")
fish_functional <- read.csv("data/recharge_fish_fun_groups.csv")

#I was given the fish_functional sheet for updated trophic levels for some species. but it doesn't have all species
#So, I'm going to make a column for functional_group in fish that has either what it is listed as in the recharge sheet
#and if not, I'm going to have it take the fine_trophic

#Capitalize the first letter of the functional group to match style of fine trophic
fish_functional$functional_group <- str_to_title(fish_functional$functional_group)

#Change the taxonomy to have a capital letter only for Genus (the first letter)
capitalize_first <- function(x) {
  # Convert the first letter to uppercase and concatenate it with the rest of the string
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}

#make the Taxonomy joining column
fish_functional$Taxonomy <- sapply(fish_functional$genus_species, capitalize_first)

#Remove NA values and duplicates
fish_functional_no_na <- fish_functional[!is.na(fish_functional$functional_group), ] %>% 
  distinct(Taxonomy, .keep_all = TRUE)

#change around some weirdly-named functional groups
fish_functional_no_na_fixed <- fish_functional_no_na %>%
  mutate(functional_group = if_else(functional_group == 'Benthicinvertebrateconsumer', 'Benthic Invertebrate Consumer', functional_group)) %>% 
  mutate(functional_group = if_else(functional_group == 'Planktivoreexclusively', 'Planktivore_exclusively', functional_group)) %>% 
  mutate(functional_group = if_else(functional_group == 'Fishscaleconsumer', 'Fish Scale Consumer', functional_group)) %>% 
  mutate(functional_group = if_else(functional_group == 'Concealedcropper', 'Concealed Cropper', functional_group)) %>% 
  mutate(functional_group = if_else(functional_group == 'Herbivoredetritivore', 'Herbivore/Detritivore', functional_group))

#Tidy them up:
  # Perform a left join to combine data frames on 'Taxonomy' column
combined_df <- left_join(fish, fish_functional_no_na_fixed[,c("herbivore_status", "Taxonomy", "functional_group")], by = "Taxonomy")

# Create the 'functional_group' column
combined_df <- combined_df %>%
  dplyr::mutate(Functional_group = coalesce(functional_group, Fine_Trophic))

#Ok, so now we need to fix all the species that it didn't know where herbivores before because the herbivore_status was only in the recharge table that only has 116 species...
herbivore_groups <- c("Cropper", "Browser", "Excavator", "Herbivore/Detritivore", "Brusher", "Scraper", "Concealed Cropper")
cleaned_fish <- combined_df %>%
  mutate(herbivore_status = if_else(functional_group %in% herbivore_groups, "Y", "N"))

#now lets summarize it for actual use:
biomass_by_functional_group_wide <- cleaned_fish %>%
  group_by(Year, Site, Habitat, Functional_group) %>%
  summarize(total_biomass = sum(Biomass, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = Functional_group, values_from = total_biomass)

biomass_by_herb_status_wide <- cleaned_fish %>%
  group_by(Year, Site, Habitat, herbivore_status) %>%
  summarize(total_biomass = sum(Biomass, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = herbivore_status, values_from = total_biomass) %>% 
  rename('Non-Herbivore Biomass' = N) %>% 
  rename('Herbivore Biomass' = Y)

output_path <- file.path("..", "data", "biomass_herb_summary.csv")
write.csv(biomass_by_herb_status_wide, output_path, row.names = FALSE, col.names = FALSE)

write_csv(biomass_by_herb_status_wide, here("data", "biomass_herb_summary.csv"))
write_csv(biomass_by_functional_group_wide, here("data", "biomass_functional_summary.csv"))
  