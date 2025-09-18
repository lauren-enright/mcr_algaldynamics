rm(list=ls()) # clean up
library(tidyverse)
library(knitr)
library(dplyr)
library(plyr)
library(car)
library(reshape)
library(reshape2)
library(tidyr)
library(effects)
library(devtools)
library(here)

#read in raw data from EDI 
data_edi <- read.csv(here::here("data", "MCR_LTER_Annual_Survey_Benthic_Cover_20231211.csv"), stringsAsFactors = F) # replacements don't work when strings are factors

#make a copy of the EDI data to edit
data <- data_edi
str(data)

data_edi %>%
  filter(Location == "LTER 5 Fringing Reef Algae Transect 3 Quad 4") %>%
  filter(Year == 2011)


sort(unique(data_edi$Taxonomy_Substrate_Functional_Group)) 

levels(as.factor(data_edi$Taxonomy_Substrate_Functional_Group)) # 86 levels
data$Site_habitat <- as.factor( paste(data$Site, data$Habitat))

#### REPLACE OUTDATED TAXA

# Julianna notes on taxa:
# Changed Caulerpa peltata to Caulerpa chemnitzia  --> YES
# Changed Chnoospora implexa to Pseudochnoospora implexa
# Changed Neogoniolithon frutescens to Neogoniolithon brassica-florida
# Changed Caulerpa pickeringii to Caulerpa webbiana --> YES
# Changed Boodlea kaeneana to Cladophoropsis membranacea
# Changed Dictyota divaricata to Dictyota cervicornis

### ADDING JULIANNA UPDATES HERE ###
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Chnoospora implexa"] <- "Pseudochnoospora implexa"

data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Caulerpa pickeringii"] <- "Caulerpa webbiana"

data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Boodlea kaeneana"] <- "Cladophoropsis membranacea"

data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Dictyota divaricata"] <- "Dictyota cervicornis"

# Caulerpa peltata is outdated --> Caulerpa chemnitzia
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Caulerpa peltata"] <- "Caulerpa chemnitzia"

# Cladophoropsis luxurians --> Cladophora fuliginosa
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Cladophoropsis luxurians"] <-
  "Cladophora fuliginosa"

# Neogoniolithon frutescens --> Neogoniolithon brassica-florida
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Neogoniolithon frutescens"] <- 
  "Neogoniolithon brassica-florida"

# remove old levels
data <- droplevels(data) 
#which levels do we lose --> none, this is to remove potential ghost categories from the renames.

levels(as.factor(data_edi$Taxonomy_Substrate_Functional_Group))
#data from original EDI has 86 levels

levels(as.factor(data$Taxonomy_Substrate_Functional_Group)) # 85 AFTER JULIANNA UPDATES
#new data has 85 levels. This is because we changed "Boodlea kaeneana" to Cladophoropsis membranacea, 
#and Cladophoropsis membranacea was already in the original EDI data

data_edi %>%
  filter(Taxonomy_Substrate_Functional_Group == "Cladophoropsis membranacea") %>%
  tally()

data_edi %>%
  filter(Taxonomy_Substrate_Functional_Group == "Boodlea kaeneana") %>%
  tally()

#1449 --> Cladophoropsis membranacea
#450 --> "Boodlea kaeneana"

data %>%
  filter(Taxonomy_Substrate_Functional_Group == "Cladophoropsis membranacea") %>%
  tally()
#1899, yep because 1449 + 450 = 1899

setdiff(data_edi$Taxonomy_Substrate_Functional_Group, data$Taxonomy_Substrate_Functional_Group)

#[1] "Caulerpa peltata"          "Chnoospora implexa"        "Cladophoropsis luxurians" 
#[4] "Neogoniolithon frutescens" "Caulerpa pickeringii"      "Boodlea kaeneana"         
#[7] "Dictyota divaricata" 

setdiff(data$Taxonomy_Substrate_Functional_Group, data_edi$Taxonomy_Substrate_Functional_Group)

#[1] "Caulerpa chemnitzia"             "Pseudochnoospora implexa"        "Cladophora fuliginosa"          
#[4] "Neogoniolithon brassica-florida" "Caulerpa webbiana"               "Dictyota cervicornis" 

# Changed Caulerpa peltata to Caulerpa chemnitzia 
# Changed Chnoospora implexa to Pseudochnoospora implexa
# Changed Cladophoropsis luxurians to Cladophora fuliginos
# Changed Neogoniolithon frutescens to Neogoniolithon brassica-florida
# Changed Caulerpa pickeringii to Caulerpa webbiana
# Changed Boodlea kaeneana to Cladophoropsis membranacea 
# Changed Dictyota divaricata to Dictyota cervicornis

quad_level_summary <- data %>%
  dplyr::group_by(Year, Location, Site, Habitat, Site_habitat, Transect, Quadrat) %>%
  dplyr::summarize(sum = sum(Percent_Cover))

non100_quad_level <- quad_level_summary %>%
  dplyr::filter(sum != 100)
  
unique(data$Habitat)

# can see that there are 99 observations that do not equal 100
dim(non100_quad_level)

# can look at which ones have no data 
data %>% 
  filter(Taxonomy_Substrate_Functional_Group == "No data")

#how many are no data (-1)
quad_level_summary %>%
  dplyr::filter(sum == -1)

#14

mistakes_quad_level <- quad_level_summary %>%
  dplyr::filter(sum != 100) %>%
  dplyr::filter(sum != -1)

#remaining 85 have something weird happening! Will send to Bob for updates. For now, 
# will filter these out

unique(quad_level_summary$sum)

#write out these 85 

#write.csv(mistakes_quad_level, "mistakes_quad_level.csv", row.names = FALSE)

### CREATE DATAFRAME THAT IS JUST ALGAE

#not_algae <- c("Ascidian", "Bare Space", "Coral", "Coral Rubble", "Corallimorpharia", "Heteractis sp.", "Millepora platyphylla", "No data", "Sand", "Sarcophyton sp.", "Shell Debris", "Soft Coral", "Sponge", "Tridacna sp.", "Zooxanthellae")

### CREATE DATAFRAME THAT IS JUST MACROALGAE
# Maintaining mat-forming cyanos even though they're not macroalgae in the traditional sense
# "Coelothrix irregularis", "Symploca hydnoides" 

#not_macro <- c("Algal Turf", "Crustose Corallines", "Cyanophyta", "Damselfish Turf", "Lithophyllum kotschyanum", "Neogoniolithon brassica-florida", "Sporolithon sp." )


comb_wide <- reshape::cast(data, Year + Location + Site + Habitat + Site_habitat + Transect + Quadrat ~ Taxonomy_Substrate_Functional_Group, fun.aggregate = mean, value = "Percent_Cover") # this is the correct number of rows given the missing data
colnames(data)

#combwide --> 21000 obs of 92 variables

#test_old <- read.csv(here::here("data", "allbenthicdata_cleaned.csv"))
#this old, but has 92 columns so I think the old code notes about 91 are just wrong?
#has only 19701 obserbations 
test_old2 <- read.csv(here::here("data", "allbenthicdata_cleaned_colnames.csv"))
#has 19750  obserbations 
#10 columns 
#7 are meta data 

colnames(test_old2)


#change NAs to 0s 

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

comb_wide[is.nan.data.frame(comb_wide)] <- 0

#21000 obs of 92 again, 92 variables now

colnames(comb_wide)


#### QAQC WIDE DATAFRAME ####

comb_wide_quad_sum <- comb_wide %>%
  mutate(row_sums = rowSums(dplyr::select(., 8:92)))

#do we have data for all transects?

comb_wide_quad_sum %>%
  dplyr::group_by(Year, Site, Habitat) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::filter(count != 50)  # 50 quadrats per habitat/year/site (this is correct)
 
#yay, 0

comb_wide_quad_sum %>%
  dplyr::group_by(Year, Site) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::filter(count != 200) 

#should be 200 per year per site (50 at each habitat * 4 habitats)
#18 that are not --> 
#2005 and 2006 are one large collective survey
#2021 sites are at 100 (did not do forereef habitats due to COVID)

#do all quads = 100 ?

non100_wide_quad_level <- comb_wide_quad_sum %>%
  dplyr::filter(row_sums != 100) 

#also 99 observations that do not equal 100, which matches what we found in the long data :) 

#how many are no data (-1)
comb_wide_quad_sum %>%
  dplyr::filter(row_sums == -1) %>% dim()

#14

# grab the mistake quadrats 
mistakes_wide_quad_level <- comb_wide_quad_sum %>%
  dplyr::filter(row_sums != 100) %>%
  dplyr::filter(row_sums != -1) #these are from no data 

##remaining 85 have something weird happening! Will send to Bob for updates. For now, will filter them out.

comb_wide_quad_sum %>%
  dplyr::filter(row_sums != 100) %>% 
  dplyr::filter(Year == 2020) %>% dim()

#looking just at 2020 ones ^^  --> 23 

#want to make sure these 85 are the same we found using the long data

setdiff(mistakes_wide_quad_level$Location, mistakes_quad_level$Location)

setdiff(mistakes_quad_level$Location, mistakes_wide_quad_level$Location)

#no differences, YAY!

#### DEAL WITH 99s IN WIDE DATAFRAME ####
#LE talked to Hillary and we found that of the 85 rows that have sums that do not = 100, 49 of them are because a "-1" was accidentally added to the no data column, even though data was taken. (100 percent cover - 1 = 99). This happened only in 2017. Hillary will fix this eventually on EDI, but for now I am going to manually remove these -1 from the data set so we can continue with our analyses 

#show this error in the data


no_data_mistakes <- mistakes_wide_quad_level %>%
  filter(row_sums == 99)

#yep, 49 obvs

unique(no_data_mistakes$`No data`)

#remove the -1 from the No data column

comb_wide_quad_sum$`No data`[comb_wide_quad_sum$row_sums == 99 & comb_wide_quad_sum$`No data` == -1] <- 0

#check to see if it worked

cleaned_sumrows99 <- comb_wide_quad_sum %>%
  filter(row_sums == 99)

unique(cleaned_sumrows99$`No data`)

comb_wide_quad_sum %>%
  filter(row_sums == 99) %>% dim()
#49
         
#this is 49. 

#need to add these "fixed" quads back in 
comb_wide_quad_sum_final <- comb_wide_quad_sum %>%
  dplyr::select(-row_sums) %>% #remove old columns
  mutate(row_sums_final = rowSums(dplyr::select(., 8:92)))

#### FILTER RAMP UP YEARS ####

#we want to filter out 2005 and 2006 - these were "ramp up years"
#also want to filter out rows where the sum does not = 0 
wide_alldata_filtered <- comb_wide_quad_sum_final %>%
  dplyr::filter(Year != 2005 & Year != 2006) %>%
  dplyr::filter(row_sums_final == 100)

#should be 19750 obs --> yay! This is with "99" quads added back in!! 
dim(wide_alldata_filtered)


#writing one file so column names will work with Noam's code
#write.csv(wide_alldata_filtered, "data/allbenthicdata_cleaned.csv", row.names = FALSE)

#clean names so column names are not so hard to work with

wide_alldata_filtered_cleaned_column_names <- 
  wide_alldata_filtered %>%
  janitor::clean_names() %>% 
  # Julianna is adding this code here IF we want it to clean the inside:
  mutate(location = gsub(" ", "_", str_to_lower(location)),
         site = gsub(" ", "_", str_to_lower(site)),
         habitat = gsub(" ", "_", str_to_lower(habitat)),
         site_habitat = gsub(" ", "_", str_to_lower(site_habitat)),
  ) 

#this is the only csv we need to write out of this script as of september 15, 2025
write.csv(wide_alldata_filtered_cleaned_column_names, "data/allbenthicdata_cleaned_colnames_09162025.csv", row.names = FALSE)


