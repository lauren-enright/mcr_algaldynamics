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

data <- read.csv(here("data", "MCR_LTER_Annual_Survey_Benthic_Cover_20231211.csv"), stringsAsFactors = F) # replacements don't work when strings are factors

str(data)

levels(as.factor(data$Taxonomy_Substrate_Functional_Group)) # 86 levels
data$Site_habitat <- as.factor( paste(data$Site, data$Habitat))

#### REPLACE OUTDATED TAXA

# Julianna notes on taxa:
# Changed Caulerpa peltata to Caulerpa chemnitzia 
# Changed Chnoospora implexa to Pseudochnoospora implexa
# Changed Neogoniolithon frutescens to Neogoniolithon brassica-florida
# Changed Caulerpa pickeringii to Caulerpa webbiana
# Changed Boodlea kaeneana to Cladophoropsis membranacea
# Changed Dictyota divaricata to Dictyota cervicornis

### ADDING JULIANNA UPDATES HERE ###
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Chnoospora implexa"] <- "Pseudochnoospora implexa"

data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Caulerpa pickeringii"] <- "Caulerpa webbiana"

data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Boodlea kaeneana"] <- "Cladophoropsis membranacea"

data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Dictyota divaricata"] <- "Dictyota cervicornis"

# Caulerpa peltata is outdated --> Caulerpa racemosa JULIANNA UPDATE = Caulerpa chemnitzia
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Caulerpa peltata"] <- "Caulerpa chemnitzia"


### EVERYTHING ELSE BELOW AS BEFORE ###


# Boodlea kaeneana --> Cladophoropsis membranacea
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Boodlea kaeneana"] <- 
  "Cladophoropsis membranacea"

# Cladophoropsis luxurians --> Cladophora fuliginosa
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Cladophoropsis luxurians"] <-
  "Cladophora fuliginosa"

# Neogoniolithon frutescens --> Neogoniolithon brassica-florida
data$Taxonomy_Substrate_Functional_Group[data$Taxonomy_Substrate_Functional_Group == "Neogoniolithon frutescens"] <- 
  "Neogoniolithon brassica-florida"

# remove old levels
data <- droplevels(data)
levels(as.factor(data$Taxonomy_Substrate_Functional_Group)) # 84 levels --> 85 AFTER JULIANNA UPDATES


quad_level_summary <- data %>%
  dplyr::group_by(Year, Location, Site, Habitat, Site_habitat, Transect, Quadrat) %>%
  dplyr::summarize(sum = sum(Percent_Cover))

non100_quad_level <- quad_level_summary %>%
  dplyr::filter(sum != 100)


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

not_algae <- c("Ascidian", "Bare Space", "Coral", "Coral Rubble", "Corallimorpharia", "Heteractis sp.", "Millepora platyphylla", "No data", "Sand", "Sarcophyton sp.", "Shell Debris", "Soft Coral", "Sponge", "Tridacna sp.", "Zooxanthellae")

### CREATE DATAFRAME THAT IS JUST MACROALGAE
# Maintaining mat-forming cyanos even though they're not macroalgae in the traditional sense
# "Coelothrix irregularis", "Symploca hydnoides" 

not_macro <- c("Algal Turf", "Crustose Corallines", "Cyanophyta", "Damselfish Turf", "Lithophyllum kotschyanum", "Neogoniolithon brassica-florida", "Sporolithon sp." )


comb_wide <- cast(data, Year + Location + Site + Habitat + Site_habitat + Transect + Quadrat ~ Taxonomy_Substrate_Functional_Group, fun.aggregate = mean, value = "Percent_Cover") # this is the correct number of rows given the missing data

#21000 obs of 91 variables

#change NAs to 0s 

is.nan.data.frame <- function(x){
  do.call(cbind, lapply(x, is.nan))}

comb_wide[is.nan.data.frame(comb_wide)] <- 0

#21000 obs of 91 variables

#### QAQC WIDE DATAFRAME ####

comb_wide_quad_sum <- comb_wide %>%
  mutate(row_sums = rowSums(dplyr::select(., 8:91)))

#do we have data for all transects?

comb_wide_quad_sum %>%
  dplyr::group_by(Year, Site, Habitat) %>%
  dplyr::summarise(count = n()) # 50 quadrats per habitat/year/site (this is correct)

comb_wide_quad_sum %>%
  dplyr::group_by(Year, Site) %>%
  dplyr::summarise(count = n()) 

#should be 200 per year per site (50 at each habitat * 4 habitats)
#2005 and 2006 are one large collective survey
#2021 sites are at 100 (did not do forereef habitats due to COVID)

#do all quads = 100 ?

non100_wide_quad_level <- comb_wide_quad_sum %>%
  dplyr::filter(row_sums != 100)

#also 99 observations that do not equal 100, which matches what we found in the long data

#how many are no data (-1)
comb_wide_quad_sum %>%
  dplyr::filter(row_sums == -1) %>% dim()

#14

# grab the mistake quadrats 
mistakes_wide_quad_level <- comb_wide_quad_sum %>%
  dplyr::filter(row_sums != 100) %>%
  dplyr::filter(row_sums != -1)

comb_wide_quad_sum %>%
  dplyr::filter(row_sums != 100) %>% 
  dplyr::filter(Year == 2020) %>% dim()

#remaining 85 have something weird happening! Will send to Bob for updates. For now, 
# will filter these out

#want to make sure these 85 are the same we found using the long data

setdiff(mistakes_wide_quad_level$Location, mistakes_quad_level$Location)

setdiff(mistakes_quad_level$Location, mistakes_wide_quad_level$Location)

#### DEAL WITH 99s IN WIDE DATAFRAME ####
#LE talked to Hillary and we found that of the 85 rows that have sums that do not = 100, 49 of them are because a "-1" was accidentally added to the no data column, even though data was taken. (100 percent cover - 1 = 99). This happened only in 2017. Hillary will fix this eventually on EDI, but for now I am going to manually remove these -1 from the data set so we can continue with our analyses 

#show this error in the data

no_data_mistakes <- mistakes_wide_quad_level %>%
  filter(row_sums == 99)

unique(no_data_mistakes$`No data`)

#remove the -1 from the No data column

comb_wide_quad_sum$`No data`[comb_wide_quad_sum$row_sums == 99 & comb_wide_quad_sum$`No data` == -1] <- 0

#check to see if it worked

cleaned_sumrows99 <- comb_wide_quad_sum %>%
  filter(row_sums == 99)

unique(cleaned_sumrows99$`No data`)

#### FILTER RAMP UP YEARS ####

#we want to filter out 2005 and 2006 - these were "ramp up years"
#also want to filter out rows where the sum does not = 0 
wide_alldata_filtered <- comb_wide_quad_sum_final %>%
  dplyr::filter(Year != 2005 & Year != 2006) %>%
  dplyr::filter(row_sums == 100)

#should be 19750 obs
dim(wide_alldata_filtered)

#writing one file so column names will work with Noam's code
write.csv(wide_alldata_filtered, "data/allbenthicdata_cleaned.csv", row.names = FALSE)

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


write.csv(wide_alldata_filtered_cleaned_column_names, "data/allbenthicdata_cleaned_colnames.csv", row.names = FALSE)

#we also want a copy that only includes taxa we care about 
#not_algae character string created by Noam in Chunk 4 (Subsetting the data) and includes 14 
#categories  -- these categories will be listed in a separate document as well 

wide_algaeonly_filtered <- wide_alldatsa_filtered[ , -which(names(wide_alldata_filtered) 
                                                            %in% not_algae)]

#need to still remove the row_sums column for algae only 

wide_algaeonly_filtered <- wide_algaeonly_filtered %>%
  dplyr::select(-row_sums)

write.csv(wide_algaeonly_filtered, "algaeonly_cleaned.csv", row.names = FALSE)

#clean names so column names are not so hard to work with

wide_algaeonly_cleaned_column_names <- 
  wide_algaeonly_filtered %>%
  # clean column names
  janitor::clean_names() %>% 
  # clean the inside as well
  mutate(location = gsub(" ", "_", str_to_lower(location)),
         site = gsub(" ", "_", str_to_lower(site)),
         habitat = gsub(" ", "_", str_to_lower(habitat)),
         site_habitat = gsub(" ", "_", str_to_lower(site_habitat)),
  ) 

write.csv(wide_algaeonly_cleaned_column_names, "data/algaeonly_cleaned_colnames.csv", row.names = FALSE)

#### FILTERED DATA QAQC ####
# you can use this to see which sites lost quads during filtering process 
#should be 50 per year/site/habitat combo

wide_alldata_filtered %>%
  dplyr::group_by(Year, Site, Habitat) %>%
  dplyr::summarise(count = n())

#should be 200 per year/site combo

wide_alldata_filtered %>%
  dplyr::group_by(Year, Site) %>%
  dplyr::summarise(count = n())