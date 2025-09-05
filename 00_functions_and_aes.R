#### 
# Functions for Manuscript 1 #
####

# coefficient of variation
CV<-function(x){
  return(sd(x,na.rm=T)/mean(x,na.rm=T))
}

inv_logit <- function(x){
  exp(x)/(1+exp(x))
}

# smithson verkuilen transformation for 0-1 continuous data
sv_trans <- function(prop, s = 0.000005){
  (prop*(length(prop) - 1) + s)/length(prop)
  # where prop is the vector of the proportional value you're transforming, 
  # N is the sample size, which is specified by taking the number of rows/observations from a given dataframe,
  # and s is a small offset 
}

# standard error
se <- function(x){
  sd(x)/sqrt(n())
}

# function to extract the ranges of the data for plotting
extract_ranges <- function(df, # a dataframe
                           group, # grouping variable (e.g. habitat)
                           columns # columns you want to summarize (e.g. richness, cover, synchrony)
){
  df %>% # in your dataframe
    dplyr::group_by(across(all_of(group))) %>% 
    dplyr::summarise(across(all_of(columns), # summarize the columns in your group
                            list(min = ~min(.x, na.rm = TRUE), max = ~max(.x, na.rm = TRUE)), # by taking the min/max of them
                            .names = "{.fn}_{.col}")) # and assigning them to the columns name "min/max_value"
}

# function for filtering ranges of emtrends to only fit range of actual data
filter_ranges <- function(trend, # emtrends df created from emmip
                          range_obj, # output df from extract_ranges() custom function
                          group, # group over which to summarize (e.g. habitat)
                          value # value to filter range (e.g. richness)
){
  trend %>% # take emtrend
    left_join(range_obj, by = group) %>% # join with range object
    filter(.data[[value]] >= .data[[paste0("min_", value)]] & # filter so emtrend only covers range of actual values
             .data[[value]] <= .data[[paste0("max_", value)]])
}


#### PLOT AESTHETICS ####
trends_theme <- theme_bw() + 
  theme(axis.title.x = element_text(size=24), 
        axis.text.x = element_text(size = 22, angle = 45, hjust = 1),
        axis.title.y = element_text(size=24), 
        axis.text.y = element_text(size = 22),
        plot.title = element_text(size = 24),
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_rect(fill = "white"),
        legend.text = element_text(size=24),
        legend.title = element_blank(),
        legend.position = 'bottom', # move legend to bottom,
        legend.key.size = unit(2, 'cm'), # increase legend size
        legend.box = 'horizontal', # make legend horizontal
        legend.box.background = element_rect(colour = "black")) 

model_themes <-   theme_bw() + 
  theme(axis.title.x = element_text(size=24), 
        axis.title.y = element_text(size=24), 
        axis.text.x = element_text(size=22), 
        axis.text.y = element_text(size=22),
        legend.text = element_text(size=24),
        plot.title = element_text(size=26), 
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_rect(fill = "white"),
        legend.title = element_blank(),
        legend.key.size = unit(2, 'cm'), # increase legend size
        legend.position = 'bottom', # move legend to bottom
        legend.box = 'horizontal', # make legend horizontal
        legend.box.background = element_rect(colour = "black"))  # add box around legend

habitat_colours <- c("Fringing" = "#D81B60",
                     "Backreef" = "#FFC107",
                     "Forereef 10m" = "#1E88E5",
                     "Forereef 17m" = "#004D40")

functional_group_colours <- c("Coral" = "darkmagenta",
                              "CCA" = "deeppink3",
                              "Macroalgae" = "chartreuse4",
                              "Turf" =  "#004D40")