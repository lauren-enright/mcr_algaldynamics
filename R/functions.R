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
    group_by(across(all_of(group))) %>% 
    summarise(across(all_of(columns), # summarize the columns in your group
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
