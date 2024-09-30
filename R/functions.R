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

se <- function(x){
  sd(x)/sqrt(n())
}