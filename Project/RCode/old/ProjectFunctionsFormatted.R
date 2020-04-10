# 
# Course: IST687
# Name: Joyce Woznica
# Project Code: Package Functions
# Due Date: 03/19/2019
# Date Submitted:
#

# function section
# ------------------------------------------------------------------
# helpful functions for removing spaces
# need to clean out extra spaces from ends of lines
trim.leading<-function(x) {sub("^\\s+","",x)}
trim.trailing<-function(x) {sub("\\s+$","",x)}
trim<-function(x) {sub("^\\s+|\\s+$","",x)}
trimCity<-function(x) {sub("\\,.*$","",x)}
trimSlash<-function(x) {sub("/.*$","",x)}

# Need to find "buckets" for the information
# maybe pick under 25%, 25% to 50%, 50% to 75%, 75% and up?
buildCutOffs<- function(mini, maxi, numcuts)
{
  index<-numcuts
  cutoffs<-c(0)
  while(index>=1)
  {
    cutoffs<- c(cutoffs, round(maxi/index))
    index<-index-1
  }
  return(cutoffs)
}

# convert exponential to decimal
toDecimal<-function(x) {format(x,scientific = FALSE)}

# State Name to Abbreviation
name2abbr <- function (st)
{
  statesDF <- data.frame(state.abb, state.name)
  colnames(statesDF)<-c("abbr","name")
  res<-statesDF$abb[match(st, statesDF$name, nomatch="NA")]
  substr(res,1,3)
}
