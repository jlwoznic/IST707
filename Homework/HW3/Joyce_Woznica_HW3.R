# 
# Course: IST 707
# Name: Joyce Wozninca
# Homework #3
# Code: HW 3 R Code
# Due Date: 10/25/2019
#

# ----------------------------------------------------------------------------------------------------------------------
# This loads applicable libraries and packages (and some extras)

# vspecify the packages of interest
packages <- c("ggplot2", "arules", "arulesViz")
#packages=c("ggplot2", "caret", "stargazer", "pastecs", "reshape2", "psych", "tidyverse", "arules", "arulesViz")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

install.packages("plyr", dependencies=TRUE)
library(plyr)
install.packages("dplyr", dependencies=TRUE)
library(dplyr)

#verify they are loaded
search()

# ----------------------------------------------------------------------------------------------------------------------
# Data loading
# load bankDF using read.csv
bankDF <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Homework/HW3/bankdata_csv_all.csv", header = TRUE, na.strings = "")
str(bankDF)

# verify no missing data
Total <-sum(is.na(bankDF))
cat("The number of missing values in Bank Data is ", Total )

# Total PEPs purchased
tPep <- length(which(bankDF$pep == "pep=YES"))
# 274 YES, 326 NO

# ----------------------------------------------------------------------------------------------------------------------
# Data Preparation
# Significant portions taken from Jeremy Bolton's code
# Create bins for the ages
bankDF$age <- cut(bankDF$age, breaks = c(0,10,20,30,40,50,60,Inf),
              labels=c("child","teens","twenties","thirties","fourties","fifties","old"))

# Discretize income by equal-width bin
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

incomeCuts<-buildCutOffs(min(bankDF$income), max(bankDF$income), 4)

# opted to use code provided by Jeremy Bolton, but changed to not provide scientific notation
min_income <- round(min(bankDF$income),2)
max_income <- round(max(bankDF$income),2)
bins <- 3 
width <- round((max_income - min_income)/bins,2);
bankDF$income <- cut(bankDF$income, breaks=seq(min_income, max_income, width),dig.lab=7)

# Convert numeric to nominal for "children"
bankDF$children=factor(bankDF$children)

# Now the second step of conversion, changing "YES" to "[variable_name]=YES".
#bankDF$married=dplyr::recode(bankDF$married, YES="married=YES", NO="married=NO")
#bankDF$car=dplyr::recode(bankDF$car, YES="car=YES", NO="car=NO")
#bankDF$save_act=dplyr::recode(bankDF$save_act, YES="save_act=YES", NO="save_act=NO")
#bankDF$current_act=dplyr::recode(bankDF$current_act, YES="current_act=YES", NO="current_act=NO")
#bankDF$mortgage=dplyr::recode(bankDF$mortgage, YES="mortgage=YES", NO="mortgage=NO")
#bankDF$pep=dplyr::recode(bankDF$pep, YES="pep=YES", NO="pep=NO")


# function to run associative rule mining
# df - dataframe
# conf_int - confidence interval
# supp_num = supp
# max_length - maximum length
myARM <- function (df, conf_int, supp_num, min_length, max_length)
{
  apriori(df, parameter=list(supp=supp_num, conf=conf_int, minlen=min_length, maxlen=max_length),
          appearance = list(default="lhs", rhs="pep=pep=YES"),
          control = list(verbose=F))
}

myRules <- myARM(bankDF, 0.9, 0.001, 2, 4)
myRules <- myARM(bankDF, 0.9, 0.07, 2, 4)
myRules <- myARM(bankDF, 0.95, 0.05, 2, 4)

# Best set of rules
myRules <- myARM(bankDF, 0.9, 0.035, 2, 5)

inspect(myRules)
# inspect the rules
inspect(myRules[1:25])
## sorted
SortedmyRules_conf <- sort(myRules, by="confidence", decreasing=TRUE)
inspect(SortedmyRules_conf[1:20])

SortedmyRules_sup <- sort(myRules, by="support", decreasing=TRUE)
inspect(SortedmyRules_sup[1:20])

SortedmyRules_lift <- sort(myRules, by="lift", decreasing=TRUE)
inspect(SortedmyRules_lift)

# ----------------------------------------------------------------------------------------------------------------------
# Visualize the Rules
plot (SortedmyRules_sup[1:10],method="graph",interactive=TRUE,shading="confidence") 
plot (SortedmyRules_conf[1:10],method="graph",interactive=TRUE,shading="confidence") 
arulesViz::plotly_arules(SortedmyRules_conf[1:30])
arulesViz::plotly_arules(SortedmyRules_sup[1:30])
