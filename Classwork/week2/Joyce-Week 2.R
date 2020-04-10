# 
# Course: IST707
# Name: Joyce Woznica
# Subject: Week 2 Exercises
# Due Date: 10/19/2019
# Date Submitted:
#
# This loads applicable libraries
install.packages("plyr", dependencies=TRUE)
library(plyr)

#specify the packages of interest
packages=c("readxl", "arules",  "arulesViz", "kernlab", "e1071", "gridExtra", "ggplot2", "caret", "CRAN", "zipcodes",
           "stargazer", "gmodels", "pastecs", "Hmisc", "reshape2", "plyr", "plotly", "psych", "maps", "ggmap",
           "dplyr")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()

# I find this does not always work, so added to install when required here
install.packages("psych", dependencies = TRUE)
library(psych)
require(dplyr)

# load titanic dataset using read.csv
TitanicData <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week2/titanic-train.csv", header = TRUE, na.strings = "")
# convert the passenger to nominal
str(TitanicData)
TitanicData$PassengerId <- factor(TitanicData$PassengerId)
TitanicData$Survived <- factor(TitanicData$Survived)
TitanicData$Pclass <- ordered(TitanicData$Pclass)
str(TitanicData)

# look at missing values

(is.na(TitanicData))  ## rows with NA
(sum(is.na(TitanicData)))

## You can list all the complete rows
## Note: complete.cases(TitanicTestData)   are the rows
## you want to see. and the ,] means all columns
(TitanicData[complete.cases(TitanicData),])
(sum(complete.cases(TitanicData)))
(nrow(TitanicData))

colnames(TitanicData)

## How many rows are complete?
cat("The Titanic Test data has a total of ", nrow(TitanicData), "rows.")
CompleteRows <- (nrow(TitanicData[complete.cases(TitanicData),]))
cat("The Titanic Test data has a total of ", CompleteRows, "complete rows.")
## This will give you the count of all elements that are NA (rather than rows)
(length(which(!is.na(TitanicData))))   ## Total number of NAs 

for(colname in names(TitanicData)){
  NAbycol <- sum(is.na(TitanicData[colname]))
  cat("\nThe num of missing values in column ", colname, "is  ", NAbycol)
}

# remove the Cabin column
TitanicData["Cabin"] <- NULL   #get rid of Cabin
str(TitanicData)

# remove any rows with NA from dataset
TitanicData <- na.omit(TitanicData)

nrow(TitanicData)

# Summary Statistics
summary(TitanicData)
# to find the mode
table(TitanicData$Age)[which.max(table(TitanicData$Age))]
# Variance
var(TitanicData$Age)
# Standard Dev
sd(TitanicData$Age)

# a much faster way
install.packages("pastecs")
library(pastecs)
genStat <- stat.desc(TitanicData, basic=F)
View(genStat)

qt <- quantile(TitanicData$Age)
# Interquartile Range
IQR <- qt[['75%']]-qt[['25%']]

# for mode for nominal variables
Gender <- table(TitanicData$Sex)
table(TitanicData$Sex[which.max(table(TitanicData$Sex))])

table(TitanicData$Pclass[which.max(table(TitanicData$Pclass))])

# boxplot  3rd class passengers and fares
fill <- "lightblue"
line <- "blue"

Titanic3rdClass <- TitanicData[which(TitanicData$Pclass==3),]
g<- ggplot(Titanic3rdClass, aes(x=Pclass,y=Fare)) 
g<- g + geom_boxplot(fill=fill, color=line)
g<- g + ggtitle("Fare by 3rd Class") + theme(plot.title=element_text(hjust=0.5))
g

# histogram of Fares Paid in 3rd class
g<- ggplot(data=Titanic3rdClass, aes(Titanic3rdClass$Fare))
g<- g + geom_histogram(col="blue", fill="green", bins=15)
g<- g + ggtitle("Fare by 3rd Class")  + theme(plot.title=element_text(hjust=0.5))
g

tab<-table(TitanicData$Embarked, TitanicData$Survived)
rownames(tab)<-c("Cherbourg", "Queenstown", "Southampton")
rownames(tab)
colnames(tab)<-c("Did Not Survive", "Survived")
colnames(tab)
tab
barplot(tab, beside=T, legend=rownames(tab), col=c("blue","green","orange"), las=1,
        main="Survival by Embarkment Port", ylab="Number of Individuals", ylim=c(0,375))

# read in sales data
sales <- read.csv("/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week2/sales.csv", header = TRUE, na.strings = "")
salesByRegion=aggregate(cbind(Mon, Tue, Wed, Thu, Fri, Sat, Sun)~Region, data=sales, sum)

# sales in weekend
InWeekend <- rowSums(sales[,c("Sat","Sun")])
salesNew <- data.frame(sales,InWeekend)

salesInWeekend <- aggregate(cbind(InWeekend)~Region, data=salesNew, mean)

# Aggregate fares for men and women in Titanic
fareByGender <- aggregate(cbind(Fare)~Sex, data=TitanicData, mean)

# 2.10.1
# Discretization of Fares
FareBins <- cut(TitanicData$Fare, breaks=c(0,10,20,30,40,50, 60,70,80,90, Inf),
               labels=c("< 10", "10s", "20s", "30s", "40s", "50s", "60s", "70s", " 80s", "> 80"))
barplot(table(FareBins), main="Fare Distribution", xlab="Fare", ylab="Number of Passengers", ylim=c(0,275), las=1,col=c("blue"))

# Log Transformation
plot(TitanicData$Fare,log(TitanicData$Fare), col="red", main="Titanic Fare to Log of Titanic Fare", xlab="Titanic Fare",
     ylab="Log of Titanic Fare", pch=20)

# zScore
zTitanic <- scale(TitanicTestData$Fare, center=TRUE, scale=TRUE)
hist(zTitanic, main="Histogram of Z-scores for Titanic Fares", xlab="Z-Score of Fare", las=1, col="goldenrod")

# min/max
MinMaxTitanic <- (TitanicData$Fare - min(TitanicData$Fare, 
                                      na.rm=TRUE))/(max(TitanicData$Fare) - min(TitanicData$Fare))
hist(MinMaxTitanic, main="Min and Max Titanic Fares", las=1, col="purple")


# Sampling
# random sample of 100 of Titanic Data
sampleTitanic <- TitanicData[sample(nrow(TitanicData),100, replace=FALSE),]

# systematic sampling
systemSampleTitanic <- TitanicData[seq(1,nrow(TitanicData),10),]

