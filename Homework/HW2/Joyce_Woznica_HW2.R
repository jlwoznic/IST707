# 
# Course: IST 707
# Name: Joyce Wozninca
# Homework #2
# Code: HW 2 R Code
# Due Date: 10/18/2019
#

# ----------------------------------------------------------------------------------------------------------------------
# This loads applicable libraries and packages (and some extras)

# vspecify the packages of interest
# cran is not available for 3.6.1 - "CRAN", "zipcodes",
packages=c("ggplot2", "caret", "stargazer", "pastecs", "reshape2", "psych", "tidyverse")

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
#install.packages("dplyr")
#library(dplyr)
#install.packages("plyr")
#library(plyr)

#verify they are loaded
search()

# ----------------------------------------------------------------------------------------------------------------------
# Data loading
# load CourseData using read.csv
MyStoryData <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Homework/HW2/data-storyteller.csv", header = TRUE, na.strings = "")

## See all the "dots" in the column names?
colnames(MyStoryData)
## This is not good.
## Update the column names in MyStoryData...
colnames(MyStoryData) <- gsub("[.]", "", colnames(MyStoryData))
colnames(MyStoryData) <- gsub("[[:digit:]]", "", colnames(MyStoryData))

# verify no missing data
Total <-sum(is.na(MyStoryData))
cat("The number of missing values in StoryTeller data is ", Total )

## To clean this data, we can look through the variables and make sure that the data for each variable is in
## the proper range.
## The data shows the *number of students* in each category.
## This value cannot be negative - so 0 is the min. We do not know the max, but we
## might be suspecious of very large numbers. 

## Let's check each numerical variable to see that it is >= 

for(varname in names(MyStoryData)){
  ## Only check numeric variables
  if(sapply(MyStoryData[varname], is.numeric)){
    cat("\n", varname, " is numeric\n")
    ## Get median
    (Themedian <- sapply(MyStoryData[varname],FUN=median))
    ##print(Themedian)
    ## check/replace if the values are <=0 
    MyStoryData[varname] <- replace(MyStoryData[varname], MyStoryData[varname] < 0, Themedian)
  }
}
# for this data, section needs to be a factor
str(MyStoryData)
MyStoryData$Section <- factor(MyStoryData$Section)
str(MyStoryData)

# reorder the columns
MyStoryData <- MyStoryData[c("School", "Section", "Completed", "VeryAhead", "Middling", "Behind", "MoreBehind", "VeryBehind")]

# ----------------------------------------------------------------------------------------------------------------------
# Descriptive Statistics
summary(MyStoryData)

# from pastecs package
# only works on numeric variables in the set - provides additional information over "summary"
genStat <- stat.desc(MyStoryData, basic=T)
View(genStat)

# Can also use describe for similar information in a different format
describe(MyStoryData)

# descriptive statistics from using stargazer package
# nicely formatted for the statistics around where students are at this time
stargazer(MyStoryData, type="text")

## Measures - mean, median, sums
## The following will sum all rows for each "School" and per variable in the data
## Let's save this new aggregated result as a DF
SumBySchoolDF <- ddply(MyStoryData, "School", numcolwise(sum))

# Add a column for total number of students per section
allStudentbySchool <- rowSums(SumBySchoolDF[,2:7], na.rm=TRUE, dims=1)
SumBySchoolDF$Total <- allStudentbySchool

# obtain the total number of students in the study
totalStudents <- sum(SumBySchoolDF$Total)

allStudentsbyStatus <- data.frame(colSums(SumBySchoolDF[,2:7], na.rm=TRUE, dims=1))
statnames <- rownames(allStudentsbyStatus)
rownames(allStudentsbyStatus) <- NULL
allStudentsbyStatus <- cbind(statnames, allStudentsbyStatus)
colnames(allStudentsbyStatus) <- c("Status", "NumStudents")

ratio <- function (x) { 
  return (x/totalStudents)}

StatusList <- c("Completed", "VeryAhead", "Middling", "Behind", "MoreBehind", "VeryBehind")
newcol <- c(ratio(allStudentsbyStatus$NumStudents))
allStudentsbyStatus$Percentage <- newcol
  
# transform the data for better plots
meltedData <- melt(SumBySchoolDF)
colnames(meltedData) <- c("School", "Status", "Students")
meltMyStoryData <- melt(MyStoryData)
colnames(meltMyStoryData) <- c("School", "Section", "Status", "NumStudents")

# find the mean for the number of students each Lesson Status Level by School
schoolLessonStatmeans<- ddply(meltedData, .(Status), summarize, lStatusValue = mean(Students))

# ----------------------------------------------------------------------------------------------------------------------
# INITIAL VISUALIZATION
# Plot to look at data
ggplot(stack(MyStoryData), aes(x = ind, y = values, color=ind)) +
  geom_boxplot()
## 

JustSchoolA<-subset(MyStoryData, School == "A" )
(str(JustSchoolA))

# boxplot for just School A by section
ggplot(JustSchoolA, aes(x = Section, y = Middling, color=Section)) +
  geom_boxplot()

# plot bar chart by Lesson Status, School, and Number of Students
g <- ggplot(meltedData, aes(x=Status, y = Students, fill = School))
g <- g + geom_bar(stat = "identity")
g <- g+ ggtitle("Students by Lesson Status and School") + theme(plot.title= element_text(hjust=0.5))
g <- g + xlab("Lesson Status") + ylab("Number of Students")
g

# barcharts by Lesson Status and School
g<- ggplot(meltedData, aes(fill=School, y=Students, x=Status))
g<- g + geom_bar(position="dodge", stat="identity")
g <- g+ ggtitle("Students by Lesson Status and School") + theme(plot.title= element_text(hjust=0.5))
g <- g + xlab("Lesson Status") + ylab("Number of Students")
g

# barcharts by Lesson Status and School without Totals
g<- ggplot(meltedData[1:30,], aes(fill=School, y=Students, x=Status))
g<- g + geom_bar(position="dodge", stat="identity")
g <- g+ ggtitle("Students by Lesson Status and School without Totals") + theme(plot.title= element_text(hjust=0.5))
g <- g + xlab("Lesson Status") + ylab("Number of Students")
g

# barcharts by School and Lesson Status
g<- ggplot(meltedData[1:30,], aes(fill=Status, y=Students, x=School))
g<- g + geom_bar(position="dodge", stat="identity")
g <- g+ ggtitle("Students by School and Lesson Status") + theme(plot.title= element_text(hjust=0.5))
g <- g + xlab("School") + ylab("Number of Students") 
g

## Now, I want the total number of students for A - E
## I want to sum the columns for each row
## I will start with:
SumOfStudents <- rowSums(SumBySchoolDF[,c("VeryAhead", "Middling", "Behind", 
                                          "MoreBehind","VeryBehind","Completed")])

TotalPerSchool <- data.frame("School" = SumBySchoolDF$School, 
                             "Total" = SumOfStudents)


# histogram of total students by school
g <- ggplot(TotalPerSchool, aes(x=School, y=Total))
g <- g + geom_bar(stat="identity", fill="blue")
g <- g + ggtitle("Total Students by School")
g <- g + xlab("School") + ylab("Number of Students")
g

# Number of sections by school
# plot the number of sections by each school
schoolList <- c("A", "B","C", "D", "E")
sections <- c(length(which(MyStoryData$School=='A')), length(which(MyStoryData$School=='B')), 
              length(which(MyStoryData$School=='C')), length(which(MyStoryData$School=='D')), 
              length(which(MyStoryData$School =='E')))
sectbySchool <- data.frame(schoolList, sections)
colnames(sectbySchool)<- c("School", "Sections")
# histogram of sections by school
g <- ggplot(sectbySchool, aes(x=School, y=Sections))
g <- g + geom_bar(stat="identity", fill="purple")
g <- g + ggtitle("Total Sections by School")
g <- g + xlab("School") + ylab("Number of Sections")
g

# mean by Lesson Status to get a better handle on how many in each level
# histogram of sections by school
g <- ggplot(schoolLessonStatmeans, aes(x=Status, y=lStatusValue))
g <- g + geom_bar(stat="identity", fill="goldenrod")
g <- g + ggtitle("Mean Number of Students in Each Lesson Status Category")
g <- g + xlab("Lesson Status Category") + ylab("Mean Number of Students")
g

# Look at all the data via a scatter plot
g <- ggplot(meltMyStoryData, aes(x=School, y=NumStudents))
g <- g + geom_point(aes(color=Section, shape=Status))
g <- g + ggtitle("Number of Students by School, Status and Section")
g <- g + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("School Name") + ylab("Number of Students") + ylim(0,60)
g

# obtain graphs of School B and School E only
justSchoolBnE <- melt(SumBySchoolDF[which(SumBySchoolDF$School == "B" | SumBySchoolDF$School == "E"),])
colnames(justSchoolBnE) <- c("School", "Status", "NumStudents")

g <- ggplot(justSchoolBnE, aes(x=School, y=NumStudents))
g <- g + geom_point(aes(color=Status))
g <- g + ggtitle("Number of Students by School, Status and Section")
g <- g + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("School Name") + ylab("Number of Students") 
g

# barcharts by School and Lesson Status
g<- ggplot(justSchoolBnE, aes(fill=Status, y=NumStudents, x=School))
g<- g + geom_bar(position="dodge", stat="identity")
g <- g+ ggtitle("Students by School and Lesson Status") + theme(plot.title= element_text(hjust=0.5))
g <- g + xlab("School") + ylab("Number of Students") 
g


