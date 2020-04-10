# Course: IST 707
# Name: Joyce Wozninca
# Homework #6
# Code: HW6 R Code
# Due Date: 11/22/2019
#
# ----------------------------------------------------------------------------------------------------------------------
## Load the packages
# Package Section
# ------------------------------------------------------------------
#specify the packages of interest
packages <- c("e1071", "naivebayes", "caret", "randomForest", "caret", "caretEnsemble","rpart", "psych", 
              "rattle", "rpart.plot", "tidyr", "readr", "reshape")

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
require(dplyr)

install.packages("plyr",dependencies=TRUE)
library(plyr)

#---------------------------------------------------------------------------------------------------------
# Load and Clean Data
# load DigitTotalDF using read.csv
DigitTotalDF <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Homework/HW6-Week7/Kaggle-digit-train.csv", 
                         header = TRUE, na.strings = "", stringsAsFactors = TRUE)
# check the structure of the dataframe
str(DigitTotalDF)
# Factor the labels so not integers
DigitTotalDF$label <- as.factor(DigitTotalDF$label)
# verify factor worked
str(DigitTotalDF)
# check the size/dimension of the dataframe
dim(DigitTotalDF)
# view the begnning of the data frame
head(DigitTotalDF)

#---------------------------------------------------------------------------------------------------------------------
# create a random sample
# 1/4 the size of the original 42,000
percent <- .25
set.seed(275)
DigitSplit <- sample(nrow(DigitTotalDF),nrow(DigitTotalDF)*percent)
DigitDF <- DigitTotalDF[DigitSplit,]
dim(DigitDF)
nrow(DigitDF)

# read in test data
TestTotalDF <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Homework/HW6-Week7/Kaggle-digit-test.csv", 
                                 header = TRUE, na.strings = "", stringsAsFactors = TRUE)
#TestTotalDF$label<-as.factor(TestTotalDF$label)
dim(TestTotalDF)
head(TestTotalDF)
str(TestTotalDF)

# create a random sample
percent <- .25
set.seed (275)
TestSplit <- sample(nrow(TestTotalDF), nrow(TestTotalDF)*percent)
TestDF <- TestTotalDF[TestSplit,]
dim(TestDF)

#---------------------------------------------------------------------------------------------------------
# Decision Tree Preparation
numTrees <- 25
# remove the labels from the training set
trainNL <- DigitTotalDF[-1]
TrainLabels <- DigitTotalDF[,1]

#train_orig <- DigitTotalDF
# use sample
train_orig <- DigitDF
train_orig_labels <- train_orig[, 1]
summary(train_orig_labels)
# remove blank labels
# rerun everything with TotalDF instead of TotalTestDF
test_orig <- TestDF[-1]

# on entire training set
#randF <- randomForest(DigitTotalDF[-1], TrainLabels, xtest=TestTotalDF, ntree=numTrees)
# on random Sample
# start timer
startTime <- proc.time()
randF <- randomForest(train_orig[-1], train_orig_labels, xtest=test_orig, 
                      ntree=75, importance=TRUE, keep.forest=TRUE)
proc.time() - startTime
randF # to look at the confusion matrix
randFCM <- randF$confusion
print(paste('Accuracy for RandomForest', sum(diag(randFCM)) / sum(randFCM)))

startTime <- proc.time()
predictions <- data.frame(ImageID=1:nrow(test_orig), 
                          label=levels(train_orig_labels)[randF$test$predicted])
proc.time() - startTime

head(predictions)
str(predictions)

# Not sure how to plot this
randF$ntree
tree1<-getTree(randF, 1, labelVar=TRUE)
# just cannot find a way to plot tree1 or the other 25 trees :(

#-------------------------------------------------------------------------------------------------------------
# Decision Trees
# Create k-folds for k-fold cross validation 
# code used from code provided by Jeremy Bolton, Professor at Syracuse University
## Number of observations
N <- nrow(DigitDF)
## Number of desired splits
kfolds <- 10
## Generate indices of holdout observations
## Note if N is not a multiple of folds you will get a warning, but is OK.
# these are sets of observations to use as samples - there are 10 sets
holdout <- split(sample(1:N), 1:kfolds)
head(holdout)

dtree <- function (traindf, testdf, c_p, min_split, max_depth, graph_pall)
{
  # build the model
  Dtree <- rpart(label ~., data = traindf, method="class", control=rpart.control(cp=c_p, minsplit=min_split, maxdepth=max_depth))
  # reflect the error
  rsq.rpart(Dtree)
  # plot the tree
  rpart.plot(Dtree, extra=104, box.palette="GnBu", branch.lty=3, shadow.col="gray", nn=TRUE)
  return(Dtree)
}

Para_DTreeDF <- NULL

#Run training and Testing for each of the k-folds
# decision trees for kfolds
AllResultsDT <-list()
AllLabelsDT <-list()

startTime <- proc.time()
for (k in 1:kfolds)
  {
  # grab a set to be Test set
  DigitDF_Test <- DigitDF[holdout[[k]], ]
  DigitDF_Train=DigitDF[-holdout[[k]], ]
  ## View the created Test and Train sets
  (head(DigitDF_Train))
  (table(DigitDF_Test$Label))
  ## Make sure you take the labels out of the testing data
  (head(DigitDF_Test))
  DigitDF_Test_noLabel<-DigitDF_Test[-c(1)]
  DigitDF_Test_justLabel<-DigitDF_Test$label
  (head(DigitDF_Test_noLabel))
  
  # decision tree modeling with Train
  dt_Train <- rpart(label ~., data = DigitDF_Train, method="class", control=rpart.control(cp=0, minsplit=4, maxdepth=25))
  rpart.plot(dt_Train, extra=104, box.palette="GnBu", cex=NULL, branch.lty=3, shadow.col="gray", nn=TRUE)
  dt_Pred <- predict(dt_Train, DigitDF_Test_noLabel, type="class") # maybe withoutlabels?

    #Testing accurancy of decision tree with Kaggle train data sub set
  (confusionMatrix(dt_Pred, DigitDF_Test$label))
  
  # Accumulate results from each fold, if you like
  AllResultsDT <- c(AllResultsDT,dt_Pred)
  AllLabelsDT <- c(AllLabelsDT, DigitDF_Test_justLabel)
}
proc.time() - startTime

### end crossvalidation -- present results for all folds   
dt_table <- table(unlist(AllResultsDT),unlist(AllLabelsDT))
# test accuracy
print(paste('Accuracy for CV Decision Trees', sum(diag(dt_table)) / sum(dt_table)))
# build a matrix with the parameters and accuracy so we can use this to determine the best tree
# Run each of the above changing the minsplit and maxdepth
Para_DTreeDF <- rbind(Para_DTreeDF, c(2, 4, sum(diag(dt_table)) / sum(dt_table)))
Para_DTreeDF <- rbind(Para_DTreeDF, c(3, 5, sum(diag(dt_table)) / sum(dt_table)))
Para_DTreeDF <- rbind(Para_DTreeDF, c(4, 4, sum(diag(dt_table)) / sum(dt_table)))
Para_DTreeDF <- rbind(Para_DTreeDF, c(4, 6, sum(diag(dt_table)) / sum(dt_table)))
Para_DTreeDF <- rbind(Para_DTreeDF, c(4, 8, sum(diag(dt_table)) / sum(dt_table)))
Para_DTreeDF <- rbind(Para_DTreeDF, c(2, 10, sum(diag(dt_table)) / sum(dt_table)))
Para_DTreeDF <- rbind(Para_DTreeDF, c(4, 25, sum(diag(dt_table)) / sum(dt_table)))

rownames(Para_DTreeDF) <- c()
colnames(Para_DTreeDF) <- c("minsplit", "maxdepth", "accuracy")
Para_DTreeDF <- data.frame(Para_DTreeDF)


#-------------------------------------------------------------------------------------------------------------
# Naive Bayes
# Create k-folds for k-fold cross validation 
# code used from code provided by Jeremy Bolton, Professor at Syracuse University
## Number of observations
N <- nrow(DigitDF)
## Number of desired splits
kfolds <- 10
## Generate indices of holdout observations
## Note if N is not a multiple of folds you will get a warning, but is OK.
holdout <- split(sample(1:N), 1:kfolds)

#Run training and Testing for each of the k-folds
# naive bayes for kfolds
AllResults<-list()
AllLabels<-list()

startTime <- proc.time()

for (k in 1:kfolds){
  
  DigitDF_Test <- DigitDF[holdout[[k]], ]
  DigitDF_Train=DigitDF[-holdout[[k]], ]
  ## View the created Test and Train sets
  (head(DigitDF_Train))
  (table(DigitDF_Test$Label))
  ## Make sure you take the labels out of the testing data
  (head(DigitDF_Test))
  DigitDF_Test_noLabel<-DigitDF_Test[-c(1)]
  DigitDF_Test_justLabel<-DigitDF_Test$label
  (head(DigitDF_Test_noLabel))
  
  #### Naive Bayes prediction ussing e1071 package
  #Naive Bayes Train model
  train_naibayes<-naiveBayes(label~., data=DigitDF_Train, na.action = na.pass)
  train_naibayes
  #summary(train_naibayes)
  
  #Naive Bayes model Prediction 
  nb_Pred <- predict(train_naibayes, DigitDF_Test_noLabel)
  nb_Pred
  
  
  #Testing accurancy of naive bayes model with Kaggle train data sub set
  (confusionMatrix(nb_Pred, DigitDF_Test$label))
  
  # Accumulate results from each fold, if you like
  AllResults<- c(AllResults,nb_Pred)
  AllLabels<- c(AllLabels, DigitDF_Test_justLabel)
  
}
proc.time() - startTime

### end crossvalidation -- present results for all folds   
nb_table <- table(unlist(AllResults),unlist(AllLabels))
# test accuracy
print(paste('Accuracy for CV Naive Bayes', sum(diag(nb_table)) / sum(nb_table)))

# code used from code provided by Jeremy Bolton, Professor at Syracuse University
## using naivebayes package
## https://cran.r-project.org/web/packages/naivebayes/naivebayes.pdf

NB_object<- naive_bayes(label~., data=DigitDF)
NB_prediction<-predict(NB_object, DigitDF_Test_noLabel, type = c("class"))
head(predict(NB_object, DigitDF_Test_noLabel, type = "prob"))
NB_Table <- table(NB_prediction,DigitDF_Test_justLabel)
sum(diag(NB_Table)) / sum(NB_Table)
print(paste('Accuracy for CV Naive Bayes', sum(diag(NB_Table)) / sum(NB_Table)))


#Getting started with Naive Bayes in mlr
#Install the package
install.packages ("mlr")
#Loading the library
library(mlr)

startTime <- proc.time()
#Create a classification task for learning on entire DigitDF dataset (not 10-fold)
task <- makeClassifTask(data = DigitDF, target = "label")
#Initialize the Naive Bayes classifier
selected_model <- makeLearner("classif.naiveBayes")
#Train the model
NB_mlr <- train(selected_model, task)

#Read the model learned  
NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = DigitDF_Test_noLabel))

##Confusion matrix to check accuracy
NB_mlr_table <- table(predictions_mlr[,1],DigitDF_Test$label)
print(paste('Accuracy for MLR Naive Bayes', sum(diag(NB_mlr_table)) / sum(NB_mlr_table)))
proc.time() - startTime
