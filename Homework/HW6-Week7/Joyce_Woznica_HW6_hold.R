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
# Decision Tree Preparation - for Random Forest
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
randF <- randomForest(train_orig[-1], train_orig_labels, xtest=test_orig, 
                      ntree=25, importance=TRUE, keep.forest=TRUE)
randF # to look at the confusion matrix
randFCM <- randF$confusion
print(paste('Accuracy for RandomForest', sum(diag(randFCM)) / sum(randFCM)))

predictions <- data.frame(ImageID=1:nrow(test_orig), 
                          label=levels(train_orig_labels)[randF$test$predicted])
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

#Run training and Testing for each of the k-folds
# decision trees for kfolds
AllResultsDT <-list()
AllLabelsDT <-list()

Para_DTreeDF <- NULL
#Para_DTreeDF <- list()

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
  rpart.plot(dt_Train, extra=104, box.palette="GnBu", branch.lty=3, shadow.col="gray", nn=TRUE)
  dt_Pred <- predict(dt_Train, DigitDF_Test_noLabel, type="class") # maybe withoutlabels?

    #Testing accurancy of decision tree with Kaggle train data sub set
  (confusionMatrix(dt_Pred, DigitDF_Test$label))
  
  # Accumulate results from each fold, if you like
  AllResultsDT <- c(AllResultsDT,dt_Pred)
  AllLabelsDT <- c(AllLabelsDT, DigitDF_Test_justLabel)
}

### end crossvalidation -- present results for all folds   
dt_table <- table(unlist(AllResultsDT),unlist(AllLabelsDT))
# test accuracy
print(paste('Accuracy for CV Decision Trees', sum(diag(dt_table)) / sum(dt_table)))
# build a matrix with the parameters and accuracy so we can use this to determine the best tree
# Run each of the above changing the minsplit and maxdepth
Para_DTreeDF <- c(Para_DTreeDF, 2, 4, sum(diag(dt_table)) / sum(dt_table))
Para_DTreeDF <- c(Para_DTreeDF, 3, 5, sum(diag(dt_table)) / sum(dt_table))
Para_DTreeDF <- c(Para_DTreeDF, 4, 4, sum(diag(dt_table)) / sum(dt_table))
Para_DTreeDF <- c(Para_DTreeDF, 4, 6, sum(diag(dt_table)) / sum(dt_table))
Para_DTreeDF <- c(Para_DTreeDF, 4, 8, sum(diag(dt_table)) / sum(dt_table))
Para_DTreeDF <- c(Para_DTreeDF, 2, 9, sum(diag(dt_table)) / sum(dt_table))
Para_DTreeDF <- c(Para_DTreeDF, 2, 10, sum(diag(dt_table)) / sum(dt_table))
Para_DTreeDF <- c(Para_DTreeDF, 4, 25, sum(diag(dt_table)) / sum(dt_table))

# find a particular tree to prune - which one?
printcp(train_tree4)
plotcp(train_tree4)
ptrain_tree4 <- prune(train_tree4, cp = train_tree4$cptable[which.min(train_tree4$cptable[,"xerror"]), "CP"])
fancyRpartPlot(ptrain_tree4, uniform=TRUE, cex=0.5, main="Pruned Classification Tree 4")
rpart.plot(ptrain_tree4, extra=104, box.palette="GnBu", branch.lty=3, shadow.col="gray", nn=TRUE)

predictedpt4 <- predict(ptrain_tree4, test, type="class")

ptree4_table <- table(Outcome=predictedpt4, true=test$outcome)
pruned_accuracy <- sum(diag(ptree4_table)) / sum(ptree4_table)
pruned_accuracy

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


