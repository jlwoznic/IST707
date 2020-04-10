# 
# Course: IST 707
# Name: Joyce Wozninca
# Homework #7 (Week 8)
# Code: HW7 R Code
# Due Date: 11/29/2019
#
# ----------------------------------------------------------------------------------------------------------------------
# Load the required packages.
#specify the packages of interest
packages=c("sqldf", "ggplot2", "class", "dplyr", "e1071",  "randomForest")

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

# Load plyr last
install.packages("plyr", dependencies=TRUE)
library(plyr)

# ----------------------------------------------------------------------------------------------------------------------
# This loads applicable libraries
# Load the train dataset. 
trainset <- read.csv("/Users/Joyce/Desktop/Syracuse/IST707/Homework/HW7-Week8/Kaggle-digit-train.csv")
trainset$label <- factor(trainset$label)

#Create a random sample of n% of train data set
percent <- .15
dimReduce <- .10
set.seed(275)
DigitSplit <- sample(nrow(trainset),nrow(trainset)*percent)

trainset <- trainset[DigitSplit,]
dim(trainset)

# Setting static variables used throughout the Models section
N <- nrow(trainset)
kfolds <- 2
set.seed(30)
holdout <- split(sample(1:N), 1:kfolds)

# Function for model evaluation
get_accuracy_rate <- function(results_table, total_cases) {
  diagonal_sum <- sum(c(results_table[[1]], results_table[[12]], results_table[[23]], results_table[[34]],
                        results_table[[45]], results_table[[56]], results_table[[67]], results_table[[78]],
                        results_table[[89]], results_table[[100]]))
  (diagonal_sum / total_cases)*100
}

# Discretizing at 87%
binarized_trainset <- trainset
for (col in colnames(binarized_trainset)) {
  if (col != "label") {
    binarized_trainset[, c(col)] <- ifelse(binarized_trainset[, c(col)] > 131, 1, 0)
  }
}
for (col in colnames(binarized_trainset)) {
  if (col != "label") {
    binarized_trainset[, c(col)] <- as.factor(binarized_trainset[, c(col)])
  }
}

# why sqldf???
digit_freq <- sqldf("SELECT label, COUNT(label) as count
                     FROM trainset
                     GROUP BY label")
ggplot(digit_freq, aes(x=reorder(label, -count), y=count)) + geom_bar(stat="identity") + xlab("Written Digit") + ylab("Frequency Count") + ggtitle("Written Digit by Frequency Count")

color_freq <- data.frame("0"=c(), "1"=c())
for (col in colnames(binarized_trainset)) {
  if (col != "label") {
    zero <- c(length(which(binarized_trainset[,c(col)] == 0)))
    one <- c(length(which(binarized_trainset[,c(col)] == 1)))
    color_freq <- rbind(color_freq, data.frame("0"=zero, "1"=one))
  }
}
colnames(color_freq) <- c("zero", "one")
color_freq <- data.frame("number"=c("zero", "one"), "count"=c(sum(color_freq$zero), sum(color_freq$one)))

ggplot(color_freq, aes(x=number, y=count)) + geom_bar(stat="identity") + xlab("Color Number") + ylab("Count") + ggtitle("Color Number by Count")

# ----------------------------------------------------------------------------------------------------------------------
# KNN - K-Nearest Neighbor

k_guess = 7 # round(sqrt(nrow(trainset)))
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- trainset[holdout[[k]], ]
  new_train <- trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  pred <- knn(train=new_train, test=new_test, cl=new_train$label, k=k_guess, prob=FALSE)
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

# k = 3
k_guess = 3#round(sqrt(nrow(trainset)))
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- trainset[holdout[[k]], ]
  new_train <- trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  pred <- knn(train=new_train, test=new_test, cl=new_train$label, k=3, prob=FALSE)
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)

get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

# k= 5
k_guess = 5 #round(sqrt(nrow(trainset)))
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- trainset[holdout[[k]], ]
  new_train <- trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  pred <- knn(train=new_train, test=new_test, cl=new_train$label, k=3, prob=FALSE)
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)

get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

# k = 8
k_guess = 8 #round(sqrt(nrow(trainset)))
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- trainset[holdout[[k]], ]
  new_train <- trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  pred <- knn(train=new_train, test=new_test, cl=new_train$label, k=3, prob=FALSE)
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))


# k = 4
k_guess = 4 #round(sqrt(nrow(trainset)))
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- trainset[holdout[[k]], ]
  new_train <- trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  pred <- knn(train=new_train, test=new_test, cl=new_train$label, k=3, prob=FALSE)
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))


# ----------------------------------------------------------------------------------------------------------------------
# Support Vector Machines (SVM)
cols_to_remove = c() 
for (col in colnames(trainset)) 
{ 
  if (col != 'label') 
  { 
    if (length(unique(trainset[, c(col)])) == 1) 
    { 
      cols_to_remove <- c(cols_to_remove, col) 
    } 
  } 
}

svm_trainset <- trainset[-which(colnames(trainset) %in% cols_to_remove)]
# Discretizing at 87%
# Baseline SVM - no changes to data - issues cannot get this to work!
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- svm_trainset[holdout[[k]], ]
  new_train <- svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(label ~ ., new_train, na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

# Binarizing preprocessed SVM trainset 
binarized_svm_trainset <- svm_trainset 
for (col in colnames(binarized_svm_trainset)) 
{ 
  if (col != 'label') 
  { 
    binarized_svm_trainset[, c(col)] <- ifelse(binarized_svm_trainset[, c(col)] > 131, 1, 0) 
  } 
} 

for (col in colnames(binarized_svm_trainset)) 
{ 
  if (col != 'label') 
  { 
    binarized_svm_trainset[, c(col)] <- as.factor(binarized_svm_trainset[, c(col)]) 
  } 
}

cols_to_remove = c() 
for (col in colnames(binarized_svm_trainset)) 
{ 
  if (col != 'label') 
  { 
    if (length(unique(binarized_svm_trainset[, c(col)])) == 1) 
    { 
      cols_to_remove <- c(cols_to_remove, col) 
    } 
  } 
}

binarized_svm_trainset <- binarized_svm_trainset[-which(colnames(binarized_svm_trainset) %in% cols_to_remove)]

#### ?? DEFAULT??  Kernel
all_results <- data.frame(orig=c(), pred=c()) 
for (k in 1:kfolds) 
  {
  new_test <- binarized_svm_trainset[holdout[[k]], ] 
  new_train <- binarized_svm_trainset[-holdout[[k]], ]
  new_test_no_label <- new_test[-c(1)] 
  new_test_just_label <- new_test[c(1)]
  test_model <- svm(label ~ ., new_train, na.action=na.pass) 
  pred <- predict(test_model, new_test_no_label, type=c('class'))
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred)) 
  } 
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred),length(all_results$pred))


#### Polynomial Kernel - confirm by naming in function
all_results <- data.frame(orig=c(), pred=c()) 
for (k in 1:kfolds) 
{
  new_test <- binarized_svm_trainset[holdout[[k]], ] 
  new_train <- binarized_svm_trainset[-holdout[[k]], ]
  new_test_no_label <- new_test[-c(1)] 
  new_test_just_label <- new_test[c(1)]
  test_model <- svm(label ~ ., new_train, kernel="polynomial", na.action=na.pass) 
  pred <- predict(test_model, new_test_no_label, type=c('class'))
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred)) 
} 
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred),length(all_results$pred))


#### Sigmoid Kernel
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- binarized_svm_trainset[holdout[[k]], ]
  new_train <- binarized_svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(label ~ ., new_train, kernel="sigmoid", na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

#### Radial Kernel
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- binarized_svm_trainset[holdout[[k]], ]
  new_train <- binarized_svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(label ~ ., new_train, kernel="radial", na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

#### linear  Kernel
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- binarized_svm_trainset[holdout[[k]], ]
  new_train <- binarized_svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(label ~ ., new_train, kernel="linear", na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

# try modifying cost - run with higher cost
cost <- 10000
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- binarized_svm_trainset[holdout[[k]], ]
  new_train <- binarized_svm_trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- svm(label ~ ., new_train, kernel="radial", cost=cost, na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
#

new_result <- get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))
new_result
previous_result <- new_result

if (new_result > previous_result) {
  previous_result <- new_result
} else {
  best_cost <- cost
  best_result <- new_result
  break
}

# ----------------------------------------------------------------------------------------------------------------------
### Random Forest
all_results <- data.frame(orig=c(), pred=c())
for (k in 1:kfolds) {
  new_test <- trainset[holdout[[k]], ]
  new_train <- trainset[-holdout[[k]], ]
  
  new_test_no_label <- new_test[-c(1)]
  new_test_just_label <- new_test[c(1)]
  
  test_model <- randomForest(label ~ ., new_train, na.action=na.pass)
  pred <- predict(test_model, new_test_no_label, type=c("class"))
  
  all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
}
table(all_results$orig, all_results$pred)
get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))

# Random Forest
prev_result <- 0
best_result <- 0
best_number_trees <-0
for (trees in 1:10) {
  if (trees %% 5 == 0) {
    all_results <- data.frame(orig=c(), pred=c())
    for (k in 1:kfolds) {
      new_test <- trainset[holdout[[k]], ]
      new_train <- trainset[-holdout[[k]], ]
      
      new_test_no_label <- new_test[-c(1)]
      new_test_just_label <- new_test[c(1)]
      
      test_model <- randomForest(label ~ ., new_train, replace=TRUE, na.action=na.pass)
      pred <- predict(test_model, new_test_no_label, type=c("class"))
      
      all_results <- rbind(all_results, data.frame(orig=new_test_just_label$label, pred=pred))
    }
    #table(all_results$orig, all_results$pred)
    new_result <- get_accuracy_rate(table(all_results$orig, all_results$pred), length(all_results$pred))
    prev_result
    new_result
    trees
    
    if (new_result > prev_result) {
      prev_result <- new_result
    } else {
      best_number_trees <- trees
      best_result <- new_result
      break
    }
  }
}  

if (best_number_trees == 0) {
  best_number_trees <- trees
  best_result <- new_result}

paste("Best Number of Trees:", best_number_trees, "- Best Result:", best_result, sep=" ")
table(all_results$orig, all_results$pred)




