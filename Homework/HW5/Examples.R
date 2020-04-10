library(rpart)
library(caret)
install.packages("rpart.plot")
library(rpart.plot)

data <- read.csv('C:/Users/Joyce/Desktop/Syracuse/IST707/Homework/HW5/fedPapers85.csv')

#Data pre-processing
#For training the model, we need essays from Hamilton and Madison only. So we filter those from the dataset for training dataset.
#while test dataset, we just get disputed essays.

data <- subset(data, select=-c(filename))
train.data <- data[data$author %in% c('Hamilton','Madison'), ]
train.data$author <- factor(train.data$author)
test.data <- data[data$author %in% c('dispt'), ]
test.data$author <- factor(test.data$author)

#Performing DT analysis

#We use rpart on train.data and author as predicting variable.

dt.model <-  train(author ~ ., data = train.data, metric="Accuracy", method = "rpart")
rpart.plot(dt.model$finalModel)

#Prediction
#we use model to predict author for disputed essays.
#We found that model predicts all essys were writted from Madison.

dt.predict <- predict(dt.model, newdata = test.data, na.action = na.omit, type = "raw")
dt.predict

##  [1] Madison Madison Madison Madison Madison Madison Madison Madison
##  [9] Madison Madison Madison
## Levels: Hamilton Madison

#Tuning DT analysis
#For tuning, we setup tunelength 10, but model stays the same. This means we do not need the tuning for the model.

dt.model <-  train(author ~ ., data = train.data, metric="Accuracy", method = "rpart", tuneLength=10)
rpart.plot(dt.model$finalModel)

dt.predict <- predict(dt.model, newdata = test.data, na.action = na.omit, type = "raw")
dt.predict
