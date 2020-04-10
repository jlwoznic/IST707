svm.develop <- satSurvey[sample(nrow(satSurvey), size=100, replace=FALSE),]
table(svm.develop$Satisfaction)/length(svm.develop$Satisfaction)

svm.validate <- satSurvey[sample(nrow(satSurvey), size=100, replace=FALSE),]
table(svm.develop$Satisfaction)/length(svm.validate$Satisfaction)

install.packages("e1071")
require(e1071)

svm.model<- svm(Satisfaction~Age+Class+Status+TravelType+PriceSens+PercOther,data=svm.develop)
summary(svm.model)

svm.validate$PredS<-predict(svm.model,svm.validate)
# Compare Observed and Predicted
table.svm <- table(pred = svm.validate$PredS,
                   true = svm.validate$Satisfaction)/length(svm.validate$Satisfaction)

svm.simulate <- function(){
  gamma.value <- c(10^seq(-5,-1, by=1))
  cost.value <-c(10^seq(-3,1, by=1))
  svm.df <-data.frame()
  for(g in gamma.value){
    for(c in cost.value){
      # train model
      svm.model <- svm(Satisfaction~Age+Class+Status+TravelType+PriceSens+PercOther,
                       kernel="radial",
                       gamma=g,
                       cost=c,
                       data=svm.develop)
      # validate model
      svm.validate$PredS <- predict(svm.model, 
                                          svm.validate)
      # Compare Observed and Predicted
      table.svm <- prop.table(table(pred = svm.validate$PredS,
                                    true = svm.validate$Satisfaction)/length(svm.validate$Satisfaction),1)
      in.df <- data.frame(gamma=g,
                          cost=c,
                          accuracy=table.svm[4] )
      svm.df <- rbind(svm.df,in.df)
    }
  }
  svm.df
}

svm.simulate.out <-svm.simulate()


# create Training Data (2/3) and Test Data set (1/3) of set
randIndex <- sample(1:dim(satSurvey)[1])
summary(randIndex)
length(randIndex)
head(randIndex)
cutPoint2_3 <- floor(2 * dim(satSurvey)[1]/3)
cutPoint2_3
trainData<- satSurvey[randIndex[1:cutPoint2_3],]
testData<- satSurvey[randIndex[(cutPoint2_3+1):dim(satSurvey)[1]],]

# Kernel is type of kernel
# C is Cost
# Low C 1,2,3 - more mistakes might mean more general model with larger margin of separation
# High C 20,30,40 - less mistakes means more specific model with smaller margin of separation
# ** Due to the time it takes to run this output - I elected to take a random sample of 15,000 of the TrainData
# ** and 5,000 of the test data to test this model
trainData.small <-sample_n(trainData,300,replace=FALSE)
testData.small <-sample_n(testData, 100, replace=FALSE)
trainData.small <- as.factor(as.data.frame(trainData.small))
testData.small <- as.factor(as.data.frame(testData.small))

ksvmOutputM <- ksvm(Satisfaction~.,data=trainData.small,kernel="rbfdot",kpar="automatic",C=10,cross=3,prob.model=TRUE)
ksvmOutput <- ksvmOutputM


ksvmOutputM <- ksvm(Satisfaction~.,data=svm.develop,kernel="rbfdot",kpar="automatic",C=10,cross=3,prob.model=TRUE)
ksvmOutput <- ksvmOutputM

# test the model
ksvmPred <- predict(ksvmOutput, testData.small, type="votes")
str(ksvmPred)
compTable <- data.frame(testData.small[,1],ksvmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMEksvm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMEksvm

# SVM 
svmOutput <- svm(Satisfaction~.,data=trainData.small)
# create prediction
svmPred <- predict(svmOutput, testData.small, type="votes")
svmPred <- (data.frame(svmPred))
compTable <- data.frame(testData[,1],svmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMEsvm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMEsvm

# Linear Regression Prediction
# predict with the model we created for testData
# here I can use the larger set of data for Prediction (testData, not testData.small)
lmPred <- predict(Final.Model, testData, type="response")
lmPred <- data.frame(lmPred)
compTable <- data.frame(testData[,1],lmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMElm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMElm

# plot some results
# compute absolute error for each case
compTable$error <- abs(compTable$test - compTable$Pred)
# create new dataframe for plotting
# Reflected in Figure 28
lmPlot <- data.frame(compTable$error, testData$PriceSens, testData$Status)
colnames(lmPlot) <- c("error", "PriceSens", "Status")
plotlm1 <- ggplot(lmPlot, aes(x=PriceSens, y=Status)) +
  geom_point(aes(size=error, color=error)) +
  ggtitle("Linear Regression with Price Sensitivity and Loyalty Status") +
  xlab("Price Sensitivity") + ylab("Loyalty Card Status")
plotlm1

# Reflected in Figure 29
lmPlot <- data.frame(compTable$error, testData$Gender, testData$Status, testData$PriceSens)
colnames(lmPlot) <- c("error", "Gender", "Status", "PriceSens")
plotlm2 <- ggplot(lmPlot, aes(x=PriceSens, y=Status)) +
  geom_point(aes(size=error, color=Gender)) +
  ggtitle("Linear Regression with Price Sensitivity, Gender and Loyalty Status") +
  xlab("Price Sensitivity") + ylab("Loyalty Card Status")
plotlm2

# Reflected in Figure 30
lmPlot <- data.frame(compTable$error, testData$DeptDelayMins, testData$ArrDelayMins, testData$SchDeptHour)
colnames(lmPlot) <- c("error", "DeptDelay", "ArrivalDelay", "ScheduledDept")
plotlm3 <- ggplot(lmPlot, aes(x=DeptDelay, y=ArrivalDelay)) +
  geom_point(aes(size=error, color=ScheduledDept)) +
  ggtitle("Linear Regression with Departure and Arrival Delays and Scheduled Departure Hour") +
  xlab("Departure Delay in Minutes") + ylab("Arrival Delay in Minutes")
plotlm3

# change the scale
# Reflected in Figure 31
plotlm3 <- ggplot(lmPlot, aes(x=DeptDelay, y=ArrivalDelay)) +
  geom_point(aes(size=error, color=ScheduledDept)) +
  ggtitle("Linear Regression with Departure and Arrival Delays and Scheduled Departure Hour") +
  scale_x_continuous(limits=c(0,450)) + scale_y_continuous(limits=c(0,500)) +
  xlab("Departure Delay in Minutes") + ylab("Arrival Delay in Minutes")
plotlm3

# change the scale again
# Reflected in Figure 32
plotlm3 <- ggplot(lmPlot, aes(x=DeptDelay, y=ArrivalDelay)) +
  geom_point(aes(size=error, color=ScheduledDept)) +
  ggtitle("Linear Regression with Departure and Arrival Delays and Scheduled Departure Hour") +
  scale_x_continuous(limits=c(0,200)) + scale_y_continuous(limits=c(0,200)) + 
  xlab("Departure Delay in Minutes") + ylab("Arrival Delay in Minutes")
plotlm3

RSME.vector <- NULL
# plot the top 3 airline model predictions
lmPred <- predict(BestTopModel, testData, type="response")
lmPred <- data.frame(lmPred)
compTable <- data.frame(testData[,1],lmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMElm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMElm
RSME.vector <- c(RSMElm)

# Naive Bayes
nbOutput <- naiveBayes(Satisfaction~.,data=trainData.small, userkernel=T)
str(nbOutput)
# create prediction
nbPred <- predict(nbOutput, testData.small)
nbPred <- as.data.frame(nbPred)
str(nbPred)
compTable <- data.frame(testData.small[,1],nbPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RSME
RSMEnb <- sqrt(mean(compTable$test-compTable$Pred)^2)
RSMEnb

# Step 6: Which are the best models for this data?
#         Review what you have done and state which is the best and why
#   ANSWER: If you just review the Root Mean Squared Error (RMSE) for predicting Ozone from 
#           the other variables: Wind, Temp, Solar.R, you see that depending on the training data and the 
#           test data - the "best model" varies. I ran this code multiple times and I found that
#           the usually the svm had the lowest RMSE, so I would think it would be the best model; however,
#           this was not always the case. It depended on the random training and test data.
#           For example, in the run that I turned in for homework - these are the values.
#           for ksvm, RSME = 16.99584
#           for svm, RSME = 13.63764
#           for lm, RSME = 14.15575
#           we would conclude that svm is the best model for this set of data since it has the smallest
#           Root Mean Squared Error.
#       
#           The same occurred when reviewing the Good/Bad Ozone prediction and look at the models 
#           and the "Percent Good" with varying values for the percent that was properly predicted.
#           For example, in the run that I turned in for homework - you would see the following:
#           perc_ksvm = 72.54902%
#           perc_svm = 74.5098%
#           perc_nb = 80.39216%
#           We would again conclude that the Naive Bayes (nb) is the best model for this set of data since 
#           it carries the highest percent of correct predictions
#           
#           The plots also show the same, the svm plot shows smaller (less error) size and darker color 
#           points in the plot. The plot for good/bad ozone, the nb plot shows many smaller (correct) 
#           triangles in the nb plot.


