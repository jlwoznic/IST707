###########
## Naive Bayes tutorial
## Uses the LaveledDataRiskHeart.csv
## Gates
###########
#install.packages("e1071")
#install.packages("naivebayes")
library(e1071)
library(naivebayes)


## This tutorial will use a pretend dataset
## that contains both qual and quant data.

setwd("C:\\Users\\jerem\\Google Drive\\Online\\iCuse\\IST707\\Week7")

filename="LabeledDataRiskHeart.csv"
RiskDF <- read.csv(filename, header = TRUE, stringsAsFactors = TRUE)
(head(RiskDF))
(str(RiskDF))
(nrow(RiskDF))
RiskDF$StressLevel<-as.factor(RiskDF$StressLevel)
RiskDF$Cholesterol<-as.numeric(RiskDF$Cholesterol)
RiskDF$Weight<-as.numeric(RiskDF$Weight)
RiskDF$Height<-as.numeric(RiskDF$Height)
(str(RiskDF))

###############################################################
#############  Create k-folds for k-fold validation ###########
###############################################################


# Number of observations
N <- nrow(RiskDF)
# Number of desired splits
kfolds <- 5
# Generate indices of holdout observations
# Note if N is not a multiple of folds you will get a warning, but is OK.
holdout <- split(sample(1:N), 1:kfolds)


#####  Run training and Testing for each of the k-folds
AllResults<-list()
AllLabels<-list()
for (k in 1:kfolds){
  
  RiskDF_Test=RiskDF[holdout[[k]], ]
  RiskDF_Train=RiskDF[-holdout[[k]], ]
  ## View the created Test and Train sets
  (head(RiskDF_Train))
  (table(RiskDF_Test$Label))
  
  ## Make sure you take the labels out of the testing data
  (head(RiskDF_Test))
  RiskDF_Test_noLabel<-RiskDF_Test[-c(1)]
  RiskDF_Test_justLabel<-RiskDF_Test$Label
  (head(RiskDF_Test_noLabel))
  
  
  #### e1071
  ## formula is label ~ x1 + x2 + .  NOTE that label ~. is "use all to create model"
  NB_e1071<-naiveBayes(Label~., data=RiskDF_Train, na.action = na.pass)
  NB_e1071_Pred <- predict(NB_e1071, RiskDF_Test_noLabel)
  NB_e1071
  
  ## Accumulate results from each fold
  AllResults<- c(AllResults,NB_e1071_Pred)
  AllLabels<- c(AllLabels, RiskDF_Test_justLabel)
  
}
### end crossvalidation -- present results for all folds   
table(unlist(AllResults),unlist(AllLabels))



##Visualize
#plot(NB_e1071, ylab = "Density", main = "Naive Bayes Plot")



## using naivebayes package
## https://cran.r-project.org/web/packages/naivebayes/naivebayes.pdf

NB_object<- naive_bayes(Label~., data=RiskDF_Train)
NB_prediction<-predict(NB_object, RiskDF_Test_noLabel, type = c("class"))
head(predict(NB_object, RiskDF_Test_noLabel, type = "prob"))
table(NB_prediction,RiskDF_Test_justLabel)
plot(NB_object, legend.box = TRUE)



