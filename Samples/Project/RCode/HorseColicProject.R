# 
# Course: IST707
# Name: Joyce Woznica
# Project Code: Vizualizations and Modeling
# Due Date: 12/6/2019
# Date Submitted:
#
# ------------------------------------------------------------------
# Set working directory
setwd("C:/Users/Joyce/Desktop/Syracuse/IST707/Project/RCode")
# Run ProjectPackages2
source("ProjectPackages2.R", local=TRUE) # sometimes have to run twice - not sure why
# Run ProjectFunctions
source("ProjectFunctions.R", local=TRUE)
# Run ColicDataLoadCleanup
source("ColicDataLoadCleanup.R", local=TRUE)

# ------------------------------------------------------------------
# force no scientific notation
options(scipen=999)
# General Statistics and Analysis
summary(colicData)
genStat <- stat.desc(colicData, basic=F)
View(genStat)

# combine in a plot
table(colicData$outcome)
barplot(table(colicData$outcome), col=rainbow(20))

# overall description (lengthy)
describe(colicData)

test<-as.matrix(colicData)
summary(test)

# descriptive statistics from using stargazer package
stargazer(colicData, type="text")


# ------------------------------------------------------------------
# Initial Visualizations
# boxplot of mucous_membrane
# boxplot of pain level
# abdomen
# histograms of Rectal_temp, pulse, respiratory rate, temp of extremeties, pulse, reflux
hist(colicData$rectal_temp, col=topo.colors(5), main="Histogram of Rectal Temperatures")
hist(colicData$pulse, col=terrain.colors(5), main="Histogram of Pulse")
hist(colicData$respiratory_rate, col=rainbow(5), main="Histogram of Respiratory Rate")
hist(colicData$total_protein, col=topo.colors(5), main="Histogram of Total Protein")
hist(colicData$packed_cell_volume, col=cm.colors(5), main="Histogram of Packed Cell Volume")

# count these and group by how many of each
# do a which on the values to get totals
# separate data be outcome for plotting reflux
gastroTbl <- table(colicData$outcome, colicData$nasogastric_reflux)
gastroDF <- data.frame(gastroTbl)
colnames(gastroDF)<-c("outcome", "reflux", "horse_count")

barplot(gastroTbl,beside=TRUE, legend=TRUE, col=c("red", "orange", "green"),
        main="Horses showing Naso Gastric Reflux by Outcome",
        axisnames=TRUE)

# Abdomen Presentation
barplot(table(colicData$outcome, colicData$abdomen), beside=TRUE, legend=TRUE,
        col=c("red", "blue", "green"), main="Abdomen Presentation and Outcome",
        axisnames=TRUE)

# lesion1A - redo with ggplot later!
barplot (table(colicData$outcome, colicData$lesion1A), beside=TRUE, legend=TRUE,
         col=c("red", "blue", "green"), main="Lesion 1A Location and Outcome",
         axisnames=TRUE)

# Lesion 1A (Site)
g <- ggplot(colicData, aes(x=lesion1A, fill=factor(outcome)))
g <- g + ggtitle("Lesion 1A (Site) Location by Outcome")
g <- g + geom_bar(position="dodge")
g <- g + xlab("Lesion Site") + ylab("Number of Horses")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.title = element_text(hjust=0.5))
g

# Lesion 1B (Type)
g <- ggplot(colicData, aes(x=lesion1B, fill=factor(outcome)))
g <- g + ggtitle("Lesion 1B (Type) Location by Outcome")
g <- g + geom_bar(position="dodge")
g <- g + xlab("Lesion Type") + ylab("Number of Horses")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.title = element_text(hjust=0.5))
g

# Lesion 1C (SubType)
g <- ggplot(colicData, aes(x=lesion1C, fill=factor(outcome)))
g <- g + ggtitle("Lesion 1C (SubType) Location by Outcome")
g <- g + geom_bar(position="dodge")
g <- g + xlab("Lesion SubType") + ylab("Number of Horses")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.title = element_text(hjust=0.5))
g

# Lesion 1D (Code)
g <- ggplot(colicData, aes(x=lesion1D, fill=factor(outcome)))
g <- g + ggtitle("Lesion 1D (Code) Location by Outcome")
g <- g + geom_bar(position="dodge")
g <- g + xlab("Lesion Code") + ylab("Number of Horses")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + theme(plot.title = element_text(hjust=0.5))
g

# count these and group by how many of each
# do a which on the values to get totals
# separate data be outcome for rectal exam feces
fecesTbl <- table(colicData$outcome, colicData$rectal_exam_feces)
fecesDF <- data.frame(fecesTbl)
colnames(fecesDF)<-c("outcome", "feces", "horse_count")

barplot(fecesTbl,beside=TRUE, legend=TRUE, col=c("red", "orange", "green"),
        main="Number of Horses Feces Status by Colic Outcome",
        axisnames=TRUE)

surgTbl <- table(colicData$outcome, colicData$surgery)
barplot(surgTbl,beside=TRUE, legend=TRUE, col=c("red", "orange", "green"),
        main="Number of Horses who had Surgery by Colic Outcome",
        axisnames=TRUE)

# boxplot of mucous membrane by outcome
# this doesn't make sense - fix it!
# with boxplot
g <- ggplot(colicData, aes(group=mucous_membrane,x=mucous_membrane,y=outcome)) 
g <- g + geom_boxplot(aes(fill=factor(mucous_membrane)))
g <- g+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + ggtitle("Outcome by Mucous Membrane") + theme(plot.title=element_text(hjust=0.5))
g

# Some initial plotting of the data to see what we have
plot(colicData$outcome,colicData$pulse)
# boxplot pulse rate by Outcome
# with boxplot
g <- ggplot(colicData, aes(group=outcome,x=outcome,y=pulse)) 
g <- g + geom_boxplot(aes(fill=factor(outcome)))
g <- g+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab("Outcome") + ylab("Pulse")
g <- g + ggtitle("Pulse Rate by Outcome") + theme(plot.title=element_text(hjust=0.5))
g

# Some initial plotting of the data to see what we have
plot(colicData$outcome,colicData$pulse)
# boxplot of total protein by outcome
# with boxplot
g <- ggplot(colicData, aes(group=outcome,x=outcome,y=total_protein)) 
g <- g + geom_boxplot(aes(fill=factor(outcome)))
g <- g+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + xlab("Outcome") + ylab("Total Protein")
g <- g + ggtitle("Total Protein by Outcome") + theme(plot.title=element_text(hjust=0.5))
g

# Plot pulse rate by outcome
g <- ggplot(colicData, aes(x=outcome, y=pulse)) + geom_point()
g <- g + stat_smooth(method= "lm", col=rainbox(20))
g <- g + ggtitle("Pulse Rates by Outcome")
g <- g + xlab("Outcome") + ylab("Pulse")
g

g<- ggplot(colicData, aes(x=age, fill=factor(outcome)))
g<- g+ ggtitle("Outcome by Age")
g<- g+ xlab("Age") + ylab("Outcome")
g<- g + geom_bar(position="dodge")
g

# Look at some of the data via a scatter plot
g <- ggplot(colicData, aes(x=temp_of_extremities, y=respiratory_rate))
g <- g + geom_point(aes(color=outcome, size=age))
g <- g + ggtitle("Outcome using Age, Respiratory Rate and Extremity Temperature")
g <- g + theme(plot.title=element_text(hjust=0.5))
g <- g + xlab("Extremity Temperature") + ylab("Respiratory Rate")
g

#Data Visualization
#Visual 1
ggplot(colicData, aes(packed_cell_volume, colour = outcome)) +
  geom_freqpoly(binwidth = 1) + labs(title="Packed Cell Volume Distribution by Outcome")

#visual 2
c <- ggplot(colicData, aes(x=nasogastric_reflux_ph, fill=outcome, color=outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Nasogastric Reflux PH Distribution by Outcome")
c + theme_bw()

#visual 3
P <- ggplot(colicData, aes(x=total_protein, fill=outcome, color=outcome)) +
  geom_histogram(binwidth = 1) + labs(title="Total Protein Distribution by Outcome")
P + theme_bw()

# -----------------------------------------------------------------------------
# Clustering that Means Nothing to me
## k means visualization results!
X <- colicData
distance1 <- get_dist(X,method = "manhattan")
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance2 <- get_dist(X,method = "euclidean")
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# ------------------------------------------------------------------
# Decision Tree
##Make Train and Test sets
trainRatio <- .70
set.seed(11) # Set Seed so that same sample can be reproduced in future also
sample <- sample.int(n = nrow(colicData), size = floor(trainRatio*nrow(colicData)), replace = FALSE)
train <- colicData[sample, ]
test <- colicData[-sample, ]
train_orig <- train
test_orig <- test
# train / test ratio
length(sample)/nrow(colicData)

##--------------------------------------------------------------------------------------------------------
#Decision Tree Models 
# build accuracy vector
treeAccuracyV <- list()

#Train Tree Model 1
# start with first Training Tree 1
train_tree1 <- rpart(outcome ~ ., data = train, method="class", control=rpart.control(cp=0))
summary(train_tree1)
#predict the test dataset using the model for train tree No. 1
predicted1 <- predict(train_tree1, test, type="class")
#plot number of splits
rsq.rpart(train_tree1)
#predict the test dataset using the model for train tree No. 1
predicted1 <- predict(train_tree1, test, type="class")
plotcp(train_tree1)
#plot the decision tree
fancyRpartPlot(train_tree1, cex=0.6, caption="Training Tree 1")
fancyRpartPlot(train_tree1, cex=0.6, caption="Training Tree 1", palettes=c("Greys", "Blues", "Oranges"))
#confusion matrix to find correct and incorrect predictions
tree1_table <- table(Outcome=predicted1, true=test$outcome)
treeAccuracyV <- c(treeAccuracyV, sum(diag(tree1_table)) / sum(tree1_table))

#Train Tree Model 2 
train_tree2 <- rpart(outcome ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 2, maxdepth = 5))
summary(train_tree2)
#predict the test dataset using the model for train tree No. 2
predicted2<- predict(train_tree2, test, type="class")
#plot number of splits
rsq.rpart(train_tree2)
#plot the decision tree
fancyRpartPlot(train_tree2, cex=0.6, caption="Training Tree 2")
#confusion matrix to find correct and incorrect predictions
tree2_table <- table(Outcome=predicted2, true=test$outcome)
treeAccuracyV <- c(treeAccuracyV, sum(diag(tree2_table)) / sum(tree2_table))

#Train Tree Model 3
train_tree3 <- rpart(outcome ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 5, maxdepth = 5))
summary(train_tree3)
#predict the test dataset using the model for train tree No. 3
predicted3 <- predict(train_tree3, test, type="class")
#plot number of splits
rsq.rpart(train_tree3)
#plot the decision tree
fancyRpartPlot(train_tree3, cex=0.6, caption="Training Tree 3")
#confusion matrix to find correct and incorrect predictions
tree3_table <- table(Outcome=predicted3, true=test$outcome)
treeAccuracyV <- c(treeAccuracyV, sum(diag(tree3_table)) / sum(tree3_table))

#Train Tree Model 4 
train_tree4 <- rpart(outcome ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 3, maxdepth = 5))
summary(train_tree4)
#predict the test dataset using the model for train tree No. 4
predicted4 <- predict(train_tree4, test, type="class")
#plot number of splits
rsq.rpart(train_tree4)
#plot the decision tree
fancyRpartPlot(train_tree4, cex=0.6, caption="Training Tree 4")
#confusion matrix to find correct and incorrect predictions
tree4_table <- table(Outcome=predicted4, true=test$outcome)
treeAccuracyV <- c(treeAccuracyV, sum(diag(tree4_table)) / sum(tree4_table))

#Train Tree Model 5 ** BEST MODEL **
train_tree5 <- rpart(outcome ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 3, maxdepth = 4))
summary(train_tree5)
#predict the test dataset using the model for train tree No. 5
predicted5 <- predict(train_tree5, test, type="class")
#plot number of splits
rsq.rpart(train_tree5)
#plot the decision tree
fancyRpartPlot(train_tree5, cex=0.6, caption="Training Tree 5")
#confusion matrix to find correct and incorrect predictions
tree5_table <- table(Outcome=predicted5, true=test$outcome)
treeAccuracyV <- c(treeAccuracyV, sum(diag(tree5_table)) / sum(tree5_table))

#Train Tree Model 6 
train_tree6 <- rpart(outcome ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 3, maxdepth = 3))
summary(train_tree6)
#predict the test dataset using the model for train tree No. 6
predicted6 <- predict(train_tree6, test, type="class")
#plot number of splits
rsq.rpart(train_tree6)
#plot the decision tree
fancyRpartPlot(train_tree6, cex=0.6, caption="Training Tree 6")
#confusion matrix to find correct and incorrect predictions
tree6_table <- table(Outcome=predicted6, true=test$outcome)
treeAccuracyV <- c(treeAccuracyV, sum(diag(tree6_table)) / sum(tree6_table))

# show other confusion  matrices to determine which is best
treeAccuracyV <- data.frame(treeAccuracyV)
colnames(treeAccuracyV) <- c("tree1", "tree2", "tree3", "tree4", "tree5", "tree6")
treeAccuracyV

# Play around with tuning
# rpart.control(minsplit = 20, minbucket = round(minsplit/3), maxdepth = 30)
# Arguments:
#  -minsplit: Set the minimum number of observations in the node before the algorithm perform a split
#  -minbucket:  Set the minimum number of observations in the final note i.e. the leaf
#  -maxdepth: Set the maximum depth of any node of the final tree. The root node is treated a depth 0

accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, test_orig, type = 'class')
  table_mat <- table(test_orig$outcome, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}

control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(outcome~., data = train_orig, method = 'class', control = control)
accuracy_tune(tune_fit)

minsplit = 4
minbucket= round(5/3)
maxdepth = 3
cp=0	

ind <- 1
while ( ind <= ncol(treeAccuracyV)) 
{ 
  print(paste('Accuracy for test', ind, treeAccuracyV[,ind]))
  ind <- 1 + ind
  }
##--------------------------------------------------------------------------------------------------------
# Pruning the tree - find the best model (5)
printcp(train_tree5)
plotcp(train_tree5)
ptrain_tree5 <- prune(train_tree5, cp = train_tree5$cptable[which.min(train_tree5$cptable[,"xerror"]), "CP"])
fancyRpartPlot(ptrain_tree5, uniform=TRUE, cex=0.5, main="Pruned Classification Tree 5")
rpart.plot(ptrain_tree5, extra=104, box.palette="GnBu", branch.lty=3, shadow.col="gray", nn=TRUE)

predictedpt5 <- predict(ptrain_tree5, test, type="class")

ptree5_table <- table(Outcome=predictedpt5, true=test$outcome)
pruned_accuracy <- sum(diag(ptree4_table)) / sum(ptree4_table)
pruned_accuracy

#---------------------------------------------------------------------------------------------------------
# Decision Trees with cross validation
# Create k-folds for k-fold cross validation 
# code used from code provided by Jeremy Bolton, Professor at Syracuse University
## Number of observations
N <- nrow(colicData)
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
  Dtree <- rpart(outcome ~., data = traindf, method="class", control=rpart.control(cp=c_p, minsplit=min_split, maxdepth=max_depth))
  # reflect the error
  rsq.rpart(Dtree)
  # plot the tree
  rpart.plot(Dtree, extra=104, box.palette="GnBu", branch.lty=3, shadow.col="gray", nn=TRUE)
  return(Dtree)
}

Para_DTreeDF <- NULL

#Run training and Testing for each of the k-folds
# decision trees for kfolds
AllResultsCD <-list()
AllLabelsCD <-list()

# repeat from here
startTime <- proc.time()
for (k in 1:kfolds)
{
  # grab a set to be Test set
  colicData_Test <- colicData[holdout[[k]], ]
  colicData_Train <- colicData[-holdout[[k]], ]
  ## View the created Test and Train sets
  (head(colicData_Train))
  (table(colicData_Test$outcome))
  ## Make sure you take the labels out of the testing data
  (head(colicData_Test))
  # this needs to remove outcome, 
  colicData_Test_noOutcome <- subset(colicData_Test, select = -outcome)
  colicData_Test_justOutcome <- colicData_Test$outcome
  (head(colicData_Test_noOutcome))
  
  # decision tree modeling with Train
  dt_Train <- rpart(outcome ~., data = colicData_Train, method="class", control=rpart.control(cp=0, minsplit=4, maxdepth=25))
  rpart.plot(dt_Train, extra=104, box.palette="GnBu", cex=NULL, branch.lty=3, shadow.col="gray", nn=TRUE)
  dt_Pred <- predict(dt_Train, colicData_Test_noOutcome, type="class")
  
  #Testing accurancy of decision tree with Kaggle train data sub set
  (confusionMatrix(dt_Pred, colicData_Test$outcome))
  
  # Accumulate results from each fold, if you like
  AllResultsCD <- c(AllResultsCD,dt_Pred)
  AllLabelsCD <- c(AllLabelsCD, colicData_Test_justOutcome)
}
proc.time() - startTime

### end crossvalidation -- present results for all folds   
dt_table <- table(unlist(AllResultsCD),unlist(AllLabelsCD))
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

##--------------------------------------------------------------------------------------------------------
# Associative Rule Mining
# must do some data cleanup first
colicDataARM <- colicData
# Create bins for the nasogastric_reflux_ph
summary(colicDataARM$nasogastric_reflux_ph)
colicDataARM$nasogastric_reflux_ph <- cut(colicDataARM$nasogastric_reflux_ph, breaks = c(0, 1.5, 3, 4.5, 6, 7.5),
                labels=c("< 1.5", "1.5-3", "3-4.5", "4.5-6", "6-7.5"))
# repeat for abdomo_protein
summary(colicDataARM$abdomo_protein)
colicDataARM$abdomo_protein <- cut(colicDataARM$abdomo_protein, breaks = c(0, 1.5, 3, 4.5, 6, 7.5, 9, 10.5),
                                          labels=c("< 1.5", "1.5-3", "3-4.5", "4.5-6", "6-7.5", "7.5-9", "9-10.5"))
# need to fix columns 3, 4, 5, 18 and 19 to be logical or factor
# 3-rectal_temp
summary(colicDataARM$rectal_temp)
colicDataARM$rectal_temp <- cut(colicDataARM$rectal_temp, breaks = c(34, 35, 36, 37, 38, 39, 40, 41),
                                          labels=c("34-35", "35-36", "36-37", "37-38", "38-39", "39-40", "40-41"))
# 4-pulse
summary(colicDataARM$pulse)
colicDataARM$pulse <- cut(colicDataARM$pulse, breaks = c(30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180, 195),
                                labels=c("30-45", "45-60", "60-75", "75-90", "90-105", "105-120", "120-135", "135-150", "150-165",
                                         "165-180", "180-195"))
# 5-respiratory_rate
summary(colicDataARM$respiratory_rate)
colicDataARM$respiratory_rate <- cut(colicDataARM$respiratory_rate, breaks = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105),
                                labels=c("5-15", "15-25", "25-35", "35-45", "45-55", "55-65", " 65-75", 
                                "75-85", "85-95", "95-105"))

# 18-packed_cell_volume
summary(colicDataARM$packed_cell_volume)
colicDataARM$packed_cell_volume <- cut(colicDataARM$packed_cell_volume, breaks = c(20, 30, 40, 50, 60, 70, 80),
                                     labels=c("20 -30", "30-40", "40-50", "50-60", "60-70", "70-80"))

# 19-total_protein
summary(colicDataARM$total_protein)
colicDataARM$total_protein <- cut(colicDataARM$total_protein, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90),
                                       labels=c("0-10", "10-20", "20 -30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90"))

# function to run associative rule mining
# df - dataframe
# conf_int - confidence interval
# supp_num = supp
# max_length - maximum length
myARMlived <- function (df, conf_int, supp_num, min_length, max_length)
{
        apriori(df, parameter=list(supp=supp_num, conf=conf_int, minlen=min_length, maxlen=max_length),
                appearance = list(default="lhs", rhs="outcome=lived"),
                control = list(verbose=F))
}

myARMdied <- function (df, conf_int, supp_num, min_length, max_length)
{
  apriori(df, parameter=list(supp=supp_num, conf=conf_int, minlen=min_length, maxlen=max_length),
          appearance = list(default="lhs", rhs=c("outcome=died", "outcome=euthanized")),
          control = list(verbose=F))
}

# START HERE 12/5/2019
myRules <- myARMlived(colicDataARM, 0.9, 0.001, 2, 4)
myRules <- myARMlived(colicDataARM, 0.9, 0.07, 2, 4)
myRules <- myARMlived(colicDataARM, 0.95, 0.05, 2, 4)

# Best set of rules
myRules <- myARMlived(colicDataARM, 0.9, 0.035, 2, 5)

# died rules
myRules <- myARMdied(colicDataARM, 0.9, 0.001, 2, 4)
myRules <- myARMdied(colicDataARM, 0.9, 0.07, 2, 4)
myRules <- myARMdied(colicDataARM, 0.95, 0.05, 2, 4)

# Best set of rules
myRules <- myARMdied(colicDataARM, 0.9, 0.035, 2, 5)

# inspection error
detach(package:tm, unload=TRUE)
# Error in UseMethod("inspect", x) : 
# inspect the rules
inspect(myRules[1:25])
## sorted
SortedmyRules_conf <- sort(myRules, by="confidence", decreasing=TRUE)
inspect(SortedmyRules_conf[1:20])

SortedmyRules_sup <- sort(myRules, by="support", decreasing=TRUE)
inspect(SortedmyRules_sup[1:20])

SortedmyRules_lift <- sort(myRules, by="lift", decreasing=TRUE)
inspect(SortedmyRules_lift[1:20])

# need to review what is more important and what values make sense for support and lift and confidence

# ----------------------------------------------------------------------------------------------------------------------
# Visualize the Rules
plot (SortedmyRules_sup[1:10],method="graph",interactive=TRUE,shading="confidence") 
plot (SortedmyRules_conf[1:10],method="graph",interactive=TRUE,shading="confidence") 
arulesViz::plotly_arules(SortedmyRules_conf[1:30])
arulesViz::plotly_arules(SortedmyRules_sup[1:30])

# ----------------------------------------------------------------------------------------------------------------------
# Naive Bayes

#visual 5
# takes too long!
#ggpairs(colicData)

#split data into training and test data sets
indxTrain <- createDataPartition(y = colicData$outcome,p = 0.75,list = FALSE)
training <- colicData[indxTrain,]
testing <- colicData[-indxTrain,]

#Check dimensions of the split
prop.table(table(colicData$outcome)) * 100
prop.table(table(training$outcome)) * 100
prop.table(table(testing$outcome)) * 100


#create objects x which holds the predictor variables and y which holds the response variables
y = training$outcome
x = training[,-22] #remove outcome

# best Accuracy is with 5
model <- train(x,y,'nb',trControl=trainControl(method='cv',number=5))
model

nb<-naiveBayes(outcome~., data = training, laplace = 0, na.action = na.pass)

#Model Evaluation
#Predict testing set
#Predict <- predict(model,newdata = testing , type=c("class", "raw"), threshold=0.01, eps=0)
nbPredict <- predict(nb, newdata = testing)
nbPredict <- predict(nb, newdata = testing , type=c("class", "raw"), lplace = 1, threshold=0.0001, eps=0)

#Get the confusion matrix to see accuracy value and other parameter values

nbCM <- confusionMatrix(nbPredict, testing$outcome )
nbCM$table
sum(diag(nbCM$table)) / sum(nbCM$table)
# shows an accuracy of 79% (better than decision tree)

#Plot Variable performance
# plot difference or probabilities?

# ----------------------------------------------------------------------------------------------------------------------
# SVM



# ----------------------------------------------------------------------------------------------------------------------
# REGRESSION ANALYSIS

# Just some test regressions on certain variables to see how they are correlated, if at all
# Reflected in Figure 23
testReg1 <- lm(satSurvey$Satisfaction ~ satSurvey$FlightMins)
summary(testReg1)

# Reflected in Figure 24
testReg2 <- lm(satSurvey$Satisfaction ~ satSurvey$Status)
s.model <- summary(testReg2)
# Reflected in Figure 24
testReg3 <- lm(satSurvey$Satisfaction ~ satSurvey$NumFlights)
s.model <- summary(testReg3)
# Reflected in Figure 24
testReg4 <- lm(satSurvey$Satisfaction ~ satSurvey$PriceSens)
s.model <- summary(testReg4)


coefficients(testReg1) # model coefficients
confint(testReg1, level=0.95) # CIs for model parameters

# create a smaller dataframe of the top 3 airlines based on mean satisfaction
topAsatSurvey1 <- as.data.frame(subset(satSurvey, AirlineCode == "VX"))
topAsatSurvey2 <- as.data.frame(subset(satSurvey, AirlineCode == "HA"))
topAsatSurvey3 <- as.data.frame(subset(satSurvey, AirlineCode =="AS"))
topAsatSurvey <- rbind(topAsatSurvey1, topAsatSurvey2, topAsatSurvey3)

allTopReg <- lm(topAsatSurvey$Satisfaction ~., data = as.data.frame(topAsatSurvey))
top.model <- summary(allTopReg)
top.model$adj.r.squared
TopAICModel <- step(allTopReg, data=topAsatSurvey, direction="backward")
# AIC Results
#Step:  AIC=-4362.92
#topAsatSurvey$Satisfaction ~ Status + Age + Gender + PriceSens + 
#  FFYear + PercOther + TravelType + AirlineCode + SchDeptHour + 
#  Cancelled + ArrDelayGT5
# Reflected Figure 25
BestTopModel <- lm(formula=topAsatSurvey$Satisfaction ~ Status + Age + Gender + PriceSens + 
                     FFYear + PercOther + TravelType + AirlineCode + SchDeptHour + 
                     Cancelled + ArrDelayGT5, data = as.data.frame(topAsatSurvey))
summary(BestTopModel)

# output the data in a more readable format
#stargazer(BestlmModel, allReg, type="text", title="Linear Regression Output", align=TRUE)
SG <- stargazer(BestTopModel, type="text", title="Linear Regression Output", align=TRUE)

# look at linear regression on all variables
# this is useless - want a Multiple R-squared and p-value for those that matter!
# multiple linear regression to see if the multiple R-squared show correlation
# and review the p-values
allReg <- lm(satSurvey$Satisfaction ~., data=as.data.frame(satSurvey))
s.model <- summary(allReg)
s.model$coef[,4]
s.model$adj.r.squared

# step through to get the best model
AICModels<-step(allReg,data=satSurvey, direction="backward")
#AIC Results were as follows:
# Call:
#  lm(formula = satSurvey$Satisfaction ~ Status + Age + Gender + 
#       PriceSens + FFYear + PercOther + TravelType + ShopAmount + 
#       EatDrink + Class + SchDeptHour + Cancelled + ArrDelayGT5, 
#     data = as.data.frame(satSurvey))
# AIC = -85696.57
BestlmModel <- lm(formula = satSurvey$Satisfaction ~ Status + Age + Gender + 
                    PriceSens + FFYear + PercOther + TravelType + ShopAmount + 
                    EatDrink + Class + SchDeptHour + Cancelled + ArrDelayGT5, 
                  data = as.data.frame(satSurvey))
# Reflected Figure 25
summary(BestlmModel)

# Reflected in Figure 26
Final.Model <- lm(formula = satSurvey$Satisfaction ~ Status + Age + Gender + 
                    PriceSens + FFYear + PercOther + TravelType + ShopAmount + 
                    Class + SchDeptHour + Cancelled + ArrDelayGT5, 
                  data = as.data.frame(satSurvey))
summary(Final.Model)

# output the data in a more readable format
#stargazer(BestlmModel, allReg, type="text", title="Linear Regression Output", align=TRUE)
stargazer(Final.Model, type="text", title="Linear Regression Output", align=TRUE)
summary.Model<- summary(Final.Model)

# SVM, KSVM, NB
# a new method for sampling the data for SVM and NB
# ** Due to the time it takes to run this output - I elected to take a random sample of 15,000 of the TrainData
# ** and 5,000 of the test data to test this model
svm.train <- satSurvey[sample(nrow(satSurvey), size=15000, replace=FALSE),]
table(svm.train$Satisfaction)/length(svm.train$Satisfaction)

svm.test <- satSurvey[sample(nrow(satSurvey), size=5000, replace=FALSE),]
nb.test <- satSurvey[sample(nrow(satSurvey), size=5000, replace=FALSE),]
ksvm.test <- satSurvey[sample(nrow(satSurvey), size=5000, replace=FALSE),]
lm.test <- satSurvey[sample(nrow(satSurvey), size=5000, replace=FALSE),]

table(svm.test$Satisfaction)/length(svm.test$Satisfaction)

# Linear Regression Prediction
lmPred <- predict(Final.Model, lm.test, type="response")
lmPred <- data.frame(lmPred)
compTable <- data.frame(lm.test[,1],lmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMElm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMElm

# plot some LM results
# compute absolute error for each case
compTable$error <- abs(compTable$test - compTable$Pred)
# create new dataframe for plotting
# Reflected in Figure 28
lmPlot <- data.frame(compTable$error, lm.test$PriceSens, lm.test$Status)
colnames(lmPlot) <- c("error", "PriceSens", "Status")
plotlm1 <- ggplot(lmPlot, aes(x=PriceSens, y=Status)) +
  geom_point(aes(size=error, color=error)) +
  ggtitle("Linear Regression with Price Sensitivity and Loyalty Status") +
  xlab("Price Sensitivity") + ylab("Loyalty Card Status")
plotlm1

# Reflected in Figure 29
lmPlot <- data.frame(compTable$error, lm.test$TravelType, lm.test$Status, lm.test$PriceSens)
colnames(lmPlot) <- c("error", "TravelType", "Status", "PriceSens")
plotlm2 <- ggplot(lmPlot, aes(x=PriceSens, y=Status)) +
  geom_point(aes(size=error, color=TravelType)) +
  ggtitle("Linear Regression with Price Sensitivity, Travel Type and Loyalty Status") +
  xlab("Price Sensitivity") + ylab("Loyalty Card Status")
plotlm2

# Reflected in Figure 30
lmPlot <- data.frame(compTable$error, lm.test$DeptDelayMins, lm.test$ArrDelayMins, lm.test$SchDeptHour)
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


# SUpport Vector Machine (SVM)
svm.model<- svm(Satisfaction~Status+Age+PriceSens+PercOther+TravelType+Class+DeptDelayMins+ArrDelayMins,data=svm.train)
summary(svm.model)

svm.test$PredS<-predict(svm.model,svm.test, type="votes")
# Compare Observed and Predicted
table.svm <- table(pred = svm.validate$PredS,
                   true = svm.validate$Satisfaction)/length(svm.validate$Satisfaction)

# SVM 
# create prediction
svmPred <- predict(svm.model, svm.test, type="votes")
svmPred <- (data.frame(svmPred))
compTable <- data.frame(svm.test[,1],svmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMEsvm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMEsvm

# plot some SVM results
# compute absolute error for each case
compTable$error <- abs(compTable$test - compTable$Pred)
# create new dataframe for plotting
# Reflected in Figure 33
svmPlot <- data.frame(compTable$error, svm.test$PriceSens, svm.test$Status)
colnames(svmPlot) <- c("error", "PriceSens", "Status")
plotsvm1 <- ggplot(svmPlot, aes(x=PriceSens, y=Status)) +
  geom_point(aes(size=error, color=error)) +
  ggtitle("Support Vector Machine with Price Sensitivity and Loyalty Status") +
  xlab("Price Sensitivity") + ylab("Loyalty Card Status")
plotsvm1

# Reflected in Figure 34
svmPlot <- data.frame(compTable$error, svm.test$TravelType, svm.test$Status, svm.test$PriceSens)
colnames(svmPlot) <- c("error", "TravelType", "Status", "PriceSens")
plotsvm2 <- ggplot(svmPlot, aes(x=PriceSens, y=Status)) +
  geom_point(aes(size=error, color=TravelType)) +
  ggtitle("Support Vector Machine with Price Sensitivity, Travel Type and Loyalty Status") +
  xlab("Price Sensitivity") + ylab("Loyalty Card Status")
plotsvm2

# Reflected in Figure 35
svmPlot <- data.frame(compTable$error, svm.test$DeptDelayMins, svm.test$ArrDelayMins, svm.test$SchDeptHour)
colnames(svmPlot) <- c("error", "DeptDelay", "ArrivalDelay", "ScheduledDept")
plotsvm3 <- ggplot(svmPlot, aes(x=DeptDelay, y=ArrivalDelay)) +
  geom_point(aes(size=error, color=ScheduledDept)) +
  ggtitle("Support Vector Machine with Departure and Arrival Delays and Scheduled Departure Hour") +
  scale_x_continuous(limits=c(0,200)) + scale_y_continuous(limits=c(0,200)) + 
  xlab("Departure Delay in Minutes") + ylab("Arrival Delay in Minutes")
plotsvm3

# KSVM
ksvmOutput <- ksvm(Satisfaction~Status+Age+PriceSens+PercOther+TravelType+Class+DeptDelayMins+ArrDelayMins,
                   data=svm.train,kernel="rbfdot",kpar="automatic",C=10,cross=3,prob.model=TRUE)

# test the model
ksvmPred <- predict(ksvmOutput, ksvm.test, type="votes")
ksvmPred <- data.frame(ksvmPred)
str(ksvmPred)
compTable <- data.frame(ksvm.test[,1],ksvmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMEksvm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMEksvm

# Naive Bayes
nb.train <- satSurvey[sample(nrow(satSurvey), size=3000, replace=FALSE),]
nb.test <- satSurvey[sample(nrow(satSurvey), size=1000, replace=FALSE),]

nbOutput <- naiveBayes(Satisfaction~Age+PriceSens+PercOther,
                       data=nb.train)
str(nbOutput)
# create prediction
nbPred <- predict(nbOutput, nb.test)
nbPred <- as.data.frame(nbPred)
str(nbPred)
compTable <- data.frame(nb.test[,1],nbPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RSME
RSMEnb <- sqrt(mean(compTable$test-compTable$Pred)^2)
RSMEnb

# Reflected in Figure 36
RSME.frame<-NULL
RSME.frame <- c(RSMEsvm, RSMElm, RSMEksvm)
RSME.frame<-melt(RSME.frame)
RSME.frame$model <- c("RSMEsvm", "RSMElm", "RSMEksvm")
g <- ggplot(data=RSME.frame, aes(x=model, y=value))
g <- g + geom_bar(stat="identity", position="dodge")
g <- g + ggtitle("RSME Values by Model Type")
g <- g + xlab("RSME Model") + ylab("RSME Value")
g




