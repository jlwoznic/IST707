TitanicTrainData <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week5/week5_resources_2_2_2_2/Titanic_Training_Data.csv", na.string=c(""))
##print it...
(head(TitanicTrainData, n=5))
filename="C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week5/week5_resources_2_2_2_2/Titanic_Testing_Data.csv"
## Note: na.string=c("") will replace empty with NA
TitanicTestData <- read.table(filename, sep=",", header=TRUE, na.string=c(""))
##print it...
(head(TitanicTestData,n=5))

## Clean and prepare the data

## (1)
## Look at the structure
(str(TitanicTestData))
(str(TitanicTrainData))
## Let's consider each attribute - 
## Passenger ID - we cannot use this - so we can remove it. 
## If we want to look up a passenger ID later, we can remove the
## column after we clean and prep everything, so that it stays aligned.
## For this case, I do not need it. So I will remove it now.
## I am also going to remove the Cabin, Name, and the Ticket
## I like to use Temp df's so as to keep the originals 

TempTitanicTrain <- TitanicTrainData
TempTitanicTest <- TitanicTestData

# Remove PassengerID, Name, Ticket, and Cabin 
TempTitanicTrain <-TempTitanicTrain[ , -which(names(TempTitanicTrain) %in% 
                                                c("PassengerId","Name","Ticket","Cabin"))]
(head(TempTitanicTrain, n=10))

TempTitanicTest <- TempTitanicTest[ , -which(names(TempTitanicTest) %in% 
                                               c("PassengerId","Name","Ticket","Cabin"))]
(head(TempTitanicTest, n=10))

## Next - check and maybe remove all the blanks/NAs
## How many NAs
(is.na(TempTitanicTrain))
(sum(is.na(TempTitanicTrain)))
(is.na(TempTitanicTest))
(sum(is.na(TempTitanicTest)))

## The AGE variable has a lot of missing items - let's 
## think about what do do about this. We can remove the age
## variable, we can remove the rows with NA, or we can try to 
## fill in the missing ages with the age mean or median.
## Filling in the values is not a good idea because
## we are trying to build a predictor (a decision-maker)
## Using false ages will only generate potentially incorrect
## results. 
## So - we can remove the column or remove the rows with NA.
## There is no perfect choice. So let us see how many rows we have left 
## after removing the rows with NA

## How many rows are complete?
cat("The Titanic Test data has a total of ", nrow(TempTitanicTest), "rows.")
cat("The Titanic Train data has a total of ", nrow(TempTitanicTrain), "rows.")
TotalCompleteRowsTrain <- (nrow(TempTitanicTrain[complete.cases(TempTitanicTrain),]))
TotalCompleteRowsTest <- (nrow(TempTitanicTest[complete.cases(TempTitanicTest),]))
cat("The Titanic Train data has a total of ", TotalCompleteRowsTrain, "complete rows.")
cat("The Titanic Test data has a total of ", TotalCompleteRowsTest, "complete rows.")

## The above tells us that we will still have a large testing and training
## set - even if we remove all rows with NA

TempTitanicTrain <- TempTitanicTrain[complete.cases(TempTitanicTrain),]
TempTitanicTest <- TempTitanicTest[complete.cases(TempTitanicTest),]
## double check - both of these should be 0 - which they are
(nrow(TempTitanicTrain[!complete.cases(TempTitanicTrain),]))
(nrow(TempTitanicTest[!complete.cases(TempTitanicTest),]))

## Now its time to discretize
## Let's look at the str and tables
(str(TempTitanicTest))
(str(TempTitanicTrain))
## change "survived" to afactor
TempTitanicTrain$Survived=factor(TempTitanicTrain$Survived)
##Pclass is the classification of the ticket - 1st, 2nd, 3rd.
## Make this into a factor
TempTitanicTrain$Pclass=factor(TempTitanicTrain$Pclass)
TempTitanicTest$Pclass=factor(TempTitanicTest$Pclass)
## Age is quantitative and must be binned and discretized
## Check Age for errors and look at values
(freq=table(TempTitanicTrain$Age))
## We see that there are some incorrect ages: .42, .67, .75, .83, and .92 are not correct. 
## Remove those rows first
(freq=table(TempTitanicTest$Age))  ## Same errors in Test data

## Place NA for any ages that are < 1
TempTitanicTrain$Age <- ifelse(TempTitanicTrain$Age < 1, "NA", TempTitanicTrain$Age)
(freq=table(TempTitanicTrain$Age))
TempTitanicTest$Age <- ifelse(TempTitanicTest$Age < 1, "NA", TempTitanicTest$Age)
(freq=table(TempTitanicTest$Age))
## Remove NAs

TempTitanicTrain <- TempTitanicTrain[complete.cases(TempTitanicTrain),]
TempTitanicTest <- TempTitanicTest[complete.cases(TempTitanicTest),]

## Now we can discretize the Age
TempTitanicTrain$Age[TempTitanicTrain$Age <= 22] <- 1
TempTitanicTrain$Age[TempTitanicTrain$Age > 22 & TempTitanicTrain$Age <=38] <- 2
TempTitanicTrain$Age[TempTitanicTrain$Age > 38] <- 3
TempTitanicTrain$Age=factor(TempTitanicTrain$Age)
(TempTitanicTrain$Age)

TempTitanicTest$Age[TempTitanicTest$Age <= 22] <- 1
TempTitanicTest$Age[TempTitanicTest$Age > 22 & TempTitanicTest$Age <=38] <- 2
TempTitanicTest$Age[TempTitanicTest$Age > 38] <- 3
TempTitanicTest$Age=factor(TempTitanicTest$Age)
(TempTitanicTest$Age)

## check it
(freq=table(TempTitanicTrain$Age))
(freq=table(TempTitanicTest$Age))

##Look at the str again
(str(TempTitanicTest))
(str(TempTitanicTrain))

## Now we will discretize Sibsp (number of siblings on board)
## and Parch (number of parents or children)
## WHile I will not do this here - it might also be interesting to 
## add these to create a vew attribute called Family

(freq=table(TempTitanicTrain$SibSp))
(freq=table(TempTitanicTest$SibSp))

(freq=table(TempTitanicTrain$Parch))
(freq=table(TempTitanicTest$Parch))

## Given that so much of the data is at 0 or 1, I will group
## SibSp and Parch so that they are 0 (none) or 1 (one or more)

TempTitanicTrain$SibSp[TempTitanicTrain$SibSp == 0] <- 0
TempTitanicTrain$SibSp[TempTitanicTrain$SibSp > 0] <- 1
TempTitanicTrain$SibSp=factor(TempTitanicTrain$SibSp)
(TempTitanicTrain$SibSp)

TempTitanicTrain$Parch[TempTitanicTrain$Parch == 0] <- 0
TempTitanicTrain$Parch[TempTitanicTrain$Parch > 0] <- 1
TempTitanicTrain$Parch=factor(TempTitanicTrain$Parch)
(TempTitanicTrain$Parch)

TempTitanicTest$SibSp[TempTitanicTest$SibSp == 0] <- 0
TempTitanicTest$SibSp[TempTitanicTest$SibSp > 0] <- 1
TempTitanicTest$SibSp=factor(TempTitanicTest$SibSp)
(TempTitanicTest$SibSp)

TempTitanicTest$Parch[TempTitanicTest$Parch == 0] <- 0
TempTitanicTest$Parch[TempTitanicTest$Parch > 0] <- 1
TempTitanicTest$Parch=factor(TempTitanicTest$Parch)
(TempTitanicTest$Parch)


(freq=table(TempTitanicTrain$SibSp))
(freq=table(TempTitanicTest$SibSp))

(freq=table(TempTitanicTrain$Parch))
(freq=table(TempTitanicTest$Parch))

## Continue the process...look at the currect str
(str(TempTitanicTest))
(str(TempTitanicTrain))

## The last step is to discretize the Fare
(freq=table(TempTitanicTrain$Fare))
(freq=table(TempTitanicTest$Fare))
## We can see that the range is within 0 - 550
## We could also have gotten the min and max

## Place into three groups: 1: Low, 2: Mediun, 3: High  Fare rate
TempTitanicTrain$Fare[TempTitanicTrain$Fare <= 10] <- 1
TempTitanicTrain$Fare[TempTitanicTrain$Fare > 10 & TempTitanicTrain$Fare <=30] <- 2
TempTitanicTrain$Fare[TempTitanicTrain$Fare > 30] <- 3
TempTitanicTrain$Fare=factor(TempTitanicTrain$Fare)
#(TempTitanicTrain$Fare)

TempTitanicTest$Fare[TempTitanicTest$Fare <= 10] <- 1
TempTitanicTest$Fare[TempTitanicTest$Fare > 10 & TempTitanicTest$Fare <=30] <- 2
TempTitanicTest$Fare[TempTitanicTest$Fare > 30] <- 3
TempTitanicTest$Fare=factor(TempTitanicTest$Fare)
#(TempTitanicTest$Fare)

(freq=table(TempTitanicTrain$Fare))
(freq=table(TempTitanicTest$Fare))
(str(TempTitanicTest))
(str(TempTitanicTrain))

## Our datasets are finally clean and ready to use.
## Let's move the temp to new dataframe names
CleanTest <- TempTitanicTest
CleanTrain <- TempTitanicTrain


install.packages("RWeka")
library(RWeka)
#Note: If you are installing the RWeka package on a windows pc, you may need to set
#the Java environment first by execute the following script in R:
#  Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_51\\jre")
#Step2: READ FILES (change file path to your own file path)
#trainset <- read.csv("/Users/byu/Documents/R/train.csv")
#testset <- read.csv("/Users/byu/Documents/R/test.csv")
trainset<-TitanicTrainData
testset<-TitanicTestData

NN <- make_Weka_filter("weka/filters/unsupervised/attribute/NumericToNominal") #
#build a function using RWeka filter interface
#Apply the filter function to both training and test datasets.
trainset <- NN(data=trainset, control= Weka_control(R="1-3"), na.action = NULL)
testset <- NN(data=testset, control= Weka_control(R="1,3"), na.action = NULL)
#. Deal with missing values
MS <- make_Weka_filter("weka/filters/unsupervised/attribute/ReplaceMissingValues") #
#build a function using RWeka filter interface
#Apply the filter function to both training and test datasets.
trainset <-MS(data=trainset, na.action = NULL)
testset <-MS(data=testset, na.action = NULL)


m=J48(Survived~., data = trainset)
m=J48(Survived~., data = trainset, control=Weka_control(U=FALSE, M=2,C=0.5))
#* View parameters with function WOW:
WOW(J48)
 

e <- evaluate_Weka_classifier(m,
                              numFolds = 10,
                              seed = 1, class = TRUE)
summary(e)
e$details

pred<-predict (m, newdata = testset, type =c("class"))
# above doesn't work

write.csv(pred, file="C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week5/jwtitanic.csv")

myVars=c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Survived")
testvars=c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare")
newtrain=trainset[myVars]
newtest=testset[testvars]
m=J48(Survived~., data = newtrain)
m=J48(Survived~., data = newtrain, control=Weka_control(U=FALSE, M=2, C=0.5))
e=evaluate_Weka_classifier(m, seed=1, numFolds=10)
pred=predict (m, newdata = newtest, type = c("class"))
myids=c("PassengerId")
id_col=testset[myids]
newpred=cbind(id_col, pred)
colnames(newpred)=c("Passengerid", "Survived")
View(newpred)

write.csv(newpred, file="C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week5/titanic-J48-pred.csv",
          row.names=FALSE)
InfoGainAttributeEval(Survived ~ . , data = trainset)
