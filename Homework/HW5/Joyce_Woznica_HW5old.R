
# Course: IST 707
# Name: Joyce Wozninca
# Homework #5
# Code: HW4 R Code
# Due Date: 11/8/2019
#
# ----------------------------------------------------------------------------------------------------------------------
## Load the packages
require(caret)
require(dplyr)
require(ggplot2)
require(rpart)
require(rpart.plot)
require(e1071)
install.packages("rattle")
library(rattle)
require(rattle)
# This loads applicable libraries and packages (and some extras)
## a format that we can evaluate.
library(wordcloud)
## Loading required package: RColorBrewer
library(tm)
## Loading required package: NLP
library(slam)
library(quanteda)
library(SnowballC)

#---------------------------------------------------------------------------------------------------------
# Option 2 Approach
setwd("C:/Users/Joyce/Desktop/Syracuse/IST707/Homework/HW4")
## Next, load in the documents (the corpus)
FedPapersCorpus <- Corpus(DirSource("FedPapersCorpus"))

##The following will show you that you read in all the documents
(summary(FedPapersCorpus))
(meta(FedPapersCorpus[[1]]))
(meta(FedPapersCorpus[[1]],5))

# create stop words to ignore
(MyStopwords <- c("will","one","two", "may","less","publius","madison","alexand", "alexander", "jame", "james", "hamilton","jay", 
                  "well","might","without","small", "single", "several", "but", "very", "can", "must", "also", "any", "and", 
                  "are", "however", "into", "almost", "can","for", "add", "author" ))
(STOPS <- c(MyStopwords, stopwords("english")))
# remove stop words
FedPapersCorpus <- tm_map(FedPapersCorpus, removeWords, STOPS)
# convert to lower case
FedPapersCorpus <- tm_map(FedPapersCorpus, tolower)
# strip out the white space
FedPapersCorpus <- tm_map(FedPapersCorpus, stripWhitespace)
# Remove punctuation
FedPapersCorpus <- tm_map(FedPapersCorpus, removePunctuation)
# stemming
FedPapersCorpus <- tm_map(FedPapersCorpus, stemDocument)


(getTransformations())
## [1] "removeNumbers" "removePunctuation" "removeWords"
## [4] "stemDocument" "stripWhitespace"

(nFedPapersCorpus<-length(FedPapersCorpus))
## [1] 85

# set up some parameters around the papers to rule out very rare and very common words
### ignore extremely rare words
# set new for HW5
(minTermFreq <- 30)
(maxTermFreq <- 1000)

# generate the document term matrix, removing punctuation, numbers, converting to lower case,
# stemming of words, removing separators and removing stopwords
Papers_DTM <- DocumentTermMatrix(FedPapersCorpus,
                                 control = list(
                                   stopwords = TRUE,
                                   wordLengths=c(3, 15),
                                   removePunctuation = T,
                                   removeNumbers = T,
                                   tolower=T,
                                   stemming = T,
                                   remove_separators = T,
                                   stopwords = STOPS,
                                   #removeWords = STOPS,
                                   removeWords = STOPS,
                                   bounds = list(global = c(minTermFreq, maxTermFreq))))

#inspect FedPapers Document Term Matrix (DTM)
DTM <- as.matrix(Papers_DTM)

## Look at word frequencies across all the papers
WordFreq <- colSums(as.matrix(Papers_DTM))
(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
# these are the words that occur most frequently
(WordFreq[tail(ord)])

# Row sums per Fed Paper
(Row_Sum_Per_doc <- rowSums((as.matrix(Papers_DTM))))
## Create a normalized version of Papers_DTM
Papers_M <- as.matrix(Papers_DTM)
Papers_M_N1 <- apply(Papers_M, 1, function(i) round(i/sum(i),3))
Papers_Matrix_Norm <- t(Papers_M_N1)

## Convert to matrix and view
Papers_dtm_matrix = as.matrix(Papers_DTM)

## Also convert to DF
Papers_DF <- as.data.frame(as.matrix(Papers_Matrix_Norm))
Papers_DF1<- Papers_DF%>%add_rownames()

names(Papers_DF1)[1]<-"Author"
Papers_DF1[1:11,1]="Disputed"
Papers_DF1[12:62,1]="Hamilton"
Papers_DF1[63:65,1]="HM"
Papers_DF1[66:70,1]="Jay"
Papers_DF1[71:85,1]="Madison"

# What about Jay and HM papers? This is incorrect???
#Papers_DF1[1:11,1]="dispt"
#Papers_DF1[12:62,1]="hamil"
#Papers_DF1[63:77,1]="madis"
#head(Papers_DF1)

str(Papers_DF1)
nrow(Papers_DF1)

#Wordcloud Visualization Hamilton, Madison and Disputed Papers
#subset the papers in Disputed, Hamilton, Madison and Jay
Disputed <- Papers_dtm_matrix[1:11,]
Hamilton <- Papers_dtm_matrix[12:62,]
HM <- Papers_dtm_matrix[63:65,]
Jay <- Papers_dtm_matrix[66:70,]
Madison <- Papers_dtm_matrix[71:85,]

DisputedPapersWC<- wordcloud(colnames(Papers_dtm_matrix), Disputed, colors=brewer.pal(6,"Dark2"))
HamiltonPapersWC <- wordcloud(colnames(Papers_dtm_matrix), Hamilton,colors=brewer.pal(6,"Dark2"))
MadisonPapersWC <- wordcloud(colnames(Papers_dtm_matrix), Madison,colors=brewer.pal(6,"Dark2"))
HMPapersWC <- wordcloud(colnames(Papers_dtm_matrix), HM, colors=brewer.pal(6,"Dark2"))
JayPapersWC <- wordcloud(colnames(Papers_dtm_matrix), Jay, colors=brewer.pal(6,"Dark2"))

##Make Train and Test sets
trainRatio <- .60
set.seed(11) # Set Seed so that same sample can be reproduced in future also
sample <- sample.int(n = nrow(Papers_DF1), size = floor(trainRatio*nrow(Papers_DF1)), replace = FALSE)
train <- Papers_DF1[sample, ]
test <- Papers_DF1[-sample, ]
# train / test ratio
length(sample)/nrow(Papers_DF1)

##Decision Tree Models 
#Train Tree Model 1
train_tree1 <- rpart(Author ~ ., data = train, method="class", control=rpart.control(cp=0))
summary(train_tree1)

#predict the test dataset using the model for train tree No. 1
predicted1 <- predict(train_tree1, test, type="class")
#plot number of splits
rsq.rpart(train_tree1)

plotcp(train_tree1)
#plot the decision tree
fancyRpartPlot(train_tree1, cex=0.8)

#confusion matrix to find correct and incorrect predictions
table(Authorship=predicted1, true=test$Author)

#Train Tree Model 2
train_tree2 <- rpart(Author ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 2, maxdepth = 5))
summary(train_tree2)
#predict the test dataset using the model for train tree No. 2
predicted2<- predict(train_tree2, test, type="class")
#plot number of splits
rsq.rpart(train_tree2)

#plot the decision tree
fancyRpartPlot(train_tree2, cex=0.7)

#confusion matrix to find correct and incorrect predictions
table(Authorship=predicted2, true=test$Author)

#Train Tree Model 3
train_tree3 <- rpart(Author ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 3, maxdepth = 10))
summary(train_tree2)
#predict the test dataset using the model for train tree No. 2
predicted3 <- predict(train_tree3, test, type="class")
#plot number of splits
rsq.rpart(train_tree3)

#plot the decision tree
fancyRpartPlot(train_tree3, cex=0.7)

#confusion matrix to find correct and incorrect predictions
table(Authorship=predicted3, true=test$Author)

#--------------------------------------------------------------------------------------------------------
names(Papers_DF1)[1]<-"Author"
Papers_DF1[1:11,1]="Disputed"
Papers_DF1[12:62,1]="Hamilton"
Papers_DF1[63:65,1]="HM"
Papers_DF1[66:70,1]="Jay"
Papers_DF1[71:85,1]="Madison"

# During prediction - test without the Disputed Papers
Papers_DF1woDisp <- subset(Papers_DF1, Author != "Disputed")
str(Papers_DF1)
nrow(Papers_DF1)

##Make Train and Test sets
trainRatio <- .60
set.seed(11) # Set Seed so that same sample can be reproduced in future also
sample <- sample.int(n = nrow(Papers_DF1), size = floor(trainRatio*nrow(Papers_DF1)), replace = FALSE)
train <- Papers_DF1[sample, ]
test <- Papers_DF1[-sample, ]
# train / test ratio
length(sample)/nrow(Papers_DF1)

##Decision Tree Models 
#Train Tree Model 1
train_tree1 <- rpart(Author ~ ., data = train, method="class", control=rpart.control(cp=0))
summary(train_tree1)

#predict the test dataset using the model for train tree No. 1
predicted1 <- predict(train_tree1, test, type="class")
#plot number of splits
rsq.rpart(train_tree1)

plotcp(train_tree1)
#plot the decision tree
fancyRpartPlot(train_tree1, cex=0.8)

#confusion matrix to find correct and incorrect predictions
table(Authorship=predicted1, true=test$Author)

#Train Tree Model 2
train_tree2 <- rpart(Author ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 2, maxdepth = 5))
summary(train_tree2)
#predict the test dataset using the model for train tree No. 2
predicted2<- predict(train_tree2, test, type="class")
#plot number of splits
rsq.rpart(train_tree2)

#plot the decision tree
fancyRpartPlot(train_tree2, cex=0.7)

#confusion matrix to find correct and incorrect predictions
table(Authorship=predicted2, true=test$Author)

#Train Tree Model 3
train_tree3 <- rpart(Author ~ ., data = train, method="class", control=rpart.control(cp=0, minsplit = 3, maxdepth = 10))
summary(train_tree2)
#predict the test dataset using the model for train tree No. 2
predicted3 <- predict(train_tree3, test, type="class")
#plot number of splits
rsq.rpart(train_tree3)

#plot the decision tree
fancyRpartPlot(train_tree3, cex=0.7)

#confusion matrix to find correct and incorrect predictions
table(Authorship=predicted3, true=test$Author)



