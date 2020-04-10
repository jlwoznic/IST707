#########################################################
##
##          Tutorial: Text Mining and NLP             
##
##           Topics:
##             - Tokenization
##             - Vectorization
##             - Normalization
##             - classification/Clustering
##             - Visualization
##
##         
#########################################################
## Gates
#########################################################


library(tm)
#install.packages("tm")
library(stringr)
library(wordcloud)
# ONCE: install.packages("Snowball")
## NOTE Snowball is not yet available for R v 3.5.x
## So I cannot use it  - yet...
##library("Snowball")
##set working directory
## ONCE: install.packages("slam")
library(slam)
library(quanteda)
## ONCE: install.packages("quanteda")
## Note - this includes SnowballC
library(SnowballC)
library(arules)
## ONCE: install.packages("wordcloud")
library(wordcloud)
##ONCE: install.packages('proxy')
library(proxy)
library(cluster)
library(stringi)
library(proxy)
library(Matrix)
library(tidytext) # convert DTM to DF
library(plyr) ## for adply
library(ggplot2)
library(factoextra) # for fviz
library(mclust) # for Mclust EM clustering

#install.packages("slam")
library(slam)
#install.packages("tm")
library(tm)
#install.packages("factoextra")
library(factoextra)



#setwd("C:\\Users\\profa\\Documents\\R\\RStudioFolder_1\\DrGExamples\\SYR\\IST707\\Week4")
setwd("C:\\Users\\jerem\\Google Drive\\Online\\iCuse\\IST707\\Week4")
## Next, load in the documents (the corpus)
##NovelsCorpus <- Corpus(DirSource("Novels_Corpus"))
NovelsCorpus <- Corpus(DirSource("FedPapersCorpus"))
(getTransformations())
(ndocs<-length(NovelsCorpus))

##The following will show you that you read in all the documents
(summary(NovelsCorpus))
(meta(NovelsCorpus[[1]]))
(meta(NovelsCorpus[[1]],5))

# ignore extremely rare words i.e. terms that appear in less then 1% of the documents
(minTermFreq <- ndocs * 0.0001)
# ignore overly common words i.e. terms that appear in more than 50% of the documents
(maxTermFreq <- ndocs * 1)
(MyStopwords <- c("maggie", "philip", "tom", "glegg", "deane", "stephen","tulliver"))
  #stopwords))
(STOPS <-stopwords('english'))
Novels_dtm <- DocumentTermMatrix(NovelsCorpus,
                         control = list(
                           stopwords = TRUE, 
                           wordLengths=c(3, 15),
                           removePunctuation = T,
                           removeNumbers = T,
                           tolower=T,
                           stemming = T,
                           remove_separators = T,
                           stopwords = MyStopwords,
                           #removeWords(STOPS),
                           #removeWords(MyStopwords),
                           bounds = list(global = c(minTermFreq, maxTermFreq))
                         ))
## Have a look
#inspect(Novels_dtm)
DTM_mat <- as.matrix(Novels_dtm)
(DTM_mat[1:13,1:5])
#Novels_dtm <- weightTfIdf(Novels_dtm, normalize = TRUE)
#Novels_dtm <- weightTfIdf(Novels_dtm, normalize = FALSE)

## Look at word freuqncies
(WordFreq <- colSums(as.matrix(Novels_dtm)))

(head(WordFreq))
(length(WordFreq))
ord <- order(WordFreq)
(WordFreq[head(ord)])
(WordFreq[tail(ord)])
## Row Sums
(Row_Sum_Per_doc <- rowSums((as.matrix(Novels_dtm))))

## I want to divide each element in each row by the sum of the elements
## in that row. I will test this on a small matrix first to make 
## sure that it is doing what I want. YOU should always test ideas
## on small cases.
## Create a small pretend matrix
## Using 1 in apply does rows, using a 2 does columns
(mymat = matrix(1:12,3,4))
freqs2 <- apply(mymat, 1, function(i) i/sum(i))
## Oddly, this re-organizes the matrix - so I need to transpose back
(t(freqs2))
## OK - so this works. Now I can use this to control the normalization of
## my matrix...

## Create a normalized version of Novels_dtm
Novels_M <- as.matrix(Novels_dtm)
Novels_M_N1 <- apply(Novels_M, 1, function(i) round(i/sum(i),3))
## transpose
Novels_Matrix_Norm <- t(Novels_M_N1)
## Have a look at the original and the norm to make sure
(Novels_M[c(1:6),c(1000:1005)])
(Novels_Matrix_Norm[c(1:6),c(1000:1005)])
## From the line of code
## (Row_Sum_Per_doc <- rowSums((as.matrix(Novels_dtm))))
## above, we can see that Austen_Sense has a row sum of 53102
## So, we can confirm correctness. For abandon we should have
## 1/53102 = 1.88 x10-5 which is what we have. 

## Sometimes it is better to normalize your own matrix so that
## YOU have control over the normalization. For example
## scale used diectly may not work - why?

## Convert to matrix and view
Novels_dtm_matrix = as.matrix(Novels_dtm)
str(Novels_dtm_matrix)
(Novels_dtm_matrix[c(1:3),c(2:4)])

## Also convert to DF
Novels_DF <- as.data.frame(as.matrix(Novels_dtm))
str(Novels_DF)
(Novels_DF$aunt)
(nrow(Novels_DF))  ## Each row is a novel
## Fox DF format



wordcloud(colnames(Novels_dtm_matrix), Novels_dtm_matrix[13, ], max.words = 70)
(head(sort(as.matrix(Novels_dtm)[13,], decreasing = TRUE), n=20))

############## Distance Measures ######################

m  <- Novels_dtm_matrix
m_norm <- Novels_Matrix_Norm
# # # m <- m[1:2, 1:3]
distMatrix_E <- dist(m, method="euclidean")
#print(distMatrix_E)
distMatrix_C <- dist(m, method="cosine")
print(distMatrix_C)
distMatrix_C_norm <- dist(m_norm, method="cosine")

############# Clustering #############################
## Hierarchical

## Euclidean
groups_E <- hclust(distMatrix_E,method="ward.D")
plot(groups_E, cex=0.9, hang=-1)
rect.hclust(groups_E, k=4)

## Cosine Similarity
groups_C <- hclust(distMatrix_C,method="ward.D")
plot(groups_C, cex=0.9, hang=-1)
rect.hclust(groups_C, k=4)

## Cosine Similarity for Normalized Matrix
groups_C_n <- hclust(distMatrix_C_norm,method="ward.D")
plot(groups_C_n, cex=0.9, hang=-1)
rect.hclust(groups_C_n, k=4)

### NOTES: Cosine Sim works the best. Norm and not norm is about
## the same because the size of the novels are not sig diff.

####################   k means clustering -----------------------------
X <- m_norm
## Remember that kmeans uses a matrix of ONLY NUMBERS
## We have this so we are OK.
## Manhattan gives the best vis results!
distance1 <- get_dist(X,method = "manhattan")
fviz_dist(distance1, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance2 <- get_dist(X,method = "pearson")
fviz_dist(distance2, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance3 <- get_dist(X,method = "canberra")
fviz_dist(distance3, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance4 <- get_dist(X,method = "spearman")
fviz_dist(distance4, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

## Next, our current matrix does NOT have the columns as the docs
## so we need to transpose it first....
## Run the following twice...
X <- t(X)
## Now scale the data
#X <- scale(X)
str(X)
## k means
kmeansFIT_1 <- kmeans(X,centers=4)
#(kmeansFIT1)
summary(kmeansFIT_1)
#(kmeansFIT_1$cluster)
fviz_cluster(kmeansFIT_1, data = X)
## --------------------------------------------

################# Expectation Maximization ---------
## When Clustering, there are many options. 
## I cannot run this as it requires more than 18 Gigs...

#ClusFI <- Mclust(X,G=6)
#(ClusFI)
#summary(ClusFI)
#plot(ClusFI, what = "classification")


########### Frequencies and Associations ###################

## FInd frequenct words...
(findFreqTerms(Novels_dtm, 2500))
## Find assocations with aselected conf
(findAssocs(Novels_dtm, 'aunt', 0.95))

##############  NOTE ############################
## The following is an alternative method
## This code can take a long time to run.
## It is commented out for now.
#################################################

##Next, there are several steps needed to prepare the texts
## You will need to remove punctuation, make everything lowercase
## normalize, remove common and useless words like "and", "the", "or"
## Uselses words are called "Stopwords"
## Don't forget to remove numbers as well. 

## The function : getTransformations() will show all the functions
## that process the data - such as removeNumbers, removePunctuation, etc
## run getTransformations() to see this.
## Also note that tolower() will change all case to lowercase.

## The tm_map function allows you to perform the same 
## transformations on all of your texts at once
#tm_corpus <- tm_map(tm_corpus, (meta(NovelsCorpus[[1]],5)))
#CleanNovelsCorpus <- tm_map(NovelsCorpus, content_transformer(removePunctuation))
#(meta(CleanNovelsCorpus[[1]]))  ## Now the metadata is gone - the id is lost

## Remove all Stop Words
#CleanNovelsCorpus <- tm_map(CleanNovelsCorpus, removeWords, stopwords("english"))

## You can also remove words that you do not want
#MyStopWords <- c("like", "very", "can", "I", "also", "lot")
#CleanNovelsCorpus <- tm_map(CleanNovelsCorpus, removeWords, MyStopWords)
## NOTE: If you have many words that you do not want to include
## you can create a file/list
## MyList <- unlist(read.table("PATH TO YOUR STOPWORD FILE", stringsAsFactors=FALSE)
## MyStopWords <- c(MyList)

##Make everything lowercase
#CleanNovelsCorpus <- tm_map(CleanNovelsCorpus, content_transformer(tolower))
## Next, we can apply lemmitization
## In other words, we can combine variations on words such as
## sing, sings, singing, singer, etc.
## I will not do this - but it is an option
#CleanNovelsCorpus <- tm_map(CleanNovelsCorpus, stemDocument)
#inspect(CleanNovelsCorpus)

## Let's see where we are so far...
## This will be large - so I am commenting it out.
## It is a good idea to inspect it once. 
## WHen you do- you will see many \n that you may
## have to deal with...
#inspect(CleanNovelsCorpus)

## You can use this view/information to add Stopwords and then re-run.
## In other words, I see from inspection that the word "can" is all over
## the place. But it does not mean anything. So I added it to my MyStopWords

## Next, I will write all cleaned docs  - the entire cleaned and prepped corpus
## to a file - in case I want to use it for something else.

## This will be commented out unless it is needed....
#(Novelsdataframe <- data.frame(text=sapply(CleanNovelsCorpus, identity), 
 #                         stringsAsFactors=F))
#write.csv(Novelsdataframe, "NovelsCorpusoutput.csv")

## Note: There are several other functions that also clean/prep text data
## stripWhitespace and
## myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 

###################################################################
######################### NEXT STEPS ##############################
###################################################################

## After you complete the above, you do not need to run those lines
## again, as they take a long time.

## The next steps are to tokenize the documents and vectorize
## each into record data such that the words are the variables
## (column names)

## Make the Term Document Matrix
## TMD stands for Term Document Matrix
#(Novels_TDM <- TermDocumentMatrix(CleanNovelsCorpus))
#inspect(Novels_TDM)
## In the DTM - doc term matrix, the words are the vars
#(Novels_DocTM <- DocumentTermMatrix(CleanNovelsCorpus))
#inspect(Novels_DocTM)
## FInd frequenct words...
#(findFreqTerms(Novels_DocTM, 1000))
## Find assocations with aselected conf
#(findAssocs(Novels_DocTM, 'world', 0.60))

## VISUALIZE
#Novels_DocTM_DF <- as.data.frame(inspect(Novels_DocTM))
#NovelsDFScale <- scale(Novels_DocTM_DF) # normalize
#d <- dist(NovelsDFScale,method="euclidean")
#d <- dist(NovelsDFScale,method="cosine")
#fit <- hclust(d, method="ward.D2")
#plot(fit)

