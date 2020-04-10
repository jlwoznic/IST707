###
##
### Document Similarity Using Measures
##
## Gates
## ANother good resource:
## https://rstudio-pubs-static.s3.amazonaws.com/66739_c4422a1761bd4ee0b0bb8821d7780e12.html
## http://www.minerazzi.com/tutorials/cosine-similarity-tutorial.pdf
## Book: Text Mining in R
## https://www.tidytextmining.com/
######## Example 1 ----------------------
##
## Whenever you learn something new, always create a very small
## example that you can practice with. 

## I have created a small "Corpus" (collections of documents or books)
## They are called, Doc1, Doc2, ..., Doc5.
## The documents are in sentence format.

## The goal is to see how similar the documents are.

## First, we must read in the documents and convert them to 
## a format that we can evaluate.

##If you install from the source....
#Sys.setenv(NOAWT=TRUE)
## ONCE: install.packages("wordcloud")
library(wordcloud)
## ONCE: install.packages("tm")
library(tm)
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
##ONCE: install.packages('proxy')
library(proxy)

setwd("C:\\Users\\jerem\\Google Drive\\Online\\iCuse\\IST707\\Week4")
## Next, load in the documents (the corpus)
TheCorpus <- Corpus(DirSource("Corpus"))
##The following will show you that you read in 5 documents
(TheCorpus)

##Next, there are several steps needed to prepare the texts
## You will need to remove punctuation, make everything lowercase
## normalize, remove common and useless words like "and", "the", "or"
## Uselses words are called "Stop Words"
## Don't forget to remove numbers as well. 

## The function : getTransformations() will show all the functions
## that process the data - such as removeNumbers, removePunctuation, etc
## run getTransformations() to see this.
## Also note that tolower() will change all case to lowercase.

## The tm_map function allows you to perform the same 
## transformations on all of your texts at once
CleanCorpus <- tm_map(TheCorpus, removePunctuation)

## Remove all Stop Words
CleanCorpus <- tm_map(CleanCorpus, removeWords, stopwords("english"))

## You can also remove words that you do not want
MyStopWords <- c("like", "very", "can", "I", "also", "lot")
CleanCorpus <- tm_map(CleanCorpus, removeWords, MyStopWords)

## NOTE: If you have many words that you do not want to include
## you can create a file/list
## MyList <- unlist(read.table("PATH TO YOUR STOPWORD FILE", stringsAsFactors=FALSE)
## MyStopWords <- c(MyList)

##Make everything lowercase
CleanCorpus <- tm_map(CleanCorpus, content_transformer(tolower))

## Next, we can apply lemmitization
## In other words, we can combine variations on words such as
## sing, sings, singing, singer, etc.
## NOTE: This will NOT WORK for R version 3.5.x yet - so its
## just for FYI. This required package Snowball which does not yet
## run under the new version of R
#CleanCorpus <- tm_map(CleanCorpus, stemDocument)
#inspect(CleanCorpus)



## Let's see where we are so far...
(CleanCorpus)
## You can use this view/information to add Stopwords and then re-run.
## In other words, I see from inspection that the word "can" is all over
## the place. But it does not mean anything. So I added it to my MyStopWords

## Next, I will write all cleaned docs  - the entire cleaned and prepped corpus
## to a file - in case I want to use it for something else.

(Cdataframe <- data.frame(text=sapply(CleanCorpus, identity), 
                        stringsAsFactors=F))
write.csv(Cdataframe, "Corpusoutput.csv")

## Note: There are several other functions that also clean/prep text data
## stripWhitespace and
## myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) 

## ------------------------------------------------------------------
## Now, we are ready to move forward.....
##-------------------------------------------------------------------

## View corpus as a document matrix
## TMD stands for Term Document Matrix
(MyTDM <- TermDocumentMatrix(CleanCorpus))
inspect(MyTDM)


## By inspecting this matrix, I see that the words "also" and "lot" is there, but not useful
## I will add these to my MyStopWords and will re-run the above code....
##--------------NOTE
## ABOUT DocumentTermMatrix vs. TermDocumentMatrix - yes these are NOT the same :)
##TermDocument means that the terms are on the vertical axis and the documents are 
## along the horizontal axis. DocumentTerm is the reverse

## Before we normalize, we can look at the overall frequencies of words 
## This will find words that occur more than 3 times in the entire corpus
findFreqTerms(MyTDM, 1)
## Find assocations with aselected conf
findAssocs(MyTDM, 'coffee', 0.20)

## VISUALIZE
CleanDF <- as.data.frame(inspect(MyTDM))
(CleanDF)
CleanDFScale <- scale(CleanDF)
d <- dist(CleanDFScale,method="euclidean")
fit <- hclust(d, method="ward.D2")
plot(fit)
## NOw I have agood matrix that allows me to see all the key words of interest 
## and their frequency in each document
## HOWEVER - I still need to normalize!
## Even though this example is very small and all docs in this example are about the
## same size, this will not always be the case. If a document has 10,000 words, it
## will easily have a greater frequency of words than a doc with 1000 words.

(MyDTM <- DocumentTermMatrix(CleanCorpus))
inspect(MyDTM)

## NOrmalize the Term Doc Matrix from above and then visualize it again
## Warning!! It is easy to mix up the DTM and the TDM- be carefull

NormalizedTDM <- TermDocumentMatrix(CleanCorpus, control = list(weighting = weightTfIdf, stopwords = TRUE))
inspect(NormalizedTDM)

## Visualize normalized DTM
## The dendrogram:
## Terms higher in the plot appear more frequently within the corpus
## Terms grouped near to each other are more frequently found together
CleanDF_N <- as.data.frame(inspect(NormalizedTDM))
CleanDFScale_N <- scale(CleanDF_N )
d <- dist(CleanDFScale_N,method="euclidean")
fit <- hclust(d, method="ward.D2")
rect.hclust(fit, k = 4) # cut tree into 4 clusters 
plot(fit)

## Wordcloud
inspect(MyTDM)

m <- as.matrix(MyTDM)   ## You can use this or the next for m
(m)
#m <- as.matrix(CleanDF_N)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq*2, min.freq = 2,
          random.order = F)

## Use kmeans to cluster the documents

ClusterM <- t(m) # transpose the matrix to cluster documents 
(ClusterM)
#set.seed(100) # set a fixed random seed
k <- 3 # number of clusters
kmeansResult <- kmeans(ClusterM, k)
#round(kmeansResult$centers, digits = 3) # cluster centers

## See the clusters  - this shows the similar documents
## This does not always work well and can also depend on the
## starting centroids
(kmeansResult$cluster)

library("factoextra")
fviz_cluster(kmeansResult, data = ClusterM,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
#, color=TRUE, shade=TRUE,
         #labels=2, lines=0)

## Let's try to find similarity using cosine similarity
## Let's look at our matrix

m2<-NormalizedTDM
inspect(NormalizedTDM)
str(m2)
cosine_dist_mat <- 
  1 - crossprod_simple_triplet_matrix(m2)/
  (sqrt(col_sums(m2^2) %*% t(col_sums(m2^2))))

(cosine_dist_mat)

## What these results mean:
## (1) Notice that the cosine sim between Doc 1 and Doc 1 is 0. 
## This is because there is no distance between them. 
## The Cosine similarity between Doc 1 and Doc 4 is 1, this means they are
## maximally far apart.
## Some people will use 1 - cosine sim to get the nearness - in that case - 1 is nearest.

(cos_sim_matrix <-(1 - cosine_dist_mat))
## Now, larger means closer or more similar
## Docs 1 and 2 are similar
## Docs 3 and 4 are similar
## If we force 2 clusters, Doc 5 is most similar to Doc 3.

## Notice that this works MUCH BETTER than k means!

## This is a small example of cosine similarity so you can see how it works
## I will comment it out...
######  m3 <- matrix(1:9, nrow = 3, ncol = 3)
######   (m3)
######   ((crossprod(m3))/(  sqrt(col_sums(m3^2) %*% t(col_sums(m3^2))   )))
 
#heatmap https://www.rdocumentation.org/packages/stats/versions/3.5.0/topics/heatmap
heatmap(cos_sim_matrix) 



