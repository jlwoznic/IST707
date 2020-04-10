## First, we must read in the documents and convert them to 
## a format that we can evaluate.
library(network)
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
## ONCE: if needed:  install.packages("stringr")
library(stringr)
## ONCE: install.packages("textmineR")
library(textmineR)
library(igraph)
library(lsa)

library(tidyr)

Myfile="LieDataSMALLSAMPLE.csv"
setwd("C:\\Users\\profa\\Documents\\R\\RStudioFolder_1\\DrGExamples\\SYR\\IST707\\Week9")

MyData <- read.csv(Myfile)
print(str(MyData))
## First - keep columns 1 and 2
LIE=MyData$lie
print(LIE)
SENT=MyData$sentiment
print(SENT)
## Remove these from the DF
MyData<-MyData[-c(1,2)]
print(head(MyData))
## NOw - join/merge the columns in MyData together
MyData<-unite(MyData, c(1:1000))
#, na.string=c(""))
##print it...
(head(MyData, n=5))

NEW_DF_LIE <- data.frame(LIE,MyData)
print(NEW_DF_LIE)

dfCorpus = Corpus(VectorSource(MyData)) 
inspect(dfCorpus)

## Term Doc Matrix
(MyTermDM <- TermDocumentMatrix(dfCorpus))
inspect(MyTermDM)
