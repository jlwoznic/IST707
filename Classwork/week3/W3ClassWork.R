library(arules)
library(arulesViz)
library(datasets)

Groceries <- read.csv("C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week3/groceries.csv", header=FALSE)
data(Groceries)

itemFrequencyPlot(Groceries,topN=20,type="absolute")
# Get the rules
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# Show the top 5 rules, but only 2 digits
options(digits=2)
inspect(rules[1:5])

summary(rules)

rules<-sort(rules, by="confidence", decreasing=TRUE)
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
rules.pruned <- rules[!redundant]
rules<-rules.pruned

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
               appearance = list(default="lhs",rhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:5])

rules<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])

library(arulesViz)
plot(rules,method="graph",interactive=TRUE,shading=NA)

# retail dataset
retail <- read.transactions("C:/Users/Joyce/Desktop/Syracuse/IST707/Classwork/week3/retail.csv", format="basket", sep=",")
rules <- apriori(retail, parameter = list(supp = 0.001, conf = 0.8,maxlen=3))
rules<-sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])


