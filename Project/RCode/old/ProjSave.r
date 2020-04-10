# 
# Course: IST687
# Name: Joyce Woznica
# Project Code: Descriptive Statistics
# Due Date: MM/DD/2019
# Date Submitted:
#

# ----------------------------------------------------------------------------------------------------------------------
# DESCRIPTIVE STATISTICS
# from pastecs package
# only works on numeric variables in the set
# I did not find this useful
stat.desc(satSurvey, basic=F)

# overall description (lengthy)
describe(satSurvey)

test<-as.matrix(satSurvey)
summary(test)

# descriptive statistics from using stargazer package
stargazer(satSurvey, type="text")
# general Analysis
summary(satSurvey$Age)
summary(satSurvey$Satisfaction)
summary(satSurvey$DeptDelayMins)
summary(satSurvey$ArrDelayMins)
summary(satSurvey$Satisfaction)
mean(satSurvey$NumFlights)

# find the mean satisfaction by airline
airlineSatmeans<- ddply(satSurvey, .(Airline), summarize, SatValue = mean(Satisfaction))
newASM <-airlineSatmeans[order(-airlineSatmeans$SatValue),]
newASM
summary(airlineSatmeans)
range(airlineSatmeans$SatValue)

# from gmodels package - not right for this, but just playing around with statiistics
chisq.test(satSurvey$Gender,satSurvey$Satisfaction)

# ----------------------------------------------------------------------------------------------------------------------
# INITIAL VISUALIZATION
g <- ggplot(newASM) 
g <- g + geom_bar( aes(x = reorder(Airline, -SatValue), y=SatValue), stat="identity", fill="blue", alpha=0.7) 
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g <- g + scale_y_continuous(name="Satisfaction Rating", limits=c(0, 4), breaks=seq(0,4,.15))
g

# satisfaction mean by location (state) - Origination
OrigState.means<- ddply(satSurvey, .(OrigState), summarize, SatValue = mean(Satisfaction))
colnames(OrigState.means)<- c("State", "OrigStateSat")
Ordered.OS.SatMeans <- OrigState.means[order(-OrigState.means$OrigStateSat),]

# satisfaction mean by location (state) - Destination
DestState.means<- ddply(satSurvey, .(DestState), summarize, SatValue = mean(Satisfaction))
colnames(DestState.means)<- c("State", "DestStateSat")
Ordered.DS.SatMeans <- DestState.means[order(-DestState.means$DestStateSat),]
StateSatdf <- merge(OrigState.means, DestState.means, by = "State")

# Melt for plotting
meltedStatedf <- melt(StateSatdf, id.vars = "State")
g <- ggplot(meltedStatedf, aes(x=State, y=value, fill=variable))
g <- g + geom_bar(stat='identity', position='dodge')
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g

# Some initial plotting of the data to see what we have
# maybe try sampling data from our set (maybe 1000 values) and using that
testS<-sample(satSurvey$Satisfaction,1000,replace=FALSE)
testDD<-sample(satSurvey$DeptDelayMins,1000, replace=FALSE)
testdf<-data.frame(testS,testDD)
plot(testS,testDD)

g <- ggplot(testdf, aes(x=testDD, y=testS)) + geom_point()
g <- g + stat_smooth(method= "lm", col="red")
g

# Are men or women more satisfied?
g<- ggplot(satSurvey, aes(x=Gender, fill=factor(Satisfaction)))
g<- g + geom_bar(position="dodge")
g

# this is a very nice plot - keep it
# Are men or women more satisfied?
g<- ggplot(satSurvey, aes(x=Gender, fill=factor(Satisfaction)))
g<- g + geom_bar(position="dodge")
g

# boxplot male/female by satistfaction
# with boxplot
# do this again with buckets or something!
g<- ggplot(satSurvey, aes(group=Gender,x=Gender,y=Satisfaction)) 
g<- g + geom_boxplot(aes(fill=factor(Gender)))
g<- g + ggtitle("Satisfaction by Gender") + theme(plot.title=element_text(hjust=0.5))
g

# also did against month - just for my own benefit
g<- ggplot(satSurvey, aes(group=AirlineCode,x=AirlineCode,y=Satisfaction)) 
g<- g + geom_boxplot(aes(fill=factor(AirlineCode)))
g<- g + ggtitle("Satisfaction by Airline Code") + theme(plot.title=element_text(hjust=0.5))
g

hist(satSurvey$Satisfaction)
table(satSurvey$Gender)

# which airline has the best satisfaction
g<- ggplot(satSurvey, aes(x=AirlineCode, fill=factor(Satisfaction)))
g<- g + geom_bar(position="dodge")
g

# Does Travel Type affect Satisfaction?
g<- ggplot(satSurvey, aes(x=TravelType, fill=factor(Satisfaction)))
g<- g + geom_bar(position="dodge")
g

# Does Status affect Satisfaction?
g<- ggplot(satSurvey, aes(x=Status, fill=factor(Satisfaction)))
g<- g + geom_bar(position="dodge")
g

# Does Shopping and Eating in the Airport affect Satisfaction?
# Add scale for y axis
g <- ggplot(satSurvey, aes(x=EatDrink, y = Satisfaction, 
                           fill = ShopAmount))
g <- g + geom_bar(stat = "identity")
g

# maybe plot by age buckets?
# create bins/buckets for box plot
# really want to check by Satisfaction though
plotBuckets<-buildCutOffs(min(satSurvey$Age),max(satSurvey$Age),5)
SatSurvWB <- satSurvey
SatSurvWB$Bucket<-cut(satSurvey$Age,plotBuckets)
g<- ggplot(SatSurvWB)  
g<- g + geom_boxplot(aes(Bucket, Age, fill=factor(Bucket)))
g<- g + ggtitle("Age BoxPlot") + theme(plot.title=element_text(hjust=0.5))
g

# now do a line plot against Date for satisfaction
# Does time of year matter?
g <- ggplot(satSurvey, aes(x=FlightDate, y=Satisfaction))
g <- g + geom_line(size=1, color="navy")
g <- g + ylab("Satisfaction")
g <- g + ggtitle("Satisfaction by Date")+theme(plot.title=element_text(hjust=0.5))
g

# data needs to be cleaned for missing values
# Add scale for y axis
g <- ggplot(satSurvey, aes(x=Age, y=Satisfaction, group=Gender ))
g <- g + geom_bar(stat="identity", position="dodge", color="steelblue", 
                  aes(fill=factor(Gender)))
g

# Look at all the data via a scatter plot
#         Create a scatter chart (geom_point), 
#         x-axis is AirlineCode
#         y-axis is Satisfaction
#         dot size represents Status
#         color represents DeptDelayMins
g <- ggplot(satSurvey, aes(x=AirlineCode, y=Satisfaction))
g <- g + geom_point(aes(color=Status, size=DeptDelayMins))
g <- g + ggtitle("Satisfaction versus Airline Code, Status and Delay Minutes")
g <- g + theme(plot.title=element_text(hjust=0.5))
g

#         Create a scatter chart (geom_point), 
#         x-axis is AirlineCode
#         y-axis is Satisfaction
#         dot size represents ArrDelayMins
#         color represents DeptDelayMins
g <- ggplot(satSurvey, aes(x=AirlineCode, y=Satisfaction))
g <- g + geom_point(aes(color=ArrDelayMins, size=DeptDelayMins))
g <- g + ggtitle("Satisfaction versus Airline Code, Arrival and Delay Minutes")
g <- g + theme(plot.title=element_text(hjust=0.5))
g

# Histogram of satisfaction rating vs number of flights
g <- ggplot(satSurvey, aes(x=Satisfaction,y=NumFlights, fill=AirlineCode))
g <- g + geom_bar(stat="identity")
g

# Look at all the data via a scatter plot
#         Create a scatter chart (geom_point), 
#         x-axis is First Flight Year
#         y-axis is Satisfaction
#         dot size represents Number of Flights
#         color represents Flights on other airlines
g <- ggplot(satSurvey, aes(x=FFYear, y=Satisfaction))
g <- g + geom_point(aes(color=PercOther, size=NumFlights))
g <- g + ggtitle("Satisfaction versus First Flight Year, Percentage on Other Airlines, Number of Flights")
g <- g + theme(plot.title=element_text(hjust=1))
g <- g + scale_x_continuous(name="Year of First Flight", limits=c(min(satSurvey$FFYear), max(satSurvey$FFYear)))
g

# play around with bar graphs
g <- ggplot(satSurvey, aes(x=Satisfaction, y = Age, 
                           fill = AirlineCode))
g <- g + geom_bar(stat = "identity")
g

# ----------------------------------------------------------------------------------------------------------------------
# MAP VISUALIZATIONS

us <- map_data("state")

# need to create a new frame - elected to create two frames (one for Orignation Airport, other for Destination Airport)
# Satisfaction
# Origination - which is OrigCity, OrgState (but the state needs to be an abbrevation)
# Destation - which is DestCity, DestState (but the state needs to be an abbreviation)
# first remove all columns except the 3 we need
orig.mapDF <- data.frame(satSurvey$Satisfaction, satSurvey$OrigCity, satSurvey$OrigState)
dest.mapDF <- data.frame(satSurvey$Satisfaction, satSurvey$DestCity, satSurvey$DestState)
colnames(orig.mapDF) <- c("Satisfaction", "OrigCity", "OrigState")
colnames(dest.mapDF) <- c("Satisfaction", "DestCity", "DestState")
# trim the City to remove everything after the "/"
orig.mapDF$OrigCity <- trimSlash(orig.mapDF$OrigCity)
dest.mapDF$DestCity <- trimSlash(dest.mapDF$DestCity)
# get state abbreviations
orig.mapDF$OrigStateAbbr <- name2abbr(orig.mapDF$OrigState)
dest.mapDF$DestStateAbbr <- name2abbr(dest.mapDF$DestState)

# get rid of pacific territories and other non-states
orig.mapDF<-na.omit(orig.mapDF)
dest.mapDF<-na.omit(dest.mapDF)

# remove Hawaii and Alaska (for easier plotting)
orig.mapDF <- subset(orig.mapDF, orig.mapDF$OrigStateAbbr !="HI")
dest.mapDF <- subset(dest.mapDF, dest.mapDF$DestStateAbbr !="HI")
orig.mapDF <- subset(orig.mapDF, orig.mapDF$OrigStateAbbr !="AK")
dest.mapDF <- subset(dest.mapDF, dest.mapDF$DestStateAbbr !="AK")

# create combination Origination and Destination with city, ST in lower case
orig.mapDF$Origination <- paste(orig.mapDF$OrigCity,orig.mapDF$OrigStateAbbr, sep=', ')
dest.mapDF$Destination <- paste(dest.mapDF$DestCity,dest.mapDF$DestStateAbbr, sep=', ')
# convert to all lower case
orig.mapDF$Origination<-tolower(orig.mapDF$Origination)
dest.mapDF$Destination<-tolower(dest.mapDF$Destination)

# remove the unnecessary columns now
orig.mapDF <- data.frame(orig.mapDF$Origination, orig.mapDF$Satisfaction, orig.mapDF$OrigState)
colnames(orig.mapDF)<-c("Origination", "Satisfaction", "State")
dest.mapDF <- data.frame(dest.mapDF$Destination,dest.mapDF$Satisfaction, dest.mapDF$DestState)
colnames(dest.mapDF)<-c("Destination", "Satisfaction", "State")

# need to summarize by mean satisfaction
# satisfaction mean by location (state) - Destination
Dest.means <- ddply(dest.mapDF, .(Destination), summarize, SatValue = mean(Satisfaction))
Orig.means <- ddply(orig.mapDF, .(Origination), summarize, Satvalue = mean(Satisfaction))

# add Latitudes and longitudes
#first.omeans <-head(Orig.means)
#first.omeans$geoCode <- NewLatLon(first.omeans$Origination)
Orig.means$geoCode<-NewLatLon(Orig.means$Origination)
Dest.means$geoCode<-NewLatLon(Dest.means$Destination)

# plot by Origination Airport Location
oplot <- ggplot(Orig.means,aes(geoCode$lon,geoCode$lat))
oplot <- oplot + geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill='white')
oplot <- oplot + geom_point(aes(color = Satvalue),size=5)
oplot <- oplot +  xlim(-125,-65)+ylim(20,50)
oplot <- oplot + ylab("Latitude") + xlab("Longitude")
oplot <- oplot + ggtitle("Average Satisfaction by Origination Airport Location")
oplot

# plot by Destination Airport Location
dplot <- ggplot(Orig.means,aes(geoCode$lon,geoCode$lat))
dplot <- dplot + geom_polygon(data=us,aes(x=long,y=lat,group=group),color='gray',fill='white')
dplot <- dplot + geom_point(aes(color = Satvalue),size=5)
dplot <- dplot +  xlim(-125,-65)+ylim(20,50)
dplot <- dplot + ylab("Latitude") + xlab("Longitude")
dplot <- dplot + ggtitle("Average Satisfaction by Destimation Airport Location")
dplot

# random plotting
plot(satSurvey$FlightDate, satSurvey$Satisfaction)

# ----------------------------------------------------------------------------------------------------------------------
# REGRESSION ANALYSIS

# Just some test regressions on certain variables to see how they are correlated, if at all
testReg1 <- lm(satSurvey$Satisfaction ~ satSurvey$FlightMins)
summary(testReg1)

testReg2 <- lm(satSurvey$Satisfaction ~ satSurvey$Status)
s.model <- summary(testReg2)

testReg3 <- lm(satSurvey$Satisfaction ~ satSurvey$NumFlights)
s.model <- summary(testReg3)

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
summary(BestlmModel)

Final.Model <- lm(formula = satSurvey$Satisfaction ~ Status + Age + Gender + 
                    PriceSens + FFYear + PercOther + TravelType + ShopAmount + 
                    Class + SchDeptHour + Cancelled + ArrDelayGT5, 
                  data = as.data.frame(satSurvey))
summary(Final.Model)

# output the data in a more readable format
#stargazer(BestlmModel, allReg, type="text", title="Linear Regression Output", align=TRUE)
stargazer(Final.Model, type="text", title="Linear Regression Output", align=TRUE)
summary.Model<- summary(Final.Model)

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
trainData.small <-sample(trainData,15000,replace=FALSE)
testData.small <-sample(testData, 5000, replace=FALSE)

ksvmOutputM <- ksvm(Satisfaction~.,data=trainData.small,kernel="rbfdot",kpar="automatic",C=10,cross=3,prob.model=TRUE)
#ksvmOutputTA <- ksvm(Satisfaction~. data)
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
svmOutput <- svm(Satisfaction~.,data=trainData)
# create prediction
svmPred <- predict(svmOutput, testData, type="votes")
svmPred <- (data.frame(svmPred))
compTable <- data.frame(testData[,1],svmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMEsvm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMEsvm

# LM
# predict with the model we created for testData
lmPred <- predict(Final.Model, testData, type="response")
lmPred <- data.frame(lmPred)
compTable <- data.frame(testData[,1],lmPred[,1])
colnames(compTable) <- c("test", "Pred")
# this is the RMSE (how low)
RSMElm<-sqrt(mean((compTable$test-compTable$Pred)^2))
RSMElm

# Naive Bayes
nbOutput <- naiveBayes(Satisfaction~.,data=trainData)
# create prediction
nbPred <- predict(nbOutput, testData)
nbPred <- data.frame(nbPred)
compTable <- data.frame(testData[,1],nbPred[,1])
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



