# Map here
#  Load the zipcode package
install.packages("zipcode")
library(zipcode)
library(maps)

data(zipcode)
us <- map_data("state")



latlong <- NewLatLon("hartford, ct")

# test US Map
map.simple<-ggplot(dummyDF, aes(map_id=state))
map.simple<-map.simple + geom_map(map = us, fill="white", color="black")
map.simple<- map.simple + expand_limits(x=us$long, y=us$lat)
map.simple<-map.simple + coord_map() + ggtitle("basic USA map")
map.simple<-map.simple + geom_point(aes(x=latlong$lon,y=latlon$lat), color="darkred", size=3)
map.simple

cities<-c("Manhattan, NY", "Boston, MA", "Philadelphia, PA", "Tampa, FL", "Chicago, IL", 
          "Boise, ID", "San Francisco, CA", "Seattle, WA", "Houston, TX")
cities<-tolower(cities)
bus<-c(10,7,6,5,7,3,10,7,5)
weather<-c(5,3,6,7,3,6,10,7,2)
living<-c(7,6,6,7,5,4,6,8,2)
city.df<-data.frame(cities, bus, weather, living)
city.df$geoCode<-NewLatLon(cities)

map.simple <- map.simple + geom_point(data=city.df, aes(x=geoCode$lon,y=geoCode$lat))
map.simple <- map.simple + geom_point(data=city.df, aes(x=geocode$lon, y=geoCode$lat, size=bus, color=weather))
map.simple

# satisfaction mean by location (state) - Origination
OrigState.means<- ddply(satSurvey, .(OrigState), summarize, SatValue = mean(Satisfaction))
colnames(OrigState.means)<- c("State", "OrigStateSat")
Ordered.OS.SatMeans <- OrigState.means[order(-OrigState.means$OrigStateSat),]


statesDF <- data.frame(state.abb, state.name)
colnames(statesDF)<-c("abbr","name")
name2abbr <- function (st)
{
 statesDF$abb[which(statesDF$name == st)]
}

aaa<-name2abbr("Texas")


omit.nonstates <- function (stname)
{
  omit.na(mapDF)
}

subset(statesDF, name == "New York")[,"abbr"]

testName <- c("New York", "California", "Texas")
testDF <- data.frame(testName)
colnames(testDF)<- c("name")
testDF$abbr <- name2abbr(testName$name)
state2abbr("New York")
match("New York",statesDF$name, nomatch="NA")


statesDF$abb[which(statesDF$name=="New York")]
# need to create a new frame
# Satisfaction
# Origination - which is OrigCity, OrgState (but the state needs to be an abbrevation)
# Destation - which is DestCity, DestState (but the state needs to be an abbreviation)
# first remove all columns except the 3 we need
orig.mapDF <- data.frame(satSurvey$Satisfaction, satSurvey$OrigCity, satSurvey$OrigState)
dest.mapDF <- data.frame(satSurvey$Satisfaction, satSurvey$DestCity, satSurvey$DestState)
colnames(orig.mapDF) <- c("Satisfaction", "OrigCity", "OrigState")
colnames(dest.mapDF) <- c("Satisfaction", "DestCity", "DestState")
# get state abbreviations
orig.mapDF$OrigStateAbbr <- (orig.mapDF$OrigState)
dest.mapDF$DestStateAbbr <- state.abb(dest.mapDF$DestState)

# need to summarize by mean satisfaction???
# satisfaction mean by location (state) - Destination
DestState.means<- ddply(orig.mapDF, .(Origination), summarize, SatValue = mean(Satisfaction))

# get rid of pacific territories


# remove HI
mapDF <- subset(mapDF, mapDF$OrigStateAbbr !="HI")
mapDF <- subset(mapDF, mapDF$DestStateAbbr !="HI")

# create combination Origination and Destination
mapDF$Origination <- paste(mapDF$OrigCity,", ",mapDF$OrigStateAbbr)
mapDF$Destination <- paste(mapDF$DestCity,", ",mapDF$DestStateAbbr)
# convert to all lower case
mapDF<-tolower(mapDF)
mapDF$OriggeoCode<-NewLatLon(mapDF$Origination)
mapDF$DestgeoCode<-NewLatLon(mapDF$Destination)

# get list of states
# need to strip the State from City and make separate column
# then need to match city/state with zipcode, lat long with zipcodes#
# try mapping the satisfaction by city location (departure, then by arrival)
map.popColor <- ggplot(dfStates, aes(map_id = state))
map.popColor <- map.popColor +
  geom_map(map = us, aes(fill=july11pop))+
  scale_color_gradient(low="light blue",high="dark blue")
map.popColor <- map.popColor +
  expand_limits(x = us$long, y = us$lat)
map.popColor <- map.popColor +
  coord_map() + ggtitle("state population")
map.popColor

# create Training Data (2/3) and Test Data set (1/3) of set
# this will be used for SVM and other modeling
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
svmOutputL <- ksvm(Satisfaction~.,data=trainData,kernel="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
svmOutputH <- ksvm(Satisfaction~.,data=trainData,kernel="rbfdot",kpar="automatic",C=50,cross=3,prob.model=TRUE)

svmOutput <- svmOutputL

svmPred <- predict(svmOutput, testData, type="votes")
compTable <- data.frame(testData[,58],svmPred[1,])
table(compTable)
results <- table(compTable)

# may be wrong - need to check the data before determining row/col
totalCorrect <- results[1,1] + results[2,2]
totalinTest <- nrow(testData)
accuracy <- totalCorrect/totalinTest

# 34 cases not spam, but classified as Spam
# 898 cases not spam classified correctly
# 548 spam classified correction
# 54 non spam classified as spam
# implies: 34 + 54 = 88 error cases
# 88/1534 - length of TestData = total error rate = 0.05736636 or 5.7%, so accuracy rate is about 94.3%
# Cross Validation Error = 
# Accuracy Rate = 
