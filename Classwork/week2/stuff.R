#Note: Depending on your R version and environment, you might encounter an error when running the aggregation 
# command "salesByRegion=aggregate(cbind('Mon','Tue','Wed','Thu','Fri','Sat','Sun'),by=list(Group.region=Region),FUN=sum)".
#
#If that happens, try this command instead 
# "salesByRegion=aggregate(cbind(Mon, Tue, Wed, Thu, Fri, Sat, Sun)~Region, data=sales, sum)".
#
#If you encounter programming error when repeating what's shown in course material during exercises, you can report the error and move on. I will address them later.