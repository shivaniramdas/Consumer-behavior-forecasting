
newdata.df=read.csv("datafor2013.csv")
newdata.df=newdata.df[,-c(1,2,3)]

newdata.df$date_time=as.Date(newdata.df$date_time)
week_of_travel<-format(newdata.df$date_time, "%V")
newdata.df<-cbind(newdata.df,week_of_travel)
newdata.df$week_of_travel=as.integer(newdata.df$week_of_travel)

newdata.df$srch_ci=as.Date(newdata.df$srch_ci)
week_of_booking<-format(newdata.df$srch_ci, "%V")
newdata.df<-cbind(newdata.df,week_of_booking)
newdata.df$week_of_booking=as.integer(newdata.df$week_of_booking)

max(newdata.df$hotel_market)
hotel_rank=(2117-newdata.df$hotel_market)+1
newdata.df<-cbind(newdata.df,hotel_rank)
newdata.df$hotel_rank=as.integer(newdata.df$hotel_rank)

newdata.df=newdata.df[,c(3,8,9,10,11,14,15,16,18,21,24,25,28,29,26,27,30)]

nd=newdata.df[,-c(2,11)]
table(nd$is_package)

library(sqldf)
x<-sqldf("select * from nd where is_package=1")
y<-sqldf("select * from nd where is_package=0 limit 164005")

newdata.df=rbind(x,y)

library(caret)
#newdata.df$is_package=ordered(newdata.df$is_package,level=c(1,2),label=c(0,1))
cols <- names(newdata.df)
cols=cols[-c(5,6,7,15)]
newdata.df[cols] <- lapply(newdata.df[cols], factor)

# Split the data into training and test set
library(caTools)
set.seed(1)
split <- sample.split(newdata.df$is_package, SplitRatio = 0.7)
train.df <- subset(newdata.df, split == TRUE)
test.df <- subset(newdata.df, split == FALSE)


test.df$is_package=factor(test.df$is_package)
train.df$is_package=factor(train.df$is_package)

#plot(train.df, main = "Error rate of random forest")

library(randomForest)
classifier=randomForest(x=train.df[-3] , y=train.df$is_package , ntree=50 )

# Predicting the Test set results
y_pred = predict(classifier, newdata = test.df[-3])

# Making the Confusion Matrix
confusionMatrix(y_pred,test.df[,3])

varImpPlot(classifier, pch = 20, main = "Importance of Variables")

newdata.df=read.csv("datafor2014.csv")
newdata.df=newdata.df[,-c(1,2,3)]

newdata.df$date_time=as.Date(newdata.df$date_time)
week_of_travel<-format(newdata.df$date_time, "%V")
newdata.df<-cbind(newdata.df,week_of_travel)
newdata.df$week_of_travel=as.integer(newdata.df$week_of_travel)

newdata.df$srch_ci=as.Date(newdata.df$srch_ci)
week_of_booking<-format(newdata.df$srch_ci, "%V")
newdata.df<-cbind(newdata.df,week_of_booking)
newdata.df$week_of_booking=as.integer(newdata.df$week_of_booking)

max(newdata.df$hotel_market)
hotel_rank=(2117-newdata.df$hotel_market)+1
newdata.df<-cbind(newdata.df,hotel_rank)
newdata.df$hotel_rank=as.integer(newdata.df$hotel_rank)

newdata.df=newdata.df[,c(3,8,9,10,11,14,15,16,18,21,24,25,28,29,26,27,30)]

nd=newdata.df[,-c(2,11)]
table(nd$is_package)

library(sqldf)
x<-sqldf("select * from nd where is_package=1")
y<-sqldf("select * from nd where is_package=0 limit 164005")

newdata.df=rbind(x,y)

library(caret)
#newdata.df$is_package=ordered(newdata.df$is_package,level=c(1,2),label=c(0,1))
cols <- names(newdata.df)
cols=cols[-c(5,6,7,15)]
newdata.df[cols] <- lapply(newdata.df[cols], factor)

# Split the data into training and test set
library(caTools)
set.seed(1)
split <- sample.split(newdata.df$is_package, SplitRatio = 0.7)
train.df <- subset(newdata.df, split == TRUE)
test.df <- subset(newdata.df, split == FALSE)

#test.df$is_package=as.integer(test.df$is_package)
#train.df$is_package=as.integer(train.df$is_package)

#train.df$is_package[train.df$is_package == 1] <- 0
#train.df$is_package[train.df$is_package == 2] <- 1

#test.df$is_package[test.df$is_package == 1] <- 0
#test.df$is_package[test.df$is_package == 2] <- 1

test.df$is_package=factor(test.df$is_package)
train.df$is_package=factor(train.df$is_package)

#plot(train.df, main = "Error rate of random forest")

library(randomForest)
classifier=randomForest(x=train.df[-3] , y=train.df$is_package , ntree=50 )

# Predicting the Test set results
y_pred = predict(classifier, newdata = test.df[-3])

# Making the Confusion Matrix
confusionMatrix(y_pred,test.df[,3])

varImpPlot(classifier, pch = 20, main = "Importance of Variables")
