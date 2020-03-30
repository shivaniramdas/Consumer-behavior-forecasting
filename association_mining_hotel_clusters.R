

library(caret)
library(e1071)

d13<-read.csv("datafor2013.csv")
data2013=d13

data2013$date_time=as.Date(data2013$date_time)
week_of_travel<-format(data2013$date_time, "%V")
data2013<-cbind(data2013,week_of_travel)
data2013$week_of_travel=as.integer(data2013$week_of_travel)

data2013$srch_ci=as.Date(data2013$srch_ci)
week_of_booking<-format(data2013$srch_ci, "%V")
data2013<-cbind(data2013,week_of_booking)
data2013$week_of_booking=as.integer(data2013$week_of_booking)

max(data2013$hotel_market)
hotel_rank=(2117-data2013$hotel_market)+1
data2013<-cbind(data2013,hotel_rank)
data2013$hotel_rank=as.integer(data2013$hotel_rank)

data2013$month_of_travel=as.integer(data2013$month_of_travel)

#24
#23,27
#removed 27 hotel cluster
#6
data2013$srch_destination_type_id=factor(data2013$srch_destination_type_id)
#data2013$hotel_cluster=as.factor(data2013$hotel_cluster)
data2013$is_mobile=as.factor(data2013$is_mobile)
data2013$is_package=as.factor(data2013$is_package)
data2013$channel=as.factor(data2013$channel)
#data2013$site_name=factor(data2013$site_name)
data2013$hotel_continent=factor(data2013$hotel_continent)
data2013$posa_continent=factor(data2013$posa_continent)

data2013$srch_adults_cnt=factor(data2013$srch_adults_cnt)
data2013$srch_children_cnt=factor(data2013$srch_children_cnt)
data2013$srch_rm_cnt=factor(data2013$srch_rm_cnt)

data2013$month_booked=factor(data2013$month_booked)
data2013$month_of_travel=factor(data2013$month_of_travel)

data2013$week_of_booking=data2013$week_of_booking%%4+1
data2013$week_of_travel=data2013$week_of_travel%%4+1

data2013$week_of_booking=factor(data2013$week_of_booking)
data2013$week_of_travel=factor(data2013$week_of_travel)

data2013$hotel_cluster=factor(data2013$hotel_cluster)



#---------------------------Loading 2014 data-------------------------------------------------------------------------
data2014=read.csv("datafor2014.csv")
data2014$date_time=as.Date(data2014$date_time)
week_of_travel<-format(data2014$date_time, "%V")
data2014<-cbind(data2014,week_of_travel)
data2014$week_of_travel=as.integer(data2014$week_of_travel)

data2014$srch_ci=as.Date(data2014$srch_ci)
week_of_booking<-format(data2014$srch_ci, "%V")
data2014<-cbind(data2014,week_of_booking)
data2014$week_of_booking=as.integer(data2014$week_of_booking)

max(data2014$hotel_market)
hotel_rank=(2117-data2014$hotel_market)+1
data2014<-cbind(data2014,hotel_rank)
data2014$hotel_rank=as.integer(data2014$hotel_rank)

month_of_travel<-format(data2014$srch_ci, "%m")
data2014<-cbind(data2014,month_of_travel)
data2014$week_of_booking=as.integer(data2014$month_of_travel)

data2014$month_of_travel=as.integer(data2014$month_of_travel)

data2014$srch_destination_type_id=factor(data2014$srch_destination_type_id)

data2014$hotel_cluster=as.factor(data2014$hotel_cluster)

data2014$is_mobile=as.factor(data2014$is_mobile)
data2014$is_package=as.factor(data2014$is_package)
data2014$channel=as.factor(data2014$channel)

#------------------for apriori----------------------------------------------------------------
#24
#23,27
#removed 27 hotel cluster
#6

data2014$site_name=factor(data2014$site_name)
data2014$hotel_continent=factor(data2014$hotel_continent)
data2014$posa_continent=factor(data2014$posa_continent)


#---------------------------------Apriori 2013 for type of vacation and type of hotel-------------------------------------------------------------------
#--------------------for type of vacation--------------------------------------
library(sqldf)
apridata=sqldf("select srch_destination_type_id,user_id from data2013")
#install.packages("arules")
#install.packages("dplyr")
#install.packages("sqldf")
library(dplyr)
library(arules)

#distinct(apridata,srch_destination_type_id,keep_all=F)
apdata<-unique(apridata[,1:2])
apdata<-sqldf("select * from apdata order by user_id")
#write.csv(apdata,"apdata.csv")

ap<-read.transactions("apdata.csv",format = "single", sep = ",",cols=c(2:1))
ap

summary(ap)
itemFrequencyPlot(ap,topN=6)+title("Destination Type Frequency 2013")
10/length(ap)

rules=apriori(data=ap , parameter=list(support=0.004 , confidence= 0.2))

#Visualizing the results
inspect(sort(rules , by='lift')[1:20])


#------------------for hotel cluster-----------------------------------------------------------------
library(sqldf)
apridatahotel=sqldf("select hotel_cluster,user_id from data2013")

library(dplyr)
library(arules)
#install.packages("ggplot2")
library(ggplot2)

#distinct(apridata,srch_destination_type_id,keep_all=F)
apdatahotel<-unique(apridatahotel[,1:2])
apdatahotel<-sqldf("select * from apdatahotel order by user_id")
#write.csv(apdatahotel,"apdatahotel.csv")

aph<-read.transactions("apdatahotel.csv",format = "single", sep = ",",cols=c(2:1))
aph


summary(itemFrequency(aph))

itemFrequencyPlot(aph,topN=10)+title("Hotel Cluster Preference 2013")
10/length(aph)

rulesh=apriori(data=aph , parameter=list(support=0.001 , confidence= 0.2))

#Visualizing the results
inspect(sort(rulesh , by='lift')[1:20])


#-------------------------------------------------------------------------------------------------------------------



#---------------------------------------------------------------------------------
#---------------------------------Apriori 2014 for type of vacation and type of hotel-------------------------------------------------------------------
#--------------------for type of vacation--------------------------------------
library(sqldf)
apridata4=sqldf("select srch_destination_type_id,user_id from data2014")
#install.packages("arules")
#install.packages("dplyr")
#install.packages("sqldf")
library(dplyr)
library(arules)

#distinct(apridata,srch_destination_type_id,keep_all=F)
apdata4<-unique(apridata4[,1:2])
apdata4<-sqldf("select * from apdata4 order by user_id")

rm(xd)

#write.csv(apdata4,"apdata4.csv")

ap4<-read.transactions("apdata4.csv",format = "single", sep = ",",cols=c(1:2))
ap4

summary(itemFrequency(ap4))

itemFrequencyPlot(ap4,topN=8)+title("Destination Type Frequency 2014")

10/length(ap4)

rules4=apriori(data=ap4 , parameter=list(support=9.595944e-06 , confidence= 0.2))


#Visualizing the results
inspect(sort(rules4 , by='lift')[1:20])


#------------------for hotel cluster 2014-----------------------------------------------------------------
library(sqldf)
apridatahotel4=sqldf("select hotel_cluster,user_id from data2014")

library(dplyr)
library(arules)
#install.packages("ggplot2")
library(ggplot2)

#distinct(apridata,srch_destination_type_id,keep_all=F)
apdatahotel4<-unique(apridatahotel4[,1:2])
apdatahotel4<-sqldf("select * from apdatahotel4 order by user_id")
#write.csv(apdatahotel4,"apdatahotel4.csv")

aph4<-read.transactions("apdatahotel4.csv",format = "single", sep = ",",cols=c(1:2))
aph4


summary(itemFrequency(aph4))

itemFrequencyPlot(aph4,topN=10)+ title("Hotel Cluster Preference 2014")
10/length(aph4)


rulesh4=apriori(data=aph4 , parameter=list(support=0.001 , confidence= 0.4))

#Visualizing the results
inspect(sort(rulesh4 , by='lift')[1:20])

#-------------------------------------------------------------------------------------------------------------------
#---------------------------------------Time Series-----------------------------------------------
library(sqldf)
#install.packages("forecast")

data2013=data2013[,c(4,6,7)]
x<-sqldf("select distinct date_time,count(date_time) as count_booking from data2013 group by date_time")

library(sqldf)
data2014=data2014[,c(3,5,6)]
y<-sqldf("select distinct date_time,count(date_time) as count_booking from data2014 group by date_time")

timeserdata<-rbind(x,y)
timeserdata$date_time=format(as.Date(timeserdata$date_time),"%Y-%m-%d")
timeserdata$date_time=as.Date(timeserdata$date_time)

timeserdata<-sqldf("select distinct date_time,sum(count_booking) from timeserdata group by date_time ")
timeserdata$`sum(count_booking)`=timeserdata$`sum(count_booking)`/1000
timeserdata=timeserdata[-1,]
d = ts(timeserdata[,2],frequency = 7)

#plot(d)
require(forecast)
ARIMAfit=auto.arima(log10(d),D=1)
ARIMAfit=auto.arima(d,D=1)

#ARIMAfit = auto.arima(log10(d), approximation=FALSE,trace=FALSE)
#par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 6)

plot(d,type='l',xlim=c(1,60),ylim=c(1,5),xlab = '',ylab = '')
lines(7^(pred$pred),col='blue')
lines(7^(pred$pred+2*pred$se),col='orange')
lines(7^(pred$pred-2*pred$se),col='orange')
