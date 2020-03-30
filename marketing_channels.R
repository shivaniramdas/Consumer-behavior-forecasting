library(sqldf)

data2013 <- read.csv("C:/Users/ssr180000/Documents/Expedia_Data")
table(data2013$channel)

##channeldata0

channeldata0<-sqldf("select * from data2013 where channel=0")
channeldata0<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata0 group by date_time")

channeldata0$date_time=format(as.Date(channeldata0$date_time),"%Y-%m-%d")
channeldata0$date_time=as.Date(channeldata0$date_time)

channeldata0<-sqldf("select distinct date_time,sum(count_booking) from channeldata0 group by date_time ")
channeldata0$`sum(count_booking)`=channeldata0$`sum(count_booking)`/100

#channeldata=channeldata[-1,]
d = ts(channeldata0[,2],frequency = 7)

install.packages("forecast")
#plot(d)
require(forecast)
ARIMAfit=auto.arima(log10(d),D=1)

pred = predict(ARIMAfit, n.ahead=60)

#plot(d,type='l')
plot(d,type='l',xlim=c(1,70),ylim=c(0,5),xlab = 'Weeks',ylab = 'Bookings x 100', main = '2013 Hotel Booking Data for Channel 0')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

##channeldata1

channeldata1<-sqldf("select * from data2013 where channel=1")
channeldata1<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata1 group by date_time")

channeldata1$date_time=format(as.Date(channeldata1$date_time),"%Y-%m-%d")
channeldata1$date_time=as.Date(channeldata1$date_time)

channeldata1<-sqldf("select distinct date_time,sum(count_booking) from channeldata1 group by date_time ")
channeldata1$`sum(count_booking)`=channeldata1$`sum(count_booking)`/100

#channeldata=channeldata[-1,]
d = ts(channeldata1[,2],frequency = 7)

require(forecast)
ARIMAfit=auto.arima(log10(d),D=1,approximation=T, trace=FALSE, allowdrift=F)

pred = predict(ARIMAfit, n.ahead=60)

#plot(d,type='l')
plot(d,type='l',xlim=c(1,70),ylim=c(0,5),xlab = 'Weeks',ylab = 'Bookings x 100', main = '2013 Hotel Booking Data for Channel 1')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

##channeldata2

channeldata2<-sqldf("select * from data2013 where channel=2")
channeldata2<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata2 group by date_time")
channeldata2$date_time=format(as.Date(channeldata2$date_time),"%Y-%m-%d")
channeldata2$date_time=as.Date(channeldata2$date_time)
channeldata2<-sqldf("select distinct date_time,sum(count_booking) from channeldata2 group by date_time ")
channeldata2$`sum(count_booking)`=channeldata2$`sum(count_booking)`/100

#channeldata=channeldata[-1,]
d = ts(channeldata2[,2],frequency = 7)

#plot(d)
require(forecast)
ARIMAfit=auto.arima(log10(d),D=1)

pred = predict(ARIMAfit, n.ahead=60)

#plot(d,type='l')
plot(d,type='l',xlim=c(1,70),ylim=c(0,5),xlab = 'Weeks',ylab = 'Bookings x 100', main = '2013 Hotel Booking Data for Channel 2')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

##channeldata3

channeldata3<-sqldf("select * from data2013 where channel=3")
channeldata3<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata3 group by date_time")
channeldata3$date_time=format(as.Date(channeldata3$date_time),"%Y-%m-%d")
channeldata3$date_time=as.Date(channeldata3$date_time)
channeldata3<-sqldf("select distinct date_time,sum(count_booking) from channeldata3 group by date_time ")
channeldata3$`sum(count_booking)`=channeldata3$`sum(count_booking)`/100

#channeldata=channeldata[-1,]
d = ts(channeldata3[,2],frequency = 7)

#plot(d)
require(forecast)
ARIMAfit=auto.arima(log10(d),D=1)

pred = predict(ARIMAfit, n.ahead=60)

#plot(d,type='l')
plot(d,type='l',xlim=c(1,70),ylim=c(0,2),xlab = 'Weeks',ylab = 'Bookings x 100', main = '2013 Hotel Booking Data for Channel 3')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

##channeldata9

channeldata9<-sqldf("select * from data2013 where channel=9")
channeldata9<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata9 group by date_time")
channeldata9$date_time=format(as.Date(channeldata9$date_time),"%Y-%m-%d")
channeldata9$date_time=as.Date(channeldata9$date_time)
channeldata9<-sqldf("select distinct date_time,sum(count_booking) from channeldata9 group by date_time ")
channeldata9$`sum(count_booking)`=channeldata9$`sum(count_booking)`/100

#channeldata=channeldata[-1,]
d = ts(channeldata9[,2],frequency = 7)

#plot(d)
require(forecast)
ARIMAfit=auto.arima(log10(d),D=1,approximation=T, trace=FALSE, allowdrift=F)

pred = predict(ARIMAfit, n.ahead=60)

#plot(d,type='l')
plot(d,type='l',xlim=c(1,70),ylim=c(1,25),xlab = 'Weeks',ylab = 'Bookings x 100', main = '2013 Hotel Booking Data for Channel 9')
lines(12^(pred$pred),col='blue')
lines(12^(pred$pred+2*pred$se),col='orange')
lines(12^(pred$pred-2*pred$se),col='orange')

data2014<- read.csv("~/Documents/UTD_Sem_1/BA_with_R/R_Project/datafor2014.csv")
table(data2014$channel)

##channeldata2014_0

channeldata2014_0<-sqldf("select * from data2014 where channel=0")
channeldata2014_0<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata2014_0 group by date_time")

channeldata2014_0$date_time=format(as.Date(channeldata2014_0$date_time),"%Y-%m-%d")
channeldata2014_0$date_time=as.Date(channeldata2014_0$date_time)

channeldata2014_0<-sqldf("select distinct date_time,sum(count_booking) from channeldata2014_0 group by date_time ")
channeldata2014_0$`sum(count_booking)`=channeldata2014_0$`sum(count_booking)`/100

d2014 = ts(channeldata2014_0[,2],frequency = 7)

install.packages("forecast")

require(forecast)
ARIMAfit0=auto.arima(log10(d2014),D=1,approximation=T, trace=FALSE, allowdrift=F)
pred = predict(ARIMAfit0, n.ahead=60)

#plot(d,type='l')
plot(d2014,type='l',xlim=c(1,70),ylim=c(0,15),xlab = 'Weeks',ylab = 'Bookings X 100', main = "2014 Hotel Booking Data for Channel 0")
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

##channeldata2014_1

channeldata2014_1<-sqldf("select * from data2014 where channel=1")
channeldata2014_1<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata2014_1 group by date_time")

channeldata2014_1$date_time=format(as.Date(channeldata2014_1$date_time),"%Y-%m-%d")
channeldata2014_1$date_time=as.Date(channeldata2014_1$date_time)

channeldata2014_1<-sqldf("select distinct date_time,sum(count_booking) from channeldata2014_1 group by date_time ")
channeldata2014_1$`sum(count_booking)`=channeldata2014_1$`sum(count_booking)`/100

#channeldata2014_=channeldata2014_[-1,]
d2014 = ts(channeldata2014_1[,2],frequency = 7)

#plot(d)
require(forecast)
ARIMAfit1=auto.arima(log10(d2014),D=1,approximation=T, trace=FALSE, allowdrift=F)
pred = predict(ARIMAfit1, n.ahead=60)

#plot(d,type='l')
plot(d2014,type='l',xlim=c(1,70),ylim=c(0,10),xlab = 'Weeks',ylab = 'Bookings X 100', main = "2014 Hotel Booking Data for Channel 1")
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

##channeldata2014_2

channeldata2014_2<-sqldf("select * from data2014 where channel=2")
channeldata2014_2<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata2014_2 group by date_time")
channeldata2014_2$date_time=format(as.Date(channeldata2014_2$date_time),"%Y-%m-%d")
channeldata2014_2$date_time=as.Date(channeldata2014_2$date_time)
channeldata2014_2<-sqldf("select distinct date_time,sum(count_booking) from channeldata2014_2 group by date_time ")
channeldata2014_2$`sum(count_booking)`=channeldata2014_2$`sum(count_booking)`/100

d2014 = ts(channeldata2014_2[,2],frequency = 7)

require(forecast)
ARIMAfit2=auto.arima(log10(d2014),D=1,approximation=T, trace=FALSE, allowdrift=F)

pred = predict(ARIMAfit2, n.ahead=60)

#plot(d,type='l')
plot(d2014,type='l',xlim=c(1,70),ylim=c(0,15),xlab = 'Weeks',ylab = 'Bookings X 100', main = "2014 Hotel Booking Data for Channel 2")
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

##channeldata2014_3

channeldata2014_3<-sqldf("select * from data2014 where channel=3")
channeldata2014_3<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata2014_3 group by date_time")
channeldata2014_3$date_time=format(as.Date(channeldata2014_3$date_time),"%Y-%m-%d")
channeldata2014_3$date_time=as.Date(channeldata2014_3$date_time)
channeldata2014_3<-sqldf("select distinct date_time,sum(count_booking) from channeldata2014_3 group by date_time ")
channeldata2014_3$`sum(count_booking)`=channeldata2014_3$`sum(count_booking)`/100

d2014 = ts(channeldata2014_3[,2],frequency = 7)

require(forecast)
ARIMAfit3=auto.arima(log10(d2014),D=1,approximation=T, trace=FALSE, allowdrift=F)

pred = predict(ARIMAfit3, n.ahead=60)

#plot(d,type='l')
plot(d2014,type='l',xlim=c(1,70),ylim=c(0,5),xlab = 'Weeks',ylab = 'Bookings X 100', main = "2014 Hotel Booking Data for Channel 3")
lines(10^(pred$pred),col='blue')
lines(6^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')

##channeldata2014_9

channeldata2014_9<-sqldf("select * from data2014 where channel=9")
channeldata2014_9<-sqldf("select distinct date_time,count(date_time) as count_booking from channeldata2014_9 group by date_time")
channeldata2014_9$date_time=format(as.Date(channeldata2014_9$date_time),"%Y-%m-%d")
channeldata2014_9$date_time=as.Date(channeldata2014_9$date_time)
channeldata2014_9<-sqldf("select distinct date_time,sum(count_booking) from channeldata2014_9 group by date_time ")
channeldata2014_9$`sum(count_booking)`=channeldata2014_9$`sum(count_booking)`/100

d2014 = ts(channeldata2014_9[,2],frequency = 7)

#plot(d)
require(forecast)
ARIMAfit2014=auto.arima(log10(d2014),D=1,approximation=T, trace=FALSE, allowdrift=F)

pred = predict(ARIMAfit2014, n.ahead=60)

#plot(d,type='l')
plot(d2014,type='l',xlim=c(1,70),ylim=c(10,50),xlab = 'Weeks',ylab = 'Bookings X 100', main = "2014 Hotel Booking Data for Channel 9")
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')
