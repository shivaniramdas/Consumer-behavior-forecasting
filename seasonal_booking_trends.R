library(sqldf)
library(forecast)

getwd()
setwd("C:/Users/ssr180000/Documents/Expedia_Data")

# Read the data
booking_data <- read.csv("booking.csv")
head(booking_data)
summary(booking_data)
str(booking_data)

# Find the most frequented hotel country
hotel_count <- as.data.frame(table(booking_data.df$hotel_country))
hotel_sort <- sqldf("SELECT * FROM hotel_count ORDER BY Freq DESC ")
head(hotel_sort, 3)



# Running time series analysis for the Top 1 country

# Query the data of the Top 1 country
hotel_count_top1 <- sqldf("SELECT * FROM booking_data WHERE hotel_country = 50")


# Find the number of bookings for the year 2013
hotel_count_top1$srch_ci <- format(as.Date(hotel_count_top1$srch_ci), "%Y-%m-%d")
booking_count_2013_top1 <- sqldf("SELECT DISTINCT srch_ci, count(srch_ci) as booking_count FROM hotel_count_top1 WHERE srch_ci LIKE '2013%' GROUP BY srch_ci")

max(booking_count_2013_top1$booking_count)
min(booking_count_2013_top1$booking_count)

# Fit the ARIMA Model
d_2013_top1 = ts(booking_count_2013_top1[,2], frequency = 7)
arimafit_2013_top1 = auto.arima(log10(d_2013_top1), D = 1)
pred_2013_top1 = predict(arimafit_2013_top1, n.ahead = 60)

# Visualization of Time series
plot(d_2013_top1,type = 'l', xlim = c(1,70), ylim = c(1,7000), xlab = "Weeks of 2013", ylab = "Number of bookings", main = "# of bookings for the Top 1 hotel country" )
axis(1, at = seq(0, 70, by = 5))
lines(10^(pred_2013_top1$pred), col = "Green")
lines(10^(pred_2013_top1$pred + 2*pred_2013_top1$se), col = "Orange")
lines(10^(pred_2013_top1$pred - 2*pred_2013_top1$se), col = "Orange")


#Find the most number of bookings for the year 2014
booking_count_2014_top1 <- sqldf("SELECT DISTINCT srch_ci, count(srch_ci) as booking_count FROM hotel_count_top1 WHERE srch_ci LIKE '2013%' GROUP BY srch_ci")

max(booking_count_2014_top1$booking_count)
min(booking_count_2014_top1$booking_count)

# Fit the ARIMA Model
d_2014_top1 = ts(booking_count_2014_top1[,2], frequency = 7)
arimafit_2014_top1 = auto.arima(log10(d_2014_top1), D = 1)
pred_2014_top1 = predict(arimafit_2014_top1, n.ahead = 60)

# Visualization of Time series
plot(d_2014_top1,type = 'l', xlim = c(1,70), ylim = c(1,7000), xlab = "Weeks of 2014", ylab = "Number of bookings", main = "# of bookings for the Top 1 hotel country" )
axis(1, at = seq(0, 70, by = 5))
lines(10^(pred_2014_top1$pred), col = "Green")
lines(10^(pred_2014_top1$pred + 2*pred_2014_top1$se), col = "Orange")
lines(10^(pred_2014_top1$pred - 2*pred_2014_top1$se), col = "Orange")




# Running time series analysis for the Top 2 country

# Query the data of the top2 country
hotel_count_top2 <- sqldf("SELECT * FROM booking_data WHERE hotel_country = 198")


# Find the number of bookings for the year 2013
hotel_count_top2$srch_ci <- format(as.Date(hotel_count_top2$srch_ci), "%Y-%m-%d")
booking_count_2013_top2 <- sqldf("SELECT DISTINCT srch_ci, count(srch_ci) as booking_count FROM hotel_count_top2 WHERE srch_ci LIKE '2013%' GROUP BY srch_ci")

max(booking_count_2013_top2$booking_count)
min(booking_count_2013_top2$booking_count)

# Fit the ARIMA Model
d_2013_top2 = ts(booking_count_2013_top2[,2], frequency = 7)
arimafit_2013_top2 = auto.arima(log10(d_2013_top2), D = 1)
pred_2013_top2= predict(arimafit_2013_top2, n.ahead = 60)

# Visualization of Time series
plot(d_2013_top2,type = 'l', xlim = c(1,70), ylim = c(1,800),xlab = "Weeks of 2013", ylab = "Number of bookings", main = "# of bookings for the Top 2 hotel country" )
axis(1, at = seq(0, 70, by = 5))
lines(10^(pred_2013_top2$pred), col = "Green")
lines(10^(pred_2013_top2$pred + 2*pred_2013_top2$se), col = "Orange")
lines(10^(pred_2013_top2$pred - 2*pred_2013_top2$se), col = "Orange")


# Find the number of bookings for the year 2014
booking_count_2014_top2 <- sqldf("SELECT DISTINCT srch_ci, count(srch_ci) as booking_count FROM hotel_count_top2 WHERE srch_ci LIKE '2014%' GROUP BY srch_ci")

max(booking_count_2014_top2$booking_count)
min(booking_count_2014_top2$booking_count)

# Fit the ARIMA Model
d_2014_top2 = ts(booking_count_2014_top2[,2], frequency = 7)
arimafit_2014_top2 = auto.arima(log10(d_2014_top2), D = 1)
pred_2014_top2 = predict(arimafit_2014_top2, n.ahead = 60)

# Visualization of Time series
plot(d_2014_top2,type = 'l', xlim = c(1,70), ylim = c(1,1200),xlab = "Weeks of 2014", ylab = "Number of bookings", main = "# of bookings for the Top 2 hotel country" )
axis(1, at = seq(0, 70, by = 5))
lines(10^(pred_2014_top2$pred), col = "Green")
lines(10^(pred_2014_top2$pred + 2*pred_2014_top2$se), col = "Orange")
lines(10^(pred_2014_top2$pred - 2*pred_2014_top2$se), col = "Orange")



# Running time series analysis for the Top 3 country

# Query the data of the top3 country
hotel_count_top3 <- sqldf("SELECT * FROM booking_data WHERE hotel_country = 70")


# Find the number of bookings for the year 2013
hotel_count_top3$srch_ci <- format(as.Date(hotel_count_top3$srch_ci), "%Y-%m-%d")
booking_count_2013_top3 <- sqldf("SELECT DISTINCT srch_ci, count(srch_ci) as booking_count FROM hotel_count_top3 WHERE srch_ci LIKE '2013%' GROUP BY srch_ci")

max(booking_count_2013_top3$booking_count)
min(booking_count_2013_top3$booking_count)

# Fit the ARIMA Model
d_2013_top3 = ts(booking_count_2013_top3[,2], frequency = 7)
arimafit_2013_top3 = auto.arima(log10(d_2013_top3), D = 1)
pred_2013_top3= predict(arimafit_2013_top3, n.ahead = 60)

# Visualization of Time series
plot(d_2013_top3,type = 'l', xlim = c(1,70), ylim = c(1,300), xlab = "Weeks of 2013", ylab = "Number of bookings", main = "# of bookings for the Top 3 hotel country" )
axis(1, at = seq(0, 70, by = 5))
lines(10^(pred_2013_top3$pred), col = "Green")
lines(10^(pred_2013_top3$pred + 2*pred_2013_top3$se), col = "Orange")
lines(10^(pred_2013_top3$pred - 2*pred_2013_top3$se), col = "Orange")



# Find the number of bookings for the year 2014
booking_count_2014_top3 <- sqldf("SELECT DISTINCT srch_ci, count(srch_ci) as booking_count FROM hotel_count_top3 WHERE srch_ci LIKE '2014%' GROUP BY srch_ci")

max(booking_count_2014_top3$booking_count)
min(booking_count_2014_top3$booking_count)

# Fit the ARIMA Model
d_2014_top3 = ts(booking_count_2014_top3[,2], frequency = 7)
arimafit_2014_top3 = auto.arima(log10(d_2014_top3), D = 1)
pred_2014_top3 = predict(arimafit_2014_top3, n.ahead = 60)

# Visualization of Time series
plot(d_2014_top3,type = 'l', xlim = c(1,70), ylim = c(1,500),xlab = "Weeks of 2014", ylab = "Number of bookings", main = "# of bookings for the Top 3 hotel country" )
axis(1, at = seq(0, 70, by = 5))
lines(11^(pred_2014_top3$pred), col = "Green")
lines(11^(pred_2014_top3$pred + 2*pred_2014_top3$se), col = "Orange")
lines(11^(pred_2014_top3$pred - 2*pred_2014_top3$se), col = "Orange")
