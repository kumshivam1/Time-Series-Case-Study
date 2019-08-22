#**************** Retail Giant Case Study ***********************************#
#Loading essential libraries
library(graphics)
library(forecast)
library(tseries)
library(zoo)
#Loading required files
global_store_data<-read.csv("Global Superstore.csv",stringsAsFactors = FALSE)

#Structure of data
str(global_store_data)
View(global_store_data)

#Checking for Duplicates
length(unique(global_store_data$Row.ID))==nrow(global_store_data)
#Returns True Hence No Duplicate

#Checking for NAs
sapply(global_store_data, function(x) sum(is.na(x)))
#Postal code has 41296 values of NAs,No other attributes contain NAs
#All 41296 values correspond to Postal codes outside US

#Our columns of interest in data 
#is 1.Order.Date 2.Segment 3.Market 4.Sales 5.Quantity and 6.Profit

#Changing the format of Date
global_store_data$Order.Date<-as.Date(global_store_data$Order.Date,"%d-%m-%Y")

# Converting the format of order date into month and year

global_store_data$order_monthyear<-as.yearmon(global_store_data$Order.Date,"%b%Y")

unique(global_store_data$Segment)
#3 major categories of products
#1.Consumer
#2.Corporate
#3.Home Office

unique(global_store_data$Market)

#7 different market segements
#1.US
#2.APAC
#3.EU
#4.Africa
#5.EMEA
#6.LATAM
#7.Canada

unique(global_store_data[,c("Segment","Market")])

# Total 21 buckets of data
#**********Subsetting the Data into 21 buckets***************************#

data_consumer_US<-global_store_data[which(global_store_data$Segment=="Consumer"& global_store_data$Market== "US"),]

data_consumer_APAC<-global_store_data[which(global_store_data$Segment=="Consumer"& global_store_data$Market== "APAC"),]

data_consumer_EU<-global_store_data[which(global_store_data$Segment=="Consumer"& global_store_data$Market== "EU"),]

data_consumer_Africa<-global_store_data[which(global_store_data$Segment=="Consumer"& global_store_data$Market== "Africa"),]

data_consumer_EMEA<-global_store_data[which(global_store_data$Segment=="Consumer"& global_store_data$Market== "EMEA"),]

data_consumer_LATAM<-global_store_data[which(global_store_data$Segment=="Consumer"& global_store_data$Market== "LATAM"),]

data_consumer_Canada<-global_store_data[which(global_store_data$Segment=="Consumer"& global_store_data$Market== "Canada"),]

data_Corporate_US<-global_store_data[which(global_store_data$Segment=="Corporate"& global_store_data$Market== "US"),]

data_Corporate_APAC<-global_store_data[which(global_store_data$Segment=="Corporate"& global_store_data$Market== "APAC"),]

data_Corporate_EU<-global_store_data[which(global_store_data$Segment=="Corporate"& global_store_data$Market== "EU"),]

data_Corporate_Africa<-global_store_data[which(global_store_data$Segment=="Corporate"& global_store_data$Market== "Africa"),]

data_Corporate_EMEA<-global_store_data[which(global_store_data$Segment=="Corporate"& global_store_data$Market== "EMEA"),]

data_Corporate_LATAM<-global_store_data[which(global_store_data$Segment=="Corporate"& global_store_data$Market== "LATAM"),]

data_Corporate_Canada<-global_store_data[which(global_store_data$Segment=="Corporate"& global_store_data$Market== "Canada"),]

data_Homeoffice_US<-global_store_data[which(global_store_data$Segment=="Home Office"& global_store_data$Market== "US"),]

data_Homeoffice_APAC<-global_store_data[which(global_store_data$Segment=="Home Office"& global_store_data$Market== "APAC"),]

data_Homeoffice_EU<-global_store_data[which(global_store_data$Segment=="Home Office"& global_store_data$Market== "EU"),]

data_Homeoffice_Africa<-global_store_data[which(global_store_data$Segment=="Home Office"& global_store_data$Market== "Africa"),]

data_Homeoffice_EMEA<-global_store_data[which(global_store_data$Segment=="Home Office"& global_store_data$Market== "EMEA"),]

data_Homeoffice_LATAM<-global_store_data[which(global_store_data$Segment=="Home Office"& global_store_data$Market== "LATAM"),]

data_Homeoffice_Canada<-global_store_data[which(global_store_data$Segment=="Home Office"& global_store_data$Market== "Canada"),]

#***************************************************************************************#

#Finding the two most profitable(and consistent) segment

#Deriving month from order date and storing to global store data
global_store_data$Month<-months(global_store_data$Order.Date)

#Calculating the total profit for each month and each market and segment

profit_bucket<-aggregate(global_store_data$Profit,by=list(global_store_data$Segment,global_store_data$Market,global_store_data$Month),FUN=sum)
colnames(profit_bucket)<-c("Category","Segment","Month","Monthly_Profit")

#Calculating the coefficient of variation of monthly basis

mean_monthly<-aggregate(profit_bucket$Monthly_Profit,by=list(profit_bucket$Category,profit_bucket$Segment),FUN=mean)
colnames(mean_monthly)<-c("Category","Segment","Monthly_mean")

sd_monthly<-aggregate(profit_bucket$Monthly_Profit,by=list(profit_bucket$Category,profit_bucket$Segment),FUN=sd)
colnames(sd_monthly)<-c("Category","Segment","Monthly_sd")

coefficient_variation<-merge(mean_monthly,sd_monthly,by=c("Category","Segment"))

coefficient_variation$CV<-coefficient_variation$Monthly_sd/coefficient_variation$Monthly_mean

coefficient_variation<-coefficient_variation[order(coefficient_variation$CV),]
#write.csv(coefficient_variation,"cv.csv")

# Calculating the Total Profit of each bucket
total_profit_bucket<-aggregate(global_store_data$Profit,by=list(global_store_data$Segment,global_store_data$Market),FUN=sum)
colnames(total_profit_bucket)<-c("Category","Segment","Profit")
total_profit_bucket<-total_profit_bucket[order(-total_profit_bucket$Profit),]

#write.csv(total_profit_bucket,"tp.csv")

# The 2 bucket with maximum and consistent profit are

# 1.Consumer APAC
# 2.Consumer EU
#Both of them have the least coefficient of variation and maximum profit

#****************************************************************************#
#Consumer APAC Time series Analysis
#****************************************************************************#
con_apac_sales_TS<-aggregate(Sales~order_monthyear,data_consumer_APAC,sum)
con_apac_sales_TS<-con_apac_sales_TS[order(con_apac_sales_TS$order_monthyear),]

con_apac_demand_TS<-aggregate(Quantity~order_monthyear,data_consumer_APAC,sum)
con_apac_demand_TS<-con_apac_demand_TS[order(con_apac_demand_TS$order_monthyear),]
#*****************************************************************************#
#Consumer APAC -Sales TS
#*****************************************************************************#
#Replacing months by numbers
nrow(con_apac_sales_TS)
con_apac_sales_TS$month<-1:48
con_apac_sales_TS<-subset(con_apac_sales_TS,select = c("month","Sales"))
total_timeser <- ts(con_apac_sales_TS$Sales)
#Splitting into test and train data
indata <- con_apac_sales_TS[1:42,]
test<-con_apac_sales_TS[43:48,]

timeser <- ts(indata$Sales)
plot(timeser)
#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Data containing only trend
#Modelling the data

lmfit <- lm(Sales ~ Month, data=smootheddf)

global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)
plot(resi)
adf.test(resi,alternative = "stationary")
#p value<0.05 hence stationary
kpss.test(resi)
#p value>0.05 hence stationary

#Model prediction and Evaluation
#evaluate the model using MAPE

#First, let's make a prediction for the last 6 months
timevals_out <- test$month

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,test[,2])[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black",xlab="Month from Jan2011 to Dec 2014",ylab="consumer APAC Sales Time Series")
lines(class_dec_pred, col = "red")

#ARIMA Modelling

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Model prediction and Evaluation
#evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,test[,2])[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(total_timeser, col = "black",xlab="Month from Jan2011 to Dec 2014",ylab="consumer APAC Sales Time Series")
lines(auto_arima_pred, col = "red")

#*****************************************************************************#
#Consumer APAC -Demand TS
#*****************************************************************************#
#Replacing months by numbers
nrow(con_apac_demand_TS)
con_apac_demand_TS$month<-1:48
con_apac_demand_TS<-subset(con_apac_demand_TS,select = c("month","Quantity"))
total_timeser1 <- ts(con_apac_demand_TS$Quantity)
plot(total_timeser1)
#Splitting into test and train data
indata1 <- con_apac_demand_TS[1:42,]
test1<-con_apac_demand_TS[43:48,]

timeser1 <- ts(indata1$Quantity)
plot(timeser1)
#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser1, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser1)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata1$month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Data containing only trend
#Modelling the data

lmfit <- lm(Quantity ~ Month, data=smootheddf)
global_pred1 <- predict(lmfit, Month=timevals_in)
summary(global_pred1)
lines(timevals_in, global_pred1, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred1 <- timeser1-global_pred1
plot(local_pred1, col='red', type = "l")
acf(local_pred1)
acf(local_pred1, type="partial")
armafit1 <- auto.arima(local_pred1)

tsdiag(armafit1)
armafit1

#We'll check if the residual series is white noise

resi1 <- local_pred1-fitted(armafit1)
plot(resi1)
adf.test(resi1,alternative = "stationary")
#p value<0.05 hence stationary
kpss.test(resi1)
#p value>0.05 hence stationary

#Model prediction and Evaluation
#evaluate the model using MAPE

#First, let's make a prediction for the last 6 months
timevals_out <- test1$month

global_pred_out1 <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out1

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec1 <- accuracy(fcast,test1[,2])[5]
MAPE_class_dec1

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred1 <- c(ts(global_pred1),ts(global_pred_out1))
plot(total_timeser1, col = "black",xlab="Month from Jan2011 to Dec 2014",ylab="consumer APAC Demand Time Series")
lines(class_dec_pred1, col = "red")

#ARIMA Modelling

autoarima1 <- auto.arima(timeser1)
autoarima1
tsdiag(autoarima1)
plot(autoarima1$x, col="black")
lines(fitted(autoarima1), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima1 <- timeser1 - fitted(autoarima1)

adf.test(resi_auto_arima1,alternative = "stationary")
kpss.test(resi_auto_arima1)

#Model prediction and Evaluation
#evaluate the model using MAPE

fcast_auto_arima1 <- predict(autoarima1, n.ahead = 6)

MAPE_auto_arima1 <- accuracy(fcast_auto_arima1$pred,test1[,2])[5]
MAPE_auto_arima1

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred1 <- c(fitted(autoarima1),ts(fcast_auto_arima1$pred))
plot(total_timeser1, col = "black")
lines(auto_arima_pred1, col = "red")

#****************************************************************************#
#Consumer EU Time series Analysis
#****************************************************************************#
con_eu_sales_TS<-aggregate(Sales~order_monthyear,data_consumer_EU,sum)
con_eu_sales_TS<-con_eu_sales_TS[order(con_eu_sales_TS$order_monthyear),]

con_eu_demand_TS<-aggregate(Quantity~order_monthyear,data_consumer_EU,sum)
con_eu_demand_TS<-con_eu_demand_TS[order(con_eu_demand_TS$order_monthyear),]
#*****************************************************************************#
#Consumer EU -Sales TS
#*****************************************************************************#
#Replacing months by numbers
nrow(con_eu_sales_TS)
con_eu_sales_TS$month<-1:48
con_eu_sales_TS<-subset(con_eu_sales_TS,select = c("month","Sales"))
total_timeser2 <- ts(con_eu_sales_TS$Sales)
#Splitting into test and train data
indata2 <- con_eu_sales_TS[1:42,]
test2<-con_eu_sales_TS[43:48,]

timeser2 <- ts(indata2$Sales)
plot(timeser2)
#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser2, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser2)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata2$month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Data containing only trend
#Modelling the data

lmfit <- lm(Sales ~ Month, data=smootheddf)

global_pred2 <- predict(lmfit, Month=timevals_in)
summary(global_pred2)
lines(timevals_in, global_pred2, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred2 <- timeser2-global_pred2
plot(local_pred2, col='red', type = "l")
acf(local_pred2)
acf(local_pred2, type="partial")
armafit2 <- auto.arima(local_pred2)

tsdiag(armafit2)
armafit2

#We'll check if the residual series is white noise

resi2 <- local_pred-fitted(armafit2)
plot(resi2)
adf.test(resi2,alternative = "stationary")
#p value<0.05 hence stationary
kpss.test(resi2)
#p value>0.05 hence stationary
#only white noise

#Model Prediction and Evaluation

#evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- test2$month

global_pred_out2 <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out2

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec2 <- accuracy(fcast,test2[,2])[5]
MAPE_class_dec2

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred2 <- c(ts(global_pred2),ts(global_pred_out2))
plot(total_timeser2, col = "black",xlab="Month from Jan2011 to Dec 2014",ylab="consumer EU Sales Time Series")
lines(class_dec_pred2, col = "red")

#ARIMA Modelling

autoarima2 <- auto.arima(timeser)
autoarima2
tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima2), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- timeser2 - fitted(autoarima2)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Model Prediction and Evaluation

#evaluate the model using MAPE
fcast_auto_arima2 <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima2 <- accuracy(fcast_auto_arima2$pred,test2[,2])[5]
MAPE_auto_arima2

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred2 <- c(fitted(autoarima2),ts(fcast_auto_arima2$pred))
plot(total_timeser2, col = "black",xlab="Month from Jan2011 to Dec 2014",ylab="consumer EU Sales Time Series")
lines(auto_arima_pred2,col = "red")

#*****************************************************************************#
#Consumer EU -Demand TS
#*****************************************************************************#
#Replacing months by numbers
nrow(con_eu_demand_TS)
con_eu_demand_TS$month<-1:48
con_eu_demand_TS<-subset(con_eu_demand_TS,select = c("month","Quantity"))
total_timeser3<- ts(con_apac_demand_TS$Quantity)
plot(total_timeser3)
#Splitting into test and train data
indata3 <- con_eu_demand_TS[1:42,]
test3<-con_eu_demand_TS[43:48,]

timeser3 <- ts(indata3$Quantity)
plot(timeser3)
#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- filter(timeser3, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser3)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata3$month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')

#Data containing only trend
#Modelling the data

lmfit <- lm(Quantity ~ Month, data=smootheddf)
global_pred3 <- predict(lmfit, Month=timevals_in)
summary(global_pred3)
lines(timevals_in, global_pred3, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred3 <- timeser3-global_pred3
plot(local_pred3, col='red', type = "l")
acf(local_pred3)
acf(local_pred3, type="partial")
armafit3<- auto.arima(local_pred3)

tsdiag(armafit3)
armafit3

#We'll check if the residual series is white noise

resi3 <- local_pred1-fitted(armafit3)
plot(resi3)
adf.test(resi3,alternative = "stationary")
#p value<0.05 hence stationary
kpss.test(resi3)
#p value>0.05 hence stationary
#Only white noise

#Model Prediction and Evaluation

#evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- test3$month

global_pred_out3 <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out3

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec3 <- accuracy(fcast,test3[,2])[5]
MAPE_class_dec3

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred3<- c(ts(global_pred3),ts(global_pred_out3))
plot(total_timeser3, col = "black",xlab="Month from Jan2011 to Dec 2014",ylab="consumer EU Demand Time Series")
lines(class_dec_pred1, col = "red")

#ARIMA Modelling

autoarima3 <- auto.arima(timeser3)
autoarima3
tsdiag(autoarima3)
plot(autoarima3$x, col="black")
lines(fitted(autoarima3), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima3 <- timeser3 - fitted(autoarima3)

adf.test(resi_auto_arima3,alternative = "stationary")
kpss.test(resi_auto_arima3)

#Model Prediction and Evaluation

#evaluate the model using MAPE

#Also, let's evaluate the model using MAPE
fcast_auto_arima3 <- predict(autoarima3, n.ahead = 6)

MAPE_auto_arima3 <- accuracy(fcast_auto_arima3$pred,test3[,2])[5]
MAPE_auto_arima3

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred3 <- c(fitted(autoarima3),ts(fcast_auto_arima3$pred))
plot(total_timeser3, col = "black")
lines(auto_arima_pred3, col = "red")


