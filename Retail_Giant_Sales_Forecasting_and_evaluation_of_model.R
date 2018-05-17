#**********************************************************************************************************
# Retail-Giant Sales Forecasting and evaluation of model                                                  *
# This is a group case study                                                                              *
#                                                                                                         *
# Members:                                                                                                *
# 1.Anargha Biswas                                                                                        *     
# 2.Prasasthy .K.B                                                                                        *
# 3.Sekhar Sahu                                                                                           *
# 4.Surabhi Varshney                                                                                      *
#                                                                                                         *
# Brief on subject:                                                                                       * 
#---------------------------------------------------------------------------------------------------------*
#"Global Mart" is an online store super giant having worldwide operations. It takes orders and delivers   *
#across the globe and deals with all the major product categories - consumer, corporate & home office.    * 
#The store caters to 7 different market segments and in 3 major categories. Requirement is to forecast at *
#this granular level, so need to subset the data into 21 (7*3) buckets before analysing it.               *   
#Out of which, we need to find out 2 most profitable (and consistent) segment and forecast the sales and  *
#demand for these segments.                                                                               *
#                                                                                                         *
# Input files used:                                                                                       *
# Global Superstore.csv - Dataset conatins transaction level datawhere each row represents a particular   *
# order made on the online store. There are 24 attributes related to each such transaction                *
#                                                                                                         *
#Goal:                                                                                                    *
#Determine the top 2 most profitable and stable Market & segment combo and forecast the sales and the     *
#demand for the next 6 months of the same.                                                                *
#---------------------------------------------------------------------------------------------------------*

#Libraries used

library(ggplot2)
library(forecast)
library(graphics)

######################################################################################################
#Reading input file
#####################################################################################################

global_mart_main_data <- read.csv("Global Superstore.csv") #Initial data set
global_mart_data      <- global_mart_main_data  #Moving to another data set for further manipulations

#Understanding Dimensions of train data
dim(global_mart_data)

#Structure of the mnist_train_dataset
str(global_mart_data)

#Understanding the summary
summary(global_mart_data)

#Could find that there are 3 different segments 
#Consumer   :26518 
#Corporate  :15429 
#Home Office: 9343
#Could find there are 7 different Market places
#Africa: 4587    
#APAC  :11002    
#Canada:  384    
#EMEA  : 5029     
#EU    :10000     
#LATAM :10294     
#US    : 9994  

#checking for NA value
sapply(global_mart_data, function(x) sum(is.na(x)))

#Here Postal.Code for 41296 records are unknown.
#After cross verifying in the csv file, it is found that the postal codes for the Market other than US
#are all NA values 

#checking for blank values "" in any of the fields of dataset
sapply(global_mart_data, function(x) length(which(x == ""))) 

#No blank values found in any of the fields

#Converting Postal.Code into factor
global_mart_data$Postal.Code <- as.factor(global_mart_data$Postal.Code)

#Converting date fields 
global_mart_data$Order.Date <- as.Date(global_mart_data$Order.Date,"%d-%m-%Y")
global_mart_data$Ship.Date  <- as.Date(global_mart_data$Ship.Date,"%d-%m-%Y")

#Extracting the month and year of order date
global_mart_data$order_date_month <- format(as.Date(global_mart_data$Order.Date), "%m")
global_mart_data$order_date_year  <- format(as.Date(global_mart_data$Order.Date), "%Y")

#Converting to factor
global_mart_data$order_date_year <- as.factor(global_mart_data$order_date_year)
global_mart_data$order_date_year <- as.factor(global_mart_data$order_date_year)
summary(global_mart_data$order_date_year)

#As per the summary the data set contains records of year 2011, 2012, 2013 and 2014
#Training set contains data of january 2011 to june 2014 and test set contains july-december 2014

#The dataset contains additional informations like Customer name, customer informations, product id
#address and so on. But these fields are not required for detecting the 2 top most profitable and 
#stable combination of Market&segment, also not useful for forecasting sales and quantity of these
#Market&segment combinations. So these fields are not included or used for further analysis in code.

#####################################################################################################
#Understanding the over all trend of data
#####################################################################################################

#Considering overall profit with respect to Market and segment
total_profit <- aggregate(Profit ~ Market + Segment , data = global_mart_data, sum)

names(total_profit) [3] <- "overall_profit"

Plot_Profit <- ggplot(total_profit,aes(x = Market, y = overall_profit,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Over all Profit")
Plot_Profit

#As per the above overall profit plot APAC consumer abd EU consumer are the top two profitable divisions
#Canada is the lowest in terms of total profit

#Checking total sales with respect to Market and segment
total_sales  <- aggregate(Sales ~ Market + Segment , data = global_mart_data, sum)

Plot_sales   <- ggplot(total_sales,aes(x = Market, y = Sales,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Total Sales")
Plot_sales

#Considering the total sales happening for each combo of Market & Segment, APAC consumer abd EU consumer
#have higher sales amount. Canada is the lowest sales happening market.

#Checking for percentage of profit with respecct to sales

percent_profit <- (total_profit$overall_profit/total_sales$Sales) * 100


Plot_pt_profit <- ggplot(total_sales,aes(x = total_sales$Market, y = percent_profit,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Profit in percentage") + scale_y_continuous(labels = function(x){ paste0(x, "%") }) +
  geom_text(data= total_sales, aes(total_sales$Market, percent_profit,
                                   label = paste(round(percent_profit,2), "%",sep = "" )),size=3, hjust = .4, vjust = 1.5)
Plot_pt_profit

#As per the percentage plot Canada is topping the list in all the segment 
#Canada Consumer  - 27.09%
#Canada Corporate - 26.08%

#Number of sales happening over the market and segment
count_sales  <- aggregate(Sales ~ Market + Segment , data = global_mart_data, length)

Plot_cnt_sales <- ggplot(count_sales,aes(x = Market, y = Sales,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Count of Sales")
Plot_cnt_sales

#As per the plot APAC and LATAM are highest in sales happening, also EU and US is showing 
#sufficient amount sales. Canada is the lowest.

agg_quantity  <- aggregate(Quantity ~ Market + Segment , data = global_mart_data, sum)

Plot_quantity <- ggplot(agg_quantity,aes(x = Market, y = Quantity,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Quantity")

Plot_quantity

#The above quantity plot is showing similar trends of count of sales plot which is expected as they
#are correlated to each other

#Conclusion: Though sales happening in canada is low, the percentage profit of the market is
#high compared to other markets. APAC Consumer and EU Consumer is performing high in overall
#Profit 

#Understanding yearly and monthly wise trend of data

agg_profit_month <- aggregate(Profit ~ Market + Segment + order_date_year + order_date_month,
                              data = global_mart_data, sum)


names(agg_profit_month) [5] <- "overallmonthly_profit"

Plot_yearly_Profit <- ggplot(agg_profit_month,aes(x = Market, y = overallmonthly_profit,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Over all Profit in year 2011, 2012, 2013 & 2014") + 
  facet_grid( order_date_year ~ .)

Plot_yearly_Profit

#Considering the yearly profit the trend shows APAC Consumer is most profitable. EU consumer and US consumer
#is also profitable. Canada and Africa is lowest in the case profitablity

#Checking month wise total sales with respect to Market and segment
total_monthlysales  <- aggregate(Sales ~ Market + Segment + order_date_year + order_date_month,
                                 data = global_mart_data, sum)

names(total_monthlysales) [5] <- "overallmonthly_sales"

Plot_yearly_sales <- ggplot(total_monthlysales,aes(x = Market, y = overallmonthly_sales,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Over all sales in year 2011, 2012, 2013 & 2014") + 
  facet_grid( order_date_year ~ .)
Plot_yearly_sales

#The above plot shows sales is high for APAC consumer and EU consumer for year 2012, 2013 & 2014

#Checking for percentage of profit with respect to sales

percent_profit_month <- (agg_profit_month$overallmonthly_profit/total_monthlysales$overallmonthly_sales) * 100


Plot_yearly_pt_profit <- ggplot(total_monthlysales,aes(x = total_monthlysales$Market, y = percent_profit_month,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  facet_grid( order_date_year ~ .)+
  ggtitle("percentage profit in year 2011, 2012, 2013 & 2014")  
Plot_yearly_pt_profit

#As per the percentage plot Canada is topping the list in all the segment except for 2014 consumer
#segment where US market tops the list.

#Number of sales happening over the market and segment
count_sales_month  <- aggregate(Sales ~ Market + Segment + order_date_year + order_date_month, data = global_mart_data, length)

names(count_sales_month) [5] <- "countof_monthly_sales"
Plot_yearly_cnt_sales <- ggplot(count_sales_month,aes(x = Market, y = countof_monthly_sales,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Count of Sales in year 2011, 2012, 2013 & 2014")+
  facet_grid( order_date_year ~ .)
Plot_yearly_cnt_sales

#As per the plot US and LATAM are highest in sales happening, also EU and APAC is showing 
#sufficient amount sales. Canada is the lowest.

agg_quantity_mnth  <- aggregate(Quantity ~ Market + Segment + order_date_year + order_date_month , data = global_mart_data, sum)

names(agg_quantity_mnth) [5] <- "monthly_quantity"

Plot_yearly_quantity <- ggplot(agg_quantity_mnth,aes(x = Market, y = monthly_quantity,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("Profit") +
  ggtitle("Quantity in year 2011, 2012, 2013, 2014")+
  facet_grid( order_date_year ~ .)

Plot_yearly_quantity  

#The above quantity plot is showing similar trends of count of sales plot which is expected as they
#are correlated to each other

#Further drilling down month wise in each year for understanding 

#year 2011
Plot_year2011_Profit <- ggplot(agg_profit_month,aes(x = Market, y = overallmonthly_profit,fill = Segment)) +
  geom_bar(data = subset(agg_profit_month,order_date_year == "2011"),
           stat="identity",position="dodge") + xlab("Market")+ ylab("Profit") +
  ggtitle("Over all Profit in year 2011") + 
  facet_wrap( ~ order_date_month )

Plot_year2011_Profit

#APAC consumer, EU consumer, US consumer show stability

#year 2012
Plot_year2012_Profit <- ggplot(agg_profit_month,aes(x = Market, y = overallmonthly_profit,fill = Segment)) +
  geom_bar(data = subset(agg_profit_month,order_date_year == "2012"),
           stat="identity",position="dodge") + xlab("Market")+ ylab("Profit") +
  ggtitle("Over all Profit in year 2012") + 
  facet_wrap( ~ order_date_month )

Plot_year2012_Profit

#APAC consumer and EU consumer show stability

#year 2013
Plot_year2013_Profit <- ggplot(agg_profit_month,aes(x = Market, y = overallmonthly_profit,fill = Segment)) +
  geom_bar(data = subset(agg_profit_month,order_date_year == "2013"),
           stat="identity",position="dodge") + xlab("Market")+ ylab("Profit") +
  ggtitle("Over all Profit in year 2013") + 
  facet_wrap( ~ order_date_month )

Plot_year2013_Profit

#APAC consumer and EU consumer show more stability

#year 2014
Plot_year2014_Profit <- ggplot(agg_profit_month,aes(x = Market, y = overallmonthly_profit,fill = Segment)) +
  geom_bar(data = subset(agg_profit_month,order_date_year == "2014"),
           stat="identity",position="dodge") + xlab("Market")+ ylab("Profit") +
  ggtitle("Over all Profit in year 2014") + 
  facet_wrap( ~ order_date_month )

Plot_year2014_Profit

#APAC consumer and EU consumer show more stability

#APAC consumer and EU consumer shows more stability than other Market segments over years 2011, 2012
#2013 & 2014

######################################################################################################
#Understanding coefficient of variation of given data:
#COV = SD/mean is often expressed in percentage, where SD is the standard deviation and mean is the
#average. COV shows the extent of variability in relation to the mean of the population 
######################################################################################################

Average_profit <- aggregate(overallmonthly_profit ~ Market + Segment , data = agg_profit_month, mean)
names(Average_profit) [3] <- "mean_profit"

Stddev_profit <- aggregate(overallmonthly_profit ~ Market + Segment , data = agg_profit_month, sd)
names(Stddev_profit) [3] <- "SD_profit"

COV_dataset <- cbind(Average_profit, SD_profit = Stddev_profit$SD_profit)
COV_dataset$COV_profit <- (COV_dataset$SD_profit/COV_dataset$mean_profit)*100

Plot_COV <- ggplot(COV_dataset,aes(x = Market, y = COV_profit,fill = Segment)) +
  geom_bar(stat="identity",position="dodge")+ xlab("Market")+ ylab("COV") +
  ggtitle("Coefficient of variation over different market and segment ")

Plot_COV

#####################################################################################################
#Before choosing the appropriate ones we need to know the logic behind the choice, which is as
#follows: (Details obtained from https://www.investopedia.com/ & https://en.wikipedia.org/)
#The higher the coefficient of variation, the greater the level of dispersion around the mean which 
#shows unstability. In the investing world, the coefficient of variation allows you to determine how
#much volatility, or risk, you are assuming in comparison to the amount of return you can expect from 
#your investment.
#In simple language, the lower the ratio of standard deviation to mean return, the better your 
#risk-return tradeoff.
#So as per the above understanding it is better to choose the market & segment whose COV is the
#lowest among all. Top 5 would be as follows
#-EU consumer
#-APAC consumer
#-LATAM consumer
#-APAC corporate
#-EU corporate
#####################################################################################################

#####################################################################################################
#EU Consumer
#####################################################################################################
#Prepaing and Subsetting data for EU

Agg_master_data <- cbind( total_monthlysales, monthly_quantity = agg_quantity_mnth$monthly_quantity)

EU_Consumer_data <- subset(Agg_master_data,(Agg_master_data$Market == "EU") &
                             (Agg_master_data$Segment == "Consumer"))

EU_Consumer_data <- EU_Consumer_data[order(EU_Consumer_data$order_date_year,
                                           EU_Consumer_data$order_date_month), ]

EU_Consumer_data$MonthNum <- c(1:nrow(EU_Consumer_data))

#Extracting timeseries dataset for sales and quantity separately

EU_consumer_TS_sales <- EU_Consumer_data[, c(5,7)]

EU_consumer_TS_Qnty   <- EU_Consumer_data[, c(6,7)] 

###################################################################################################
#EU consumer sales
#The dataset contains data from years 2011, 2012, 2013 & 2014 (in total 12*4 = 48 months)
#Extracting only 42 months data for model building and last 6 months data(43-48) for test
#purpose
###################################################################################################

EU_consumer_TSSales_train <- EU_consumer_TS_sales[1:42, ]
EU_consumer_TSSales_test  <- EU_consumer_TS_sales[43:48, ]

#Timeseries plotting 

EU_consumer_TSsales_total_timeser <- ts(EU_consumer_TS_sales$overallmonthly_sales)
EUconsumer_TSSales_timeser        <- ts(EU_consumer_TSSales_train$overallmonthly_sales)

#Using decompose() to understanding the seasonal, trend and irregular components using moving averages.

EUconsumer_TSSales_timeser_decom_in <- ts(EU_consumer_TSSales_train$overallmonthly_sales, frequency = 12)                                           
EUconsumer_TSSales_timeser_decomp  <- decompose(EUconsumer_TSSales_timeser_decom_in, type = "multiplicative")

plot(EUconsumer_TSSales_timeser_decomp)

# Observation of decomposed wave:
# 1. Trend is a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

plot(EUconsumer_TSSales_timeser, main ="Actual, smoothened VS predicated")


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries_EU_consumer_sales <- stats::filter(EUconsumer_TSSales_timeser, 
                                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                                  method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_EU_consumer_sales[w+2] - smoothedseries_EU_consumer_sales[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_EU_consumer_sales[i] <- smoothedseries_EU_consumer_sales[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EUconsumer_TSSales_timeser)
diff <- smoothedseries_EU_consumer_sales[n-w] - smoothedseries_EU_consumer_sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_EU_consumer_sales[i] <- smoothedseries_EU_consumer_sales[i-1] + diff
}

#Plot the smoothed time series

timevals_in_EU_consumer_sales <- EU_consumer_TSSales_train$MonthNum
lines(smoothedseries_EU_consumer_sales, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_EU_consumer_sales <- as.data.frame(cbind(timevals_in_EU_consumer_sales, 
                                                    as.vector(smoothedseries_EU_consumer_sales)))
colnames(smootheddf_EU_consumer_sales) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_consumer_sales <- lm(Sales ~ sin(0.6*Month) * poly(Month,2) + cos(0.6*Month) * poly(Month,3)
                              + cos(0.9*Month) * poly(Month,2) + Month
                              ,data=smootheddf_EU_consumer_sales)

summary(lmfit_EU_consumer_sales)

accuracy(lmfit_EU_consumer_sales)

global_pred_EU_consumer_sales <- predict(lmfit_EU_consumer_sales, Month=timevals_in_EU_consumer_sales)

summary(global_pred_EU_consumer_sales)
lines(timevals_in_EU_consumer_sales, global_pred_EU_consumer_sales, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_consumer_sales <- EUconsumer_TSSales_timeser-global_pred_EU_consumer_sales
plot(local_pred_EU_consumer_sales, col='red', type = "l")
acf(local_pred_EU_consumer_sales)
acf(local_pred_EU_consumer_sales, type="partial")
armafit_EU_consumer_sales <- auto.arima(local_pred_EU_consumer_sales)

tsdiag(armafit_EU_consumer_sales)

#We'll check if the residual series is white noise

resi_EU_consumer_sales <- local_pred_EU_consumer_sales-fitted(armafit_EU_consumer_sales)

adf.test(resi_EU_consumer_sales,alternative = "stationary")
kpss.test(resi_EU_consumer_sales)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_EU_consumer_sales    <- EU_consumer_TSSales_test$MonthNum

global_pred_out_EU_consumer_sales <- predict(lmfit_EU_consumer_sales,
                                             data.frame(Month =timevals_out_EU_consumer_sales))

fcast_EU_consumer_sales <- global_pred_out_EU_consumer_sales

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_EU_consumer_sales <- accuracy(fcast_EU_consumer_sales,
                                             EU_consumer_TSSales_test[,1])[5]
MAPE_class_dec_EU_consumer_sales

#MAPE value -[1] 30.75798 

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_EU_consumer_sales <- c(ts(global_pred_EU_consumer_sales),
                                      ts(global_pred_out_EU_consumer_sales))
plot(EU_consumer_TSsales_total_timeser, col = "black")
lines(class_dec_pred_EU_consumer_sales, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_EU_consumer_sales <- auto.arima(EUconsumer_TSSales_timeser)
autoarima_EU_consumer_sales

tsdiag(autoarima_EU_consumer_sales)
plot(autoarima_EU_consumer_sales$x, col="black")
lines(fitted(autoarima_EU_consumer_sales), col="red")
autoarima_EU_consumer_sales
#Again, let's check if the residual series is white noise

resi_auto_arima_EU_consumer_sales <- EUconsumer_TSSales_timeser - 
  fitted(autoarima_EU_consumer_sales)

adf.test(resi_auto_arima_EU_consumer_sales,alternative = "stationary")
kpss.test(resi_auto_arima_EU_consumer_sales)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_consumer_sales <- predict(autoarima_EU_consumer_sales, n.ahead = 6)


MAPE_auto_arima_EU_consumer_sales <- accuracy(fcast_auto_arima_EU_consumer_sales$pred,
                                              EU_consumer_TSSales_test$overallmonthly_sales)[5]
MAPE_auto_arima_EU_consumer_sales

#MAPE obtained - [1] 28.9226

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_EU_consumer_sales <- c(fitted(autoarima_EU_consumer_sales),
                                       ts(fcast_auto_arima_EU_consumer_sales$pred))
plot(EU_consumer_TSsales_total_timeser, col = "black")
lines(auto_arima_pred_EU_consumer_sales, col = "red")

######################################################################################################
# Forecasting EU Consumer Sales
#######################################################################################################
#forecasting using classical deocmposition for next 6 months
time_out_EU_consumer_sales_fcast    <- c(49:54)

global_predout_EU_consumer_sales_fcast <- predict(lmfit_EU_consumer_sales,
                                                  data.frame(Month =time_out_EU_consumer_sales_fcast))

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
Input_EU_consumer_sales_fcast <- append(global_pred_EU_consumer_sales,global_pred_out_EU_consumer_sales)
names(Input_EU_consumer_sales_fcast) <- c(1:48)

class_dec_pred_EU_consumer_sales_fcast <- c(ts(Input_EU_consumer_sales_fcast),
                                            ts(abs(global_predout_EU_consumer_sales_fcast)))

names(class_dec_pred_EU_consumer_sales_fcast) <- c(1:54)

#Plotting actual vs forecasted
plot(EU_consumer_TSsales_total_timeser, col = "black", main ="Classical decomposition forecasting")
lines(class_dec_pred_EU_consumer_sales_fcast, col = "red")

#plotting forecasted alone
plot(class_dec_pred_EU_consumer_sales_fcast, col = "red",type = "l",
     main ="Classical decomposition forecasting")

#forecasting using auto arima for next 6 months
fcast_auto_arima_EU_consumer_sales_fcast <- predict(autoarima_EU_consumer_sales, n.ahead = 12)

auto_arima_pred_EU_consumer_sales_fcast <- c(fitted(autoarima_EU_consumer_sales),
                                             ts(fcast_auto_arima_EU_consumer_sales_fcast$pred))

plot(EU_consumer_TSsales_total_timeser, col = "black", main ="Auto ARIMA forecasting")
lines(auto_arima_pred_EU_consumer_sales_fcast, col = "red")

#Forecasting using Holtwinters
EU_consumer_salesfcast <- HoltWinters(EUconsumer_TSSales_timeser, beta=F, gamma=F)
EU_consumer_salesfcast
plot(EU_consumer_salesfcast)

EU_consumer_salesfcast_6months <- forecast(EU_consumer_salesfcast,h=6)
EU_consumer_salesfcast_6months
plot(EU_consumer_salesfcast_6months)

# Comparing with Test data

accuracy(EU_consumer_salesfcast_6months,EU_consumer_TSSales_test$overallmonthly_sales) 

#Forcasting for next six months sales

forecast(EUconsumer_TSSales_timeser,h=6)
forecast(autoarima_EU_consumer_sales,h=6)
###################################################################################################
#EU consumer Qnty
#The dataset contains data from years 2011, 2012, 2013 & 2014 (in total 12*4 = 48 months)
#Extracting only 42 months data for model building and last 6 months data(43-48) for test
#purpose
###################################################################################################

EU_consumer_TSQnty_train <- EU_consumer_TS_Qnty[1:42, ]
EU_consumer_TSQnty_test  <- EU_consumer_TS_Qnty[43:48, ]

#Timeseries plotting 

EU_consumer_TSQnty_total_timeser <- ts(EU_consumer_TS_Qnty$monthly_quantity)
EUconsumer_TSQnty_timeser        <- ts(EU_consumer_TSQnty_train$monthly_quantity)

#Using decompose() to understanding the seasonal, trend and irregular components using moving averages.

EUconsumer_TSQnty_timeser_decom_in <- ts(EU_consumer_TSQnty_train$monthly_quantity, frequency = 12)                                           
EUconsumer_TSQnty_timeser_decomp  <- decompose(EUconsumer_TSQnty_timeser_decom_in)

plot(EUconsumer_TSQnty_timeser_decomp)

# Observation of decomposed wave:
# 1. Trend is a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

plot(EUconsumer_TSQnty_timeser, main ="Actual, smoothened VS predicated")

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries_EU_consumer_Qnty <- stats::filter(EUconsumer_TSQnty_timeser, 
                                                 filter=rep(1/(2*w+1),(2*w+1)), 
                                                 method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_EU_consumer_Qnty[w+2] - smoothedseries_EU_consumer_Qnty[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_EU_consumer_Qnty[i] <- smoothedseries_EU_consumer_Qnty[i+1] - diff
}

#Smoothing right end of the time series

n <- length(EUconsumer_TSQnty_timeser)
diff <- smoothedseries_EU_consumer_Qnty[n-w] - smoothedseries_EU_consumer_Qnty[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_EU_consumer_Qnty[i] <- smoothedseries_EU_consumer_Qnty[i-1] + diff
}

#Plot the smoothed time series

timevals_in_EU_consumer_Qnty <- EU_consumer_TSQnty_train$MonthNum
lines(smoothedseries_EU_consumer_Qnty, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_EU_consumer_Qnty <- as.data.frame(cbind(timevals_in_EU_consumer_Qnty, 
                                                   as.vector(smoothedseries_EU_consumer_Qnty)))
colnames(smootheddf_EU_consumer_Qnty) <- c('Month', 'Qnty')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_consumer_Qnty <- lm(Qnty ~ sin(0.6*Month) * poly(Month,2) + cos(0.6*Month) * poly(Month,2)
                                       +Month
                                       ,data=smootheddf_EU_consumer_Qnty)
                             
summary(lmfit_EU_consumer_Qnty)

accuracy(lmfit_EU_consumer_Qnty)

global_pred_EU_consumer_Qnty <- predict(lmfit_EU_consumer_Qnty, Month=timevals_in_EU_consumer_Qnty)

summary(global_pred_EU_consumer_Qnty)
lines(timevals_in_EU_consumer_Qnty, global_pred_EU_consumer_Qnty, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_consumer_Qnty <- EUconsumer_TSQnty_timeser-global_pred_EU_consumer_Qnty
plot(local_pred_EU_consumer_Qnty, col='red', type = "l")
acf(local_pred_EU_consumer_Qnty)
acf(local_pred_EU_consumer_Qnty, type="partial")
armafit_EU_consumer_Qnty <- auto.arima(local_pred_EU_consumer_Qnty)

tsdiag(armafit_EU_consumer_Qnty)

#We'll check if the residual series is white noise

resi_EU_consumer_Qnty <- local_pred_EU_consumer_Qnty-fitted(armafit_EU_consumer_Qnty)

adf.test(resi_EU_consumer_Qnty,alternative = "stationary")
kpss.test(resi_EU_consumer_Qnty)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_EU_consumer_Qnty    <- EU_consumer_TSQnty_test$MonthNum

global_pred_out_EU_consumer_Qnty <- predict(lmfit_EU_consumer_Qnty,
                                            data.frame(Month =timevals_out_EU_consumer_Qnty))

fcast_EU_consumer_Qnty <- global_pred_out_EU_consumer_Qnty

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_EU_consumer_Qnty <- accuracy(fcast_EU_consumer_Qnty,
                                            EU_consumer_TSQnty_test[,1])[5]
MAPE_class_dec_EU_consumer_Qnty

#MAPE obtained -[1] 31.99442

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_EU_consumer_Qnty <- c(ts(global_pred_EU_consumer_Qnty),
                                     ts(global_pred_out_EU_consumer_Qnty))
plot(EU_consumer_TSQnty_total_timeser, col = "black")
lines(class_dec_pred_EU_consumer_Qnty, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_EU_consumer_Qnty <- auto.arima(EUconsumer_TSQnty_timeser)
autoarima_EU_consumer_Qnty

tsdiag(autoarima_EU_consumer_Qnty)
plot(autoarima_EU_consumer_Qnty$x, col="black")
lines(fitted(autoarima_EU_consumer_Qnty), col="red")
autoarima_EU_consumer_Qnty
#Again, let's check if the residual series is white noise

resi_auto_arima_EU_consumer_Qnty <- EUconsumer_TSQnty_timeser - 
  fitted(autoarima_EU_consumer_Qnty)

adf.test(resi_auto_arima_EU_consumer_Qnty,alternative = "stationary")
kpss.test(resi_auto_arima_EU_consumer_Qnty)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_consumer_Qnty <- predict(autoarima_EU_consumer_Qnty, n.ahead = 6)


MAPE_auto_arima_EU_consumer_Qnty <- accuracy(fcast_auto_arima_EU_consumer_Qnty$pred,
                                             EU_consumer_TSQnty_test$monthly_quantity)[5]
MAPE_auto_arima_EU_consumer_Qnty

#MAPE obtained - [1] 30.13319

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_EU_consumer_Qnty <- c(fitted(autoarima_EU_consumer_Qnty),
                                      ts(fcast_auto_arima_EU_consumer_Qnty$pred))
plot(EU_consumer_TSQnty_total_timeser, col = "black")
lines(auto_arima_pred_EU_consumer_Qnty, col = "red")

######################################################################################################
# Forecasting EU Consumer Qnty
#######################################################################################################
#forecasting using classical deocmposition for next 6 months
time_out_EU_consumer_Qnty_fcast    <- c(49:54)

global_predout_EU_consumer_Qnty_fcast <- predict(lmfit_EU_consumer_Qnty,
                                                 data.frame(Month =time_out_EU_consumer_Qnty_fcast))

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
Input_EU_consumer_Qnty_fcast <- append(global_pred_EU_consumer_Qnty,global_pred_out_EU_consumer_Qnty)
names(Input_EU_consumer_Qnty_fcast) <- c(1:48)

class_dec_pred_EU_consumer_Qnty_fcast <- c(ts(Input_EU_consumer_Qnty_fcast),
                                           ts(abs(global_predout_EU_consumer_Qnty_fcast)))

names(class_dec_pred_EU_consumer_Qnty_fcast) <- c(1:54)

#Plotting actual vs forecasted
plot(EU_consumer_TSQnty_total_timeser, col = "black", main ="Classical decomposition forecasting")
lines(class_dec_pred_EU_consumer_Qnty_fcast, col = "red")

#Plotting forecasting alone
plot(class_dec_pred_EU_consumer_Qnty_fcast, col = "red", type = "l",
                                 main ="Classical decomposition forecasting")

#forecasting using auto arima for next 6 months
fcast_auto_arima_EU_consumer_Qnty_fcast <- predict(autoarima_EU_consumer_Qnty, n.ahead = 12)

auto_arima_pred_EU_consumer_Qnty_fcast <- c(fitted(autoarima_EU_consumer_Qnty),
                                            ts(fcast_auto_arima_EU_consumer_Qnty_fcast$pred))

plot(EU_consumer_TSQnty_total_timeser, col = "black", main ="Auto ARIMA forecasting")
lines(auto_arima_pred_EU_consumer_Qnty_fcast, col = "red")

#Forecasting using Holtwinters
EU_consumer_Qntyfcast <- HoltWinters(EUconsumer_TSQnty_timeser, beta=F, gamma=F)
EU_consumer_Qntyfcast
plot(EU_consumer_Qntyfcast)

EU_consumer_Qntyfcast_6months <- forecast(EU_consumer_Qntyfcast,h=6)
EU_consumer_Qntyfcast_6months
plot(EU_consumer_Qntyfcast_6months)

# Comparing with Test data

accuracy(EU_consumer_Qntyfcast_6months,EU_consumer_TSQnty_test$overallmonthly_Qnty) 

#Forcasting for next six months Qnty

forecast(EUconsumer_TSQnty_timeser,h=6)
forecast(autoarima_EU_consumer_Qnty,h=6)

#####################################################################################################
#APAC Consumer
#####################################################################################################
#Prepaing and Subsetting data for APAC

Agg_master_data <- cbind( total_monthlysales, monthly_quantity = agg_quantity_mnth$monthly_quantity)

APAC_Consumer_data <- subset(Agg_master_data,(Agg_master_data$Market == "APAC") &
                               (Agg_master_data$Segment == "Consumer"))

APAC_Consumer_data <- APAC_Consumer_data[order(APAC_Consumer_data$order_date_year,
                                               APAC_Consumer_data$order_date_month), ]

APAC_Consumer_data$MonthNum <- c(1:nrow(APAC_Consumer_data))

#Extracting timeseries dataset for sales and quantity separately

APAC_consumer_TS_sales <- APAC_Consumer_data[, c(5,7)]

APAC_consumer_TS_Qnty   <- APAC_Consumer_data[, c(6,7)] 

######################################################################################################
#APAC consumer sales
#The dataset contains data from years 2011, 2012, 2013 & 2014 (in total 12*4 = 48 months)
#Extracting only 42 months data for model building and last 6 months data(43-48) for test
#purpose
#####################################################################################################

APAC_consumer_TSSales_train <- APAC_consumer_TS_sales[1:42, ]
APAC_consumer_TSSales_test  <- APAC_consumer_TS_sales[43:48, ]

#Timeseries plotting 

APAC_consumer_TSsales_total_timeser <- ts(APAC_consumer_TS_sales$overallmonthly_sales)
APACconsumer_TSSales_timeser        <- ts(APAC_consumer_TSSales_train$overallmonthly_sales)

#Using decompose() to understanding the seasonal, trend and irregular components using moving averages.

APACconsumer_TSSales_timeser_decom_in <- ts(APAC_consumer_TSSales_train$overallmonthly_sales, frequency = 12)                                           
APACconsumer_TSSales_timeser_decomp  <- decompose(APACconsumer_TSSales_timeser_decom_in)

plot(APACconsumer_TSSales_timeser_decomp)

# Observation of decomposed wave:
# 1. Trend is a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

plot(APACconsumer_TSSales_timeser, main ="Actual, smoothened VS predicated")


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries_APAC_consumer_sales <- stats::filter(APACconsumer_TSSales_timeser, 
                                                    filter=rep(1/(2*w+1),(2*w+1)), 
                                                    method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_APAC_consumer_sales[w+2] - smoothedseries_APAC_consumer_sales[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_APAC_consumer_sales[i] <- smoothedseries_APAC_consumer_sales[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APACconsumer_TSSales_timeser)
diff <- smoothedseries_APAC_consumer_sales[n-w] - smoothedseries_APAC_consumer_sales[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_APAC_consumer_sales[i] <- smoothedseries_APAC_consumer_sales[i-1] + diff
}

#Plot the smoothed time series

timevals_in_APAC_consumer_sales <- APAC_consumer_TSSales_train$MonthNum
lines(smoothedseries_APAC_consumer_sales, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_APAC_consumer_sales <- as.data.frame(cbind(timevals_in_APAC_consumer_sales, 
                                                      as.vector(smoothedseries_APAC_consumer_sales)))
colnames(smootheddf_APAC_consumer_sales) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APAC_consumer_sales <- lm(Sales ~ sin(0.6*Month) * poly(Month,1) + cos(0.6*Month) * poly(Month,2)
                                + Month 
                                ,data=smootheddf_APAC_consumer_sales)

summary(lmfit_APAC_consumer_sales)

accuracy(lmfit_APAC_consumer_sales)

global_pred_APAC_consumer_sales <- predict(lmfit_APAC_consumer_sales, Month=timevals_in_APAC_consumer_sales)

summary(global_pred_APAC_consumer_sales)
lines(timevals_in_APAC_consumer_sales, global_pred_APAC_consumer_sales, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_consumer_sales <- APACconsumer_TSSales_timeser-global_pred_APAC_consumer_sales
plot(local_pred_APAC_consumer_sales, col='red', type = "l")
acf(local_pred_APAC_consumer_sales)
acf(local_pred_APAC_consumer_sales, type="partial")
armafit_APAC_consumer_sales <- auto.arima(local_pred_APAC_consumer_sales)

tsdiag(armafit_APAC_consumer_sales)

#We'll check if the residual series is white noise

resi_APAC_consumer_sales <- local_pred_APAC_consumer_sales-fitted(armafit_APAC_consumer_sales)

adf.test(resi_APAC_consumer_sales,alternative = "stationary")
kpss.test(resi_APAC_consumer_sales)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_APAC_consumer_sales    <- APAC_consumer_TSSales_test$MonthNum

global_pred_out_APAC_consumer_sales <- predict(lmfit_APAC_consumer_sales,
                                               data.frame(Month =timevals_out_APAC_consumer_sales))

fcast_APAC_consumer_sales <- global_pred_out_APAC_consumer_sales

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_APAC_consumer_sales <- accuracy(fcast_APAC_consumer_sales,
                                               APAC_consumer_TSSales_test[,1])[5]
MAPE_class_dec_APAC_consumer_sales

#MAPE obtained - [1] 27.59505

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_APAC_consumer_sales <- c(ts(global_pred_APAC_consumer_sales),
                                        ts(global_pred_out_APAC_consumer_sales))
plot(APAC_consumer_TSsales_total_timeser, col = "black")
lines(class_dec_pred_APAC_consumer_sales, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_APAC_consumer_sales <- auto.arima(APACconsumer_TSSales_timeser)
autoarima_APAC_consumer_sales

tsdiag(autoarima_APAC_consumer_sales)
plot(autoarima_APAC_consumer_sales$x, col="black")
lines(fitted(autoarima_APAC_consumer_sales), col="red")
autoarima_APAC_consumer_sales
#Again, let's check if the residual series is white noise

resi_auto_arima_APAC_consumer_sales <- APACconsumer_TSSales_timeser - 
                                                fitted(autoarima_APAC_consumer_sales)

adf.test(resi_auto_arima_APAC_consumer_sales,alternative = "stationary")
kpss.test(resi_auto_arima_APAC_consumer_sales)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_APAC_consumer_sales <- predict(autoarima_APAC_consumer_sales, n.ahead = 6)


MAPE_auto_arima_APAC_consumer_sales <- accuracy(fcast_auto_arima_APAC_consumer_sales$pred,
                                                APAC_consumer_TSSales_test$overallmonthly_sales)[5]
MAPE_auto_arima_APAC_consumer_sales

#MAPE obtained - [1] 27.68952

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_APAC_consumer_sales <- c(fitted(autoarima_APAC_consumer_sales),
                                         ts(fcast_auto_arima_APAC_consumer_sales$pred))
plot(APAC_consumer_TSsales_total_timeser, col = "black")
lines(auto_arima_pred_APAC_consumer_sales, col = "red")

######################################################################################################
# Forecasting APAC Consumer Sales
#######################################################################################################
#forecasting using classical deocmposition for next 6 months
time_out_APAC_consumer_sales_fcast    <- c(49:54)

global_predout_APAC_consumer_sales_fcast <- predict(lmfit_APAC_consumer_sales,
                                                    data.frame(Month =time_out_APAC_consumer_sales_fcast))

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
Input_APAC_consumer_sales_fcast <- append(global_pred_APAC_consumer_sales,global_pred_out_APAC_consumer_sales)
names(Input_APAC_consumer_sales_fcast) <- c(1:48)

class_dec_pred_APAC_consumer_sales_fcast <- c(ts(Input_APAC_consumer_sales_fcast),
                                              ts(abs(global_predout_APAC_consumer_sales_fcast)))

names(class_dec_pred_APAC_consumer_sales_fcast) <- c(1:54)

#Plotting forecasting along with actual
plot(APAC_consumer_TSsales_total_timeser, col = "black", main ="Classical decomposition forecasting")
lines(class_dec_pred_APAC_consumer_sales_fcast, col = "red")

#Forecasting alone plotted
plot(class_dec_pred_APAC_consumer_sales_fcast, col = "red", type = "l", main ="Classical decomposition forecasting")

#forecasting using auto arima for next 6 months
fcast_auto_arima_APAC_consumer_sales_fcast <- predict(autoarima_APAC_consumer_sales, n.ahead = 6)

auto_arima_pred_APAC_consumer_sales_fcast <- c(fitted(autoarima_APAC_consumer_sales),
                                               ts(fcast_auto_arima_APAC_consumer_sales_fcast$pred))

plot(APAC_consumer_TSsales_total_timeser, col = "black", main ="Auto ARIMA forecasting")
lines(auto_arima_pred_APAC_consumer_sales_fcast, col = "red")

#Forecasting using Holtwinters
APAC_consumer_salesfcast <- HoltWinters(APACconsumer_TSSales_timeser, beta=F, gamma=F)
APAC_consumer_salesfcast
plot(APAC_consumer_salesfcast)

APAC_consumer_salesfcast_6months <- forecast(APAC_consumer_salesfcast,h=6)
APAC_consumer_salesfcast_6months
plot(APAC_consumer_salesfcast_6months)

# Comparing with Test data

accuracy(APAC_consumer_salesfcast_6months,APAC_consumer_TSSales_test$overallmonthly_sales) 

#Forcasting for next six months sales

forecast(APACconsumer_TSSales_timeser,h=6)
forecast(autoarima_APAC_consumer_sales,h=6)

###################################################################################################
#APAC consumer Qnty
#The dataset contains data from years 2011, 2012, 2013 & 2014 (in total 12*4 = 48 months)
#Extracting only 42 months data for model building and last 6 months data(43-48) for test
#purpose
###################################################################################################

APAC_consumer_TSQnty_train <- APAC_consumer_TS_Qnty[1:42, ]
APAC_consumer_TSQnty_test  <- APAC_consumer_TS_Qnty[43:48, ]

#Timeseries plotting 

APAC_consumer_TSQnty_total_timeser <- ts(APAC_consumer_TS_Qnty$monthly_quantity)
APACconsumer_TSQnty_timeser        <- ts(APAC_consumer_TSQnty_train$monthly_quantity)

#Using decompose() to understanding the seasonal, trend and irregular components using moving averages.

APACconsumer_TSQnty_timeser_decom_in <- ts(APAC_consumer_TSQnty_train$monthly_quantity, frequency = 12)                                           
APACconsumer_TSQnty_timeser_decomp  <- decompose(APACconsumer_TSQnty_timeser_decom_in)

plot(APACconsumer_TSQnty_timeser_decomp)

# Observation of decomposed wave:
# 1. Trend is a linear positive slope line
# 2. Seasonality is a low wavelength sine curve

plot(APACconsumer_TSQnty_timeser, main ="Actual, smoothened VS predicated")


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries_APAC_consumer_Qnty <- stats::filter(APACconsumer_TSQnty_timeser, 
                                                   filter=rep(1/(2*w+1),(2*w+1)), 
                                                   method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_APAC_consumer_Qnty[w+2] - smoothedseries_APAC_consumer_Qnty[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_APAC_consumer_Qnty[i] <- smoothedseries_APAC_consumer_Qnty[i+1] - diff
}

#Smoothing right end of the time series

n <- length(APACconsumer_TSQnty_timeser)
diff <- smoothedseries_APAC_consumer_Qnty[n-w] - smoothedseries_APAC_consumer_Qnty[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_APAC_consumer_Qnty[i] <- smoothedseries_APAC_consumer_Qnty[i-1] + diff
}

#Plot the smoothed time series

timevals_in_APAC_consumer_Qnty <- APAC_consumer_TSQnty_train$MonthNum
lines(smoothedseries_APAC_consumer_Qnty, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_APAC_consumer_Qnty <- as.data.frame(cbind(timevals_in_APAC_consumer_Qnty, 
                                                     as.vector(smoothedseries_APAC_consumer_Qnty)))
colnames(smootheddf_APAC_consumer_Qnty) <- c('Month', 'Qnty')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APAC_consumer_Qnty <- lm(Qnty ~ sin(0.6*Month) * poly(Month,2) + cos(0.6*Month) * poly(Month,2) 
                               +sin(0.05*Month)* Month
                               ,data=smootheddf_APAC_consumer_Qnty)

summary(lmfit_APAC_consumer_Qnty)

accuracy(lmfit_APAC_consumer_Qnty)

global_pred_APAC_consumer_Qnty <- predict(lmfit_APAC_consumer_Qnty, Month=timevals_in_APAC_consumer_Qnty)

summary(global_pred_APAC_consumer_Qnty)
lines(timevals_in_APAC_consumer_Qnty, global_pred_APAC_consumer_Qnty, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_consumer_Qnty <- APACconsumer_TSQnty_timeser-global_pred_APAC_consumer_Qnty
plot(local_pred_APAC_consumer_Qnty, col='red', type = "l")
acf(local_pred_APAC_consumer_Qnty)
acf(local_pred_APAC_consumer_Qnty, type="partial")
armafit_APAC_consumer_Qnty <- auto.arima(local_pred_APAC_consumer_Qnty)

tsdiag(armafit_APAC_consumer_Qnty)

#We'll check if the residual series is white noise

resi_APAC_consumer_Qnty <- local_pred_APAC_consumer_Qnty-fitted(armafit_APAC_consumer_Qnty)

adf.test(resi_APAC_consumer_Qnty,alternative = "stationary")
kpss.test(resi_APAC_consumer_Qnty)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_APAC_consumer_Qnty    <- APAC_consumer_TSQnty_test$MonthNum

global_pred_out_APAC_consumer_Qnty <- predict(lmfit_APAC_consumer_Qnty,
                                              data.frame(Month =timevals_out_APAC_consumer_Qnty))

fcast_APAC_consumer_Qnty <- global_pred_out_APAC_consumer_Qnty

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_APAC_consumer_Qnty <- accuracy(fcast_APAC_consumer_Qnty,
                                              APAC_consumer_TSQnty_test[,1])[5]
MAPE_class_dec_APAC_consumer_Qnty

#MAPE obtained - [1] 31.76748

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_APAC_consumer_Qnty <- c(ts(global_pred_APAC_consumer_Qnty),
                                       ts(global_pred_out_APAC_consumer_Qnty))
plot(APAC_consumer_TSQnty_total_timeser, col = "black")
lines(class_dec_pred_APAC_consumer_Qnty, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_APAC_consumer_Qnty <- auto.arima(APACconsumer_TSQnty_timeser)
autoarima_APAC_consumer_Qnty

tsdiag(autoarima_APAC_consumer_Qnty)
plot(autoarima_APAC_consumer_Qnty$x, col="black")
lines(fitted(autoarima_APAC_consumer_Qnty), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_APAC_consumer_Qnty <- APACconsumer_TSQnty_timeser - 
                                           fitted(autoarima_APAC_consumer_Qnty)

adf.test(resi_auto_arima_APAC_consumer_Qnty,alternative = "stationary")
kpss.test(resi_auto_arima_APAC_consumer_Qnty)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_APAC_consumer_Qnty <- predict(autoarima_APAC_consumer_Qnty, n.ahead = 6)


MAPE_auto_arima_APAC_consumer_Qnty <- accuracy(fcast_auto_arima_APAC_consumer_Qnty$pred,
                                               APAC_consumer_TSQnty_test$monthly_quantity)[5]
MAPE_auto_arima_APAC_consumer_Qnty

#MAPE obtained - [1] 26.24458
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_APAC_consumer_Qnty <- c(fitted(autoarima_APAC_consumer_Qnty),
                                        ts(fcast_auto_arima_APAC_consumer_Qnty$pred))
plot(APAC_consumer_TSQnty_total_timeser, col = "black")
lines(auto_arima_pred_APAC_consumer_Qnty, col = "red")

######################################################################################################
# Forecasting APAC Consumer Qnty
#######################################################################################################
#forecasting using classical deocmposition for next 6 months
time_out_APAC_consumer_Qnty_fcast    <- c(49:54)

global_predout_APAC_consumer_Qnty_fcast <- predict(lmfit_APAC_consumer_Qnty,
                                                   data.frame(Month =time_out_APAC_consumer_Qnty_fcast))

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
Input_APAC_consumer_Qnty_fcast <- append(global_pred_APAC_consumer_Qnty,global_pred_out_APAC_consumer_Qnty)
names(Input_APAC_consumer_Qnty_fcast) <- c(1:48)

class_dec_pred_APAC_consumer_Qnty_fcast <- c(ts(Input_APAC_consumer_Qnty_fcast),
                                             ts(abs(global_predout_APAC_consumer_Qnty_fcast)))

names(class_dec_pred_APAC_consumer_Qnty_fcast) <- c(1:54)

#Plotting actual vs forecasted
plot(APAC_consumer_TSQnty_total_timeser, col = "black", main ="Classical decomposition forecasting")
lines(class_dec_pred_APAC_consumer_Qnty_fcast, col = "red")

#Plotting forecasted values alone
plot(class_dec_pred_APAC_consumer_Qnty_fcast, col = "red",type = "l",
                           main ="Classical decomposition forecasting")

#forecasting using auto arima for next 6 months
fcast_auto_arima_APAC_consumer_Qnty_fcast <- predict(autoarima_APAC_consumer_Qnty, n.ahead = 12)

auto_arima_pred_APAC_consumer_Qnty_fcast <- c(fitted(autoarima_APAC_consumer_Qnty),
                                              ts(fcast_auto_arima_APAC_consumer_Qnty_fcast$pred))

plot(APAC_consumer_TSQnty_total_timeser, col = "black", main ="Auto ARIMA forecasting")
lines(auto_arima_pred_APAC_consumer_Qnty_fcast, col = "red")

#Forecasting using Holtwinters
APAC_consumer_Qntyfcast <- HoltWinters(APACconsumer_TSQnty_timeser, beta=F, gamma=F)
APAC_consumer_Qntyfcast
plot(APAC_consumer_Qntyfcast)

APAC_consumer_Qntyfcast_6months <- forecast(APAC_consumer_Qntyfcast,h=6)
APAC_consumer_Qntyfcast_6months
plot(APAC_consumer_Qntyfcast_6months)

# Comparing with Test data

accuracy(APAC_consumer_Qntyfcast_6months,APAC_consumer_TSQnty_test$overallmonthly_Qnty) 

#Forcasting for next six months Qnty

forecast(APACconsumer_TSQnty_timeser,h=6)
forecast(autoarima_APAC_consumer_Qnty,h=6)

########################################################################################################
#Conclusions: 
#-----------
#After designing the forecasted models we can conclude that both for Classical Decomposition and Auto 
#ARIMA models the results are similar and comparable. Both have a linear trend along with seasonal
#component and white noise. 
#Based on MAPE and kpss tests the residual is concluded as white noise, which indicates that the models 
#are rightly designed and there is no seasonal component remaining in the actual data which isn't modeled. 
#########################################################################################################