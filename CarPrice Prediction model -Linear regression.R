################################################################################################################################
#CAR PRICE ASSIGNMENT
#Bussiness requirement:
#A Chinese automobile company Geely Auto aspires to enter the US market by setting up their manufacturing unit there 
#and producing cars locally to give competition to their US and European counterparts. They have contracted an 
#automobile consulting company to understand the factors on which the pricing of a car depends. Specifically, 
#they want to understand the factors affecting the pricing of cars in the American marketing, since those may be very 
#different from the Chinese market. Essentially, the company wants to know:
#
#1.Which variables are significant in predicting the price of a car
#2.How well those variables describe the price of a car
#
#Pupose : Modelling the price of cars with the available independent variables
################################################################################################################################

#Libraries used

library(car)          # for VIF
library(MASS)         # for StepAIC
library(ggplot2)      # for plotting
library(stringr)      # for string manipulation
library(dplyr)        # for data manipulation 
library(corrplot)     #for corelation matrix 

###############################################################################################################################
#Reading input file
###############################################################################################################################
car_price_initial_data <- read.csv("CarPrice_Assignment.csv")

#Moving to test dataset
car_price <- car_price_initial_data

#checcking structure of dataset
str(car_price)

########################################################################################################################
#Data preparation and cleaning
########################################################################################################################

#look for duplicate records
nrow(unique(car_price)) # No duplicates found 

#check for NA values
sum(is.na(car_price)) # No NA values

# checking for blank values "" in any of the fields of dataset
sapply(car_price, function(x) length(which(x == ""))) # No blank values found in any of the fields

#check if there are outliers in the variables
car_price_outlier      <- car_price[, c(10:14, 17, 19:25)] #Extracting only the fields required 
car_price_outlier_data <- sapply(car_price_outlier, function(x) quantile(x, seq(0,1,0.01))) 

#For Compression-ratio the value jumps from 10 to 20 at 90%. since there are 10% left which is huge, so ignoring the 
#outliers treament as its a business decision

#view(car_price_outlier_data) #No significant outlier values found

#Retaining actual data for plotting- symboling is actually insurance risk rating
car_price$symboling_fact <-  car_price$symboling

#Converting "symboling" field into factor 
car_price$symboling <-  as.factor(car_price$symboling)

summary(car_price$symboling)

#classifying to safe and risky based on data understanding
levels(car_price$symboling)[1:3] <- "safe"
levels(car_price$symboling)[2:4] <- "risky"

summary(car_price$symboling)

levels(car_price$symboling)<-c(1,0)

car_price$symboling<-as.numeric(levels(car_price$symboling))[car_price$symboling]

###############################################################################################################################
#Data cleansing for CAR NAME - Field CarName contains two part one is the car company name and other is the car model
###############################################################################################################################
summary(car_price$CarName)#there are 28 levels

#Splitting car company and car model into 2 columns
car_name <- data.frame(str_split_fixed(car_price$CarName, "[ ]", 2))

#summary on car company
summary(car_name$X1) #there are 28 levels
str(car_name)

#found lots of discripencies in naming of company, many are misspelled
levels(car_name$X1)[levels(car_name$X1)=="alfa-romero"]     <- "alfa-romeo"
levels(car_name$X1)[levels(car_name$X1)=="maxda"]           <- "mazda"
levels(car_name$X1)[levels(car_name$X1)=="nissan"]          <- "Nissan"
levels(car_name$X1)[levels(car_name$X1)=="porcshce"]        <- "porsche"
levels(car_name$X1)[levels(car_name$X1)=="toyouta"]         <- "toyota"
levels(car_name$X1)[levels(car_name$X1)=="vokswagen"]       <- "volkswagen"
levels(car_name$X1)[levels(car_name$X1)=="vw"]              <- "volkswagen"

summary(car_name$X1) #there are 22 levels

#Car companies are further classified into origin countries as there are too many levels. So for better understanding it 
#is further grouped based on head quarters of the company and also this help Chinese automobile company Geely Auto 
#to understand the data at country level.
#Classification based on data obtained from https://en.wikipedia.org/wiki/List_of_automobile_manufacturers. 
#Countries:France, Germany,	Italy, Japan,	Sweden,	U.K, U.S (22 levels are further classified as 7 levels)

car_name$company_country[car_name$X1  %in% c("alfa-romeo") ] <- "Italy"
car_name$company_country[car_name$X1  %in% c("jaguar") ]     <- "U.K"
car_name$company_country[car_name$X1  %in% c("peugeot","renault") ] <- "France"
car_name$company_country[car_name$X1  %in% c("volvo", "saab") ]     <- "Seweden"
car_name$company_country[car_name$X1  %in% c("honda","isuzu", "mazda", "mitsubishi", "Nissan", "subaru", "toyota") ] <- "Japan"
car_name$company_country[car_name$X1  %in% c("buick","chevrolet", "dodge", "mercury", "plymouth") ] <- "U.S"
car_name$company_country[car_name$X1  %in% c("audi","bmw", "porsche", "volkswagen") ]               <- "Germany"

#Converting to a factor field
car_name$company_country <- as.factor(car_name$company_country)

#checking for summary
summary(car_name$company_country) #there are 7 levels
names(car_name)[1:2] <- c("car_company","car_model")

#merging with basic dataset
car_price <- cbind(car_price, car_name)

#Creating dummy for company country
dummy_companycountry <- model.matrix(~company_country , car_price)
dummy_companycountry <- dummy_companycountry[ , -1]

#Converting into numeric by making denoting desiel type to 1 and gas type to 0 
summary(car_price$fueltype)
levels(car_price$fueltype)<-c(1,0)
car_price$fueltype<-as.numeric(levels(car_price$fueltype))[car_price$fueltype]

#Converting into numeric by making std type to 1 and turbo type to 0 
summary(car_price$aspiration)
levels(car_price$aspiration)<-c(1,0)
car_price$aspiration<-as.numeric(levels(car_price$aspiration))[car_price$aspiration]

#Converting into numeric by making two doors to 1 and four doors to 0 
summary(car_price$doornumber)
levels(car_price$doornumber)<-c(1,0)
car_price$doornumber<-as.numeric(levels(car_price$doornumber))[car_price$doornumber]

#Creating dummy for carbody
summary(car_price$carbody)
dummy_carbody <- model.matrix(~carbody - 1, car_price)
dummy_carbody <- dummy_carbody[ , -1]

#Creating dummy for drivewheel
summary(car_price$drivewheel)
dummy_drivewheel <- model.matrix(~drivewheel - 1, car_price)
dummy_drivewheel <- dummy_drivewheel[ , -1]

#Converting into numeric by making front located to 1 and back located to 0 
summary(car_price$enginelocation)
levels(car_price$enginelocation)<-c(1,0)
car_price$enginelocation<-as.numeric(levels(car_price$enginelocation))[car_price$enginelocation]

#Creating dummy for enginetype
summary(car_price$enginetype)
dummy_enginetype <- model.matrix(~enginetype - 1, car_price)
dummy_enginetype <- dummy_enginetype[ , -1]

summary(car_price$cylindernumber)
#converting to numeric for plotting purpose
car_price$cylinderno_plotting <- car_price$cylindernumber
levels(car_price$cylinderno_plotting)<-c(8,5,4,6,3,12,2)

#Creating dummy for cylindernumber
dummy_cylindernumber <- model.matrix(~cylindernumber - 1, car_price)
dummy_cylindernumber <- dummy_cylindernumber[ , -1]

#Creating dummy for fuelsystem
summary(car_price$fuelsystem)
dummy_fuelsystem <- model.matrix(~fuelsystem - 1, car_price)
dummy_fuelsystem <- dummy_fuelsystem[ , -1]

#Combining data
car_price <- cbind( car_price , dummy_companycountry, dummy_carbody, dummy_drivewheel, dummy_enginetype, 
                    dummy_cylindernumber, dummy_fuelsystem)

#Extracting relevant data required for analysis

car_price_data <- car_price[,c(26, 2, 4:6, 9:14, 17, 19:25, 32:62)]
#Here car id is not used as it is only for unique id purpose and some other fields are not taken as their converted 
#dummy or numeric fields are taken into consideration for analysis purpose

#converting from integer to numeric
car_price_data$curbweight <-  as.numeric(car_price_data$curbweight)
car_price_data$enginesize <-  as.numeric(car_price_data$enginesize)

car_price_data$horsepower <-  as.numeric(car_price_data$horsepower)
car_price_data$peakrpm    <-  as.numeric(car_price_data$peakrpm)
car_price_data$citympg    <-  as.numeric(car_price_data$citympg)
car_price_data$highwaympg <-  as.numeric(car_price_data$highwaympg)

str(car_price_data)

#Understanding the trend of price
table(car_price_data$price) #As we could see price list starts from 5118 and ends at 45400

plot_price <- ggplot(car_price_data, aes(price) ) + geom_histogram() + scale_x_continuous(breaks = seq(0,50000,5000))
plot_price

#Most of cars are priced between 5000 - 25000 mainly

plot_carcompany_country <- ggplot( car_price, aes(price) ) + geom_histogram() +
  scale_x_continuous(breaks = seq(0,50000,5000))+ facet_wrap(~company_country,ncol = 4)

#Most of the cars are sold by Japan (Honda, isuzu, mazda, mitsubishi, Nissan, subaru, toyota) but all low price cars
#ranging from 5000-25000 and high price are sold by U.K(jaguar), U.S (buick, chevrolet, dodge, mercury, plymouth)and 
#Germany(audi, bmw, porsche, volkswagen)

plot_car_company <- ggplot( car_price, aes(price) ) + geom_histogram() +
  scale_x_continuous(breaks = seq(0,50000,5000))+ facet_wrap(~car_company,ncol = 4)

#checking for correlation between price and other variables
correlation_matrix <- cor(car_price_data)
corrplot(correlation_matrix, method = "number", title = "Correlation Map", mar=c(0,0,1,0),
         type = "lower", order = "FPC",col = c("red", "orange", "blue", "green"), number.cex = .5, tl.cex = 0.5)

#As per the correlattion matrix car price is highly correlated to curbweight, carlength, enginesize, carwidth, wheelbase,
#horsepower etc +vely and -vely with highwaympg, citympg, drivewheelfwd etc. Also don't have much correlation with 
#enginelocation, compressionratio, doornumber, fueltype, stroke etc.

#Plotting independent variables against price for understanding the effect on price
ggplot(car_price, aes(price, car_company))+ scale_x_continuous(breaks = seq(0,50000,5000))+
  geom_point()

#Understanding the influence of cylinder number on price
car_price$cylinderno_plotting<-as.numeric(levels(car_price$cylinderno_plotting))[car_price$cylinderno_plotting]

plot_cylind_no <- ggplot(car_price, aes(cylinderno_plotting, price))+ scale_x_continuous(breaks = seq(0,14,2))+
  geom_point()+geom_smooth(method = 'lm', color = 'red')
plot_cylind_no 

#Understanding the influence of horsepower on price
plot_hp <- ggplot(car_price, aes( horsepower, price))+ geom_point()+geom_smooth(method = 'lm', color = 'red') 
plot_hp

#As per above plot horsepower has a positive influence on price. Also in correlation matrix plot it shows 0.8081 
#correlation

#Understanding the influence of curbweight on price
plot_curbweight <- ggplot(car_price, aes(curbweight, price))+ geom_point()+ geom_smooth(method = 'lm', color = 'red') 
plot_curbweight

#Curbweight do have a +ve influence on price.A correlation value of 0.8353

#Understanding the influence of symboling on price
plot_symboling <- ggplot(car_price, aes(symboling_fact, price)) +  scale_x_continuous(breaks = seq(-3, 3, 1))+
  geom_point()+geom_smooth(method = 'lm', color = 'red')
plot_symboling
#Symboliing doesn't have much influence on price of the car

#Understanding the influence of compression ratio on price
plot_compressionratio <- ggplot(car_price, aes(compressionratio, price)) + geom_point()+
  geom_smooth(method = 'lm', color = 'red')
plot_compressionratio
#compression ratio doesn't have much influence on price

#Understanding the influence of peak rpm on price
plot_peakrpm <- ggplot(car_price, aes(peakrpm, price)) + geom_point()+
  geom_smooth(method = 'lm', color = 'red')
plot_peakrpm
#peak rpm doesn't have much influence on price

#Understanding the influence of door number on price
plot_doornum <- ggplot(car_price, aes(as.factor(doornumber), price)) + geom_point()

plot_doornum
#door number doesn't have much influence on price

#Understanding the influence of aspiration price
plot_aspiration <- ggplot(car_price, aes(aspiration, price)) + geom_point()

plot_aspiration
#aspiration doesn't have much influence on price

#Understanding the influence of highway.mpg on price
plot_highway.mpg <- ggplot(car_price, aes(highwaympg, price)) + geom_point()+
  geom_smooth(method = 'lm', color = 'red')

plot_highway.mpg
#highway.mpg have a -ve influence on price

#Understanding the influence of city.mpg on price
plot_city.mpg <- ggplot(car_price, aes(citympg, price)) + geom_point()+
  geom_smooth(method = 'lm', color = 'red')

plot_city.mpg
#city.mpg have a -ve influence on price

######################################################################################################################
#Model building - Linear regression
######################################################################################################################

# separate training and testing data
set.seed(100)

trainindices<- sample(1:nrow(car_price_data), 0.7*nrow(car_price_data))
train <- car_price_data[trainindices, ]
test  <- car_price_data[-trainindices, ]


model_1 <-lm(price~.,data=train)
summary(model_1)

########################################################################################################################
#Model created using STEPAIC function
#######################################################################################################################

step <- stepAIC(model_1, direction="both")
step

model_2 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               carheight + curbweight + enginesize + stroke + peakrpm + 
               citympg + company_countryItaly + company_countryU.K + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
               enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree + fuelsystemspdi, 
             data = train)

summary(model_2)

sort((vif(model_2)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing curbweight as VIF and p value is high
model_3 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               carheight +  enginesize + stroke + peakrpm + 
               citympg + company_countryItaly + company_countryU.K + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + enginetypedohcv + 
               enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree + fuelsystemspdi, 
             data = train)

summary(model_3)

sort((vif(model_3)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing  carbodysedan as VIF and p value is high
model_4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                carheight +  enginesize + stroke + peakrpm + 
                citympg + company_countryItaly + company_countryU.K + carbodyhardtop + 
                carbodyhatchback +  carbodywagon + enginetypedohcv + 
                enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystemspdi, 
              data = train)


summary(model_4)

sort((vif(model_4)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing citympg as VIF and p value is high
model_5 <-   lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  carheight +  enginesize + stroke + peakrpm + 
                  company_countryItaly + company_countryU.K + carbodyhardtop + 
                  carbodyhatchback +  carbodywagon + enginetypedohcv + 
                  enginetypeohc + enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                  cylindernumbersix + cylindernumberthree + fuelsystemspdi, 
                data = train)
summary(model_5)

sort((vif(model_5)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#Removing enginetypeohc as VIF and p value is high
model_6 <-  lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 carheight +  enginesize + stroke + peakrpm + 
                 company_countryItaly + company_countryU.K + carbodyhardtop + 
                 carbodyhatchback +  carbodywagon + enginetypedohcv + 
                 enginetypeohcv + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix + cylindernumberthree + fuelsystemspdi, 
               data = train)

summary(model_6)

sort((vif(model_6)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing enginetypeohcv as VIF and p value is high
model_7 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                carheight +  enginesize + stroke + peakrpm + 
                company_countryItaly + company_countryU.K + carbodyhardtop + 
                carbodyhatchback +  carbodywagon + enginetypedohcv + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix + cylindernumberthree + fuelsystemspdi, 
              data = train)
summary(model_7)

sort((vif(model_7)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing carbodyhatchback as VIF and p value is high
model_8 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               carheight +  enginesize + stroke + peakrpm + 
               company_countryItaly + company_countryU.K + carbodyhardtop + 
               carbodywagon + enginetypedohcv + 
               cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree + fuelsystemspdi, 
             data = train)
summary(model_8)

sort((vif(model_8)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing fuelsystemspdi as VIF and p value is high

model_9 <-lm(formula = price ~ aspiration + enginelocation + carwidth + 
               carheight +  enginesize + stroke + peakrpm + 
               company_countryItaly + company_countryU.K + carbodyhardtop + 
               carbodywagon + enginetypedohcv + 
               cylindernumberfive + cylindernumberfour + 
               cylindernumbersix + cylindernumberthree , 
             data = train)
summary(model_9)

sort((vif(model_9)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing cylindernumberthree as VIF and p value is high
model_10 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 carheight +  enginesize + stroke + peakrpm + 
                 company_countryItaly + company_countryU.K + carbodyhardtop + 
                 carbodywagon + enginetypedohcv + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix  , 
               data = train)

summary(model_10)

sort((vif(model_10)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing cylindernumberfive as VIF is high
model_11 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 carheight +  enginesize + stroke + peakrpm + 
                 company_countryItaly + company_countryU.K + carbodyhardtop + 
                 carbodywagon + enginetypedohcv + 
                 cylindernumberfour + 
                 cylindernumbersix  , 
               data = train)

summary(model_11)

sort((vif(model_11)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing  enginetypedohcv as p value is high
model_12 <-  lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  carheight +  enginesize + stroke + peakrpm + 
                  company_countryItaly + company_countryU.K + carbodyhardtop + 
                  carbodywagon +  
                  cylindernumberfour + 
                  cylindernumbersix  , 
                data = train)

summary(model_12)

sort((vif(model_12)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing carbodyhardtop as p value is high

model_13 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 carheight +  enginesize + stroke + peakrpm + 
                 company_countryItaly + company_countryU.K + 
                 carbodywagon +  
                 cylindernumberfour + 
                 cylindernumbersix  , 
               data = train)

summary(model_13)

sort((vif(model_13)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing  carbodywagon as p value is high
model_14 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 carheight +  enginesize + stroke + peakrpm + 
                 company_countryItaly + company_countryU.K + 
                 cylindernumberfour + 
                 cylindernumbersix  , 
               data = train)
summary(model_14)

sort((vif(model_14)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing company_countryItaly as p value is high
model_15 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 carheight +  enginesize + stroke + peakrpm + 
                 company_countryU.K + 
                 cylindernumberfour + 
                 cylindernumbersix  , 
               data = train)

summary(model_15)

sort((vif(model_15)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing carheight as p value is high
model_16 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + 
                 company_countryU.K + 
                 cylindernumberfour + 
                 cylindernumbersix  , 
               data = train)

summary(model_16)

sort((vif(model_16)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing peakrpm as p value is high
model_17 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                 enginesize + stroke + 
                 company_countryU.K + 
                 cylindernumberfour + 
                 cylindernumbersix  , 
               data = train)

summary(model_17)

sort((vif(model_17)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing  company_countryU.K as p value is high
model_18 <-  lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  enginesize + stroke + 
                  cylindernumberfour + 
                  cylindernumbersix  , 
                data = train)
summary(model_18)

sort((vif(model_18)), decreasing = TRUE) #sorting and checking for the highest VIF valued variable

#removing cylindernumbersix as p value is high
model_19 <-  lm(formula = price ~  aspiration + enginelocation + carwidth + 
                  enginesize + stroke + 
                  cylindernumberfour  , 
                data = train)
summary(model_19)

#removing aspiration as p value is high
model_20 <- lm(formula = price ~   enginelocation + carwidth + 
                 enginesize + stroke + 
                 cylindernumberfour  , 
               data = train)
summary(model_20)

#removing stroke as p value is high
model_21 <- lm(formula = price ~   enginelocation + carwidth + 
                 enginesize + cylindernumberfour  , 
               data = train)

summary(model_21)

Predict_1 <- predict(model_21,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#####################################################################################################################
#Here Predicted R-squared: 0 .76484, R-squared:  0.8937 and Adjusted R-squared:  0.8906 which shows the model is not
#good enough. Inorder to improve the model adding independent variables which were deleted previously
####################################################################################################################

####################################################################################################################
#Reiterating the process by adding prevously deleted independent variables as the model21 created is not ideal
####################################################################################################################

#adding curbweight as it is highly correlated to price from correlation matrix
model_22 <- lm(formula = price ~   enginelocation + carwidth + curbweight +
                 enginesize + cylindernumberfour  , 
               data = train)

summary(model_22)

Predict_1 <- predict(model_22,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#Here Predicted R-squared: 0.773511, R-squared:  0.8997 and Adjusted R-squared:   0.896 which shows the model is not
#good enough. Inorder to improve the model adding and deleting independent variables which were deleted previously

#Removing enginelocation as R-squared is subsequently increased and difference between predicted R-square and R-squared
# is much high
model_23 <- lm(formula = price ~    carwidth + curbweight +
                 enginesize + cylindernumberfour  , 
               data = train)

summary(model_23)

Predict_1 <- predict(model_23,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
#Here Predicted R-squared: 0.7703973, R-squared:  0.8504 and Adjusted R-squared:  0.8461 which shows the model is not
#good enough. Inorder to improve the model adding and deleting independent variables which were deleted previously

#Removing curbweight as the p-value is much high 
model_24 <- lm(formula = price ~    carwidth + 
                 enginesize + cylindernumberfour  , 
               data = train)

summary(model_24)

Predict_1 <- predict(model_24,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#Here Predicted R-squared: 0.7636383, R-squared:  0.8454 and Adjusted R-squared: 0.8421 which shows the model is not
#good enough. Inorder to improve the model adding and deleting independent variables which were deleted previously

#Adding carbodysedan for understanding the affect also it was deleted following curbweight field
model_25 <- lm(formula = price ~    carwidth + carbodysedan+
                 enginesize + cylindernumberfour  , 
               data = train)

summary(model_25)

Predict_1 <- predict(model_25,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#Here Predicted R-squared: 0.7659408, R-squared:  0.8463 and Adjusted R-squared: 0.8418 which shows the model is not
#good enough. Inorder to improve the model adding and deleting independent variables which were deleted previously

#Adding enginetypeohc as the correlation with price is high and it was analyzed post carbodysedan field
model_26 <- lm(formula = price ~    carwidth + enginetypeohc +
                 enginesize + cylindernumberfour  , 
               data = train)

summary(model_26)

Predict_1 <- predict(model_26,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

#Here Predicted R-squared: 0.7560566, R-squared:  0.8461 and Adjusted R-squared:  0.8417 which shows the model is not
#good enough. Inorder to improve the model adding and deleting independent variables which were deleted previously

#Adding enginetypeohcv as the correlation with price is high and it was analyzed post enginetypeohc field
model_27 <- lm(formula = price ~ carwidth + enginetypeohcv + 
                                   enginesize + cylindernumberfour  , 
                                   data = train)

summary(model_27)

Predict_1 <- predict(model_27,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared

######################################################################################################################
#The model_27 looks more stable with Predicted R-squared: 0.8089351, R-squared: 0.8521 and Adjusted R-squared:0.8478
#####################################################################################################################

test$errors <- test$price - test$test_price
test$car_id <- seq(1,nrow(test))

# Plot - Actual vs Predicted price Model27
Plot_price_predicted_actual <- ggplot(test, aes(car_id,price)) + geom_line(aes(colour = "blue" )) + 
                                scale_x_continuous(name = "car_id", breaks = seq(0,65,3), limits = c(0,65)) + 
                                scale_y_continuous(name = "price", breaks = seq(0,50000,5000), limits = c(0,50000)) + 
                                geom_line(aes(x=car_id, y=test_price, colour="red")) 
Plot_price_predicted_actual

#After plotting we could see predicted price is some what similar to actual which shows model is good to fit

# Plot Model27 errors
Plot_model_error <- ggplot(test, aes(car_id, errors))  + geom_point() + #geom_line() +
                    scale_x_continuous(name = "car_id", breaks = seq(0,65,3), limits = c(0,65)) + 
                    scale_y_continuous(name = "Error") +
                    geom_hline(yintercept = 0)
Plot_model_error
#After plotting error produced by model_27 it looks like white nosie not much of a pattern

#Crrelation between price and independent variables that forms the final model_27
#-------------------------------------------------------------------------------
cor(car_price$carwidth, car_price$price)             # highly correlated
#[1] 0.7593253
cor(car_price$enginetypeohcv, car_price$price)       # correlated
#[1] 0.3859913
cor(car_price$enginesize, car_price$price)           # highly correlated
#[1] 0.8741448
cor(car_price$cylindernumberfour, car_price$price)   # correlated
#[1] -0.6977617

#Conclusion:  From the model created it shows that Car width, Engine size, Engine type and cylinder number do have 
#a high influence on price of the car. 
######################################################################################################################