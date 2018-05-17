######################################################################################################
#MNIST digit recoginition assignment using SVM
#---------------------------------------------
#
#Bussiness requirement:
#A classic problem in the field of pattern recognition is that of handwritten digit recognition.
#Suppose that you have an image of a digit submitted by a user via a scanner, a tablet, or other
#digital devices. The goal is to develop a model that can correctly identify the digit (between 0-9)
#written in an image.
#Steps included:
#---------------
#1.Business Understanding
#2.Data Understanding
#3.Data Preparation
#4.Model Building & Hyperparameter tuning and cross validation 
#  4.1 Linear kernel
#  4.2 Polynomial Kernal
#  4.3 RBF Kernel
######################################################################################################
#Business Understanding: 
######################################################################################################
#The objective is to identify each of a large number of black-and-white
#rectangular pixel displays as one of the digit(between 0-9)
#####################################################################################################
# Data Understanding:
# Train data includes-
# Number of Instances: 60000
# Number of Attributes: 785
# Test data includes-
# Number of Instances: 10000
# Number of Attributes: 785
######################################################################################################
#Data Preparation
######################################################################################################
#Libraries used
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

######################################################################################################
#Reading input file
#####################################################################################################
mnist_train_data <- read.csv("mnist_train.csv", header = F)
mnist_test_data <- read.csv("mnist_test.csv", header = F)

#Renaming the dependant column and thus retaining default column names for other columns
colnames(mnist_train_data) [1] <- "digit"
colnames(mnist_test_data) [1] <- "digit"

#Understanding Dimensions of train data
dim(mnist_train_data)

#Structure of the mnist_train_dataset
str(mnist_train_data)

#printing first few rows
head(mnist_train_data)

#Exploring the mnist_train_data
summary(mnist_train_data)

#checking for NA value
sapply(mnist_train_data, function(x) sum(is.na(x)))

# checking for blank values "" in any of the fields of dataset
sapply(mnist_train_data, function(x) length(which(x == ""))) # No blank values found in any of the fields

#Cross verifying the test data set is having a similar structure of train data set

#Understanding Dimensions of train data
dim(mnist_test_data)

#Structure of the mnist_train_dataset
str(mnist_test_data)

#printing first few rows
head(mnist_test_data)

#Exploring the mnist_train_data
summary(mnist_test_data)

#checking for NA value
sapply(mnist_test_data, function(x) sum(is.na(x)))

# checking for blank values "" in any of the fields of dataset
sapply(mnist_test_data, function(x) length(which(x == ""))) # No blank values found in any of the fields

#Making our target class to factor
mnist_train_data$digit <- factor(mnist_train_data$digit)
summary(mnist_train_data)

mnist_test_data$digit <- factor(mnist_test_data$digit)
summary(mnist_test_data)

#scaling train
mnist_train_data_final <- cbind(mnist_train_data[ ,1], mnist_train_data[, -1]/255)

#scaling test
mnist_test_data_final <- cbind(mnist_test_data[ ,1], mnist_test_data[, -1]/255)

set.seed(1)

#Sampling 10% of data of train dataset as the dataset is huge and time consuming during cross
#validations using train()
train.indices = sample(1:nrow(mnist_train_data_final), 0.10*nrow(mnist_train_data_final))
train = mnist_train_data_final[train.indices, ]

test.indices = sample(1:nrow(mnist_test_data_final), 0.10*nrow(mnist_test_data_final))
test = mnist_test_data_final[test.indices, ]

colnames(train) [1] <- "digit"
colnames(test) [1]  <- "digit"

#####################################################################################################
#Constructing Model using linear kernel
#####################################################################################################
Model_linear = ksvm(digit~ ., data = train, scale = FALSE, kernel = "vanilladot" )
Eval_linear<- predict(Model_linear, test)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$digit)

kernlab::plot(Eval_linear, data = train)

#Observation:
#Accuracy : 0.9164

####################################################################################################
#Hyperparameter tuning and Cross Validation  for linear
####################################################################################################
# We will use the train function from caret package to perform Cross Validation. 
#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(1)
grid <- data.frame(C=c( 0.001, 0.01, 0.1,0.25, 0.50, 0.75, 1))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm_linear <- train(digit~., data=train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm_linear)

#Observation:
#Accuracy was used to select the optimal model using the largest value.
#The final value used for the model was C = 0.1.
#Accuracy obtained at C=0.1 is 0.9181708

plot_linear <- ggplot(fit.svm_linear) + ggtitle("Linear")
plot_linear

#Observations:
#As the cost increases from 0.001 to 1.00, the accuracy first increases from 0.88 and reaches at its
#max 0.918(C= 0.1) and then decreases as C increases and reaches 0.904 at C=1.00
#The model have an accuracy of 0.9164 and when cross validated, the resulting accuracy is 0.918

#####################################################################################################
#Constructing Model for polynomial type
#####################################################################################################
#Using polyinomial Kernel
Model_poly <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "polydot", C = 1, cross=3)
Eval_poly<- predict(Model_poly, test)

#confusion matrix - polynomial Kernel
confusionMatrix(Eval_poly,test$digit)
kernlab::plot(Eval_poly, data = train)

#Observation:
#Accuracy : 0.9164

####################################################################################################
#Hyperparameter tuning and Cross Validation  for polynomial
####################################################################################################
#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

#Ranges of cost, scale and degree
#C= c(0.25, 0.5), degree = c(3,4), scale = c(0.01, 0.1)

grid <- expand.grid(degree = seq(3, min(2, 4)),      
                    scale = 10 ^((2:1) - 3),
                    C = 2 ^((2:1) - 3))

fit.svm_poly <- train(digit~., data=train, method="svmPoly", metric=metric, 
                        tuneGrid=grid, trControl=trainControl)

print(fit.svm_poly)
#Observation:
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were degree = 2, scale = 0.1 and C = 0.25.
#Accuracy: 0.9484988 

plot_poly <- ggplot(fit.svm_poly) + ggtitle("Polynomial")
plot_poly

#Observations:
#Initial accuracy obtained from the model is 0.9164 and when cross validated, the resulting accuracy
#is 0.9484988. Here the cost values taken 0.25 and 0.5 have same accuarcy for degree = 2 and scale
#0.1, while choosing the optimal value the minimum value for c 0.25 is choosen as it allows more
#miscalculations, there by avoiding overfitting 

#####################################################################################################
#Constructing Model using RBF Kernal
#####################################################################################################
Model_RBF <- ksvm(digit~ ., data = train, scale = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$digit)

kernlab::plot(Eval_RBF, data = train)

#Observation:
#Accuracy : 0.9518

####################################################################################################
#Hyperparameter tuning and Cross Validation 
####################################################################################################
#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )

fit.svm_RBF <- train(digit~., data=train, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm_RBF)

#Observation:
#Accuracy was used to select the optimal model using the largest value.
#The final values used for the model were sigma = 0.025 and C = 2.
#Accuracy: 0.9600002

plot_RBF <- ggplot(fit.svm_RBF) + ggtitle("RBF")
plot_RBF

#Observations:
#Initial accuracy of model is 0.9518 and after cross validation the resulting accuracy is 0.96
#here the optimal model is choosen for highest accuracy value 

grid.arrange(plot_linear,plot_poly,plot_RBF, ncol = 2, top = "Accuracy plot of different Kernel models") 

#Observation & Conclusion:
#------------------------
#For linear optimal model C is 0.1 and accuracy is 0.918
#For polynomial optimal model c is 0.25 and accuracy is 0.9484
#For RBF optimal model c is 2 , sigma is 0.025 and accuracy is 0.9600
#Obviously trend is as c increases, accuracy increases. C is nothing but cost of miscalculation
#which should be high as per SVM formulation where as C in KSVM() is the reverse of it, which says c
#should be less, thus allowing more miscalculations and avoiding overfitting of the model
#Here highest C value is for RBF model which have high non linearilty compared to other two models
