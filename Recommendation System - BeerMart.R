###############################################################################################################################
#Assignment - Recommendation system for BEERMART(online beer store)
#"BeerMart" is an online beer store in the United States.The requirement is to build a recommendation system (collaborative) 
#for the store, where customers will be recommended the beer that they are most likely to buy. 
#----------------------------------------------------------------------------------------------------------------------------
#Input files:
#------------
#beer_data.csv  - Includes three different columns, beer id, profile name of user and ratings provided by user
#Each record is composed of a beer's name, the name of the user along with ratings provided by users. All ratings are 
#on a scale from 1 to 5 with 5 being the best.
#----------------------------------------------------------------------------------------------------------------------------
#Objective : To create a Reommendation system that will recommend customers different beer type that they are most likely to
#buy         
#------------------------------------------------------------------------------------------------------------------------------------
#Packages used:
#------------------------------------------------------------------------------------------------------------------------------------
library(recommenderlab)
library(ggplot2)
library(dplyr)
library(data.table)

#-----------------------------------------------------------------------------------------------------------------------------------
#Reading input file
#-----------------------------------------------------------------------------------------------------------------------------------
Beer_data_initial <- read.csv("beer_data.csv", stringsAsFactors = F)

#Moving to test dataset
beer_dataset <- Beer_data_initial

#Basic checks of EDA
nrow(beer_dataset)         #475984

#checking structure of dataset
str(beer_dataset)

#Checking summary
summary(beer_dataset)

#Converting character field to lower case before checking for duplicate records
beer_dataset$review_profilename <- as.factor(tolower(beer_dataset$review_profilename))

#checking for unique number of rows
nrow(unique(beer_dataset)) #475404

# checking for unique profile names
length(unique(beer_dataset$review_profilename))
#[1] 22498

#checking for unique beer id's
length(unique(beer_dataset$beer_beerid))
#[1] 40308

#around 500+ records are duplicate, removing duplicate records
beer_dataset <- beer_dataset[-which(duplicated(beer_dataset) == TRUE), ]

#Checking for NA values in dataset
sapply(beer_dataset, function(x) sum(is.na(x))) #No NA values found

#beer_beerid review_profilename     review_overall 
#  0                  0                  0 

#Checking for balnk values in records
sapply(beer_dataset, function(x) length(which(x == "")))

#Around 100 records have reviewer profile name as blanks 
#beer_beerid review_profilename     review_overall 
#0                100                  0

#Since there are only 100 records with profile name blanks, removing the records
beer_dataset <- beer_dataset[-which(beer_dataset$review_profilename == ""), ]

#Checking for range of values in  review column
range(beer_dataset$review_overall) 
#[1] 0 5

#Checking number of records with rating 0
nrow(beer_dataset[which(beer_dataset$review_overall==0),]) #6 rows with 0 as ratings

#removing records having 0 as ratings as it is not a valid value for rating field
beer_dataset <- beer_dataset[-which(beer_dataset$review_overall==0), ]

nrow(beer_dataset) #475298
range(beer_dataset$review_overall) #1 5

# checking for unique profile names
length(unique(beer_dataset$review_profilename))
#[1] 22497

#checking for unique beer id's
length(unique(beer_dataset$beer_beerid))
#[1] 40302

# understanding distribution of rating
ggplot(beer_dataset,aes(x=review_overall))+geom_histogram()
#Around 175000 records of rating 4, less number of records for rating 1, 1.5 and 2 

#Checking for number of records where same beer is rated twice by the same user with different ratings
nrow(beer_dataset[duplicated(beer_dataset[c(1,2)]), ]) #842

#-------------------------------------------------------------------------------------------------------------------------------------
#As per the anlaysis there are records where the users have rated the same beer, more than once with different ratings
#To avoid the discrepancies, average rating is taken as the final rating for those records. for example if a user named ALAN has
#rated beer id 52211 twice with rating 5 and rating 3 respectively, then final rating for beer id 52211 by ALAN would be taken
#as (5+3)/2 = 4. There are 842 such records, so after averaging out the ratings, the total record count should be 475298-842 = 474456
#-------------------------------------------------------------------------------------------------------------------------------------

beer_data_agg <- aggregate(review_overall ~ beer_beerid + review_profilename, beer_dataset, mean )

nrow(beer_data_agg) #474456 As expected
range(beer_data_agg$review_overall) #1 5

# checking for unique profile names
length(unique(beer_data_agg$review_profilename))
#[1] 22497

#checking for unique beer id's
length(unique(beer_data_agg$beer_beerid))
#[1] 40302

#-----------------------------------------------------------------------------------------------------------------------------------
#Data preparation
#1. Choose only those beers that have at least N number of reviews
#-----------------------------------------------------------------------------------------------------------------------------------
#Grouping data by beer id to know the number of reviews recieved for each beer id
grp_sum_data  <- summarise(group_by(beer_data_agg, beer_beerid), count_review = n())

# plotting distribution of beer with number of reviews recieved 
quantile(grp_sum_data$count_review,seq(0,1,.01))
boxplot(grp_sum_data$count_review)

# understanding distribution of rating
ggplot(grp_sum_data,aes(x=count_review))+geom_histogram() + scale_y_continuous(limits=c(0, 1000))

#As per the plots 90% of beers have number of reviews less than 20.

head(grp_sum_data[order(-grp_sum_data$count_review), ])
# beer_beerid count_review
#1        2093          977
#2         412          966
#3        1904          902
#4        1093          840
#5          92          812
#6        4083          798

summary(grp_sum_data)
#beer_beerid     count_review   
#Min.   :    3   Min.   :  1.00  
#1st Qu.:16884   1st Qu.:  1.00  
#Median :37368   Median :  2.00  
#Mean   :36975   Mean   : 11.77  
#3rd Qu.:56235   3rd Qu.:  5.00  
#Max.   :77317   Max.   :977.00

#Grouping data by beer id to know the number of reviews recieved for each beer id
grp_sum_data_prof  <- summarise(group_by(beer_data_agg, review_profilename), cnt_reviewbyprof = n())

# plotting distribution of beer with number of reviews recieved 
quantile(grp_sum_data_prof$cnt_reviewbyprof,seq(0,1,.01))
boxplot(grp_sum_data_prof$cnt_reviewbyprof)

# understanding distribution of rating
ggplot(grp_sum_data_prof,aes(x=cnt_reviewbyprof))+geom_histogram() + scale_y_continuous(limits=c(0, 1000))

head(grp_sum_data_prof[order(-grp_sum_data_prof$cnt_reviewbyprof), ])
# review_profilename cnt_reviewbyprof
#1 northyorksammy                 1842
#2 mikesgroove                    1373
#3 buckeyenation                  1336
#4 thorpe429                      1072
#5 chaingangguy                   1046
#6 nerofiddled                    1027

summary(grp_sum_data_prof)
#review_profilename cnt_reviewbyprof 
#0110x011   :    1    Min.   :   1.00  
#01ryan10   :    1    1st Qu.:   1.00  
#03svtcobra :    1    Median :   3.00  
#04101brewer:    1    Mean   :  21.09  
#05harley   :    1    3rd Qu.:  11.00  
#0beerguy0  :    1    Max.   :1842.00  
#(Other)    :22491                 

#Checking for number of records after filtering for threshold values
nrow(filter(beer_data_agg, (beer_data_agg$beer_beerid %in%
                           grp_sum_data$beer_beerid[grp_sum_data$count_review > 7]) &
                          (beer_data_agg$review_profilename %in%
                          grp_sum_data_prof$review_profilename[grp_sum_data_prof$cnt_reviewbyprof > 15])))

#[1] 356321 
#Based on the mean and median obtained from aggregated data of count of reviews for beer id and profile names, the threshold count of
#reviews for beer id is choosen as 7 and threshold count of reviews for profile names is choosen as 15. Number of records that 
#satisfy the condition the that minimum count of reviews for each beer id is greater than 7 and minmum count of reviews for each 
#profile name is greater 15, is around 356321. Total cleaned records 474456. Total records after filtering 356321 which is
#around 75% of cleaned data 

#Filtering data for records for thresholds values of minimum number of count of reviews 
beer_final_dataset <- filter(beer_data_agg, (beer_data_agg$beer_beerid %in%
                                               grp_sum_data$beer_beerid[grp_sum_data$count_review > 7]) &
                               (beer_data_agg$review_profilename %in%
                               grp_sum_data_prof$review_profilename[grp_sum_data_prof$cnt_reviewbyprof > 15]))

nrow(beer_final_dataset)                 # 356321
range(beer_final_dataset$review_overall) # 1 5

#2. Convert this data frame to a "realratingMatrix" before you build your collaborative filtering models

#Rearranging the columns of dataset for further analysis
beer_final_dataset <- beer_final_dataset[ , c(2,1,3)]

beer_final_dataset$review_profilename <- as.factor(beer_final_dataset$review_profilename)
beer_final_dataset$beer_beerid        <- as.factor((beer_final_dataset$beer_beerid))
beer_final_dataset$review_overall     <- as.numeric(beer_final_dataset$review_overall)
beer_final_dataset                    <- as.data.frame(beer_final_dataset)

beer_final_datamatrix <- as(beer_final_dataset, "realRatingMatrix")

length(getRatings(beer_final_datamatrix)) # 356321
range(getRatings(beer_final_datamatrix))  # 1 5

class(beer_final_datamatrix)
# [1] "realRatingMatrix"
# attr(,"package")
# [1] "recommenderlab"

#getting some information
# get some informtaion
dimnames(beer_final_datamatrix)
rowCounts(beer_final_datamatrix)
colCounts(beer_final_datamatrix)
rowMeans(beer_final_datamatrix)

#The following plot displays the heatmap of the rating matrix:
image(beer_final_datamatrix, main = "Heatmap of the rating matrix")

image(beer_final_datamatrix[1:100, 1:100], main = "Heatmap of the first 100 rows and columns")

#Top percentile of users and beer using quantile function
min_n_beers <- quantile(rowCounts(beer_final_datamatrix), 0.99)
min_n_beers

min_n_users <- quantile(colCounts(beer_final_datamatrix), 0.99)
min_n_users

#Plotting top users and beers using heat map 
image(beer_final_datamatrix[rowCounts(beer_final_datamatrix) > min_n_beers, colCounts(beer_final_datamatrix) > min_n_users], 
      main = "Heatmap of the top users and beers")

#Here we have considered only top beer id's. Some rows are darker than the others, this might mean that some users give higher
#ratings to all the beer id's. 
#--------------------------------------------------------------------------------------------------------------------------------------
#Data Exploration
#1.Determine how similar the first ten users are with each other and visualise it
#2.Compute and visualise the similarity between the first 10 beers
#--------------------------------------------------------------------------------------------------------------------------------------

#How similar are the first ten users are with each other
similar_users <- similarity(beer_final_datamatrix[1:10,],
                            method = "cosine",
                            which = "user")

#Similarity matrix
as.matrix(similar_users)

#Visualise similarity matrix
image(as.matrix(similar_users), main = "Users similarity")

#Observation:
#Profile name's (0110x011,05harley),(0110x011,100floods),(1759girl,1099),(100floods,1adam12),(1fastz28,1adam12),(1morebeer,1759girl)
#(1whiskey,05harley),( 2xhops,1morebeer) are similar to each other

#How similar are the first ten items are with each other
similar_beer <- similarity(beer_final_datamatrix[,1:10 ],
                           method = "cosine",
                           which = "items")

#Similarity matrix
as.matrix(similar_beer)

#Visualise similarity matrix
image(as.matrix(similar_beer), main = "items similarity")

#Observation:
#Beer ID's (5,9),(6,7),(7,5),(8,5),(9,7),(10,7),(11,13),(12,11) are similar to each other

#What are the unique values of ratings?

ratings_data <- as.numeric(getRatings(beer_final_datamatrix))

range(getRatings(beer_final_datamatrix))  
# 1 5

#Checking for unique ratings
sort(round(unique(ratings_data), 2))

# [1] 1.00 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 3.83 4.00 4.25 4.50 4.75 5.00

#The ratings are  in the range 1-5. Let's count the occurrences of each of them. 
table_ratings <- table(ratings_data) 
table_ratings
# ratings_data
#     1              1.5             1.75                  2        2.25           2.5          2.75                3 
#  1966             2406                1               7766           7         12206            20            35876 
#  3.25              3.5             3.75    3.83333333333333          4          4.25           4.5             4.75 
#    44            69000              170                  1      134362           236          73834              84 
#     5 
# 18342 
#---------------------------------------------------------------------------------------------------------------------------
#Here unique ratings like 1.75 , 2.25, 3.83 etc appears as we have taken average ratings for those provided multiple ratings
#for the same beerid's. from the initial dataset(after EDA) if we check for distinct ratings, it would be as below.
#--------------------------------------------------------------------------------------------------------------------------
beer_dataset_round <- beer_dataset
table(beer_dataset_round$review_overall)

# 1    1.5      2    2.5      3    3.5      4    4.5      5 
#3289   3814  11601  17581  49490  90725 174527  97156  27115 

#After ceiling and flooring the distinct values would range from 1-5 as follows
beer_dataset_round[ , -c(1,2)] <- round(beer_dataset_round[ , -c(1,2)], 0)

table(beer_dataset_round$review_overall)
#     1      2      3      4      5 
# 3289  32996  49490 362408  27115

#similarly we can do ceiling and flooring for beer_data_agg also
beer_data_agg_round <- beer_data_agg
table(beer_data_agg_round$review_overall)
#   1             1.25              1.5             1.75                2             2.25              2.5 
#  3282            2               3805                2            11575                8            17536 
#  2.75            3               3.25              3.5             3.75    3.83333333333333           4 
#    23          49371             53              90483              192                1           174034 
#   4.25          4.5              4.75                5 
#   261           96790             91             26947
beer_data_agg_round[ , -c(1,2)] <- round(beer_data_agg_round[ , -c(1,2)], 0)

table(beer_data_agg_round$review_overall)

#1      2      3      4      5 
#3284  32926  49447 361761  27038

#------------------------------------------------------------------------------------------------------------------------
#It totally depends on the business decision how the rating should be included. 
#-----------------------------------------------------------------------------------------------------------------------
#Now, we can build a frequency plot of the ratings   
ratings_data <- factor(ratings_data) 

#Let's visualize the distribution of ratings
qplot(ratings_data) + ggtitle("Distribution of the ratings")

#Lets do a histogram of ratings
qplot(getRatings(beer_final_datamatrix), binwidth = 1, main = "Histogram of ratings", xlab = "Rating")

summary(getRatings(beer_final_datamatrix))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   3.500   4.000   3.829   4.500   5.000 
# Skewed to the right

#Let's normalize the ratings and plot it
qplot(getRatings(normalize(beer_final_datamatrix, method = "Z-score")),
      main = "Histogram of normalized ratings", xlab = "Rating") 

summary(getRatings(normalize(beer_final_datamatrix, method = "Z-score")))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-6.4251 -0.5578  0.1470  0.0000  0.6781  3.4047
# seems better

#------------------------------------------------------------------------------------------------------------------------------
#Visualise the rating values and notice:
#--------------------------------------------------------------------------------------------------------------------------------

#The average beer ratings:
average_ratings_beerid <- colMeans(beer_final_datamatrix)

#Let's visualize the distribution of the average beer rating:
qplot(average_ratings_beerid) + stat_bin(binwidth = 0.1) + ggtitle("Distribution of the average beer rating")
#The highest value is between 3.5 - 4. Skewed towards right

beer_rating_avg <- aggregate(review_overall ~ beer_beerid,beer_final_dataset, mean)
summary(beer_rating_avg)
#beer_beerid   review_overall 
#5      :   1   Min.   :1.277  
#6      :   1   1st Qu.:3.553  
#7      :   1   Median :3.800  
#8      :   1   Mean   :3.731  
#9      :   1   3rd Qu.:4.000  
#10     :   1   Max.   :4.833

hist(beer_rating_avg$review_overall, breaks=100, col="blue") #Skewed towards right
#The average beer rating here is 3.7 

#The average ratings per user
average_ratings_per_user <- rowMeans(beer_final_datamatrix)

qplot(average_ratings_per_user) + stat_bin(binwidth = 0.1) +  ggtitle("Distribution of the average rating per user")

user_rating_avg <- aggregate(review_overall ~ review_profilename,beer_final_dataset, mean)
summary(user_rating_avg)
#review_profilename review_overall 
#0110x011 :   1     Min.   :2.607  
#05harley :   1     1st Qu.:3.710  
#100floods:   1     Median :3.868  
#1099     :   1     Mean   :3.849  
#1759girl :   1     3rd Qu.:4.000  
#1adam12  :   1     Max.   :4.950

#the average ratings per user is 3.8
hist(user_rating_avg$review_overall, breaks=100, col="blue")

#The average number of ratings given to the beers
average_num_beerid_ratings <- rowCounts(beer_final_datamatrix)
qplot(average_num_beerid_ratings)+ stat_bin(binwidth = 10) + ggtitle("Distribution of the average number of beer rating")

beer_num_of_ratings <-aggregate(review_overall ~ beer_beerid,beer_final_dataset, length)
summary(beer_num_of_ratings)

#beer_beerid   review_overall  
#5      :   1   Min.   :  3.00  
#6      :   1   1st Qu.: 11.00  
#7      :   1   Median : 19.00  
#8      :   1   Mean   : 44.69  
#9      :   1   3rd Qu.: 46.00  
#10     :   1   Max.   :727.00

#The average number of ratings given to beers is 44

#Plotting the same
hist(beer_num_of_ratings$review_overall, breaks=10, col="red")

#Most of them fall under 100 and skewed towards the left 

#The average number of ratings given by the users
average_num_user_ratings <- colCounts(beer_final_datamatrix)
qplot(average_num_user_ratings)+ stat_bin(binwidth = 10) + ggtitle("Distribution of the average number of user rating")

user_num_of_ratings <-aggregate(review_overall ~ review_profilename,beer_final_dataset, length)
summary(user_num_of_ratings)

#review_profilename review_overall   
#0110x011 :   1     Min.   :   5.00  
#05harley :   1     1st Qu.:  23.00  
#100floods:   1     Median :  41.00  
#1099     :   1     Mean   :  76.28  
#1759girl :   1     3rd Qu.:  87.00  
#1adam12  :   1     Max.   :1121.00  
#(Other)  :4665                    

#The average number of ratings given by the users is 76

#Plotting the same
hist(user_num_of_ratings$review_overall, breaks=10, col="red")

#Most of them fall under 100 and skewed towards the left 
#------------------------------------------------------------------------------------------------------------------------------------
#Building the recommendation model
#------------------------------------------------------------------------------------------------------------------------------------

#List of models available
recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommender_models)# 9 types of models

#description of recommendation system algorithms/models used
lapply(recommender_models, "[[", "description")

#This gives you different types of recommendation models
#In this case study , let's compare user based and item based
#collaborative filtering

#checking the parameters of these two models
recommender_models$IBCF_realRatingMatrix$parameters

#Transforming review_profilename into profile_id(numeric) for the ease of computation in reommender system
beer_recommend_dataname   <- transform(beer_final_dataset,profile_id=as.numeric(factor(review_profilename)))
beer_recommend_data       <- beer_recommend_dataname[ ,c(4,2,3)]

#To further optimize the run time of the algorithm, lets filter by beer ratings >3, Users who have rated at least 
#30 beers and beers that has got 30 ratings 
beer_recommend_data   <- beer_recommend_data[which(beer_recommend_data$review_overall >3),]

#converting to real rating matrix
beer_recommend_matrix <- as(beer_recommend_data, "realRatingMatrix") 
beer_recommend_matrix <- beer_recommend_matrix[rowCounts(beer_recommend_matrix) >30,colCounts(beer_recommend_matrix) >30 ]

#-----------------------------------------------------------------------------------------------------------------------
#Experiment with 'split' and 'cross-validation' evaluation schemes
#Using split method of evaluation scheme
#------------------------------------------------------------------------------------------------------------------------
#Objective is to find the best value of K and split to pick a model which has least RMSE value, as lower RMSE 
#indicates a better model. Lets iterate from K=1 to K=5 and split the data for training from 0.6 to 0.8 at intervals of 0.05 

#Initilizing blank DF for storing  accuracy measures for UBCF
error_UBCF  <- data.frame()
error_UBCF1 <- data.frame()

#Initilizing blank DF for storing  accuracy measures for IBCF
error_IBCF  <- data.frame()
error_IBCF1 <- data.frame()

#generating sequence for train split
train_split <- seq(0.6,0.80,by=0.05)

#generating sequnce for K 
k_split <- seq(1,5)

#Takes approximately 30 mins to complete.
for (i in train_split){
  for (j in k_split){
    
#Declaring the scheme
    set.seed(100)
    scheme <- evaluationScheme(beer_recommend_matrix, method = "split", train = i,
                               k = j, given=5 , goodRating = 4)
    
    
#--arguments
#train and test
#Here we create an evaluation scheme which splits the users 
#into a training set and test set for values indicated by K
    
#given 
#For the test set items(given argument) will be given to the
#recommender algorithm and the other items will be held out for computing the error
    
#With goodRating=4 all items with actual user rating of greater or equal 4 are considered 
#positives in the evaluation process
    
    
#Building the recommendation models
    recom_UBCF <-Recommender(getData(scheme, "train"), "UBCF") 
    recom_IBCF <- Recommender(getData(scheme, "train"), "IBCF") 
    
    # Making the Predictions
    predict_UBCF <- predict(recom_UBCF, getData(scheme, "known"), type="ratings") 
    predict_IBCF<- predict(recom_IBCF, getData(scheme, "known"), type="ratings")
    
    #Evaluating the performance parameters
    error_recom_UBCF<-calcPredictionAccuracy(predict_UBCF, getData(scheme, "unknown"))
    error_recom_IBCF<-calcPredictionAccuracy(predict_IBCF, getData(scheme, "unknown"))
    
    #Storing the result in a dataframe
    error_UBCF1 <- cbind(data.frame(iteration=paste("split_",i,"k_",j)),t(data.frame(error_recom_UBCF)))
    error_UBCF <- rbind(error_UBCF1,error_UBCF)
    
    error_IBCF1 <- cbind(data.frame(iteration=paste("split_",i,"k_",j)),t(data.frame(error_recom_IBCF)))
    error_IBCF <- rbind(error_IBCF1,error_IBCF)
  }
}

#Resetting rownames
rownames(error_IBCF) <- NULL
rownames(error_UBCF) <- NULL

#Viewing the performance measures for both UBCF and IBCF
View(error_UBCF)
View(error_IBCF)

#Lets find whats the minimum RMSE value out of the 25 iterations and pick the
#iteration values which got the min value of RMSE
min(error_UBCF$RMSE)  # 0.4308758
error_UBCF$iteration[error_UBCF$RMSE==min(error_UBCF$RMSE)]  # split_ 0.8 k_ 3

min(error_IBCF$RMSE) #0.540944
error_IBCF$iteration[error_IBCF$RMSE==min(error_IBCF$RMSE)]  # split_ 0.7 k_ 1

#----------------------------------------------------------------------------------------
# Plotting ROC for both UBCF and IBCF
#-------------------------------------------------------------------------------------
#Now that we have got the thresholds for K and split values for both UBCF and IBCF,
#lets visualize the ROC plot by generating the algorithms for both UBCF and IBCF

#Lets define the scheme as below, i.e split=0.8 and k=2
set.seed(100)
scheme <- evaluationScheme(beer_recommend_matrix, method = "split", train = .8,
                           k = 3, given=5 , goodRating = 4)
scheme

algorithms <- list(
  "user-based CF" = list(name="UBCF", param=list(normalize = "Z-score",
                                                 method="Cosine",
                                                 nn=30, minRating=4)),
  "item-based CF" = list(name="IBCF", param=list(normalize = "Z-score"
  ))
)

# run algorithms, predict next n beers

#Approximately takes 4 mins for completion

results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))

class(results)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"

# Draw ROC curve
plot(results, annotate = 1:4, legend="topleft")

# See precision / recall
plot(results, "prec/rec", annotate=3)

#Observation : Based on both the RMSE values yielded above and the ROC plot,UBCF is a better model than the IBCF for 
#this dataset

#-----------------------------------------------------------------------------------------------------------------
#using cross-validation evaluation schema
#-----------------------------------------------------------------------------------------------------------------
#Checking for a three fold evaluation scheme for cross validation method
eschema_Cross <- evaluationScheme(beer_recommend_matrix, method="cross-validation",
                            k=3, given=5, goodRating = 4)
eschema_Cross

#Building the recommendation models
recom_UBCF_cross <- Recommender(getData(eschema_Cross, "train"), "UBCF") 
recom_IBCF_cross <- Recommender(getData(eschema_Cross, "train"), "IBCF") 

# Making the Predictions
predict_UBCF_cross <- predict(recom_UBCF_cross, getData(eschema_Cross, "known"), type="ratings") 
predict_IBCF_cross <- predict(recom_IBCF_cross, getData(eschema_Cross, "known"), type="ratings")

#Evaluating the performance parameters
error_recom_UBCF_cross <-calcPredictionAccuracy(predict_UBCF_cross, getData(eschema_Cross, "unknown"))
error_recom_IBCF_cross <-calcPredictionAccuracy(predict_IBCF_cross, getData(eschema_Cross, "unknown"))

#Check for error values obtained for UBCF and IBCF
error_recom_UBCF_cross 
#RMSE       MSE       MAE 
#0.4408162 0.1943190 0.3523867 

#RMSE obtained for UBCF is 0.4393332 for K =3

error_recom_IBCF_cross 
#RMSE       MSE       MAE 
#0.5766396 0.3325132 0.4325492

#RMSE obtained for IBCF is 0.5766396 for K =3

# run algorithms, predict next n beers
#Approximately takes 4 mins for completion

results_cross <- evaluate(eschema_Cross, algorithms, n=c(1, 3, 5, 10, 15, 20))

class(results_cross)
# [1] "evaluationResultList"
# attr(,"package")
# [1] "recommenderlab"

#get the avg of all the runs
avg(results_cross)

#-------------------------------------------------------------------------------------------------------------------
# Plotting ROC for both UBCF and IBCF
#--------------------------------------------------------------------------------------------------------------------
plot(results_cross, annotate = 1:4, legend="topleft")

# See precision / recall
plot(results, "prec/rec", annotate=3)

#Observation : Based on both the RMSE values yielded above and the ROC plot,UBCF is a better model than the IBCF for 
#this dataset

#---------------------------------------------------------------------------------------------------------------
#Identifying the profile_id's for user names , as we have generated unique ID for each unique profile name
#The objective of the assignment is to recommend top 5 beers to users "cokes", "genog" & "giblet"
#----------------------------------------------------------------------------------------------------------------

profilename_data <- c("cokes","genog","giblet") #vector containing the 3 users to whom we are suggesting

reviewer_id <- c()
for (i in 1:length(profilename_data)){
  
reviewer_id<- c(reviewer_id,
                beer_recommend_dataname$profile_id[beer_recommend_dataname$review_profilename == profilename_data[i]][1])
}

reviewer_id
#[1]  978 1645 1671, i.e cokes=978,genog=1645,giblet=1671


#-----------------------------------------------------------------------------------------------------------------------
#Prediction phase
#-----------------------------------------------------------------------------------------------------------------------

#1. Lets build individual recommendation models for UBCF and IBCF based on the best cut off values obtained above
#2. Predict top 5 beer's to the users using both UBCF and IBCF

#------------------------------------------------------------------------------------------------------------------------
#Method = split in evaluationScheme()
#Predicting using the UBCF
#------------------------------------------------------------------------------------------------------------------------
#Lets build the UBCF with split=0.8 and  k= 3

#Lets make use of the same scheme used for generating ROC as it as the same parameters, i.e split=0.8 and k=2

recom_UBCF<-Recommender(getData(scheme, "train"), "UBCF")

#Making predictions using UBCF
# recommending top 5 items for for cokes , user ID=978
recommended.items.user978 <- predict(recom_UBCF, beer_recommend_matrix["978",], n=5)

# to display them
as(recommended.items.user978, "list")
#$`978`
#[1] "7971"  "1558"  "64228" "3833"  "22227" 

# recommending top 5 items for for genog , user ID =1645
recommended.items.user1645 <- predict(recom_UBCF, beer_recommend_matrix["1645",], n=5)

# to display them
as(recommended.items.user1645, "list")
#$`1645`
#[1] "2751"  "7971"  "92"    "599"   "30502"

# recommending top 5 items for for giblet , user ID =1671
recommended.items.user1671 <- predict(recom_UBCF, beer_recommend_matrix["1671",], n=5)

# to display them
as(recommended.items.user1671, "list")
#$`1671`
#[1] "224"  "694"  "3158" "61"   "1545"

#Now that we have recommended the top 5 beers to the 3 specified users using UBCF using the best threshold parameters
# of k and split, lets proceed to see what the recommendations look like using the IBCF
#We will do a similar exercise as the above

#--------------------------------------------------------------------------------------------------------
#Predicting using the IBCF
#--------------------------------------------------------------------------------------------------------

#Lets build the IBCF with split=0.8 and  k= 3

#Building recommender model using IBCF

recom_IBCF <- Recommender(getData(scheme, "train"), "IBCF") 

#Making predictions using IBCF
# recommending top 5 items for for cokes , user ID=978
recommended.items.user978.ibcf <- predict(recom_IBCF, beer_recommend_matrix["978",], n=5)

# to display them
as(recommended.items.user978.ibcf, "list")
#$`978`
#[1] "209" "314" "650" "811" "960" 

# recommending top 5 items for for genog , user ID =1645
recommended.items.user1645.ibcf <- predict(recom_IBCF, beer_recommend_matrix["1645",], n=5)

# to display them
as(recommended.items.user1645.ibcf, "list")
#$`1645`
#[1] "353"  "590"  "619"  "799"  "1018"

# recommending top 5 items for for giblet , user ID =1671
recommended.items.user1671.ibcf <- predict(recom_IBCF, beer_recommend_matrix["1671",], n=5)

# to display them
as(recommended.items.user1671.ibcf, "list")
#$`1671`
#[1] "89"  "96"  "193" "271" "288"

#Let's try predicting using cross validation scheme also for UBCF and IBCF
#------------------------------------------------------------------------------------------------------------------------
#Method = cross validation in evaluationScheme()
#Predicting using the UBCF
#------------------------------------------------------------------------------------------------------------------------
# k=3

recom_UBCF_cross<-Recommender(getData(eschema_Cross, "train"), "UBCF")

#Making predictions using UBCF
# recommending top 5 items for for cokes , user ID=978
recommended.items.user978_cross <- predict(recom_UBCF_cross, beer_recommend_matrix["978",], n=5)

# to display them
as(recommended.items.user978_cross, "list")
#$`978`
#[1] "7971" "2751" "6549" "3833" "1881"

# recommending top 5 items for for genog , user ID =1645
recommended.items.user1645_cross <- predict(recom_UBCF_cross, beer_recommend_matrix["1645",], n=5)

# to display them
as(recommended.items.user1645_cross, "list")
#$`1645`
#[1] "1093"  "1161"  "6075"  "2758"  "40418"

# recommending top 5 items for for giblet , user ID =1671
recommended.items.user1671_cross <- predict(recom_UBCF_cross, beer_recommend_matrix["1671",], n=5)

# to display them
as(recommended.items.user1671_cross, "list")
#$`1671`
#[1] "61"    "224"   "226"   "3158"  "11757"

#Now that we have recommended the top 5 beers to the 3 specified users using UBCF, lets proceed to see what 
#the recommendations look like using the IBCF We will do a similar exercise as the above

#--------------------------------------------------------------------------------------------------------
#Predicting using the IBCF
#--------------------------------------------------------------------------------------------------------

# k= 3

#Building recommender model using IBCF

recom_IBCF_cross <- Recommender(getData(eschema_Cross, "train"), "IBCF") 

#Making predictions using IBCF
# recommending top 5 items for for cokes , user ID=978
recommended.items.user978.ibcf_cross <- predict(recom_IBCF_cross, beer_recommend_matrix["978",], n=5)

# to display them
as(recommended.items.user978.ibcf_cross, "list")
#$``978`
#[1] "63"  "229" "279" "600" "631"

# recommending top 5 items for for genog , user ID =1645
recommended.items.user1645.ibcf_cross <- predict(recom_IBCF_cross, beer_recommend_matrix["1645",], n=5)

# to display them
as(recommended.items.user1645.ibcf_cross, "list")
#$`1645`
#[1] "353"  "641"  "871"  "1119" "1191"

# recommending top 5 items for for giblet , user ID =1671
recommended.items.user1671.ibcf_cross <- predict(recom_IBCF_cross, beer_recommend_matrix["1671",], n=5)

# to display them
as(recommended.items.user1671.ibcf_cross, "list")
#$`1671`
#[1] "55"  "131" "193" "206" "222"

#-------------------------------------------------------------------------------------------------------------------------
#Summary and observations:
#-------------------------------------------------------------------------------------------------------------------------
#Overall methodology followed includes below steps:
# -- EDA includes, removing balnks in profile name, ratings as 0, duplicates. There are records which are ratings given
#    to same beer id by the same user. Those records are aggregated and replaced with average ratings. Due to this distinct
#    rating value includes 1.25, 1.75, 3.83 etc. These values can be rounded by using ceiling and flooring methodology which
#    would result in ratings like 1, 2 etc as integers. This is totally a choice, that should be made based on bussiness 
#    understanding. 
# -- Threshold N values are choosen for beerId's as well as profile names, depending on mean, median etc, also keeping in 
#    mind that considerable amount of input data should be included for further analysis and creating the model
# -- realRatingMatix created based on the above assumption is used for analysing average ratings for beerid and profilename.
# -- Experimenting with 'split' and 'cross-validation' evaluation schemes
# -- Building IBCF and UBCF models for both split and cross-validation methods of evaluation schemes
# -- Both cases UBCF out performed IBCF models. RMSE calculated and ROC curve plotted for understanding the same 
# -- Predicting top 5 items for specified users using both UBCG and IBCF in both split and cross validation methods
#---------------------------------------------------------------------------------------------------------------------------
#Observations:
#Based on ROC curve plotted and the RMSE metric the UBCF model outperforms the IBCF model for both cross validation as well 
#as split method
#SPLIT method(UBCF) reommends below:
#-----------------------------
#for user coke: recommend the beer with following ID's
# "7971"  "1558"  "64228" "3833"  "22227"  
#for user genog : recommend the beer with following ID's
#  "2751"  "7971"  "92"    "599"   "30502" 
# for user giblet : recommend the beer with following ID's
#  "224"  "694"  "3158" "61"   "1545"
#
#Cross validation method(UBCF) reommends below:
#----------------------------------------------
#for user coke: recommend the beer with following ID's
# "7971" "2751" "6549" "3833" "1881" 
#for user genog : recommend the beer with following ID's
#  "1093"  "1161"  "6075"  "2758"  "40418 
# for user giblet : recommend the beer with following ID's
#  "61"    "224"   "226"   "3158"  "11757"
########################################################################################################################################################################
