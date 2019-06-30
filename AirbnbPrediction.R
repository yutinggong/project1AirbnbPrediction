#Best model, RMSE 53.50919. 

###################################importing libraries and dataset 
library(dplyr)
library(tidyr)
library(ggplot2)
library(randomForest)
library(caret)
library(corrplot)
library(cowplot)
library(DT)

data = read.csv('analysisData.csv')
scoringData = read.csv('scoringData.csv')

###################################Data Exploration. 
str(data) ##we have 96 variables and more than 29,000 rows
#combine train and test
scoringData$price <- NA
scoringData$zipcode=as.factor(scoringData$zipcode)
all <- rbind(data, scoringData)

##first, understand the price variable
#price distribution
#As you can see, the price variable is right skewed. 
#This was expected as airbnb is supposed to offer affordable housing price.
#I will keep this in mind, and take measures before modeling.
ggplot(data = all, aes(x=price)) +
  geom_histogram()

##other variables' correlation with price
numericVars <- which(sapply(all, is.numeric)) #index numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

all_numVar <- all[, numericVars]
all_numVar$id <- NULL   #remove id
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sort on decreasing correlations with price
cor_sorted <- as.matrix(sort(cor_numVar[,'price'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.2)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

##Later I will visualize the relation between Price and 3 predictors with the highest orrelation with Price
##weekly_price
g1 = ggplot(data=all, aes(x=weekly_price)) +
  geom_histogram()
c1 = ggplot(data=all, aes(x=weekly_price, y=price)) +
  geom_point()

plot_grid(g1, c1, labels = "AUTO")
##cleaning_fee
summary(all_numVar$cleaning_fee)
g2=ggplot(data=all, aes(x=cleaning_fee)) +
  geom_histogram()
c2=ggplot(data=all, aes(x=cleaning_fee, y=price)) +
  geom_point()
plot_grid(g2, c2, labels = "AUTO")
##accomodates
summary(all_numVar$accommodates)
g3=ggplot(data=all, aes(x=accommodates)) +
  geom_histogram()
c3=ggplot(data=all, aes(x=accommodates, y=price)) +
  geom_point()
plot_grid(g3, c3, labels = "AUTO")
################################### Preparing the Data for Analysis
#see what are the missing values
total = colSums(is.na(data))
percentage = total/nrow(data)
col_name = colnames(data)
missing_data = data.frame(name= col_name,total = total, percentage = percentage)
arrange(missing_data, desc(total)) ### we see that variables with too many missing data is not useful, let's do not use them
#clean thumbnail_url, medium_url, xl_picture_url, license, monthly_price, square_feet, security_deposit
#Keep weekly_price becuase it has high correlation with price
#let's check missing data in scoringData
total2 = colSums(is.na(scoringData))
col_name2 = colnames(scoringData)
missing_data2 = data.frame(name= col_name2,total = total2)
arrange(missing_data2, desc(total)) ###most of the NA can be fixed with Data. Only need to fix zipcode and reviews_per_month seperately

#For the rest of the colums, we will replace the missing values with: mean value of that column if it is a numerical variable

###cleaning_Fee missing data, assume it means it does not have a cleaning fee
### we will fix NA in scoringdata at the same time
data[is.na(data$cleaning_fee),]$cleaning_fee <- 0
scoringData[is.na(scoringData$cleaning_fee),]$cleaning_fee <- 0

#beds, impute medium for missing beds 
data[is.na(data$beds),]$beds <- median(data$beds, na.rm = TRUE)
scoringData[is.na(scoringData$beds),]$beds <- median(scoringData$beds, na.rm = TRUE)

##combine train and test data, and then split to solve the different factor levels in property_type
#property_type
scoringData$price <- NA
scoringData$zipcode=as.factor(scoringData$zipcode)
fulldata <- rbind(data, scoringData)
fulldata$property_type = as.factor(fulldata$property_type)
##36428, train has 29142 rows, scoringData has 7286 rows
data2= fulldata[1:29142,]
scoringData2=fulldata[29143:36428,]
##there are  property type that are not in train. we want to fix that, which is hut and, cottage
scoringData2[scoringData2$property_type == "Hut",]$property_type = "Other"
scoringData2[scoringData2$property_type == "Cottage",]$property_type = "Other"

#neighbourhood_cleasned
scoringData2[scoringData$neighbourhood_cleansed == "Hollis Hills",]$neighbourhood_cleansed = "Bayside"
scoringData2[scoringData$neighbourhood_cleansed == "Westerleigh",]$neighbourhood_cleansed = "Castleton Corners"

###scoringData, fill NA in review per month, since it only has 1 review, ill fill in 0 for its review per month
filter(scoringData2,is.na(reviews_per_month))[,c("first_review","last_review","number_of_reviews")]
scoringData2[is.na(scoringData2$reviews_per_month),]$reviews_per_month <- 0

###################################model techniques
###split train data based on weekly_price
data3 <- data2[is.na(data2$weekly_price),]
data4 <- data2[!is.na(data2$weekly_price),]

###split test data based on weekly_price. 
scoringData2_1 <- scoringData2[is.na(scoringData2$weekly_price),]
scoringData2_2 <- scoringData2[!is.na(scoringData2$weekly_price),]

##random forest model
rf1 <- randomForest(price~host_response_time+host_is_superhost+host_listings_count+host_total_listings_count+host_has_profile_pic+host_identity_verified+neighbourhood_group_cleansed+latitude+longitude+is_location_exact+property_type+room_type+accommodates+bathrooms+bedrooms+beds+bed_type+cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+availability_30+ availability_60+ availability_90+ availability_365+number_of_reviews+review_scores_rating+review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+instant_bookable+is_business_travel_ready+cancellation_policy+require_guest_profile_picture+require_guest_phone_verification+calculated_host_listings_count+reviews_per_month+property_type,
                    data=data3, num.trees = 500)
rf2 <- randomForest(price~weekly_price+host_response_time+host_is_superhost+host_listings_count+host_total_listings_count+host_has_profile_pic+host_identity_verified+neighbourhood_group_cleansed+latitude+longitude+is_location_exact+property_type+room_type+accommodates+bathrooms+bedrooms+beds+bed_type+cleaning_fee+guests_included+extra_people+minimum_nights+maximum_nights+availability_30+ availability_60+ availability_90+ availability_365+number_of_reviews+review_scores_rating+review_scores_accuracy+review_scores_cleanliness+review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value+instant_bookable+is_business_travel_ready+cancellation_policy+require_guest_profile_picture+require_guest_phone_verification+calculated_host_listings_count+reviews_per_month+property_type,
                    data=data4, ntree = 500)

##train rmse
predm1 <- predict(rf1)
predm2 <- predict(rf2)
predm1 <- data.frame("id"=data3$id, "prediction"=predm1)
predm2 <- data.frame("id"=data4$id, "prediction"=predm2)
predm <- rbind(predm1, predm2)
predm<- arrange(predm, id)
data2 <- arrange(data2, id)

rmsePredm = sqrt(mean((predm$prediction-data2$price)^2)); rmsePredm 
##train rmse is 55.29047

##predcition
pred_scoring1 <- predict(rf1, scoringData2_1)
pred_scoring2 <- predict(rf2, scoringData2_2)

pred_scoring1<- data.frame(id=scoringData2_1$id, price=pred_scoring1)
pred_scoring2 <- data.frame(id=scoringData2_2$id, price=pred_scoring2)
submissionFile <- rbind(pred_scoring1, pred_scoring2)

write.csv(submissionFile, 'sample_submission.csv', row.names=F)
##final submission rmse is 53.50919. 

#########################################result summary
Model = c("Linear Model", "Random Forest", "Random Forest with Cross Validation", "Cross Validation of Boosting", "Random Forest", "Random Forest") 
ParameterorDifferences= c("add log transformation on price", "ntree = 1000", "mtry=10, tree=800","ntree=1000,interaction.depth = 3,shrinkage = 0.1", "ntree=500, Start splitting by weekly_price", "Ntree= 500, splitting by weekly_price, remove log transformation on price")
RMSE = c(59.15, 56.39, 55.49, 54.91, 54.77, 53.51)
result = data.frame(Model, ParameterorDifferences, RMSE)
datatable(result)


