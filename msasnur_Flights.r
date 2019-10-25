library(readr)

# Assigning csv file to flights variable
flights<-read_csv("FlightDelays.csv")
summary(flights)

# Calling required libraries
library(caret)
library(ISLR)
library(e1071)  

# Factoring the columns
flights$DAY_WEEK<-as.factor(flights$DAY_WEEK)
flights$CRS_DEP_TIME<-as.factor(flights$CRS_DEP_TIME)
flights$`Flight Status` <- factor(flights$`Flight Status`,levels = c("delayed","ontime"),labels = c(0,1))

#Cleaing the Data
MD<-flights[,c(10,1,8,4,2,13)]
summary(MD)
str(MD)
str(flights)

# Question 1
# Partioning the dataset into Training(60%) and Validation(40%) 

Index_Train<-createDataPartition(MD$DEST, p=0.6, list=FALSE)
Train <-MD[Index_Train,]
Valid <-MD[-Index_Train,]

summary(Train)
summary(Valid)

# Question 2
# Running Naive Bayes model to predict whether the flight is delayed or not

NB_Model <-naiveBayes(Train$`Flight Status` ~ .,data = Train)
NB_Model

# Question 3
# Flights that were delayed and on-time at each of the three airports
# Count of number of flights delayed and ontime
table(Train$`Flight Status`,Train$DEST)

# Proportion of flights delayed and ontime
prop.table(table(Train$`Flight Status`,Train$DEST),margin = 1)

# Question 4
# ROC and Confusion Matrix for the Validation Data
library(pROC)

predicted_validlabels<-predict(NB_Model,Valid)

confusionMatrix(predicted_validlabels,Valid$`Flight Status`)

Predicted_labels2<-predict(NB_Model,Valid,type = "raw")

roc(Valid$`Flight Status`,Predicted_labels2[,2])

plot.roc(Valid$`Flight Status`,Predicted_labels2[,2])
