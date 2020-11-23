## read data 
org_df <- read.csv(file = "dataset/Driving_Data_KIA_SOUL.csv", header = T, stringsAsFactors = T)

##Looking at the data
str(org_df)
head(org_df)
summary(org_df)

dim(org_df)

unique(org_df$PathOrder)
df <- subset(org_df, PathOrder==1)
drop_columns <- c("Time", "PathOrder")
target_column <- c("Class")
head(df)
df <- df[,!(names(df) %in% drop_columns)]
dim(df)

#Get classes of all columns
sapply(df,class)
sapply(df,mode)

#Removing a column with all 0's 
df <- df[, colSums(df != 0) > 0]
dim(df)

#Convert Factor values to numeric
df$Class = as.numeric(as.factor(df$Class))
sapply(df,class)

#Looking for missing values in our dataset
is.na(df)

#Finding number of NA's in our dataset
sum(is.na(df))

#Removing all the values if there is any
df <- df[complete.cases(df),]

# #Data Scaling
# require(dplyr)
# new_df <- df %>% mutate_if(is.numeric, scale)
# str(new_df)
# summary(new_df)

#Data Scaling
scale(df, center=TRUE, scale=TRUE)

#Scaling the data after ignoring some specific columns whose value comes NAN after normalization )
#all the numerical variables have been standardized with a mean value of zero
df[, -c(22,23,24,27)] <- scale(df[, -c(22,23,24,27)])
summary(df)

sum(is.na(df))
str(df)

#checking dimensions of data
dim(df)

# library(dplyr)
# train <- sample_frac(df, 0.2)

# Loading library
library('randomForest')
library('Metrics')
library('ggplot2')
library('ggthemes')
library('dplyr')
library(caret)



#dividing the dataset into train and test
# Set Seed so that same sample can be reproduced in future also

set.seed(4650)
trainIndex <- createDataPartition(df$Class, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)

train <- df[ trainIndex,]
test  <- df[-trainIndex,]


# Using random forest for variable selection
rfModel <-randomForest(factor(Class) ~ ., data = train,importance = TRUE, ntree = 500)
print(rfModel)

#Evaluate variable importance
importance(rfModel)
signal_importance <- importance(rfModel)
varImp(rfModel)

varimp <- varImp(rfModel)
#plots the most important features of our data with it's value
varImpPlot(rfModel)
varimp_plot <- varImpPlot(rfModel)
ggplot(rfModel)

