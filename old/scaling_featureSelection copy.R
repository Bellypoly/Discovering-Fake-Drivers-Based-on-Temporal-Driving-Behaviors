## read data 
org_df <- read.csv(file = "Driving_Data_KIA_SOUL.csv", header = T, stringsAsFactors = T)

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
varImp(rfModel)


#plots the most important features of our data with it's value
varImpPlot(rfModel)

ggplot(rfModel)


# library(randomForest)
# library(ggplot2)
# 
# # make dataframe from importance() output
# feat_imp_df <- importance(rfModel) %>%
#   data.frame() %>%
#   mutate(feature = row.names(.))
# 
# # plot dataframe
# ggplot(feat_imp_df, aes(x = reorder(feature, MeanDecreaseGini),
#                         y = MeanDecreaseGini)) +
#   geom_bar(stat='identity') +
#   coord_flip() +
#   theme_classic() +
#   labs(
#     x     = "Feature",
#     y     = "Importance",
#     title = "Feature Importance: <Model>"
#   )
# 
# set.seed(7)
# library(caret)
# # calculate correlation matrix
# correlationMatrix <- cor(train[,1:47], use="complete.obs")
# 
# # summarize the correlation matrix
# print(correlationMatrix)
# 
# correlationMatrix <-na.omit(correlationMatrix)
# print(correlationMatrix)
# 
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# 
# # print indexes of highly correlated attributes
# print(highlyCorrelated)
# 
# #Creating a new dataframe with the most important 10 features
# sub_df <- train[,c(5,7,9,12,15,20,30,31,33,35)]
# summary(sub_df)
# str(sub_df)
# dim(sub_df)

#Time Series data
data.ts= ts(sub_data,start =1, end = 500, frequency = 4 )

#K-means clustering
library(factoextra)

k2 <- kmeans(sub_df, centers = 5, nstart = 25)
str(k2)

k2
fviz_cluster(k2, data = sub_df)

distance <- get_dist(sub_df) #computing euclidean distance matrix between the rows of a data

#Visualizing a distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

