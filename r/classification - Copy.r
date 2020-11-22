#getwd() #"/Users/hgellban
#/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code
#setwd("/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code/Discovering-Fake-Drivers-Based-on-Temporal-Driving-Behaviors")



## Splitting the dataset into the Training set and Test set 
##install.packages('caTools') 
#library(caTools)#
#
### build a decision tree with ctree() using package party
###install.packages('caret', repos='http://cran.rstudio.com/', dependencies = TRUE)


###install.packages('caret', dependencies = TRUE)
###install.packages("multcomp")
###library(multcomp)
###install.packages("coin")

##install.packages("party", dependencies = TRUE)
#library(party)
##install.packages('caret', repos='http://cran.rstudio.com/', dependencies = TRUE)
#library(caret)
#library(e1071) 

##install.packages('microbenchmark', dependencies = TRUE)
#library(microbenchmark)
#library(GGally)
#library(randomForest)

source("fg.r")


#do_drop_high_correlated_featured <- function(data){
#  tmp <- cor(data)
#  tmp[upper.tri(tmp)] <- 0
#  diag(tmp) <- 0  
#  data_new <- data[,!apply(tmp,2,function(x) any(x > 0.99))]
#  dim(data_new)
#}
#dim(fg_df)
#do_drop_high_correlated_featured(fg_df)
#cor(as.matrix(fg_df))

classifier <- function(df_train, df_test, classifier_type = "ctree") {
  cls <- NULL
  df_train <- droplevels(df_train)
  if (classifier_type == "ctree"){
    cls <- ctree(Driver ~ ., data=df_train)
  }else if(classifier_type == "svm"){
    cls <- svm(formula = Driver ~ ., data = df_train, type = 'C-classification',kernel = 'linear')     
  }#
  #  else if(classifier_type == "lr"){
  #    #cls <- lm(Driver ~ ., data = df_train)   
  #    cls <- glm( Driver ~., data = df_train, family = binomial)
  #  }#
  else if(classifier_type == "rf"){#random forest
    cls <- randomForest(formula = Driver ~., data = df_train)
  }#  
  else if(classifier_type == "xxx"){#new
    cls <- xxx( Driver ~., data = df_train)
  }#  
  
  if (classifier_type == "knn"){
    X <- subset(df_train, select = -c(Driver) )
    X_test <- subset(df_test, select = -c(Driver) )
    y <- df_train$Driver#df_train$Driver
    k <- 3
    y_pred <- knn3Train(train=X, test=X_test, cl=y, k=k, prob = FALSE)
    y_pred <- factor(y_pred)
    y_test <- df_test$Driver
    #y_test <- as.numeric(y_test)
  }else{
    y_pred <- predict(cls, newdata = df_test)
    y_test <- df_test$Driver
  }
  
  print("y_test")
  print(y_test)
  print("y_pred")
  print(y_pred)
  confusionMatrix(y_pred, y_test)
  
  # Plot the data 
  #plot(df_train)#, pch = 16) 
  #abline(cls)
}#classifier
#y <- df_train$Driver
#df_train <- droplevels(df_train)
#cls <- randomForest(Driver ~., data = df_train,ntree=100,mtry=2, importance = TRUE) 
#classifier(df_train, df_test, classifier_type = "rf")

#is.na( df_train$Driver)
#dim(df_train)
#classifier(df_train, df_test, classifier_type = "knn")
#X <- subset(df_train, select = -c(Driver) )
#X_test <- subset(df_test, select = -c(Driver) )
#y <- as.matrix(df_train$Driver)#df_train$Driver
#k <- 3
#y_pred <- knn3Train(train=X, test=X_test, cl=y, k=k)

#X = subset(df_train, select = -c(Driver) )
#y = subset(df_train, select = c(Driver) )
#m <- as.matrix(X)
#anova(m)

draw_features <- function(data){
  df_train <- data
  selected_features <- c("V1", "V11", "V21")
  featurePlot(x = df_train[, selected_features], 
              y = df_train$Driver, 
              plot = "pairs",
              auto.key = list(columns = 2))
  
  featurePlot(x = df_train[, selected_features], 
              y = df_train$Driver,
              plot = "density", 
              scales = list(x = list(relation = "free"), 
                            y = list(relation = "free")), 
              adjust = 1.5, 
              pch = "|", 
              layout = c(2, 1), 
              auto.key = list(columns = 2))
  ggpairs(df_train[, selected_features], title = "Scatterplot Matrix of the Features of the Haberman's Survival Data Set")
  
  
}

dim(fg_df)
#factor(fg_df$Driver)
fg_df$Driver = factor(fg_df$Driver) #, levels = c(0, 1))

# split data into 2 parts for pca training (75%) and prediction (25%)
#set.seed(1)
samp <- sample(nrow(fg_df), nrow(fg_df)*training_ratio)
samp
sort(samp)
nrow(fg_df)
df_train <- fg_df[samp,]
df_test <- fg_df[-samp,]


#samp = sample.split(fg_df,SplitRatio = (1-training_ratio)) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
#samp
#train1 =subset(fg_df,samp ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
#test1=subset(fg_df, samp==FALSE)
#samp

classifier(df_train, df_test, classifier_type = "ctree")
classifier(df_train, df_test, classifier_type = "svm")
classifier(df_train, df_test, classifier_type = "rf")
classifier(df_train, df_test, classifier_type = "knn")

microbenchmark(
  classifier(df_train, df_test, classifier_type = "ctree"),
  classifier(df_train, df_test, classifier_type = "svm")  
)


draw_features(df_train)
subset(df_train, select = -c(Driver) )
print("end plotting")
