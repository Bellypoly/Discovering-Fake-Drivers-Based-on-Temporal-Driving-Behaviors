getwd() #"/Users/hgellban
#/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code
setwd("/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code")



# Splitting the dataset into the Training set and Test set 
#install.packages('caTools') 
library(caTools)

## build a decision tree with ctree() using package party
##install.packages('caret', repos='http://cran.rstudio.com/', dependencies = TRUE)


##install.packages('caret', dependencies = TRUE)
##install.packages("multcomp")
##library(multcomp)
##install.packages("coin")

#install.packages("party", dependencies = TRUE)
library(party)
#install.packages('caret', repos='http://cran.rstudio.com/', dependencies = TRUE)
library(caret)
library(e1071) 

install.packages('microbenchmark', dependencies = TRUE)
library(microbenchmark)
library(GGally)

source("fg.r")
#factor(fg_df$Driver)
fg_df$Driver = factor(fg_df$Driver) #, levels = c(0, 1))

# split data into 2 parts for pca training (75%) and prediction (25%)
set.seed(1)
samp <- sample(nrow(fg_df), nrow(fg_df)*0.8)
df_train <- fg_df[samp,]
df_test <- fg_df[-samp,]
classifier <- function(df_train, df_test, classifier_type = "ctree") {
  cls <- NULL
  if (classifier_type == "ctree"){
    cls <- ctree(Driver ~ ., data=df_train)
  }else if(classifier_type == "svm"){
    cls = svm(formula = Driver ~ ., data = df_train, type = 'C-classification',kernel = 'linear')     
  }#
  else if(classifier_type == "lr"){
    cls = lm(formula = Driver ~ ., data = df_train)     
  }#
  y_pred <- predict(cls, newdata = df_test)
  y_test <- df_test$Driver
  print("y_test")
  print(y_test)
  print("y_pred")
  print(y_pred)
  confusionMatrix(y_pred, y_test)
  
  # Plot the data 
  #plot(df_train)#, pch = 16) 
  #abline(cls)
}#classifier
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

classifier(df_train, df_test, classifier_type = "ctree")
classifier(df_train, df_test, classifier_type = "svm")
#classifier(df_train, df_test, classifier_type = "lr")

microbenchmark(
  classifier(df_train, df_test, classifier_type = "ctree"),
  classifier(df_train, df_test, classifier_type = "svm")  
)


draw_features(df_train)

print("end plotting")
