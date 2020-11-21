getwd() #"/Users/hgellban

source("fg.r")
getwd() #"/Users/hgellban


#X <- fg_df[,head(colnames(fg_df),-1)]
#X_c <- cor(X)

classifier <- function(df_train, df_test, classifier_type = "ctree", k=3, ntree=1000) {
  cls <- NULL
  
  if (classifier_type == "ctree"){
    cls <- ctree(Class ~ ., data=df_train)
  }else if(classifier_type == "svm"){
    cls <- svm(formula = Class ~ ., data = df_train, type = 'C-classification',kernel = 'linear')     
  }#
  #  else if(classifier_type == "lr"){
  #    #cls <- lm(Class ~ ., data = df_train)   
  #    cls <- glm( Class ~., data = df_train, family = binomial)
  #  }#
  else if(classifier_type == "rf"){#random forest
    #cls <- randomForest(formula = Class ~., data = df_train)
    #cls <- randomForest(formula = Class ~., data = df_train, ntree=ntree, mtry=15)
    cls <- randomForest(formula = Class ~., data = df_train, ntree=ntree)
  }#  
  else if(classifier_type == "xxx"){#new
    cls <- xxx( Class ~., data = df_train)
  }#  
  
  if (classifier_type == "knn"){
    X <- subset(df_train, select = -c(Class) )
    X_test <- subset(df_test, select = -c(Class) )
    y <- df_train$Class#df_train$Class
    #k <- 1
    y_pred <- knn3Train(train=X, test=X_test, cl=y, k=k, prob = FALSE)
    y_pred <- factor(y_pred)
    y_test <- df_test$Class
    #y_test <- as.numeric(y_test)
  }else{
    y_pred <- predict(cls, newdata = df_test)
    y_test <- df_test$Class
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

draw_features <- function(data){
  df_train <- data
  #selected_features <- c("V1", "V11", "V21")
  selected_features <- c("V1", "V2", "V3", "V4", "V5")
  featurePlot(x = df_train[, selected_features], 
              y = df_train$Class, 
              plot = "pairs",
              auto.key = list(columns = 2))
  
  featurePlot(x = df_train[, selected_features], 
              y = df_train$Class,
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
#factor(fg_df$Class)
fg_df$Class = factor(fg_df$Class) #, levels = c(0, 1))
fg_df$Class

fg_df$Class2 <- as.numeric(as.character(fg_df$Class))
#dim(fg_df[fg_df$Class2 < 3])

fg_df <- subset(fg_df, Class2<=max_drivers)
fg_df$Class2 <- NULL
fg_df$Class = factor(fg_df$Class)

# split data into 2 parts for pca training (75%) and prediction (25%)
#set.seed(1)
samp <- sample(nrow(fg_df), nrow(fg_df)*training_ratio)
length(samp)
sort(samp)
nrow(fg_df)
df_train <- fg_df[samp,]
df_test <- fg_df[-samp,]
dim(df_train)
dim(df_test)

write.csv(df_train,file="dataset/df_train.csv")
write.csv(df_train,file="dataset/df_test.csv")


df_train <- droplevels(df_train)
do_class_label_count(fg_df)
do_class_label_count(df_train)
do_class_label_count(df_test)

#samp = sample.split(fg_df,SplitRatio = (1-training_ratio)) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
#samp
#train1 =subset(fg_df,samp ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
#test1=subset(fg_df, samp==FALSE)
#samp

classifier(df_train, df_test, classifier_type = "ctree")
classifier(df_train, df_test, classifier_type = "svm")
classifier(df_train, df_test, classifier_type = "rf", ntree=500)
classifier(df_train, df_test, classifier_type = "knn", k=1)
classifier(df_train, df_test, classifier_type = "knn", k=3)
classifier(df_train, df_test, classifier_type = "knn", k=5)
classifier(df_train, df_test, classifier_type = "knn", k=7)



draw_features(df_train)
subset(df_train, select = -c(Class) )
print("end plotting")
