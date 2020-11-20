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
#y <- df_train$Class
#df_train <- droplevels(df_train)
#cls <- randomForest(Class ~., data = df_train,ntree=100,mtry=2, importance = TRUE) 
#classifier(df_train, df_test, classifier_type = "rf")

#is.na( df_train$Class)
#dim(df_train)
#classifier(df_train, df_test, classifier_type = "knn")
#X <- subset(df_train, select = -c(Class) )
#X_test <- subset(df_test, select = -c(Class) )
#y <- as.matrix(df_train$Class)#df_train$Class
#k <- 3
#y_pred <- knn3Train(train=X, test=X_test, cl=y, k=k)

#X = subset(df_train, select = -c(Class) )
#y = subset(df_train, select = c(Class) )
#m <- as.matrix(X)
#anova(m)

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
samp
sort(samp)
nrow(fg_df)
df_train <- fg_df[samp,]
df_test <- fg_df[-samp,]


#Using PCA
df_train_classes <- df_train$Class
df_test_classes <- df_test$Class
# Run Principal Components Analysis
pc <- prcomp(df_train %>% select(-Class), scale = FALSE)
# Extract PCs  (e.g. 1st 3 PCs)
df_train <- tbl_df(pc$x) %>% select(PC1:PC10)
df_test <- tbl_df(predict(pc, newdata = df_test %>% select(-Class))) %>% select(PC1:PC10)
df_train$Class <- df_train_classes
df_test$Class <- df_test_classes
dim(df_train)
dim(df_test)

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
classifier(df_train, df_test, classifier_type = "rf")
classifier(df_train, df_test, classifier_type = "knn", k=1)
classifier(df_train, df_test, classifier_type = "knn", k=3)
classifier(df_train, df_test, classifier_type = "knn", k=5)
classifier(df_train, df_test, classifier_type = "knn", k=7)

cv_classification <- function(dataset, method="rf"){
  #control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
  control <- trainControl(method="repeatedcv", number=3, repeats=1, search="random")
  seed <- 1
  metric <- "Accuracy"
  set.seed(seed)
  mtry <- sqrt(ncol(dataset))
#  rf_random <- train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
  if (method == "rf" || method == "parRF"){
    model_random <- train(Class~., data=dataset, method=method, metric=metric, tuneLength=20, trControl=control)
  }
  else if(method == "lda"){
    model_random <- train(Class~., data=dataset, method=method, metric=metric, tuneLength=20, trControl=control)
  }else{
#    model_random <- train(Class ~ ., data = dataset,
#                       method = method,
#                       trControl = control,
#                       tuneGrid = expand.grid(cost = c(.25, .5, 1), weight = c(1, 5)),
#                       preProc = c("center", "scale"))  
    cctrl1 <- trainControl(method = "cv", number = 3, returnResamp = "all")
    model_random <- train(Class ~ ., data = dataset,
                                 method = method,
                                 trControl = cctrl1,
                                 tuneLength = 2,
                                 preProc = c("center", "scale"))    
    
  }
  print(model_random)
  plot(model_random)  
}
df_train_test <- rbind(df_train, df_test)
cv_classification(df_train_test, method="rf")
cv_classification(fg_df, method="rf")

cv_classification(fg_df, method="parRF")
cv_classification(fg_df, method="svmLinearWeights2")
scv_classification(fg_df, method="svmLinearWeights")
cv_classification(fg_df, method="lda")


rf_classification2 <- function(dataset){
  
  customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
  customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
  customRF$grid <- function(x, y, len = NULL, search = "grid") {}
  customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
  }
  customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata)
  customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata, type = "prob")
  customRF$sort <- function(x) x[order(x[,1]),]
  customRF$levels <- function(x) x$classes  
  
  # train model
  metric <- "Accuracy"
  control <- trainControl(method="repeatedcv", number=2, repeats=1)
  tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
  set.seed(1)
  custom <- train(Class~., data=dataset, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
  summary(custom)
  plot(custom)
}
rf_classification2(fg_df)

draw_features(df_train)
subset(df_train, select = -c(Class) )
print("end plotting")
