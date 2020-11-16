source("setup.r")
getwd()
#file_org_dataset <- "./dataset/Driving_Data_KIA_SOUL.csv"
#"D:/Ingenio/Dataset/training/facetraining.csv"

min_max_normalization <- function(data_df_X){
  X<- scale(data_df_X)
  #X <- data_df[,!(names(data_df) %in% target_column)]
  X <- as.data.frame(apply(X[, 1:ncol(X)-1], 2, function(x) (x - min(x))/(max(x)-min(x))))
  #data_df[,!(names(data_df) %in% target_column)] <- X
  return (X)
}
get_dataset_org <- function(file_org_dataset){
  data_df_org <- read.csv(file_org_dataset)
#  str(data_df_org)
#  unique(data_df_org$PathOrder)
  data_df <- subset(data_df_org, PathOrder==1)
  drop_columns <- c("Time", "PathOrder")
  target_column <- c("Class")
#  head(data_df)
  data_df <- data_df[,!(names(data_df) %in% drop_columns)]
  
  #replace letters with numbers
  data_df$Class <- as.numeric(factor(data_df$Class))  
  #unique(data_df$Class)   
  #head(data_df[,!(names(data_df) %in% target_column)])
  #sum(is.na(scale(data_df[1:10,!(names(data_df) %in% target_column)])))
  
#  #scale only X without y
#  #data_df[,!(names(data_df) %in% target_column)]<- scale(data_df[,!(names(data_df) %in% target_column)], scale = FALSE)
#  data_df[,!(names(data_df) %in% target_column)]<- scale(data_df[,!(names(data_df) %in% target_column)])
#  #data_df_scaled <- scale(data_df[,!(names(data_df) %in% target_column)])
#  #data_df[,!(names(data_df) %in% target_column)] <-  apply(data_df_scaled, 2, sd)
#  X <- data_df[,!(names(data_df) %in% target_column)]
#  X <- as.data.frame(apply(X[, 1:ncol(X)-1], 2, function(x) (x - min(x))/(max(x)-min(x))))
#  data_df[,!(names(data_df) %in% target_column)] <- X
  data_df[,!(names(data_df) %in% target_column)] <- min_max_normalization(data_df[,!(names(data_df) %in% target_column)])
  
  
  
  #x[1:2,]
  #zero_columns <- c("Filtered_Accelerator_Pedal_value", "Inhibition_of_engine_fuel_cut_off", "Fuel_Pressure", "")
  #colnames(x)[colSums(is.na(x)) > 0]
  
  #Drop Zero columns: Some columns have zero values which are useless. They result in null values after scaling data. We drop them
  zero_columns <- colnames(data_df)[colSums(is.na(data_df)) > 0]
  data_df <- data_df[,!(names(data_df) %in% zero_columns)]  
  
  return(data_df)
}

data_df_scaled <- get_dataset_org(file_org_dataset)
dim(data_df_scaled)
data_df_scaled[, selected_signals]
colnames(data_df_scaled)
#as.data.frame(apply(data_df_scaled[, 1:ncol(data_df_scaled)-1], 2, function(x) (x - min(x))/(max(x)-min(x))))
# check that we get mean of 0 and sd of 1
colMeans(data_df_scaled)  # faster version of apply(scaled.dat, 2, mean)
#apply(data_df_scaled, 2, sd)

#http://www.sthda.com/english/wiki/one-way-anova-test-in-r
#Anova
aov(Class ~ ., data = data_df_scaled)

#library(preprocessCore)
#normalize(data_df_scaled)
#normalize(x, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
data_df_scaled[,]
chisq <- chisq.test(data_df_scaled[,!(names(data_df_scaled) %in% target_column)])
# Observed counts
chisq$observed
round(chisq$expected,2)
round(chisq$residuals, 3)
library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

disc<-"equal interval width"
X <- data_df_scaled[,!(names(data_df_scaled) %in% target_column)]
select.inf.chi2(X,disc.method=disc,attrs.nominal=attrs.nominal)


# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(Class~., data=data_df_scaled, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)