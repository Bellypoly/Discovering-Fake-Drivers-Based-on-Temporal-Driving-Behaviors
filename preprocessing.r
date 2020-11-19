source("setup.r")
getwd()
#file_org_dataset <- "./dataset/Driving_Data_KIA_SOUL.csv"
#"D:/Ingenio/Dataset/training/facetraining.csv"

do_min_max_normalization <- function(data_df_X){
  X<- scale(data_df_X)
  #X <- data_df[,!(names(data_df) %in% target_column)]
#  X <- as.data.frame(apply(X[, 1:ncol(X)-1], 2, function(x) (x - min(x))/(max(x)-min(x))))
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

  data_df[,!(names(data_df) %in% target_column)] <- do_min_max_normalization(data_df[,!(names(data_df) %in% target_column)])

  
  #Drop Zero columns: Some columns have zero values which are useless. They result in null values after scaling data. We drop them
  zero_columns <- colnames(data_df)[colSums(is.na(data_df)) > 0]
  print(dim(data_df))
  print(zero_columns)
  df_classes <- data_df[, target_column]
  #data_df[,!(names(data_df) %in% zero_columns)] <- data_df[,!(names(data_df) %in% zero_columns)]  
  data_df <- data_df[,!(names(data_df) %in% zero_columns)]  
  data_df$Class <- df_classes
  print(dim(data_df))
  
  return(data_df)
}




do_chunk <- function(df,chunk_size=4,overlap_ratio=0.5){
  nrows <- NROW(df)
  step_by <- ceiling(chunk_size * overlap_ratio)
  to_row <- chunk_size * floor((nrows)/(chunk_size+1))
  start <- seq(1, to_row, by= step_by )
  #return(lapply(start, function(i) x[c(i:(i+chunk_size-1)),]))
  return(lapply(start, function(i) df[c(i:(i+chunk_size-1)),]))
}

#merge two data frames
do_merge <- function(df1, df2){                                
  merge(df1, df2, by=0, all=TRUE)
}

#input: data frame
#output: list of data frames segmented according to chunk_size and overlap_ratio
do_segmentation <- function(df, selected_signals, m, classes, chunk_size=100,overlap_ratio=0.5){
  drivers_vec <- classes
  all_lists <- list()
  y <- c()
  for (i in classes){
    c_df <- subset(df, Class==i)
#    l_df <- do_chunk(c_df, chunk_size=chunk_size,overlap_ratio=overlap_ratio)
    l_df <- do_chunk(c_df[, selected_signals], chunk_size=chunk_size,overlap_ratio=overlap_ratio)
    l_y <- rep(i, length(l_df))
    y <- c(y,l_y)
    all_lists <- c(all_lists,l_df)
    cat("\n size of list", length(all_lists),"\n")
  }
  cat("\n y", y,"\n")
  cat("\n size of y", length(y),"\n")
  #return(Reduce(do_merge, all_lists))
  #return(all_lists)
  X <- matrix(unlist(all_lists), ncol = m, byrow = TRUE) 
  l_result = list("X"=X, "y"=y)
  
}

#library(dplyr)
do_class_label_count <- function(df){
  counts <- df %>% group_by(Class) %>% summarise(no_rows = length(Class))
  print(counts)
}

#################################################
#################################################
#################################################
data_df_scaled <- get_dataset_org(file_org_dataset)
dim(data_df_scaled)
#head(data_df_scaled[,!(names(data_df_scaled) %in% target_column)])

#zero_columns <- colnames(data_df_scaled)[colSums(is.na(data_df_scaled)) > 0]
#zero_columns
#head(data_df_scaled)


do_class_label_count(data_df_scaled)
#data_df_scaled %>% 
#  group_by(Class) %>%
#  summarise(no_rows = length(Class))


data_df_selected_signals <- data_df_scaled[, selected_signals_class]
dim(data_df_selected_signals)



classes <- sort(unique(data_df_selected_signals$Class))
#list_df_segmented <- do_segmentation(data_df_selected_signals, selected_signals, m, classes, chunk_size=chunk_size,overlap_ratio=overlap_ratio)
list_segmented_X_y <- do_segmentation(data_df_selected_signals, selected_signals, m, classes, chunk_size=chunk_size,overlap_ratio=overlap_ratio)
data_m <- list_segmented_X_y$X
length(selected_signals)
dim(data_m)
y <- list_segmented_X_y$y
n <- nrow(data_m)/L
n
nrow(data_m)
print("end segemetation")


