############################################
# Program name: fg.r
# Purpose: generate the features from raw data (raw signals)
# When? run this program after signal segmentation
# How? you can run the program using RStudio by updating the constants. 
#     There is a demo random dataset

## Constants
#n <- 20 # number of driving behaviors
#m <- 10 # number of variables/ Signals
#L <- 30 #length of the signal
F <- 7 # number of feature algorithms or #features
#source("setup.r")
source("preprocessing.r")

#library(e1071) # used for kurtosis, skewness

# Fake dataset just for testing the code
get_dataset_fake <- function(n, m, L){
  input_size = n*m*L
  rnorm(input_size)
  data_m <- matrix(rnorm(input_size), ncol = m)  
}

get_dataset <- function(n, m, L){
  data_m <- get_dataset_fake(n, m, L)
  return (data_m)
}
# transform raw data using Fourier Transform. http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html
#get_fourier <- function(fourier.series, f.0, ts) {
#  w <- 2*pi*f.0
#  trajectory <- sapply(ts, function(t) fourier.series(t,w))
#  return (trajectory)
#}

# https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/fft
# Fast Discrete Fourier Transform (FFT)
get_fft <- function(data_m){
  fft_vec <- apply(data_m, 2, fft)
  fft_vec <- Re(fft_vec)  
  return (data_m)
}
# feature set generation, to generate a vector of features given a matrix
# input => m_matrix: a matrix(L,m) of a segmented raw signals, L: the length of the signal.
# output => feature_set_vec: is a vector(F*m), where F is the number of features.
generate_feature_set <- function(m_matrix){
  data_m <- m_matrix
  #data_m <- get_fft(m_matrix)
  
  #data_m <- get_fourier(function(t,w) {sin(w*t)}, 1, ts=m_matrix) 
  # we use apply with second paramater = 2, to compute a feature (e.g., mean) per column
  f1 <- apply(data_m, 2, mean) # get mean
  f2 <- apply(data_m, 2, sd) # get standard deviation
  f3 <- apply(data_m, 2, min) # get min
  f4 <- apply(data_m, 2, max) # get max
  f5 <- apply(data_m, 2, kurtosis) # get kurtosis http://www.r-tutor.com/elementary-statistics/numerical-measures/kurtosis 
  f6 <- apply(data_m, 2, skewness) # get skewness http://www.r-tutor.com/elementary-statistics/numerical-measures/skewness
  f7 <- apply(data_m, 2, mean) # get mean 
  #features_func <- c(mean, sd)# vector of function objects that will be used for generating the feature set the 
  #feature_set_vec <- 0.0
  #for(i in 1:length(features_func)){
  #  feature_set_vec <- c(feature_set_vec, features_func[i])
  #}
  feature_set_vec <- c(f1, f2, f3, f4, f5, f6, f7)
  print("feature_set_vec length")
  print(length(feature_set_vec))
  #feature_set_vec <- feature_set_vec[2:length(feature_set_vec)-1]
  return (feature_set_vec)
}

# feature generation function
# input => 
#     data: a matrix(n*L,m) of raw signals.
#     n: number of driving behaviors.
#     m: number of variables or signals.
#     L: the length of the signal.
# output => 
#     fg_df : is a dataframe(n,F*m + 1) of a feature set + its related class label.
not_used_fg_old <- function(data, n, m, L, F){
  
  #     X: is a matrix(n,F*m) of a feature set.
  X <- matrix(0.0, nrow = n, ncol = F*m)#store features in matrix
  print("dim X")
  print(dim(X))
  #     y: is the for the class labelfor the driver behavior, 
  #           where the driving behavior is a matrix(L,m).  
  y <- c(1:n)
  for (i in 1:n){
    cat("i: ", i)
    
    row_from <- (i-1)*L+1
    row_to <- i* L
    cat("\n from:", row_from, ", to:", row_to)
    X[i, ] <- generate_feature_set(data[row_from:row_to,])
    ############################################################
    #####################  Note for test only ##################
    ############################################################
    y[i] <- toString(floor((i-1)/2) + 1) # this should be changed to the actual class label
    #print(X[i, ] )
    
    #for(j in 1:m){
    #  cat(", j:", j)
    #}#for j
    print("")
  }#for i
  fg_df <- as.data.frame(X)
  fg_df$Driver <- y
  print(head(fg_df))
  print(dim(fg_df))
  return (fg_df)
}#fg function

# feature generation function
# input => 
#     data: a matrix(n*L,m) of raw signals.
#     n: number of driving behaviors.
#     m: number of variables or signals.
#     L: the length of the signal.
# output => 
#     fg_df : is a dataframe(n,F*m + 1) of a feature set + its related class label.
fg <- function(data, y, n, m, L, F){
  
  #     X: is a matrix(n,F*m) of a feature set.
  X <- matrix(0.0, nrow = n, ncol = F*m)#store features in matrix
  print("dim X")
  print(dim(X))
  #     y: is the for the class label for the driver behavior, 
  #           where the driving behavior is a matrix(L,m).  
  #y <- c(1:n)
  for (i in 1:n){
    cat("i: ", i)
    
    row_from <- (i-1)*L+1
    row_to <- i* L
    cat("\n from:", row_from, ", to:", row_to)
    X[i, ] <- generate_feature_set(data[row_from:row_to,])
    ############################################################
    #####################  Note for test only ##################
    ############################################################
    #y[i] <- toString(floor((i-1)/2) + 1) # this should be changed to the actual class label
    #print(X[i, ] )
    
    #for(j in 1:m){
    #  cat(", j:", j)
    #}#for j
    print("")
  }#for i
  fg_df <- as.data.frame(X)
  print("End creating fg_df from X")
  print(dim(fg_df))
  fg_df$Driver <- as.vector(y[1:n])#y
  fg_df$Driver <- factor(fg_df$Driver)
  print(head(fg_df))
  print(dim(fg_df))
  return (fg_df)
}#fg function
#data_m <- get_dataset(n, m, L)
dim(data_m)
n
#fg_df <- fg(data_m, y, n, m, L, F)
fg_df <- fg(data_m, y, n, m, L, F)
#target_column <- c("Driver")
#fg_df <- do_min_max_normalization(fg_df)
dim(fg_df)

length(y)
dim(fg_df)


#data_m[1:10,]
#f1 <- apply(data_m[1:10,], 2, mean) #get mean by column as second paramater = 2
#f2 <- apply(data_m[1:10,], 2, mean) #get mean by column as second paramater = 2
# apply(data_m[1:30,], 2, kurtosis)
#c(f1, f2)

#generate_feature_set(data_m[1:30,])
# matrix(1.0, nrow = 2, ncol = 3)



#x <- 1:4

#fft_vec <- fft(x)
#Re(fft_vec)
