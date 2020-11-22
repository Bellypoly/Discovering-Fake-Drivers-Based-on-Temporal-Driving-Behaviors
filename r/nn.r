
##install.packages("devtools", type = "win.binary")#for windows
install.packages("devtools", type = "mac.binary")#for mac
devtools::install_github("rstudio/keras", force = TRUE)
# Load the necessary packages
library(keras)
library(tensorflow)
###use_condaenv(condaenv = NULL, conda = "auto", required = FALSE)

use_python(python, required = FALSE)
use_virtualenv(virtualenv = NULL, required = FALSE)
use_condaenv(condaenv = NULL, conda = "auto", required = FALSE)
use_miniconda(condaenv = NULL, required = FALSE)


LOOPBACK = 240 #length of series in each sample
N_FILES = 1000 #number of samples
PROB_CLASS_1 = 0.55
SPLT = 0.8 #80% train, 20% test
X = array(0.0, dim=c(N_FILES, LOOPBACK))  
Y = array(0, dim=N_FILES) #time series class

for(fl in 1:N_FILES)
{
  z = rbinom(1, 1, PROB_CLASS_1)
  if(z==1)
    X[fl, ] = cumprod(1.0 + rnorm(LOOPBACK, 0.0, 0.01))
  else
    X[fl, ] = exp(rnorm(LOOPBACK, 0.0, 0.05))
  
  X[fl, ] = X[fl, ] / max(X[fl,]) #rescale
  Y[fl] = z
}

b = floor(SPLT*N_FILES)
x_train = X[1:b,]
x_test = X[(b+1):N_FILES,]
y_train = to_categorical(Y[1:b], 2)
y_test = to_categorical(Y[(b+1):N_FILES], 2)

model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 160, activation = 'relu', input_shape = c(LOOPBACK)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 80, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 2, activation = 'softmax')
summary(model)
model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)
history <- model %>% fit(
  x_train, y_train, 
  epochs = 200, batch_size = 11, 
  validation_split = 0.2
)
plot(history)
model %>% evaluate(x_test, y_test)



do_cut_X <- function(X,batch_size) {
  X <- X[1:(nrow(X) %/% batch_size * batch_size),]
  print("do_cut_X new shape")
  print(dim(X))
  return(X)
}
do_reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  return(X)
}

create_model <- function(x_train, y_train, x_test, y_test){
  batch_size = 20                 # must be a common factor of both the train and test samples
  # Reshape the input to 3-dim
  #dim(x_train) <- c(dim(x_train)[1], dim(x_train)[2], 1)
  #dim(x_test) <- c(dim(x_test)[1], dim(x_test)[2], 1)
  x_train <- do_cut_X(x_train,batch_size)
  x_test <- do_cut_X(x_test,batch_size)
  print("dataset dim after cutting")
  print(dim(x_train))
  print(dim(x_test))

    x_train <- do_reshape_X_3d(x_train)
  x_test <- do_reshape_X_3d(x_test)
  print("dataset dim after reshaping to 3d")
  print(dim(x_train))
  print(dim(x_test))
  

  
  # specify required arguments
  X_shape2 = dim(x_train)[2]
  X_shape3 = dim(x_train)[3]
  n_classes = dim(y_train)[2]    # number of classes
  
  #=========================================================================================
  
  model <- keras_model_sequential() 
  model%>%
    layer_lstm(units=1, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE, activation = 'tanh')%>%
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = n_classes)  
  
#  model %>% compile(
#    loss = 'mean_squared_error',
#    optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
#    metrics = c('accuracy')
#  )
    
  print(summary(model))
  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_rmsprop(),
    metrics = c('accuracy')
  )  
  Epochs = 2   
  #for(i in 1:Epochs ){
  #  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  #  model %>% reset_states()
  #}
  
  
  history <- model %>% fit(
    x_train, y_train, 
    epochs = Epochs, batch_size = batch_size, 
    validation_split = 0.2
  )
  print("Finish training NN")
  plot(history)
  print("Finish ploting NN, train and test dim")
  print(dim(x_train))
  print(dim(x_test))
  model %>% evaluate(x_test, y_test)  
}
dim(x_train)
#dim(y_train)
#dim(x_test)
#dim(y_test)
#typeof(x_train)
create_model(x_train, y_train, x_test, y_test)

