library(keras)
####library(reticulate)
####py_install("SciPy")
py_install("numpy")
library(reticulate)

# create a new environment 
conda_create("r-reticulate")

# install SciPy
conda_install("r-reticulate", "scipy")

# indicate that we want to use a specific condaenv
####use_condaenv(condaenv = NULL, conda = "auto", required = FALSE)
use_condaenv("r-reticulate")
reticulate::conda_install("keras")
# import SciPy (it will be automatically discovered in "r-reticulate")
scipy <- import("scipy")
numpy <- import("numpy")
# Parameters --------------------------------------------------------------

batch_size <- 32
epochs <- 200
data_augmentation <- TRUE


# Data Preparation --------------------------------------------------------

# See ?dataset_cifar10 for more info
cifar10 <- dataset_cifar10()

# Feature scale RGB values in test and train inputs  
x_train <- cifar10$train$x/255
x_test <- cifar10$test$x/255
#y_train <- to_categorical(cifar10$train$y, 10)
y_train <- to_categorical(cifar10$train$y, num_classes = 10)
y_test <- to_categorical(cifar10$test$y, num_classes = 10)

dim(x_train)
length(cifar10$train$y)
dim(x_test)
# Defining Model ----------------------------------------------------------

# Initialize sequential model
model <- keras_model_sequential()

model %>%
  
  # Start with hidden 2D convolutional layer being fed 32x32 pixel images
  layer_conv_2d(
    filter = 32, kernel_size = c(3,3), padding = "same", 
    input_shape = c(32, 32, 3)
  ) %>%
  layer_activation("relu") %>%
  
  # Second hidden layer
  layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # 2 additional hidden 2D convolutional layers
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same") %>%
  layer_activation("relu") %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3)) %>%
  layer_activation("relu") %>%
  
  # Use max pooling once more
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # Flatten max filtered output into feature vector 
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(512) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%
  
  # Outputs from dense layer are projected onto 10 unit output layer
  layer_dense(10) %>%
  layer_activation("softmax")

opt <- optimizer_rmsprop(lr = 0.0001, decay = 1e-6)

model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = opt,
  metrics = "accuracy"
)


# Training ----------------------------------------------------------------

if(!data_augmentation){
  
  model %>% fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = epochs,
    validation_data = list(x_test, y_test),
    shuffle = TRUE
  )
  
} else {
  
  datagen <- image_data_generator(
    rotation_range = 20,
    width_shift_range = 0.2,
    height_shift_range = 0.2,
    horizontal_flip = TRUE
  )
  
  datagen %>% fit_image_data_generator(x_train)
  
  model %>% fit_generator(
    flow_images_from_data(x_train, y_train, datagen, batch_size = batch_size),
    steps_per_epoch = as.integer(50000/batch_size), 
    epochs = epochs, 
    validation_data = list(x_test, y_test)
  )
  
}





# Download data -----------------------------------------------------------

download.file(
  "https://download.microsoft.com/download/3/E/1/3E1C3F21-ECDB-4869-8368-6DEBA77B919F/kagglecatsanddogs_3367a.zip", 
  destfile = "cats-dogs.zip"
)

# Pre-processing ----------------------------------------------------------

zip::unzip("cats-dogs.zip", exdir = "data-raw")

# We will organize images in the following structure:
# data/
#     train/
#          Cat/
#          Dog/
#     validation
#          Cat/
#          Dog/
#     test/
#          images/
#

all_imgs <- fs::dir_ls(
  "data-raw/PetImages/", 
  recursive = TRUE, 
  type = "file",
  glob = "*.jpg"
)

# some images are corrupt and we exclude them
# this will make sure all images can be read.
for (im in all_imgs) {
  out <- try(magick::image_read(im), silent = TRUE)
  if (inherits(out, "try-error")) {
    fs::file_delete(im)
    message("removed image: ", im)
  }
}

# re-list all imgs
all_imgs <- fs::dir_ls(
  "data-raw/PetImages/", 
  recursive = TRUE, 
  type = "file",
  glob = "*.jpg"
)

set.seed(5)

training_imgs <- sample(all_imgs, size = length(all_imgs)/2)
validation_imgs <- sample(all_imgs[!all_imgs %in% training_imgs], size = length(all_imgs)/4)         
testing_imgs <- all_imgs[!all_imgs %in% c(training_imgs, validation_imgs)]

# create directory structure
fs::dir_create(c(
  "data/train/Cat",
  "data/train/Dog",
  "data/validation/Cat",
  "data/validation/Dog",
  "data/test/images"
))

# copy training images
fs::file_copy(
  path = training_imgs, 
  new_path = gsub("data-raw/PetImages", "data/train", training_imgs)
)

# copy valid images
fs::file_copy(
  path = validation_imgs, 
  new_path = gsub("data-raw/PetImages", "data/validation", validation_imgs)
)

# copy testing imgs
fs::file_copy(
  path = testing_imgs,
  new_path = gsub("data-raw/PetImages/(Dog|Cat)/", "data/test/images/\\1", testing_imgs)
)

# Image flow --------------------------------------------------------------

library(keras)

training_image_gen <- image_data_generator(
  rotation_range = 20,
  width_shift_range = 0.2,
  height_shift_range = 0.2,
  horizontal_flip = TRUE,
  preprocessing_function = imagenet_preprocess_input
)

validation_image_gen <- image_data_generator(
  preprocessing_function = imagenet_preprocess_input
)

training_image_flow <- flow_images_from_directory(
  directory = "data/train/", 
  generator = training_image_gen, 
  class_mode = "binary",
  batch_size = 100,
  target_size = c(224, 224), 
)

validation_image_flow <- flow_images_from_directory(
  directory = "data/validation/", 
  generator = validation_image_gen, 
  class_mode = "binary",
  batch_size = 100,
  target_size = c(224, 224), 
  shuffle = FALSE
)

# Model -------------------------------------------------------------------

mob <- application_mobilenet(include_top = FALSE, pooling = "avg")
freeze_weights(mob)

model <- keras_model_sequential() %>% 
  mob() %>% 
  layer_dense(256, activation = "relu") %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% 
  compile(loss = "binary_crossentropy", optimizer = "adam", metrics = "accuracy")

model %>% fit_generator(
  generator = training_image_flow, 
  epochs = 1, 
  steps_per_epoch = training_image_flow$n/training_image_flow$batch_size,
  validation_data = validation_image_flow,
  validation_steps = validation_image_flow$n/validation_image_flow$batch_size
)

# now top layers weights are fine, we can unfreeze the lower layer weights.
unfreeze_weights(mob)

model %>% 
  compile(loss = "binary_crossentropy", optimizer = "adam", metrics = "accuracy")

model %>% fit_generator(
  generator = training_image_flow, 
  epochs = 3, 
  steps_per_epoch = training_image_flow$n/training_image_flow$batch_size,
  validation_data = validation_image_flow,
  validation_steps = validation_image_flow$n/validation_image_flow$batch_size
)

# Generate predictions for test data --------------------------------------

test_flow <- flow_images_from_directory(
  generator = validation_image_gen,
  directory = "data/test", 
  target_size = c(224, 224),
  class_mode = NULL,
  shuffle = FALSE
)

predictions <- predict_generator(
  model, 
  test_flow,
  steps = test_flow$n/test_flow$batch_size
)

magick::image_read(testing_imgs[1])
predictions[1]

magick::image_read(testing_imgs[6250])
predictions[6250]