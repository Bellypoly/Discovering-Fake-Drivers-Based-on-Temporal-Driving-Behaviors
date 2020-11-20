getwd() #"/Users/hgellban
#/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code
#setwd("/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code/Discovering-Fake-Drivers-Based-on-Temporal-Driving-Behaviors")
getwd()
#setwd("C:/Users/habugell/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code/Discovering-Fake-Drivers-Based-on-Temporal-Driving-Behaviors")
setwd("/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code/Discovering-Fake-Drivers-Based-on-Temporal-Driving-Behaviors")
getwd()
file_org_dataset <- "./dataset/Driving_Data_KIA_SOUL.csv"
#dataset description: https://www.iit.cnr.it/sites/default/files/human-behavior-characterization.pdf

chunk_size <- 10#100
overlap_ratio <- 0.5#0.90#0.5

training_ratio <- 0.8#0.98#0.9 #0.8 #0.5 #0.8

W <- 2#10#2#1#2 #number of windows for each splitted sequence during feature generation

max_drivers <- 10

set.seed(1)

selected_signals_class <- c(
  "Accelerator_Pedal_value",
  #"Short_Term_Fuel_Trim_Bank1",
  #"Intake_air_pressure",
  #"Filtered_Accelerator_Pedal_value",
  #"Engine_soacking_time",
  "Acceleration_speed_._Longitudinal",
  "Indication_of_brake_switch_ON.OFF",
  #"Master_cylinder_pressure",
  "Calculated_road_gradient",
  "Acceleration_speed_._Lateral",
  "Steering_wheel_speed",
  "Steering_wheel_angle",
  "Class"
)
selected_signals_class <- c(
  "Accelerator_Pedal_value",
  #"Short_Term_Fuel_Trim_Bank1",
  #"Intake_air_pressure",
  #"Filtered_Accelerator_Pedal_value",
  #"Engine_soacking_time",
  "Acceleration_speed_._Longitudinal",
  "Indication_of_brake_switch_ON.OFF",
  #"Master_cylinder_pressure",
  "Calculated_road_gradient",
  "Acceleration_speed_._Lateral",
  "Steering_wheel_speed",
  "Steering_wheel_angle",
  "Class"
)

selected_signals_class <- c(
  "Accelerator_Pedal_value",
  "Short_Term_Fuel_Trim_Bank1",
  "Intake_air_pressure",
  #"Filtered_Accelerator_Pedal_value",
  "Engine_soacking_time",
  "Acceleration_speed_._Longitudinal",
  "Indication_of_brake_switch_ON.OFF",
  "Master_cylinder_pressure",
  "Calculated_road_gradient",
  "Acceleration_speed_._Lateral",
  "Steering_wheel_speed",
  "Steering_wheel_angle",
  "Class"
)
selected_signals <- head(selected_signals_class, -1)
selected_signals
m <- length(selected_signals)
m
L <- chunk_size
target <- c(tail(selected_signals_class, n=1))
target

library(e1071) # used for kurtosis, skewness
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

#install.packages('microbenchmark', dependencies = TRUE)
library(microbenchmark)
library(GGally)
library(randomForest)

require(caTools)

library(dplyr)

