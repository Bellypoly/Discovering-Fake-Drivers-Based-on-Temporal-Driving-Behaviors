# # SEQUENCE #
# 1. setup.r
# 2. preprocessing.r
# 3. fg.r
# 4. classifier

getwd()
setwd("/Users/suwaphit/Desktop/R/Discovering-Fake-Drivers-Based-on-Temporal-Driving-Behaviors")
file_org_dataset <- "./dataset/Driving_Data_KIA_SOUL.csv"
#dataset description: https://www.iit.cnr.it/sites/default/files/human-behavior-characterization.pdf

#segmentation
chunk_size <- 60#10#20#30#60
overlap_ratio <- 0.5#0.5#0.90#0.5

training_ratio <- 0.8#0.98#0.9 #0.8 #0.5 #0.8

W <-2#1#2#5#6 #number of windows for each splitted sequence during feature generation

max_drivers <- 10 #2#4#6#10
is_drop_high_correlated <- TRUE#FALSE

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
# signal description: https://www.iit.cnr.it/sites/default/files/human-behavior-characterization.pdf
#according to Random Forest feature selection
selected_signals_class <- c(
  "Engine_soacking_time",
  "Long_Term_Fuel_Trim_Bank1",
  "Engine_coolant_temperature.1",
  "Torque_of_friction",
  "Maximum_indicated_engine_torque",
  "Intake_air_pressure",
  "Calculated_road_gradient",
  "Steering_wheel_angle",
  "Flywheel_torque",
  "Acceleration_speed_._Lateral",
  "Flywheel_torque_.after_torque_interventions.",  
  "Class"
)
selected_signals <- head(selected_signals_class, -1)
selected_signals
m <- length(selected_signals)
m
L <- chunk_size
target <- c(tail(selected_signals_class, n=1))
target

#not used as they are discovered through program
vector_of_zero_signals <- c(
  "Filtered_Accelerator_Pedal_value"
  ,"Inhibition_of_engine_fuel_cut_off"         
  ,"Fuel_Pressure" 
  ,"Torque_scaling_factor.standardization."    
  ,"Standard_Torque_Ratio"
  ,"Requested_spark_retard_angle_from_TCU"     
  ,"Target_engine_speed_used_in_lock.up_module"
  ,"Glow_plug_control_request"    
)
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

#library (ROCR)

#for gradient
library(pracma)
