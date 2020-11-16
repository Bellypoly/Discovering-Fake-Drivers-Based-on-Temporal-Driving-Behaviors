getwd() #"/Users/hgellban
#/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code
setwd("/Users/hgellban/Documents/111_one_drive/OneDrive - Texas Tech University/ttu-cs/2020fall/programming_with_R/project/code/Discovering-Fake-Drivers-Based-on-Temporal-Driving-Behaviors")
file_org_dataset <- "./dataset/Driving_Data_KIA_SOUL.csv"
#dataset description: https://www.iit.cnr.it/sites/default/files/human-behavior-characterization.pdf


selected_signals <- c(
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
  "Steering_wheel_angle"
)
