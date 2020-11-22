source("setup.r")
getwd()
file <- "./dataset/Driving_Data_KIA_SOUL.csv"
#"D:/Ingenio/Dataset/training/facetraining.csv"
data_df_org <- read.csv(file)

#dataset description: https://www.iit.cnr.it/sites/default/files/human-behavior-characterization.pdf

str(data_df_org)
unique(data_df_org$PathOrder)
data_df <- subset(data_df_org, PathOrder==1)
drop_columns <- c("Time", "PathOrder")
target_column <- c("Class")
head(data_df)
data_df <- data_df[,!(names(data_df) %in% drop_columns)]

#data_df$Class <- factor(data_df$Class)                  
#data_df$Class
#unique(as.numeric(factor(data_df$Class)))

#replace letters with numbers
data_df$Class <- as.numeric(factor(data_df$Class))  
unique(data_df$Class)   
#head(data_df[,!(names(data_df) %in% target_column)])
sum(is.na(scale(data_df[1:10,!(names(data_df) %in% target_column)])))

#scale only X without y
data_df[,!(names(data_df) %in% target_column)]<- scale(data_df[,!(names(data_df) %in% target_column)])
#x[1:2,]
#zero_columns <- c("Filtered_Accelerator_Pedal_value", "Inhibition_of_engine_fuel_cut_off", "Fuel_Pressure", "")
#colnames(x)[colSums(is.na(x)) > 0]

#Drop Zero columns: Some columns have zero values which are useless. They result in null values after scaling data. We drop them
zero_columns <- colnames(data_df)[colSums(is.na(data_df)) > 0]
data_df <- data_df[,!(names(data_df) %in% zero_columns)]
head(data_df)
dim(data_df)
