# Run this script after the following steps:
# 1) download original data set listed in data sources chapter
# 2) extract the compressed folder, then you should have the original data in D1NAMO folder

# provide path to your extracted folder here
folder_location <- "../D1NAMO"

# glucose measurements data for healthy group:
# excluding patient number 12 data as he is a diabetes patient
file_locations <- c(paste(folder_location,"/healthy_subset/","00",2:9,sep=""),
                    paste(folder_location,"/healthy_subset/","0",10:11,sep=""),
                    paste(folder_location,"/healthy_subset/","0",13:20,sep=""))
patient_count <- 1
healthy_df <- read.csv(paste(folder_location,"/healthy_subset/001/glucose.csv",sep=""))
healthy_df <- healthy_df[c(1:4)]
healthy_df$patient <- patient_count
for (file_path in file_locations) {
  patient_count <- patient_count + 1
  temp_df <- read.csv(paste(file_path,"glucose.csv",sep="/"))
  temp_df <- temp_df[c(1:4)]
  temp_df$patient <- patient_count
  healthy_df <- rbind(healthy_df, temp_df)
}

write.csv(healthy_df, "./data/healthy_patients_glucose.csv", row.names = FALSE)


# glucose measurements data for diabetes patients:
file_locations <- paste(folder_location,"/diabetes_subset/","00",2:9,sep="")
patient_count <- 1
diabetes_df <- read.csv(paste(folder_location,"/diabetes_subset/001/glucose.csv",sep=""))
diabetes_df <- diabetes_df[c(1:4)]
diabetes_df$patient <- patient_count
for (file_path in file_locations) {
  patient_count <- patient_count + 1
  temp_df <- read.csv(paste(file_path,"glucose.csv",sep="/"))
  temp_df <- temp_df[c(1:4)]
  temp_df$patient <- patient_count
  diabetes_df <- rbind(diabetes_df, temp_df)
}

write.csv(diabetes_df, "./data/diabetes_patients_glucose.csv", row.names = FALSE)


# reading other sensors data for diabetes patient 9 for use in this analysis
library(tidyverse)
library(lubridate)
p9_sensor_data <- read.csv(paste(folder_location,
                               "/diabetes_subset/009/sensor_data/2014_10_03-08_21_59/2014_10_03-08_21_59_Summary.csv",sep=""))
df <- p9_sensor_data[c("Time", "HR", "BR", "Activity", "ECGAmplitude")]
df <- df %>%
  mutate(Time = substr(Time, 1, 16))
df <- df %>%
  group_by(Time) %>%
  summarise(HR = max(HR), BR = max(BR),
            Activity = max(Activity), ECGAmplitude = max(ECGAmplitude))

write.csv(df, "./data/diabetes_patient9_sensor_data.csv", row.names = FALSE)
