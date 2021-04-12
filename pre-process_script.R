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

#reading data pertaining to diet 

datalist = list()
for(i in 1:9) {
  path <- paste0(folder_location,"/diabetes_subset/00",i, "/food.csv")
  
  food_data<-data.frame(read.csv(path,header=T,sep=','))
  balance_food<-factor(food_data$balance,c('Unbalance','Balance'),labels=c('Unbalance','Balance'))
  food_table<-table(balance_food)
  food_table <- addmargins(food_table) 
  df <- as.data.frame(food_table) 
  df$Freq <- df$Freq/df[3,2]
  df <- df[-nrow(df),]
  labels <- 'Diabetes'
  df <- cbind(df, Person = labels)
  df$i <- i  
  datalist[[i]] <- df
}

final_data_dia= do.call(rbind, datalist)

datalist_healthy = list()
for(i in 1:20) {
  if (i<10) {
    path <- paste0(folder_location,"/healthy_subset/00",i, "/food.csv")
  }
  else {
    path <- paste0(folder_location,"/healthy_subset/0",i, "/food.csv")
  }
  
  food_data<-data.frame(read.csv(path,header=T,sep=','))
  balance_food<-factor(food_data$balance,c('Unbalance','Balance'),labels=c('Unbalance','Balance'))
  food_table<-table(balance_food)
  food_table <- addmargins(food_table) 
  df <- as.data.frame(food_table) 
  df$Freq <- df$Freq/df[3,2]
  df <- df[-nrow(df),]
  labels <- 'Healthy'
  df <- cbind(df, Person = labels)
  df$i <- i  
  datalist_healthy[[i]] <- df
}

final_data_healthy= do.call(rbind, datalist_healthy)

df_new_healthy <- as.data.frame(aggregate(final_data_healthy$Freq, by=list(Category=final_data_healthy$balance_food), FUN=sum))
labels <- 'Healthy'
df_new_healthy <- cbind(df_new_healthy, Person = labels)
df_new_healthy$x<- (df_new_healthy$x/20)*100

df_new_dia = as.data.frame(aggregate(final_data_dia$Freq, by=list(Category=final_data_dia$balance_food), FUN=sum))
labels <- 'Diabetes'
df_new_dia <- cbind(df_new_dia, Person = labels)
df_new_dia$x<- (df_new_dia$x/9)*100


food_final_df <- rbind(df_new_dia,df_new_healthy)
names(food_final_df)[2] <- "Freq"
food_final_df <- rapply(food_final_df, as.character, classes="factor", how="replace")
write.csv(food_final_df, "./data/individuals_food_balanced_unbalanced.csv", row.names = FALSE)

# reading data regarding glucose level and calorie intake 

datalist = list()
for(i in 1:9) {
  path <- paste0(folder_location,"/diabetes_subset/00",i, "/food.csv")
  food_data<-data.frame(read.csv(path,header=T,sep=','))
  food_data <-  food_data[!is.na(food_data$calories), ]
  food_data$balance <- gsub("Unbalance", 0, food_data$balance)
  food_data$balance <- gsub("Balance", 1, food_data$balance)
  food_data$balance <- as.numeric(food_data$balance)
  food_quality <- c(mean(food_data$balance))
  cal <- c(mean(food_data$calories))
  person <- c(i)
  df <- data.frame(person,cal,food_quality)
  df$i <- i  
  datalist[[i]] <- df
}

final_data_dia_cal= do.call(rbind, datalist)

datalist = list()
for(i in 1:9) {
  path <- paste0(folder_location,"/diabetes_subset/00",i, "/glucose.csv")
  food_data<-data.frame(read.csv(path,header=T,sep=','))
  food_data <-  food_data[!is.na(food_data$glucose), ]
  cal <- c(mean(food_data$glucose))
  person <- c(i)
  df <- data.frame(person,cal)
  df$i <- i  
  datalist[[i]] <- df
}

final_data_dia_glu= do.call(rbind, datalist)
final_data_glu_cal_dia = merge(x = final_data_dia_glu, y = final_data_dia_cal, by = "person")
keeps <- c("person", "cal.x", "cal.y","food_quality")
final_data_glu_cal_dia = final_data_glu_cal_dia[keeps]
names(final_data_glu_cal_dia)[2] <- "glucose"
names(final_data_glu_cal_dia)[3] <- "calories"
labels <- 'Diabetes'
final_data_glu_cal_dia <- cbind(final_data_glu_cal_dia, Person = labels)

#Healthy

datalist = list()
for(i in 1:20) {
  if (i<10) {
    path <- paste0(folder_location,"/healthy_subset/00",i, "/food.csv")
  }
  else {
    path <- paste0(folder_location,"/healthy_subset/0",i, "/food.csv")
  }
  food_data<-data.frame(read.csv(path,header=T,sep=','))
  food_data$calories <- as.numeric(food_data$calories)
  food_data <-  food_data[!is.na(food_data$calories), ]
  food_data$balance <- gsub("Unbalance", 0, food_data$balance)
  food_data$balance <- gsub("Balance", 1, food_data$balance)
  food_data$balance <- as.numeric(food_data$balance)
  food_quality <- c(mean(food_data$balance))
  cal <- c(mean(food_data$calories))
  person <- c(i)
  df <- data.frame(person,cal,food_quality)
  df$i <- i  
  datalist[[i]] <- df
}
final_data_healthy_cal= do.call(rbind, datalist)

datalist = list()
for(i in 1:20) {
  if (i<10) {
    path <- paste0(folder_location,"/healthy_subset/00",i, "/glucose.csv")
  }
  else {
    path <- paste0(folder_location,"/healthy_subset/0",i, "/glucose.csv")
  }
  food_data<-data.frame(read.csv(path,header=T,sep=','))
  food_data$glucose <- as.numeric(food_data$glucose)
  food_data <-  food_data[!is.na(food_data$glucose), ]
  cal <- c(mean(food_data$glucose))
  person <- c(i)
  df <- data.frame(person,cal)
  df$i <- i  
  datalist[[i]] <- df
}

final_data_healthy_glu= do.call(rbind, datalist)
final_data_glu_cal_healthy = merge(x = final_data_healthy_glu, y = final_data_healthy_cal, by = "person")
keeps <- c("person", "cal.x", "cal.y","food_quality")
final_data_glu_cal_healthy = final_data_glu_cal_healthy[keeps]
names(final_data_glu_cal_healthy)[2] <- "glucose"
names(final_data_glu_cal_healthy)[3] <- "calories"
labels <- 'Healthy'
final_data_glu_cal_healthy <- cbind(final_data_glu_cal_healthy, Person = labels)
food_glu_final_df <- rbind(final_data_glu_cal_healthy,final_data_glu_cal_dia)
food_glu_final_df$food_quality[is.na(food_glu_final_df$food_quality)] <- 0.5
food_glu_final_df <-food_glu_final_df[food_glu_final_df$person != 12, ]
write.csv(food_glu_final_df, "./data/individuals_glucose_food_intake.csv", row.names = FALSE)

## getting data for D3 interactive plot:

# food information for healthy group:
# excluding patient number 12 data as he is a diabetes patient
file_locations <- c(paste(folder_location,"/healthy_subset/","00",2:9,sep=""),
                    paste(folder_location,"/healthy_subset/","0",10:11,sep=""),
                    paste(folder_location,"/healthy_subset/","0",13:20,sep=""))
patient_count <- 1
healthy_df2 <- read.csv(paste(folder_location,"/healthy_subset/001/food.csv",sep=""), na.strings="")
healthy_df2 <- healthy_df2[c(1,2,4,5,6,7)]
healthy_df2$patient <- patient_count
for (file_path in file_locations) {
  patient_count <- patient_count + 1
  temp_df <- read.csv(paste(file_path,"food.csv",sep="/"), na.strings="")
  temp_df <- temp_df[c(1,2,4,5,6,7)]
  temp_df$patient <- patient_count
  healthy_df2 <- rbind(healthy_df2, temp_df)
}

healthy_df2$datetime = paste(healthy_df2$date, healthy_df2$time)
healthy_df2 <- healthy_df2[c("patient","datetime","description","calories","balance","quality")]
healthy_df2$calories <- as.numeric(healthy_df2$calories)

# getting glucoses values for healthy patients
healthy_glucose <- read.csv('./data/healthy_patients_glucose.csv', na.strings = "")
healthy_glucose$datetime = paste(healthy_glucose$date, healthy_glucose$time)
healthy_glucose$glucose <- as.numeric(healthy_glucose$glucose)
healthy_glucose <- healthy_glucose[c("patient","datetime","glucose")]
healthy_glucose <- na.omit(healthy_glucose)

write.csv(healthy_glucose, "./data/d3_plot_glucose.csv", row.names = FALSE)
write.csv(healthy_df2, "./data/d3_plot_food.csv", row.names = FALSE)

# getting glucose values for diabetes patients
diabetes_glucose <- read.csv('./data/diabetes_patients_glucose.csv', na.strings = "")
diabetes_glucose$datetime = paste(diabetes_glucose$date, diabetes_glucose$time)
diabetes_glucose$glucose <- as.numeric(diabetes_glucose$glucose)
diabetes_glucose <- diabetes_glucose[c("patient","datetime","glucose")]

# getting insulin values for diabetes patients
file_locations <- paste(folder_location,"/diabetes_subset/","00",2:9,sep="")
patient_count <- 1
diabetes_df2 <- read.csv(paste(folder_location,"/diabetes_subset/001/insulin.csv",sep=""))
diabetes_df2 <- diabetes_df2[c(1:3)]
diabetes_df2$patient <- patient_count
for (file_path in file_locations) {
  patient_count <- patient_count + 1
  temp_df <- read.csv(paste(file_path,"insulin.csv",sep="/"))
  temp_df <- temp_df[c(1:3)]
  temp_df$patient <- patient_count
  diabetes_df2 <- rbind(diabetes_df2, temp_df)
}
diabetes_df2$datetime = paste(diabetes_df2$date, diabetes_df2$time)
diabetes_df2 <- diabetes_df2[c("patient","datetime","fast_insulin")]
diabetes_df2 <- na.omit(diabetes_df2)

write.csv(diabetes_glucose, "./data/d3_plot_diabetes_glucose.csv", row.names = FALSE)
write.csv(diabetes_df2, "./data/d3_plot_diabetes_insulin.csv", row.names = FALSE)
