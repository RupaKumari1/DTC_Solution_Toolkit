Main_data <- read.csv("data/Sample_Data.csv")

footfall_data <- read.csv(("data/Footfall_Data.csv"))

driver_id <- unique(footfall_data$DID)

selected_data <- footfall_data[which(footfall_data$DID == driver_id[1]),]


lat_list <- c()
long_list <- c()
label_list <- c()

len <- nrow(Main_data)
for (j in seq(1,len, 1)) {
  lat_list <- c(lat_list, Main_data$Latitude.x[j], Main_data$Latitude.y[j])
  long_list <- c(long_list, Main_data$Longitude.x[j], Main_data$Longitude.y[j])
  label_list <- c(label_list, Main_data$SeqNumber[j])
}

polydata = data.frame('lat' = lat_list, 'long' = long_list, 'label' = label_list)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

get_driver_performance_data <- function(){
  Int1<-aggregate(cbind(ShiftSeqNumber) ~ ï..DID, data = Main_data, length)
  Int2<-aggregate(cbind(Total_income,Distance,No_of_accidents) ~ ï..DID, data = Main_data, sum)
  Int3<-aggregate(cbind(Tenure,Rating,Delay_in_Pickup,Delay_in_drop) ~ ï..DID, data = Main_data, mean)
  
  
  merge1<-merge(Int1,Int2)
  merge2<-merge(merge1,Int3)
  
  merge2$Acc_per_1ktrip<-(merge2$No_of_accidents/merge2$ShiftSeqNumber)*1000
  merge2$Acc_per_1ktrip <- round(merge2$Acc_per_1ktrip, digits = 0)
  
  merge2$Inc_p1k_dis<-(merge2$Total_income/merge2$Distance)*1000
  merge2$Inc_p1k_dis <- round(merge2$Inc_p1k_dis, digits = 0)
  merge2$No_of_accidents<-NULL
 
  data_norm <- as.data.frame(lapply(merge2[2:10], min_max_norm))
  data_norm$score<-data_norm$ShiftSeqNumber+data_norm$Total_income+data_norm$Distance+data_norm$Tenure+data_norm$Rating-data_norm$Delay_in_Pickup-data_norm$Delay_in_drop-data_norm$Acc_per_1ktrip+data_norm$Inc_p1k_dis
  data_norm$rank<-rank(-data_norm$score)
  
  
  Driver_Scorecard<-cbind(merge2,data_norm$rank)
  names(Driver_Scorecard)<-c("Driver ID","Trips","Income","Total Distance","Tenure","Customer Rating","Avg Delay in Pickup","Avg Delay in drop","Accident/1000 Trip","Income/1000km","Rank")
  
  return(Driver_Scorecard)
}


raw_data = read.csv("data/Revenue_Data.csv")
raw_data$Date = strptime(raw_data$Date, "%d-%b-%y")
raw_data$f_date <- as.Date(raw_data$Date, format = "%m/%d/%y")
raw_data$month <- strftime(raw_data$f_date,"%Y-%m")
#raw_data$Predicted.Revenue = as.numeric(gsub(",","", raw_data$Predicted.Revenue))
#raw_data$Actual.Revenue = as.numeric(gsub(",","", raw_data$Actual.Revenue))

pred_agg_data <- aggregate(cbind(Predicted.Revenue)~month, data = raw_data, sum)

act_agg_data <- aggregate(cbind(Actual.Revenue)~month, data = raw_data, sum, na.rm=F, na.action = NULL)
error_agg_data <- aggregate(cbind(Error)~month, data = raw_data, sum, na.rm=F, na.action = NULL)
line_df <- merge(pred_agg_data, act_agg_data)
final_line_df <- merge(line_df, error_agg_data)



# library(dplyr)
# library(tidyverse)
#x = read.csv("data/data_rev.csv")
library(pracma)
raw_data$sevenma<-movavg(raw_data$Actual.Revenue, 7, type="s")
raw_data$thirtyma<-movavg(raw_data$Actual.Revenue, 30, type="s")


# x$sevenma<-filter(x$Predicted.Revenue, rep(1/7,7))
# raw_data$thirtyma<-filter(raw_data$Predicted.Revenue, rep(1/30,30))


