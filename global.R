# Shiny Global
# DTC Solution Toolkit - This analytical framework will facilitate  to improve DTC to increase customers
# Rupa Kumari & Md Mehran Abul
# Date: July 5, 2021 

#global.R

#===============================================================================
#                               SHINYGLOBAL                                    #
#===============================================================================


# Installing required packages (No need to install if already done)
# install.packages("forecast")
# install.packages("tseries")
# install.packages("ggplot2")
# install.packages("pracma")
# install.packages("TSA")
# install.packages("data.table")

#Load packages and modules
library(forecast)
library(tseries)
library(ggplot2)
library(pracma)
library(TSA)
require(data.table)

######## Loading full data ##############
unzip("trips_2021_full.zip") 
DT <- fread("trips_2021_full.csv")

DT<-DT[,-c(1,3,4,5,9,10,20,21,23,29,30,31,34,35,37,39,45,55,56,63,64,67,68,69)]
DT$Total_Income=DT$FlagfallIncome+DT$JobIncomeMeterOff+DT$ExtrasIncome+DT$DistanceIncome+DT$WaitingIncome+DT$`FlagfallIncome (#1)`+DT$`JobIncomeMeterOff (#1)`+DT$`ExtrasIncome (#1)`+DT$`DistanceIncome (#1)`+DT$`WaitingIncome (#1)`

############ Driver Performance ################
DT$Distance<-DT$`Odometer (#1)`- DT$Odometer

agg_trip<-aggregate(cbind(ShiftSeqNumber) ~ DID, data = DT, length)
agg_inc_dis<-aggregate(cbind(Total_Income,Distance) ~ DID, data = DT, sum)
agg_data<-merge(agg_trip,agg_inc_dis)

agg_data$Income_per_1Kdis<-round((agg_data$Total_Income/agg_data$Distance)*1000,2)

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

data_norm <- as.data.frame(lapply(agg_data[2:5], min_max_norm))
data_norm$score<-data_norm$ShiftSeqNumber+data_norm$Total_Income+data_norm$Distance+data_norm$Income_per_1Kdis
data_norm$rank<-rank(-data_norm$score)

Driver_Scorecard<-cbind(agg_data,data_norm$rank)
names(Driver_Scorecard)[6]<-"Rank"
#Driver_Scorecard <- read.csv("data/Driver_Scorecard.csv")
Driver_Scorecard <- Driver_Scorecard[order(Driver_Scorecard$Rank, decreasing = FALSE),]  
table_data <- Driver_Scorecard

rm(data_norm)
rm(agg_data)
rm(agg_trip)
rm(agg_inc_dis)


############ Revenue Analysis ################
DT$Hour<-as.numeric(substr(DT$DateTimeLogged,12,13))
DT$Date<-trimws(substr(DT$DateTimeLogged,1,10))
DT$Date1<-as.Date(DT$Date,format = "%Y-%m-%d")
min_month <- strftime(min(DT$Date1),"%b %y")
max_month <- strftime(max(DT$Date1),"%b %y")
Revenue_Data<-aggregate(cbind(DT$Total_Income)~Date1, data=DT, sum)

names(Revenue_Data)<-c("Date","Total_Income")
income=ts(Revenue_Data$Total_Income,start = min(Revenue_Data$Date),frequency=7)
fit2 <- hw(income, seasonal = "additive")

Revenue_Data[,"Forecast_Income"] <- NA
for (i in 1:length(fit2$mean)){
  Revenue_Data[nrow(Revenue_Data)+1, ]$Date<-c(as.character(max(Revenue_Data$Date)+1))
}

Revenue_Data$Forecast_Income<-c(fit2$fitted,fit2$mean)
Revenue_Data$Error<-((Revenue_Data$Total_Income-Revenue_Data$Forecast_Income)/Revenue_Data$Total_Income)*100

Revenue_Data$Sevenma<-movavg(Revenue_Data$Total_Income, 7, type="s")  
Revenue_Data$Thirtyma<-movavg(Revenue_Data$Total_Income, 30, type="s")

#Revenue_Data = read.csv("data/Revenue_Data.csv")
Revenue_Data$f_date <- as.Date(Revenue_Data$Date, format = "%m/%d/%Y")
Revenue_Data$month <- strftime(Revenue_Data$f_date,"%Y-%m")


############ Trip Recommendation ################
AGG_data<-aggregate(cbind(DT$ShiftSeqNumber)~Date1+Hour+Area, data=DT, length)
names(AGG_data)<-c("Date", "Hour", "Area","Trips")

lat_long<-aggregate(cbind(Latitude, Longitude)~Area,data=DT,mean)

rm(DT)
AGG_data<-AGG_data[with(AGG_data,order(Date,Hour,Area)),]

Agg_summ<-aggregate(cbind(Date)~Area,data=AGG_data,length)
Agg_summ2<-aggregate(cbind(Trips)~Area,data=AGG_data,sum)
Agg_sum_fin<-merge(Agg_summ,Agg_summ2)
Agg_sum_sel<-Agg_sum_fin[Agg_sum_fin$Date==2160,]

Trip_Recommendation <- data.frame(Date=character(0),Area= character(0), Hour= character(0),Trip_forecast=character(0),stringsAsFactors=FALSE)

for(i in 1:length(Agg_sum_sel$Area)){
  for (j in 0:23){
    test<-AGG_data[(AGG_data$Hour==j & AGG_data$Area==(Agg_sum_sel$Area)[i]),]
    trips=ts(test$Trips,start = min(test$Date),frequency=7)
    fit <- hw(trips, seasonal = "additive")
    
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+1),Agg_sum_sel$Area[i], j,fit$mean[1])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+2),Agg_sum_sel$Area[i], j,fit$mean[2])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+3),Agg_sum_sel$Area[i], j,fit$mean[3])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+4),Agg_sum_sel$Area[i], j,fit$mean[4])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+5),Agg_sum_sel$Area[i], j,fit$mean[5])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+6),Agg_sum_sel$Area[i], j,fit$mean[6])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+7),Agg_sum_sel$Area[i], j,fit$mean[7])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+8),Agg_sum_sel$Area[i], j,fit$mean[8])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+9),Agg_sum_sel$Area[i], j,fit$mean[9])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+10),Agg_sum_sel$Area[i], j,fit$mean[10])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+11),Agg_sum_sel$Area[i], j,fit$mean[11])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+12),Agg_sum_sel$Area[i], j,fit$mean[12])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+13),Agg_sum_sel$Area[i], j,fit$mean[13])
    Trip_Recommendation[nrow(Trip_Recommendation)+1, ]<- c(as.character(max(test$Date)+14),Agg_sum_sel$Area[i], j,fit$mean[14])
    
    states <- fit$model$states[,1:3]
    colnames(states) <- cbind('Level','Trend','Seasonality')
  }
}

Trip_Recommendation<-merge(Trip_Recommendation,lat_long)
Trip_Recommendation$Hour <- as.integer(Trip_Recommendation$Hour)
Trip_Recommendation$Trip_forecast <- as.integer(Trip_Recommendation$Trip_forecast)

#Trip_Recommendation <- read.csv("data/Trip_Recommendation.csv")
Trip_Recommendation$Date <- as.Date(Trip_Recommendation$Date, format = "%Y-%m-%d")

select_date = sort(unique(Trip_Recommendation$Date))
select_hour = sort(unique(Trip_Recommendation$Hour))

########## Data for dashboard page ###########

total_trip = sum(Driver_Scorecard$ShiftSeqNumber, na.rm = TRUE)
total_trip_ui = formatC(total_trip, format="f", big.mark = ",", digits=0)

actual_income = sum(Revenue_Data$Total_Income, na.rm = TRUE)
total_income = round(actual_income/1000000, 2)  
rev_per_trip = round(actual_income/total_trip, 2)
donut_title = paste0(round(total_income, 0)," AED(in Mn) Above Target")
target = 450 ####### Assumption in Million
temp = round((total_income/target)*100, 0)
donut_text = paste0(temp, "% Target Met")

seven_days_ma = round(Revenue_Data$Sevenma[length(na.omit(Revenue_Data$Sevenma))], digits = 0)
seven_days_ui = formatC(seven_days_ma, format="f", big.mark = ",", digits=0)

thirty_days_ma = round(Revenue_Data$Thirtyma[length(na.omit(Revenue_Data$Thirtyma))], digits = 0)
thirty_days_ui = formatC(thirty_days_ma, format="f", big.mark = ",", digits=0) 

