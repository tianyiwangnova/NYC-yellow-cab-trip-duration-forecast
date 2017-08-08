library(geosphere)
library(dplyr)
library(lubridate)
train <- read.csv("Q:/Kaggle--NYC cab/train.csv",as.is=TRUE)
test  <- read.csv("Q:/Kaggle--NYC cab/test.csv",as.is=TRUE)
train <- train %>% select(-dropoff_datetime)
test$trip_duration <- -1

#Bind the data to clean the data
my_data <- rbind(train,test)

# Calculate distances between points (package geosphere)
attach(my_data)
dist=distHaversine(matrix(c(pickup_longitude, pickup_latitude), ncol = 2),
                   matrix(c(dropoff_longitude, dropoff_latitude), ncol = 2))
my_data[, 'distance_trip']=dist

#Get the time data
n <- nrow(my_data)
library(tidyr)
timedata=data.frame(pickup_datetime)
timedata=timedata%>%separate(col='pickup_datetime',c('date','time'),sep=' ')%>%
         separate(col='time',c('hour','minute'),sep=':')%>%separate(col='date',c('year','month','date'),sep="/")
library(lubridate)
fulltime=paste(timedata$year,timedata$month,timedata$date,sep="/")
wd=NULL
for (i in 1:n){
  wd=c(wd,wday(fulltime[i]))
}
wd.frame=data.frame(wd)
write.csv(wd.frame,"weekdays.csv")
timedata[,'day_of_week']=wd
###all the data related to time are stored in timedata(data frame)

#Handle categorical data "vendor_id" and "store_and_fwd_flag"
cats <- select(my_data,c("vendor_id","store_and_fwd_flag"))
for (f in colnames(cats)) {
  #cat("VARIABLE : ",f,"\n")
  levels <- unique(cats[[f]])
  cats[[f]] <- as.numeric(factor(cats[[f]], levels=levels))
}

my_data1=my_data%>%select(-c("id","pickup_datetime","passenger_count","vendor_id","store_and_fwd_flag"))
my_data1=cbind(my_data1,cats,timedata)

#delete outliers (the outliers are all in the training set)
index1=which(my_data1$trip_duration>500000)
my_data1=my_data1[-index1,]

#scale the "distance_trip" and "trip_duration"
my_data1$trip_duration=log(my_data1$trip_duration)
my_data1$distance_trip=log(my_data1$distance_trip)

write.csv(my_data1,"Q:/Kaggle--NYC cab/data1.csv",row.names = FALSE)

###my_data1 is the final result for our data cleaning this time