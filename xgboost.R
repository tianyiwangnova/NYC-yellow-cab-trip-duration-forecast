library(data.table)
library(geosphere)
library(dplyr)
library(lubridate)
library(xgboost)

rm(list=ls())
gc()

first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}

# Load Data 

train <- read.csv("Q:/Kaggle--NYC cab/train.csv",as.is=TRUE)
test  <- read.csv("Q:/Kaggle--NYC cab/test.csv",as.is=TRUE)
train <- train %>% select(-dropoff_datetime)
test$trip_duration <- -1

# Bind Train&Test for preprocessing
my_data2 <- rbind(train,test)

# Calculate distances between points (package geosphere)
attach(my_data)
dist=distHaversine(matrix(c(pickup_longitude, pickup_latitude), ncol = 2),matrix(c(dropoff_longitude, dropoff_latitude), ncol = 2))
my_data2[, 'distance_trip']=dist

# Preprocessing
attach(my_data)

# n : total rows used to create empty dataframe
n <- nrow(my_data)

# Date stuff

library(tidyr)
timedata=data.frame(pickup_datetime)
timedata=timedata%>%separate(col='pickup_datetime',c('date','time'),sep=' ')%>%separate(col='time',c('hour','minute'),sep=':')%>%separate(col='date',c('year','month','date'),sep="/")
library(lubridate)
fulltime=paste(timedata$year,timedata$month,timedata$date,sep="/")
wd=NULL
for (i in 80001:1673709){
  wd=c(wd,wday(fulltime[i]))
}
wd.frame=data.frame(wd)
write.csv(wd.frame,"weekdays.csv")
timedata[,'day_of_week']=wd

quantile(my_data$trip_duration,c(0.99,0.9999))
quantile(my_data$distance,c(0.99,0.995))

index1=which(my_data2$trip_duration>500000)
my_data=my_data[-index1,]

my_data$trip_duration=log(my_data$trip_duration)

timedata=timedata[-index1,]
  
plot(my_data$distance_trip)
my_data1=cbind(my_data1,timedata)

# categorical as factor
cats <- select(my_data,c("vendor_id","store_and_fwd_flag"))
for (f in colnames(cats)) {
  #cat("VARIABLE : ",f,"\n")
  levels <- unique(cats[[f]])
  cats[[f]] <- as.numeric(factor(cats[[f]], levels=levels))
}

my_data1=data.frame(my_data$trip_duration,my_data$distance_trip)

weidu=data.frame(my_data$pickup_longitude,my_data$pickup_latitude,my_data$dropoff_longitude,my_data$dropoff_latitude)
my_data1=cbind(my_data1,weidu)

write.csv(my_data1,"Q:/Kaggle--NYC cab/data1.csv")
  
  
trip_duration <- log(trip_duration)

detach(my_data)

# group all preprocessed parts
my_data <- as.data.frame(cbind(gps, dist, passenger_count, cats, trip_duration))

my_data1[is.na(my_data1)] <- -1

n_row   <- nrow(train)
n_col   <- ncol(my_data1)
max_row <- nrow(my_data1)

#
my_train <- my_data1[1:n_row,]
mcor <- cor(my_train)
#

x_train <- my_data1[1:n_row,2:n_col]
y_train <- my_data1[1:n_row,1]
x_test  <- my_data1[(n_row+1):max_row,2:n_col]

#rm(my_data,pickup, gps, passenger_count, cats)

dtrain = xgb.DMatrix(as.matrix(x_train), label=y_train)
dtest  = xgb.DMatrix(as.matrix(x_test))

start<-proc.time()
xgb_params = list(
  seed = 45569,
  colsample_bytree = 0.7,
  subsample = 0.8,
  eta = 0.1,
  objective = 'reg:linear',
  max_depth = 4,
  max_delta_step = 2,
  min_child_weight = 1
)
res = xgb.cv(xgb_params,
             dtrain,
             nrounds=10000,              #--- Will stop before (early_stopping_rounds)
             nfold=10,                   
             early_stopping_rounds=10,
             print_every_n=100,
             verbose=2,
             eval_metric = "rmse",
             maximize=FALSE)
print("Total runtime:")
print(proc.time()-start)
print(res)

best_nrounds = res$best_iteration
gbdt = xgb.train(xgb_params, dtrain, best_nrounds)
pred = predict(gbdt,dtest)

importance_matrix <- xgb.importance(names(my_data), model = gbdt)
importance_matrix$Gain <- importance_matrix$Gain * 100
View(importance_matrix)
xgb.ggplot.importance(importance_matrix = importance_matrix)
xgb.plot.deepness(model = gbdt)

submit_data <- as.data.frame(exp(pred))
submit_data <- cbind(test$id,submit_data)
colnames(submit_data) <- c("id","trip_duration")
write.csv(submit_data,"xgb_basic.csv", row.names = FALSE)