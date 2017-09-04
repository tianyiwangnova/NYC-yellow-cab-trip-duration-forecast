lasso=read.csv("C:/Users/tw2567/Downloads/lasso experiment.csv",as.is = TRUE)
attach(lasso)
library(geosphere)
dist=distHaversine(matrix(c(pickup_longitude, pickup_latitude), ncol = 2),
                   matrix(c(dropoff_longitude, dropoff_latitude), ncol = 2))
lasso[, 'geo_distance']=dist
data1=select(lasso,trip_duration,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,month,date,hour,minute,geo distance,street_for_each_step)
data1=select(lasso,trip_duration,pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude,month,date,hour,minute,geo_distance,street_for_each_step)
write.csv(data1,"lasso for python.csv",row.names = FALSE)

#lasso begins from here
data2=read.csv("lasso for r.csv")
xfactors <- model.matrix(trip_duration~month+date+hour,data=data2)[, -1]
x_sparse=data2[,13:7743]
colsum=colSums(x_sparse)
index_keep=as.numeric(which(colsum>=100))
x_streets=x_sparse[,index_keep]

x        <- as.matrix(data.frame(xfactors,data2$pickup_longitude,data2$pickup_latitude,data2$dropoff_longitude,
                                 data2$dropoff_latitude,data2$geo_distance,x_streets))
library(glmnet)
glmmod <- glmnet(x, y=log(data2$trip_duration), alpha=1)
coef(glmmod)[, 32]
paras_39=data.frame(parameter=names(coef(glmmod)[, 39]),coef=as.numeric(coef(glmmod)[, 32]))
write.csv(para,"lasso result.csv")

cv.glmmod <- cv.glmnet(x, y=log(data2$trip_duration), alpha=1)
best.lambda <- cv.glmmod$lambda.min
best.lambda
plot(cv.glmmod)

index_keep=which(paras_39$coef!=0)
para50=paras_39[index_keep,]
write.csv(para50,"lasso result 50.csv")
