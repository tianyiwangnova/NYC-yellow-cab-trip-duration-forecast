#Divide the train set and test set
my_data1[is.na(my_data1)] <- -1
n_row   <- nrow(train)
n_col   <- ncol(my_data1)
max_row <- nrow(my_data1)
my_train <- my_data1[1:n_row,]
x_train <- my_data1[1:n_row,2:n_col]
y_train <- my_data1[1:n_row,1]
x_test  <- my_data1[(n_row+1):max_row,2:n_col]

#XGBoost Modeling
xgb <- xgboost(data = data.matrix(x_train), 
               label = y_train, 
               subsample = 0.8,
               eta = 0.1,
               objective = 'reg:linear',
               max_depth = 4,
               max_delta_step = 2,
               min_child_weight = 1,
               nround=25
)
x_train[is.na(x_train)] <- 0

y_pred <- predict(xgb, data.matrix())

model <- xgb.dump(xgb, with.stats = T)
model[1:10]

#get the names of the features
names <- dimnames(data.matrix(x_train))[[2]]

xgb.plot.tree(feature_names = names, model = xgb)

importance_matrix <- xgb.importance(names, model = xgb)
# Nice graph
xgb.plot.importance(importance_matrix[1:10,])
