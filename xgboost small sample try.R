data1=read.csv("small sample try.csv")
train=data1[1:800,]
test=data1[801:1000,]
library(xgboost)
xtrain=train[,5:69]
ytrain=log(train[,70])
xtest=test[,5:69]
ytest=log(test[,70])
dtrain = xgb.DMatrix(as.matrix(xtrain), label=ytrain)
dtest  = xgb.DMatrix(as.matrix(xtest))


#------------------------------调参----------------------------------------
#max_depth 和 min_weight 参数调优
gridsearch2=function(var1range,var2range){
  start<-proc.time()
  numbers_of_pairs=length(var1range)*length(var2range)
  n1=length(var1range)
  n2=length(var2range)
  rmses=NULL
  best.pairs=NULL
  min.rmse=Inf
  best.rounds=NULL
  for (i in 1:n1){
    for (j in 1:n2){
        params=list(
          subsample = 0.8,
          eta = 0.1,
          objective = 'reg:linear',
          max_depth=var1range[i],
          min_child_weight=var2range[j]
        )
        res = xgb.cv(params,
                     dtrain,
                     nrounds=10000,              #--- Will stop before (early_stopping_rounds)
                     nfold=10,                   
                     early_stopping_rounds=10,
                     print_every_n=100,
                     verbose=2,
                     eval_metric = "rmse",
                     maximize=FALSE)
        best_nrounds = res$best_iteration
        rmse=as.numeric(res$evaluation_log[res$best_iteration,"test_rmse_mean"])
        rmses=c(rmses,rmse)
        if (rmse<min.rmse){
          min.rmse=rmse
          best.pairs=c(var1range[i],var2range[j])
          best.rounds=best_nrounds
        }
      }
  }
  result=list(best.pairs=best.pairs,rmse.mean.array=rmses,lowest.rmse=min.rmse,best.rounds=best.rounds)
  return(result)
  print("Total runtime:")
  print(proc.time()-start)
  }

gs2=gridsearch2(c(4,5,6),c(1:6))

max_depth=gs2$best.pairs[1]
min_weight=gs2$best.pairs[2]

#第三步：gamma参数调优
gridsearch3=function(gamma.range){
  n=length(gamma.range)
  rmses=NULL
  best.gamma=NULL
  min.rmse=Inf
  best.rounds=NULL
  for (i in 1:n){
      params=list(
        subsample = 0.8,
        eta = 0.1,
        objective = 'reg:linear',
        max_depth= max_depth,
        min_child_weight= min_weight,
        gamma=gamma.range[i]
      )
      res = xgb.cv(params,
                   dtrain,
                   nrounds=10000,              #--- Will stop before (early_stopping_rounds)
                   nfold=10,                   
                   early_stopping_rounds=10,
                   print_every_n=100,
                   verbose=2,
                   eval_metric = "rmse",
                   maximize=FALSE)
      best_nrounds = res$best_iteration
      rmse=as.numeric(res$evaluation_log[res$best_iteration,"test_rmse_mean"])
      rmses=c(rmses,rmse)
      if (rmse<min.rmse){
        min.rmse=rmse
        best.gamma=gamma.range[i]
        best.rounds=best_nrounds
      }
  }
  result=list(best.gamma=best.gamma,rmse.mean.array=rmses,lowest.rmse=min.rmse,best.rounds=best.rounds)
  return(result)
}

gs3=gridsearch3(seq(0,0.2,0.01))

gamma=gs3$best.gamma

#调整subsample参数
gridsearch4=function(sub.range){
  n=length(sub.range)
  rmses=NULL
  best.sub=NULL
  min.rmse=Inf
  best.rounds=NULL
  for (i in 1:n){
    params=list(
      subsample = sub.range[i],
      eta = 0.1,
      objective = 'reg:linear',
      max_depth= max_depth,
      min_child_weight= min_weight,
      gamma=gamma
    )
    res = xgb.cv(params,
                 dtrain,
                 nrounds=10000,              #--- Will stop before (early_stopping_rounds)
                 nfold=10,                   
                 early_stopping_rounds=10,
                 print_every_n=100,
                 verbose=2,
                 eval_metric = "rmse",
                 maximize=FALSE)
    best_nrounds = res$best_iteration
    rmse=as.numeric(res$evaluation_log[res$best_iteration,"test_rmse_mean"])
    rmses=c(rmses,rmse)
    if (rmse<min.rmse){
      min.rmse=rmse
      best.sub=sub.range[i]
      best.rounds=best_nrounds
    }
  }
  result=list(best.subsample=best.sub,rmse.mean.array=rmses,lowest.rmse=min.rmse,best.rounds=best.rounds)
  return(result)
}

gs4=gridsearch4(seq(0.7,1,0.05))
subsample=gs4$best.subsample

#学习速率调参
gridsearch5=function(eta.range){
  n=length(sub.range)
  rmses=NULL
  best.eta=NULL
  min.rmse=Inf
  best.rounds=NULL
  for (i in 1:n){
    params=list(
      eta = eta.range[i],
      subsample=subsamples,
      objective = 'reg:linear',
      max_depth= max_depth,
      min_child_weight= min_weight,
      gamma=gamma
    )
    res = xgb.cv(params,
                 dtrain,
                 nrounds=10000,              #--- Will stop before (early_stopping_rounds)
                 nfold=10,                   
                 early_stopping_rounds=10,
                 print_every_n=100,
                 verbose=2,
                 eval_metric = "rmse",
                 maximize=FALSE)
    best_nrounds = res$best_iteration
    rmse=as.numeric(res$evaluation_log[res$best_iteration,"test_rmse_mean"])
    rmses=c(rmses,rmse)
    if (rmse<min.rmse){
      min.rmse=rmse
      best.eta=eta.range[i]
      best.rounds=best_nrounds
    }
  }
  result=list(best.eta=best.eta,rmse.mean.array=rmses,lowest.rmse=min.rmse,best.rounds=best.rounds)
  return(result)
}

gs5=gridsearch5(c(0.05,0.1,0.12,0.15,0.18,0.2,0.25,0.3))

eta=gs5$best.eta






xgb_params = list(
  subsample = subsample,
  eta = eta,
  objective = 'reg:linear',
  max_depth = max_depth,
  max_delta_step = 2,
  min_child_weight = min_weight,
  gamma=gamma
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
best_nrounds = res$best_iteration
gbdt = xgb.train(xgb_params, dtrain, best_nrounds)
pred = predict(gbdt,dtest)
ypred=exp(pred)
y=c(test[,70],ypred)
x=c(1:200,1:200)
tag=c(rep("real",200),rep("predicted",200))
graph=data.frame(x,y,tag)
library(ggplot2)
p=ggplot(data=graph,aes(x=x,y=y,color=factor(tag)))
str(longdata)
p+geom_point()+geom_smooth()

#---------------------------------------调参总的函数------------------------------------------------

parameters_tuning=function(var1range=c(4,5,6),var2range=c(1:6),gamma.range=seq(0,0.2,0.01),
                           sub.range=seq(0.7,1,0.05),eta.range=c(0.05,0.1,0.12,0.15,0.18,0.2,0.25,0.3)){
  
  gridsearch2=function(var1range,var2range){
    start<-proc.time()
    numbers_of_pairs=length(var1range)*length(var2range)
    n1=length(var1range)
    n2=length(var2range)
    rmses=NULL
    best.pairs=NULL
    min.rmse=Inf
    best.rounds=NULL
    for (i in 1:n1){
      for (j in 1:n2){
        params=list(
          subsample = 0.8,
          eta = 0.1,
          objective = 'reg:linear',
          max_depth=var1range[i],
          min_child_weight=var2range[j]
        )
        res = xgb.cv(params,
                     dtrain,
                     nrounds=10000,              #--- Will stop before (early_stopping_rounds)
                     nfold=5,                   
                     early_stopping_rounds=10,
                     print_every_n=100,
                     verbose=2,
                     eval_metric = "rmse",
                     maximize=FALSE)
        best_nrounds = res$best_iteration
        rmse=as.numeric(res$evaluation_log[res$best_iteration,"test_rmse_mean"])
        rmses=c(rmses,rmse)
        if (rmse<min.rmse){
          min.rmse=rmse
          best.pairs=c(var1range[i],var2range[j])
          best.rounds=best_nrounds
        }
      }
    }
    result=list(best.pairs=best.pairs,rmse.mean.array=rmses,lowest.rmse=min.rmse,best.rounds=best.rounds)
    return(result)
    print("Total runtime:")
    print(proc.time()-start)
  }
  
  gs2=gridsearch2(var1range,var2range)
  
  max_depth=gs2$best.pairs[1]
  min_weight=gs2$best.pairs[2]
  
  #第三步：gamma参数调优
  gridsearch3=function(gamma.range){
    n=length(gamma.range)
    rmses=NULL
    best.gamma=NULL
    min.rmse=Inf
    best.rounds=NULL
    for (i in 1:n){
      params=list(
        subsample = 0.8,
        eta = 0.1,
        objective = 'reg:linear',
        max_depth= max_depth,
        min_child_weight= min_weight,
        gamma=gamma.range[i]
      )
      res = xgb.cv(params,
                   dtrain,
                   nrounds=10000,              #--- Will stop before (early_stopping_rounds)
                   nfold=5,                   
                   early_stopping_rounds=10,
                   print_every_n=100,
                   verbose=2,
                   eval_metric = "rmse",
                   maximize=FALSE)
      best_nrounds = res$best_iteration
      rmse=as.numeric(res$evaluation_log[res$best_iteration,"test_rmse_mean"])
      rmses=c(rmses,rmse)
      if (rmse<min.rmse){
        min.rmse=rmse
        best.gamma=gamma.range[i]
        best.rounds=best_nrounds
      }
    }
    result=list(best.gamma=best.gamma,rmse.mean.array=rmses,lowest.rmse=min.rmse,best.rounds=best.rounds)
    return(result)
  }
  
  gs3=gridsearch3(gamma.range)
  
  gamma=gs3$best.gamma
  
  #调整subsample参数
  gridsearch4=function(sub.range){
    n=length(sub.range)
    rmses=NULL
    best.sub=NULL
    min.rmse=Inf
    best.rounds=NULL
    for (i in 1:n){
      params=list(
        subsample = sub.range[i],
        eta = 0.1,
        objective = 'reg:linear',
        max_depth= max_depth,
        min_child_weight= min_weight,
        gamma=gamma
      )
      res = xgb.cv(params,
                   dtrain,
                   nrounds=10000,              #--- Will stop before (early_stopping_rounds)
                   nfold=5,                   
                   early_stopping_rounds=10,
                   print_every_n=100,
                   verbose=2,
                   eval_metric = "rmse",
                   maximize=FALSE)
      best_nrounds = res$best_iteration
      rmse=as.numeric(res$evaluation_log[res$best_iteration,"test_rmse_mean"])
      rmses=c(rmses,rmse)
      if (rmse<min.rmse){
        min.rmse=rmse
        best.sub=sub.range[i]
        best.rounds=best_nrounds
      }
    }
    result=list(best.subsample=best.sub,rmse.mean.array=rmses,lowest.rmse=min.rmse,best.rounds=best.rounds)
    return(result)
  }
  
  gs4=gridsearch4(sub.range)
  subsample=gs4$best.subsample
  
  #学习速率调参
  gridsearch5=function(eta.range){
    n=length(eta.range)
    rmses=NULL
    best.eta=NULL
    min.rmse=Inf
    best.rounds=NULL
    for (i in 1:n){
      params=list(
        eta = eta.range[i],
        subsample=subsample,
        objective = 'reg:linear',
        max_depth= max_depth,
        min_child_weight= min_weight,
        gamma=gamma
      )
      res = xgb.cv(params,
                   dtrain,
                   nrounds=10000,              #--- Will stop before (early_stopping_rounds)
                   nfold=5,                   
                   early_stopping_rounds=10,
                   print_every_n=100,
                   verbose=2,
                   eval_metric = "rmse",
                   maximize=FALSE)
      best_nrounds = res$best_iteration
      rmse=as.numeric(res$evaluation_log[res$best_iteration,"test_rmse_mean"])
      rmses=c(rmses,rmse)
      if (rmse<min.rmse){
        min.rmse=rmse
        best.eta=eta.range[i]
        best.rounds=best_nrounds
      }
    }
    result=list(best.eta=best.eta,rmse.mean.array=rmses,lowest.rmse=min.rmse,best.rounds=best.rounds)
    return(result)
  }
  
  gs5=gridsearch5(eta.range)
  
  eta=gs5$best.eta
  
  #wrap up
  result=list(max_depth=max_depth,min_child_weight=min_weight,gamma=gamma,subsample=subsample,
              eta=eta,lowest.rmse=gs5$lowest.rmse)
  return(result)
}

rr=parameters_tuning()

#-----------------------------------替换区域变量中的缺失值--------------------------------------
na.replace=function(x,new.value){
  j=is.na(x)
  index=which(j==TRUE)
  x[index]=new.value
  return(x)
}


8.42



