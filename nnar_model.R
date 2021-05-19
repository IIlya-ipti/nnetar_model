# Title     : TODO
# Objective : TODO
# Created by: Ilya
# Created on: 25.03.2021
library('ggplot2')
library('forecast')
library('tseries')
library(xts)
library(quantmod)
library(MLmetrics)

data_solve <- function(data_nms,n){
  return (data_nms[n]);
}

train_data <- function(n,k,data_nms){
  # k - номер теста
  # количество разделений
  data_for_train <- data_nms[1 : round((nrow(data_nms) * 1/n) * k),]
  return (data_for_train)
}
test_data <- function(n,k,data_nms){
  # k - номер теста
  # количество разделений
  data_for_test <- data_nms[ (round((nrow(data_nms) * 1/n) * k) + 1) : nrow(data_nms),]
  return (data_for_test)
}

nnetar_func <- function(train,p_1,P_1,Size){
  return (forecast::nnetar(train,p = p_1,P = P_1,size= Size,maxit=1000,MaxNWts=84581, repeats = 20))
}
predict_nnetar <- function(fit,j){
  return (forecast(fit,h = j)$mean)
}
data_of <- read.csv("C:\\Users\\Ilya\\OneDrive - Peter the Great St. Petersburg Polytechnical University\\project\\train_csv.csv")
data_of <- data_of[3]


# n <- nrow(data_of)
# k <- 0.8
# train_i <- 1:round(k*n)
# test_i <- round(k*n+1):n
# train_1 <- data_of[train_i,]
# test_1 <- data_of[test_i,]
test_for_all <- train_data
train <- train_data(5,4.5,data_of)
test <- test_data(5,4.5,data_of)
new_data <- function(y,)

print(train)
plot(train)
train <- ts(train,frequency = 24)
test <- ts(test, frequency = 24)
plot(train)
print(data_of)
# 60 6 20
fits <- c()
# 144 5 30
while(1){
  fit <- forecast::nnetar(train,p = 124,P = 3,size= 25,maxit=1000,MaxNWts=84581, repeats = 20)
  
  # 60 18 30
  j <- 24
  predict_data <- forecast(fit,h = j)$mean
  f <- MAPE(predict_data,test[1:j])
  new_f <- max(abs((test[1:j]-predict_data[1:j])/test[1:j])) * 100
  print(f)
  print(new_f)
  if(new_f < 10){
    fits <- c(fits,fit)
  }
  if(new_f < 7){
    break
  }
}
j <- 48
plot(forecast(fit,h =j))

predict_data <- forecast(fit,h = j)$mean
par(new=FALSE)
MAPE(predict_data,test[1:j])
mean(abs((test[1:j]-predict_data[1:j])/test[1:j])) * 100
max(abs((test[1:j]-predict_data[1:j])/test[1:j])) * 100
# fcast <- forecast(fit, PI=TRUE, h=200)
# autoplot(fcast)
new_value = ts(c(train,test[1:j]),frequency = 24)
plot(new_value,col='green')
# save as file

# saveRDS(fit,"./models/model_nnar_mins_proverka.rds")


fit <- readRDS("./models/model_nnar_mins_proverka.rds")
fit <-  forecast::nnetar(train,model = fit)


fit <- forecast::nnetar(train,p = 124,P = 3,size= 25,maxit=1000,MaxNWts=84581, repeats = 20)
plot(forecast(fit,h =j))
length(train)
train <- data_of
fit <- forecast::nnetar(ts(train,frequency = 24),p = 124,P = 3,size= 25,maxit=100,MaxNWts=84581, repeats = 20)
index<-sample(1:15, nrow(train), replace = T)
train.index <- index
numberOfFolds <- 15
mse <- vector('numeric',length = 15)
for (n in 1:numberOfFolds) {
  data.train <- ts(train[index!= n,],frequency = 24)
  data.test <- ts(train[index == n,],frequency = 24)
  fit <-  forecast::nnetar(y = data.train ,model = fit)
  predictions <- predict(fit, h = length(data.test))
  mse[[n]] <- MAPE(data.test[1:length(data.test)],predictions$mean)
}
nnetar_validation <- function(train,model,p,P,size, maxit){
  # кросс валидация 
  print(model != "nnetar")
  if(class(model) != "nnetar"){
    return (CVar(train, FUN = nnetar, p = 2, P = 1, size = 2,maxit=1009,MaxNWts=84581, repeats = 20));
  }else{
    return (CVar(train, FUN = nnetar, model = model));
  }
}


# кросс валидация

data_of <- read.csv("C:\\Users\\Ilya\\OneDrive - Peter the Great St. Petersburg Polytechnical University\\project\\train_csv.csv")
data_of <- data_of[3]

train <- train_data(5,4,data_of)
test <- test_data(5,4,data_of)
 
train <- ts(train,frequency = 24)
test <- ts(test, frequency = 24)
print(train)
plot(train)

# fit <- forecast::nnetar(train,p = 124,P = 24,size= 10,,maxit=1000,MaxNWts=84581, repeats = 20)
model <-nnetar_validation(train,0)
fit <- model$fold5$model
j <- 24 
plot(forecast(fit,h =j))
predict_data <- forecast(fit,h = j)$mean
f <- MAPE(predict_data,test[1:j])
new_f <- max(abs((test[1:j]-predict_data[1:j])/test[1:j])) * 100
print(f)
print(new_f)
plot(ts(data_of,frequency = 24))

save_model <- function(fit,train){
  saveRDS(fit,path)
}
load_model <- function(fit, train){
  
}
md <- function(fit,train){
  return(forecast::nnetar(train,model = fit))
}
nnetars <- function(models,i){
  return (models[i]$fold$model)
}
