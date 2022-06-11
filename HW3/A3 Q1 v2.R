
f = function(x) {
  return(0)
}
set.seed(100)
e <- rnorm(100)
set.seed(101)
x <- matrix(rnorm(100*10000),ncol=10000)
y <- f(x) + e
df <- data.frame(x,y)


get_bias = function(estimate, truth) {
  mean((mean(estimate) - truth))
}

get_mse = function(estimate, truth) {
  mean(mean((estimate - truth) ^ 2))
}

?
#b
#The irreducible error is variance(error)


library(caTools)
set.seed(2037)
train=sample(100,50)
set.seed(2038)
#c
y_new = rep(0,100)
y_new
MSE_train <- get_mse(y_new[train],df[train,'y'])
MSE_train
MSE_test <- get_mse(y_new[-train],df[-train,'y'])
MSE_test

#d
model <- lm(y ~ .,data=df,subset=train)
predict_train <- predict(model,df)[train]
predict_test <- predict(model,df)[-train]
MSE_train <- get_mse(predict_train,df[train,'y'])

MSE_train
MSE_test <- get_mse(predict_test,df[-train,'y'])
MSE_test
bias_sq <- get_bias(predict_test,0)^2
bias_sq
variance <- var(predict_test)
variance
err_var <- var(e)
err_var
