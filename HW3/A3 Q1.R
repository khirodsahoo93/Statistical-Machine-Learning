set.seed(100)
y <- rnorm(100)

set.seed(101)
x <- matrix(rnorm(100*10000),ncol=10000)

df <- data.frame(x,y)
data_x <- df[,names(df) != 'y']
model <- lm(y~x,data=df)

summary(model)
model$coefficients
predict <- predict(model,data_x)

#b
MSE <- mean((predict-y)^2)
MSE
bias_sq <- mean((mean(predict) - y)^2)
variance <- mean((mean(predict) - predict)^2)

irreducible_err <- MSE - bias_sq -variance
irreducible_err

#c

y_new = rep(0,100)
y_new
bias_sq_new <- mean((mean(y_new) - y)^2)
bias_sq_new
variance_new <- mean((mean(y_new) - y_new)^2)
variance_new
set.seed(102)
MSE_new <- mean((y_new-y)^2)
MSE_new
irre_err_new <- MSE_new - bias_sq_new - variance_new
irre_err_new
set.seed(2037)
library(caTools)
spl = sample.split(df$y, SplitRatio = 0.6)
train = subset(df, spl == TRUE)
test = subset(df, spl == FALSE)
y_new_train <- subset(y_new, spl == TRUE)
y_new_test <- subset(y_new, spl == FALSE)

MSE_new_train <- mean((y_new_train-train$y)^2)
MSE_new_train
MSE_new_test <- mean((y_new_test-test$y)^2)
MSE_new_test
#Why is the validation error less than training error sometimes? May be because of the sampling bias. A fact- 
#MSE is due to the irreducible error only. Since bias is very low in both the case and the variance is zero


#d
predict_train <- subset(predict, spl == TRUE)
predict_test <- subset(predict, spl == FALSE)
MSE_predict_train <- mean((predict_train-train$y)^2)
MSE_predict_train
MSE_predict_test <- mean((predict_test-test$y)^2)
MSE_predict_test
