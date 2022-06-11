#Q1

library(ISLR2)
df_Auto <- data.frame(Auto)
nrow(df_Auto)
ncol(df_Auto)
df_Auto$year <- as.factor(df_Auto$year)
#df_Auto$cylinders <- as.factor(df_Auto$cylinders)
df_Auto$origin_new <- relevel(factor(df_Auto$origin), ref = 3)
lm_fit <- lm(mpg ~ .-name-origin ,data=df_Auto)
summary(lm_fit)

mean(lm_fit$residuals ^2)

cylinders <- c(3)
displacement <- c(100)
horsepower <- c(85)
weight <- c(3000)
acceleration <- c(20)
year <- as.factor(c(80))
origin_new <- as.factor(c(3))
origin <- c(1)
name <- c('ford ranger')


df_test <- data.frame(cylinders,displacement,horsepower,weight,acceleration,year)

predict(lm(mpg ~ .-name-origin ,data=df_Auto),newdata= df_test, interval = 'prediction')




