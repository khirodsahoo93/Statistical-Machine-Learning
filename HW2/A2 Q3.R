df_Auto3 <- data.frame(Auto)
df_Auto3$origin <- as.factor(df_Auto3$origin)
lm_fit31 <- lm(mpg ~ origin + horsepower + origin * horsepower ,data=df_Auto3)
summary(lm_fit31)

df_Auto3$horsepower_new = df_Auto3$horsepower /1000 
lm_fit4 <- lm(mpg ~ horsepower_new + horsepower ,data=df_Auto3)
summary(lm_fit4)
