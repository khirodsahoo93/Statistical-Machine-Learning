
df_Auto2 <- data.frame(Auto)
df_Auto2$American <- ifelse(df_Auto2$origin == 1,1,0)
df_Auto2$European <- ifelse(df_Auto2$origin == 2,1,0)
lm_fit21 <- lm(mpg ~ American + European ,data=df_Auto2)
summary(lm_fit21)

df_Auto2$Japanese <- ifelse(df_Auto2$origin == 3,1,0)
lm_fit22 <- lm(mpg ~ Japanese + European ,data=df_Auto2)
summary(lm_fit22)


df_Auto2$American_new <- ifelse(df_Auto2$origin == 1,1,-1)
df_Auto2$European_new <- ifelse(df_Auto2$origin == 2,1,-1)
lm_fit23 <- lm(mpg ~ American_new + European_new ,data=df_Auto2)
summary(lm_fit23)

df_Auto2$origin_new <- ifelse(df_Auto2$origin == 3,0,ifelse(df_Auto2$origin == 1,1,2))
lm_fit24 <- lm(mpg ~ origin_new ,data=df_Auto2)
summary(lm_fit24)

