library(ISLR2)

#function to calculate validation test error in training or test
Val_test <- function(data,model,score_data=1)
{

  if(score_data==1){
    predict <- predict(model,data)
    MSE <- get_mse(predict,data$mpg)
  }
  else{
    predict <- predict(model,data)
    MSE <- get_mse(predict,data$mpg)
  }
  return(MSE)
  
}

#library(ggplot2)
seed <- c(100,200,300,400,500,600,700,800,900,1000)
degree <- c(1,2,3,4,5,6,7,8,9,10)
#df_plot <- data.frame(degree,seed)
plot_mat <- matrix(nrow=10,ncol=10)
for( j in 1:10){
  #MSE_scores <- c()
  set.seed(seed[j])
  spl = sample.split(Auto$mpg, SplitRatio = 0.5)
  train = subset(Auto, spl == TRUE)
  test = subset(Auto, spl == FALSE)
  
for(i in 1:10){
  set.seed(7000)
  model <- lm(mpg ~ poly(horsepower,degree[i]),data=train)
  MSE_scores <- append(MSE_scores,Val_test(test,model))
  plot_mat[j,i] <- Val_test(test,model)
  
  #plot1 + ggplot(data=,aes=(x=degree,y=MSE_scores))+geom_line()
}
  #print(MSE_scores)
  #df_plot <- cbind(df_plot,MSE_scores)
  #lines(degree,MSE_scores,pch=18,type='b')
}

matplot(t(plot_mat),type='l',ylim=c(15,26),ylab='Estimated MSE',xlab='degree of polynomial')

#df_plot1 <- data.frame(df_plot,fun = rep(c(1,2,3,4,5,6,7,8,9,10)))
#plot1 <- ggplot(df_plot1,aes(degree,df_plot1[,3]))+geom_line()+geom_line(aes(degree, df_plot1[,4]))+
#  geom_line(aes(degree, df_plot1[,5]))+geom_line(aes(degree, df_plot1[,6]))+geom_line(aes(degree, df_plot1[,7]))+
#  geom_line(aes(degree, df_plot1[,8]))+geom_line(aes(degree, df_plot1[,9]))+geom_line(aes(degree, df_plot1[,10]))+
#  geom_line(aes(degree, df_plot1[,11])) 



##################
set.seed(1)
train=sample(392,196)
lm.fit <- lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
predict <- predict(lm.fit,Auto[-train,])
MSE <- get_mse(predict,Auto[-train,]$mpg)
###################3

library(boot)
cv.error <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i] <- cv.glm(Auto,glm.fit)$delta[1]
}

cv.error
plot(c(1:10),cv.error,col='red',type='l',xlab='degree of polynomial',ylab='CV error')

#######################
degree <- c(1,2,3,4,5,6,7,8,9,10)
df_plot <- data.frame(degree,seed)
for (i in seed){
set.seed(i)
cv.error.10 <- rep(0,10)
for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
}
cv.error.10
df_plot <- cbind(df_plot,cv.error.10)
#plot(c(1:10),cv.error.10,col='red',type='l',xlab='degree of polynomial',ylab='CV error')
}
df_plot2 <- data.frame(df_plot,fun = rep(c(1,2,3,4,5,6,7,8,9,10)))
plot2 <- ggplot(df_plot2,aes(degree,df_plot2[,3]))+geom_line()+geom_line(aes(degree, df_plot2[,4]))+
  geom_line(aes(degree, df_plot2[,5]))+geom_line(aes(degree, df_plot2[,6]))+geom_line(aes(degree, df_plot2[,7]))+
  geom_line(aes(degree, df_plot2[,8]))+geom_line(aes(degree, df_plot2[,9]))+geom_line(aes(degree, df_plot2[,10]))+
  geom_line(aes(degree, df_plot2[,11])) + geom_line(aes(degree, df_plot2[,12]))
#######################
MSE_10 <- rep(0,10)
for (i in 1:10){
  
  lm.fit <- lm(mpg~poly(horsepower,i),data=Auto)
  MSE_10[i] <- Val_test(Auto,lm.fit)
}

plot(c(1:10),MSE_10,col='red',type='l',xlab='degree of polynomial',ylab='Training MSE')
################
lm.fit <- lm(mpg~poly(horsepower,10),data=Auto)
MSE_10 <- Val_test(Auto,lm.fit)
summary(lm.fit)
