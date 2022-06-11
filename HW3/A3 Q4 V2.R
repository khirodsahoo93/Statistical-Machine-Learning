library(ISLR2)
library(caTools)
#function to calculate validation test error in training or test
Val_test <- function(data,model)
{
  
    predict <- predict(model,data)
    MSE <- get_mse(predict,data$mpg)

  return(MSE)
  
}

seed <- c(100,200,300,400,500,600,700,800,900,1000)
degree <- c(1,2,3,4,5,6,7,8,9,10)
plot_mat <- matrix(nrow=10,ncol=10)
for( j in 1:10){
  set.seed(seed[j])
  spl = sample.split(Auto$mpg, SplitRatio = 0.5)
  train = subset(Auto, spl == TRUE)
  test = subset(Auto, spl == FALSE)
  
  for(i in 1:10){
    set.seed(7000)
    model <- lm(mpg ~ poly(horsepower,degree[i]),data=train)
    MSE_scores <- append(MSE_scores,Val_test(test,model))
    plot_mat[j,i] <- Val_test(test,model)
  }
}

matplot(t(plot_mat),type='l',ylim=c(15,26),ylab='Estimated MSE',
        xlab='degree of polynomial')
title('Validation set')

###################3

library(boot)
cv.error <- rep(0,10)
for(i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i] <- cv.glm(Auto,glm.fit)$delta[1]
}

cv.error
plot(c(1:10),cv.error,col='red',type = "b", pch = 19,
     ylim=c(16,28),xlab='degree of polynomial',ylab='Estimated MSE')
title('LOOCV')
#######################
seed <- c(100,200,300,400,500,600,700,800,900,1000)
degree <- c(1,2,3,4,5,6,7,8,9,10)
cv.error.mat <- matrix(nrow=10,ncol=10)
for (j in 1:10){
  set.seed(seed[j])
  cv.error.10 <- rep(0,10)
  for (i in 1:10){
    glm.fit <- glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.mat[j,i] <- cv.glm(Auto,glm.fit,K=10)$delta[1]
  }

}

matplot(t(cv.error.mat),type='l',ylim=c(15,26),ylab='Estimated MSE'
        ,xlab='degree of polynomial')
title('10-fold CV')

#######################
MSE_10 <- rep(0,10)
for (i in 1:10){
  
  lm.fit <- lm(mpg~poly(horsepower,i),data=Auto)
  MSE_10[i] <- Val_test(Auto,lm.fit)
}

plot(c(1:10),MSE_10,col='red',type = "b", pch = 19,xlab='degree of polynomial',
     ylab='Training Set Mean Squared Error')
title('Training MSE with all observations')
################

lm.fit <- lm(mpg~poly(horsepower,10),data=Auto)
MSE_10 <- Val_test(Auto,lm.fit)
summary(lm.fit)
