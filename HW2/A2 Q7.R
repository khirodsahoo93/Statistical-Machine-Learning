library('MASS')
set.seed(100)
sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
mu1 <- matrix(c(0,1))
mu2 <- matrix(c(2,3))
mu3 <- matrix(c(4,5))

k1 <- mvrnorm(n=50,mu1,sigma)
set.seed(901)
k2 <- mvrnorm(n=50,mu2,sigma)
set.seed(302)
k3 <- mvrnorm(n=50,mu3,sigma)

df_k1 <- data.frame(k1)
df_k2 <- data.frame(k2)
df_k3 <- data.frame(k3)

df_k1$label <- as.factor('1')
df_k2$label <- as.factor('2')
df_k3$label <- as.factor('3')

data_training <- rbind(df_k1,df_k2,df_k3)

library(tidyverse)
plot1= ggplot(data=data_training,aes(X1,X2,color=label)) + 
  geom_point()+scale_color_manual(values = c("1" = "red", "2" = "blue","3"="green"))
plot1 + ggtitle('Plot of training data') 

#sigma=matrix(cov_mat,2,2)

#sigma=matrix(cov_mat,2,2)

#Decision boundary for k=1 and k=2
A=(t(mu2) %*% solve(sigma) %*% mu2) -(t(mu1) %*% solve(sigma) %*% mu1)
B=2 * t(solve(sigma) %*% (mu2-mu1))
#B= 2*solve(sigma) %*% (mu2-mu1)
m1 <- -(B[1,1]/B[1,2])
c1 <- A[1,1]/B[1,2]

#Decision boundary for k=2 and k=3
A=(t(mu3) %*% solve(sigma) %*% mu3) -(t(mu2) %*% solve(sigma) %*% mu2)
B=2 * t(solve(sigma) %*% (mu3-mu2))
#B= 2*solve(sigma) %*% (mu3-mu2)
m2 <- -(B[1,1]/B[1,2])
c2 <- A[1,1]/B[1,2]

#Decision boundary for k=3 and k=1
A=(t(mu1) %*% solve(sigma) %*% mu1) -(t(mu3) %*% solve(sigma) %*% mu3)
B=2 * t(solve(sigma) %*% (mu1-mu3))
#B= 2* solve(sigma) %*% (mu1-mu3)
m3 <- -(B[1,1]/B[1,2])
c3 <- A[1,1]/B[1,2]

plot1= ggplot(data=data_training,aes(X1,X2,color=label)) + 
  geom_point()+scale_color_manual(values = c("1" = "red", "2" = "blue","3"="green"))
plot1 + ggtitle('Plot of training data') 
plot1 + geom_abline(intercept = c2,slope = m2,colour='green') + geom_abline(intercept = c1,slope = m1,color='red') +
  annotate('text',x=-1,y=2.5,label='Red')+annotate('text',x=5,y=0,label='Blue')+
  annotate('text',x=3,y=6.5,label='Green') + ggtitle('Decision boundaries of Bayes classifier')

drawparti(data_training$label, data_training$X1, data_training$X2, method="naiveBayes", xlab="X1", ylab="X2"
          , col.wrong = "pink", imageplot = FALSE, 
          legend.err= FALSE,
          col.mean = "pink")
text(-1, 2.5, label="class 1",col='red',cex=1)
text(5, 0, label="class 2",col='blue',cex=1)
text(3, 6.5, label="class 3",col='green',cex=1)
title(main = "Decision boundary of naiveBayes")

#### C
library('caret')
set.seed(123)
training <- data_training$label %>% 
  createDataPartition(p = 1, list = FALSE)
train.data <- data_training[training, ]
test.data <- data_training[-training, ]

model <- lda(factor(label)~., data = data_training)
predictions <- model %>% predict(data_training)

mu1_est=matrix(colMeans(df_k1[,c('X1','X2')]))
mu2_est=matrix(colMeans(df_k2[,c('X1','X2')]))
mu3_est=matrix(colMeans(df_k3[,c('X1','X2')]))
sigma_est<-(sigma_est_k1*50 + sigma_est_k2* 50 + sigma_est_k3*50)/(150-3)
#sigma_est <- matrix(cov(data_training[,c('X1','X2')]),nrow = 2)

#Decision boundary for k=1 and k=2
A=(t(mu2_est) %*% solve(sigma_est) %*% mu2_est) -(t(mu1_est) %*% solve(sigma_est) %*% mu1_est)
B=2 * t(solve(sigma_est) %*% (mu2_est-mu1_est))
#B= 2*solve(sigma_est) %*% (mu2_est-mu1_est)
m1_LDA <- -(B[1,1]/B[1,2])
c1_LDA <- A[1,1]/B[1,2]

#Decision boundary for k=2 and k=3
A=(t(mu3_est) %*% solve(sigma_est) %*% mu3_est) -(t(mu2_est) %*% solve(sigma_est) %*% mu2_est)
B=2 * t(solve(sigma_est) %*% (mu3_est-mu2_est))
#B= 2*solve(sigma_est) %*% (mu3_est-mu2_est)
m2_LDA <- -(B[1,1]/B[1,2])
c2_LDA <- A[1,1]/B[1,2]

#Decision boundary for k=3 and k=1
A=(t(mu1_est) %*% solve(sigma_est) %*% mu1_est) -(t(mu3_est) %*% solve(sigma_est) %*% mu3_est)
B=2 * t(solve(sigma_est) %*% (mu1_est-mu3_est))
#B= 2*solve(sigma_est) %*% (mu1_est-mu3_est)
m3_LDA <- -(B[1,1]/B[1,2])
c3_LDA <- A[1,1]/B[1,2]

plot1= ggplot(data=data_training,aes(X1,X2,color=label)) + 
  geom_point()+scale_color_manual(values = c("1" = "red", "2" = "blue","3"="green"))
plot1 + ggtitle('Plot of training data') 
plot1 + geom_abline(intercept = c2,slope = m2,colour='green') + 
  #geom_abline(intercept = c3,slope = m3,colour='blue') +
  geom_abline(intercept = c1,slope = m1,color='red') +
  geom_abline(intercept = c2_LDA,slope = m2_LDA,colour='green',lty=2) + 
  geom_abline(intercept = c1_LDA,slope = m1_LDA,color='red',lty=2)+
  #geom_abline(intercept = c3_LDA,slope = m3_LDA,colour='blue',lty=2) +
  annotate('text',x=-1,y=2.5,label='class 1')+annotate('text',x=5,y=0,label='class 2')+
  annotate('text',x=3,y=6.5,label='class 3') +
  ggtitle('Decision boundaries of Naive classifier vs LDA')

library(klaR)
#partimat(label~., data=data_training, method="lda")
drawparti(data_training$label, data_training$X1, data_training$X2, method="lda", xlab="X1", ylab="X2"
          , col.wrong = "pink", imageplot = FALSE, 
          legend.err= FALSE,
          col.mean = "pink")
text(-1, 2.5, label="class 1",col='red',cex=1)
text(5, 0, label="class 2",col='blue',cex=1)
text(3, 6.5, label="class 3",col='green',cex=1)
title(main = "Decision boundary of LDA")

p_train <- predictions$class
tab1 <- table(Predicted = p_train, Actual = data_training$label)
tab1

##E
set.seed(800)
sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
mu1 <- matrix(c(0,1))
mu2 <- matrix(c(2,3))
mu3 <- matrix(c(4,5))

k1 <- mvrnorm(n=50,mu1,sigma)
set.seed(501)
k2 <- mvrnorm(n=50,mu2,sigma)
set.seed(802)
k3 <- mvrnorm(n=50,mu3,sigma)

df_k1 <- data.frame(k1)
df_k2 <- data.frame(k2)
df_k3 <- data.frame(k3)

df_k1$label <- as.factor('1')
df_k2$label <- as.factor('2')
df_k3$label <- as.factor('3')

data_test <- rbind(df_k1,df_k2,df_k3)

predictions_test <- model %>% predict(data_test)
p_test <- predictions_test$class
tab2 <- table(Predicted = p_test, Actual = data_test$label)
tab2