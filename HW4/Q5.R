library(ISLR2)

par(mfrow=c(1,1))
set.seed(291)
valid=sample(1:length(train),length(train)/2)

df_Auto <- data.frame(Auto)
head(df_Auto)
library (tree)
df_Auto$year <- as.factor(df_Auto$year)
df_Auto$origin <- as.factor(df_Auto$origin)
train=sample(1:nrow(df_Auto),2*nrow(df_Auto)/3)
df_train <- df_Auto[train,][names(df_Auto)!='name']
df_test <- df_Auto[-train,][names(df_Auto)!='name']


set.seed(100)
tree.auto <- tree (mpg ~ .  , df_train,control = tree.control(nobs = length(train), mindev = 0))
summary(tree.auto)
plot (tree.auto)
text (tree.auto , pretty = 0)
cv.auto <- cv.tree(tree.auto)
plot(cv.auto$size , cv.auto$dev, type = "b")

#what is the difference between deviance and MSE
tree.size.par<- seq(2,40,1)
tree.MSE <- c()
for (i in tree.size.par){
  print(i)
  tree.mod <- prune.tree(tree.auto, best = i)
  yhat <- predict(tree.mod , newdata = df_test)
  MSE<-get_mse(yhat,df_test$mpg)
  print(MSE)
  tree.MSE<-append(tree.MSE,MSE)
  
}

plot(tree.size.par,tree.MSE,type='b',col='red',xlab='tree size',ylab='validation MSE')
title (" Validation MSE for different sizes of tree ")

prune.auto <- prune.tree(tree.auto, best = 8)
plot (prune.auto)
text (prune.auto , pretty = 0)

yhat_tree <- predict(prune.auto , newdata = df_test)
tree_MSE <- get_mse(yhat_tree,df_test$mpg)

plot(yhat_tree,df_test$mpg, xlab = "Predicted mpg", ylab = "Actual mpg", main = "Pruned Regression model")
abline(0, 1, col = "blue")


####b
## what is number of predictors
library(randomForest)
set.seed(101)

n.trees <- c(50,100,150,200,250,300,350,400,450,500,600,700,800)
bag.MSE <- c()
for (i in n.trees){
  set.seed(101)
  bag.mod <- randomForest (mpg ~ ., data = df_train[valid,] , mtry = ncol(df_train[valid,])-1,ntree=i, importance = TRUE)
  yhat <- predict(bag.mod , newdata = df_train[-valid,])
  MSE<-get_mse(yhat,df_train[-valid,]$mpg)
  bag.MSE<-append(bag.MSE,MSE)
  
}
plot(n.trees,bag.MSE,type='b',col='red',xlab='no. of trees',ylab='validation MSE')
title('Validation MSE for different number of trees' )

set.seed(101)
bag.auto <- randomForest (mpg ~ ., data = df_train , mtry = ncol(df_train)-1,ntree=600, importance = TRUE)
yhat <- predict(bag.auto , newdata = df_test)
bag.MSE<-get_mse(yhat,df_test$mpg)

plot(yhat,df_test$mpg, xlab = "Predicted mpg", ylab = "Actual mpg", main = "Bagging tree model")
abline(0, 1, col = "blue")

####c
set.seed(101)

n.trees <- c(50,100,150,200,250,300,350,400,450,500,600,700,800)
n.var <- c(2,3,4,5,6)
rf.MSE <- c()
n.tree.var <-c()
for (i in n.trees){
  for(j in n.var){
  set.seed(101)
  rf.mod <- randomForest (mpg ~ ., data = df_train[valid,] , mtry = (ncol(df_train[valid,])-1)/j,ntree=i, importance = TRUE)
  yhat <- predict(rf.mod , newdata = df_train[-valid,])
  MSE<-get_mse(yhat,df_train[-valid,]$mpg)
  rf.MSE<-append(rf.MSE,MSE)
  n.tree.var <- append(n.tree.var,paste(i,j))
  
  }
}
print('the best combination of trees and variable subset is: ')
print(n.tree.var[which.min(rf.MSE)])

set.seed(101)
#bag.auto <- randomForest (mpg ~ ., data = df_train[valid,] , mtry = ncol(df_train[valid,])-1,sampsize=length(valid), importance = TRUE)
rf.auto <- randomForest (mpg ~ ., data = df_train , mtry = (ncol(df_train[valid,])-1)/2,ntree=150, importance = TRUE)
yhat <- predict(rf.auto , newdata = df_test)
rf.MSE<-get_mse(yhat,df_test$mpg)

plot(yhat,df_test$mpg, xlab = "Predicted mpg", ylab = "Actual mpg", main = "Random Forest tree")
abline(0, 1, col = "blue")

#### d
library(gam)

gam.m1 <- gam (mpg ~ year + cylinders + s(displacement,3) + s(horsepower,3) + s(weight,3) + s(acceleration,3) + origin ,
               data = df_train)
gam.m2 <- gam (mpg ~ year + s(cylinders,3) + displacement + horsepower + weight + acceleration + origin ,
               data = df_train)
gam.m3 <- gam (mpg ~ year + cylinders + displacement + s(horsepower,3) + s(weight,3) + s(acceleration,3) + origin ,
               data = df_train)
gam.m4 <- gam (mpg ~ year + cylinders + displacement + horsepower + s(weight,3) + s(acceleration,3) + origin ,
               data = df_train)
gam.m5 <- gam (mpg ~ year + cylinders + displacement + horsepower + weight + s(acceleration,3) + origin ,
               data = df_train)
par(mfrow=c(1,3))
plot (gam.m1, se = TRUE , col = " blue ")

gam.m1.MSE <- get_mse(predict(gam.m1,df_test),df_test$mpg)
gam.m1.MSE
gam.m2.MSE <- get_mse(predict(gam.m2,df_test),df_test$mpg)
gam.m2.MSE
gam.m3.MSE <- get_mse(predict(gam.m3,df_test),df_test$mpg)
gam.m3.MSE
gam.m4.MSE <- get_mse(predict(gam.m4,df_test),df_test$mpg)
gam.m4.MSE
gam.m5.MSE <- get_mse(predict(gam.m5,df_test),df_test$mpg)
gam.m5.MSE