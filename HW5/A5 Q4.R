library(ISLR2)
df_OJ <- data.frame(OJ)
train <- sample(nrow(df_OJ),800)
df_OJ_training <- df_OJ[train,]
df_OJ_test <- df_OJ[-train,]

library(e1071)


#b
#Regression with SVM
set.seed(1)
modelsvm = svm(Purchase~.,df_OJ_training,cost=0.01,kernel='linear')
summary(modelsvm)
plot(modelsvm , df_OJ_training,PriceCH ~PriceMM)


#c
#Predict using SVM regression
predYsvm.train = predict(modelsvm, df_OJ_training )
cnf.train <- table (predict = predYsvm.train , truth = df_OJ_training$Purchase)
train.error <- (cnf.train[1,2]+cnf.train[2,1])/sum(cnf.train)*100
train.error
predYsvm.test = predict(modelsvm, df_OJ_test )
cnf.test <- table (predict = predYsvm.test , truth = df_OJ_test$Purchase)
test.error <- (cnf.test[1,2]+cnf.test[2,1])/sum(cnf.test)*100
test.error

#d

#Tune the SVM model
#,ranges=list(elsilon=seq(0,1,0.1)
set.seed(1)
OptModelsvm=tune(svm, Purchase~., data=df_OJ_training,kernel='linear', ranges=list(cost=seq(0.01,10,0.1)))
print(OptModelsvm)
#Find out the best model
BstModel=OptModelsvm$best.model
BstModel

#e
set.seed(1)
modelsvm = svm(Purchase~.,df_OJ_training,cost=BstModel$cost,kernel='linear')
summary(modelsvm)
predYsvm.train = predict(modelsvm, df_OJ_training )
cnf.train <- table (predict = predYsvm.train , truth = df_OJ_training$Purchase)
train.error <- (cnf.train[1,2]+cnf.train[2,1])/sum(cnf.train)*100
train.error
predYsvm.test = predict(modelsvm, df_OJ_test )
cnf.test <- table (predict = predYsvm.test , truth = df_OJ_test$Purchase)
test.error <- (cnf.test[1,2]+cnf.test[2,1])/sum(cnf.test)*100
test.error

#f

#-----------------------------------------------------
set.seed(1)
modelsvm = svm(Purchase~.,df_OJ_training,cost=0.01,kernel='radial')
summary(modelsvm)
plot(modelsvm , df_OJ_training,PriceCH ~PriceMM)


#Predict using SVM regression
predYsvm.train = predict(modelsvm, df_OJ_training )
cnf.train <- table (predict = predYsvm.train , truth = df_OJ_training$Purchase)
train.error <- (cnf.train[1,2]+cnf.train[2,1])/sum(cnf.train)*100
train.error
predYsvm.test = predict(modelsvm, df_OJ_test )
cnf.test <- table (predict = predYsvm.test , truth = df_OJ_test$Purchase)
test.error <- (cnf.test[1,2]+cnf.test[2,1])/sum(cnf.test)*100
test.error


#Tune the SVM model
#,ranges=list(elsilon=seq(0,1,0.1)
set.seed(1)
OptModelsvm=tune(svm, Purchase~., data=df_OJ_training,kernel='radial', ranges=list(cost=seq(0.01,10,0.1)))
print(OptModelsvm)
#Find out the best model
BstModel=OptModelsvm$best.model
BstModel

set.seed(1)
modelsvm = svm(Purchase~.,df_OJ_training,cost=BstModel$cost,kernel='radial')
summary(modelsvm)
predYsvm.train = predict(modelsvm, df_OJ_training )
cnf.train <- table (predict = predYsvm.train , truth = df_OJ_training$Purchase)
train.error <- (cnf.train[1,2]+cnf.train[2,1])/sum(cnf.train)*100
train.error
predYsvm.test = predict(modelsvm, df_OJ_test )
cnf.test <- table (predict = predYsvm.test , truth = df_OJ_test$Purchase)
test.error <- (cnf.test[1,2]+cnf.test[2,1])/sum(cnf.test)*100
test.error

#g
set.seed(1)
modelsvm = svm(Purchase~.,df_OJ_training,cost=0.01,kernel='polynomial',degree=2)
summary(modelsvm)
plot(modelsvm , df_OJ_training,PriceCH ~PriceMM)


#Predict using SVM regression
predYsvm.train = predict(modelsvm, df_OJ_training )
cnf.train <- table (predict = predYsvm.train , truth = df_OJ_training$Purchase)
train.error <- (cnf.train[1,2]+cnf.train[2,1])/sum(cnf.train)*100
train.error
predYsvm.test = predict(modelsvm, df_OJ_test )
cnf.test <- table (predict = predYsvm.test , truth = df_OJ_test$Purchase)
test.error <- (cnf.test[1,2]+cnf.test[2,1])/sum(cnf.test)*100
test.error


#Tune the SVM model
#,ranges=list(elsilon=seq(0,1,0.1)
set.seed(1)
OptModelsvm=tune(svm, Purchase~., data=df_OJ_training,kernel='polynomial',degree=2, ranges=list(cost=seq(0.01,10,0.1)))
print(OptModelsvm)
#Find out the best model
BstModel=OptModelsvm$best.model
BstModel

set.seed(1)
modelsvm = svm(Purchase~.,df_OJ_training,cost=BstModel$cost,kernel='polynomial',degree=2)
summary(modelsvm)
predYsvm.train = predict(modelsvm, df_OJ_training )
cnf.train <- table (predict = predYsvm.train , truth = df_OJ_training$Purchase)
train.error <- (cnf.train[1,2]+cnf.train[2,1])/sum(cnf.train)*100
train.error
predYsvm.test = predict(modelsvm, df_OJ_test )
cnf.test <- table (predict = predYsvm.test , truth = df_OJ_test$Purchase)
test.error <- (cnf.test[1,2]+cnf.test[2,1])/sum(cnf.test)*100
test.error
