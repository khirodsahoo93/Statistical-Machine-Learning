
df_q3 <- read.csv('superconductor.csv')
sum(is.na(df_q3))


###########
#b

set.seed(2037)
train=sample(1:nrow(df_q3),2*nrow(df_q3)/3)
X <- df_q3[, !names(df_q3) %in% c('critical_temp')] 
Y <- df_q3[,'critical_temp']
X.train <- X[train,]
Y.train <- Y[train]
X.test <- X[-train,]
Y.test <- Y[-train]
set.seed(2039)
validation=sample(1:nrow(X.train), nrow(X.train)/2)
model_lm_q3 <- lm(critical_temp ~ .,data=df_q3[-validation,])
#calculate MSE on the validation set
crit_predict <- predict(model_lm_q3,X.train[validation,])
MSE_est_test_q3 <- get_mse(crit_predict,Y.train[validation])
MSE_est_test_q3

#calculate MSE on the test set
crit_predict_test <- predict(model_lm_q3,X.test)
MSE_test_q3 <- get_mse(crit_predict_test,Y.test)
MSE_test_q3
################
#c
library(glmnet)
grid <- 10^seq(6,-2,length=100)
#########
set.seed(600)
ridge_model <- glmnet(X.train,Y.train,alpha=0,lambda=grid,thresh=1e-12)
matplot(x = ridge_model$lambda, y = t(ridge_model$beta)[,1:10], type = "l", 
        main="Ridge regression", xlab="λ", ylab="Standardized coefficients", log = "x")
nr = 10
legend("bottomright", rownames(ridge_model$beta)[1:10], col=seq_len(nr), cex=0.5, 
       lty=seq_len(nr), lwd=1)
ridge.predict <- predict(ridge_model,s=2,newx=as.matrix(X.test))
MSE_ridge <- get_mse(ridge.predict,as.matrix(Y.test))
MSE_ridge
###########
#d
grid <- 10^seq(6,-2,length=100)
set.seed(5400)
cv.out.ridge <- cv.glmnet(as.matrix(X.train),as.matrix(Y.train),alpha=0,lambda=grid,nfolds=5,
                          type.measure='mse')
best_lambda_ridge <- cv.out.ridge$lambda.min
best_lambda_ridge
cv.out.ridge

ridge.predict_bestlam <- predict(ridge_model,s=best_lambda_ridge,newx=as.matrix(X.test))
MSE_ridge_bestlam <- get_mse(ridge.predict_bestlam,as.matrix(Y.test))
MSE_ridge_bestlam

###############
#e
set.seed(600)
lasso_model <- glmnet(X.train,Y.train,alpha=1,lambda=grid)
matplot(x = lasso_model$lambda, y = t(lasso_model$beta)[,1:10], type = "l",
        main="Lasso regression", xlab="λ", ylab="Standardized coefficients", log = "x")
nr = 10
legend("bottomright", rownames(ridge_model$beta)[1:10], col=seq_len(nr), 
       cex=0.5, lty=seq_len(nr), lwd=1)
lasso.predict <- predict(lasso_model,s=2,newx=as.matrix(X.test))
MSE_lasso <- get_mse(lasso.predict,as.matrix(Y.test))
MSE_lasso
#f
set.seed(5400)
cv.out.lasso <- cv.glmnet(as.matrix(X.train),as.matrix(Y.train),alpha=1,lambda=grid,
                          type.measure='mse',nfolds=5,thresh=1e-12)
plot(cv.out.lasso)
best_lambda_lasso <- cv.out.lasso$lambda.min
best_lambda_lasso
cv.out.lasso

lasso.predict_bestlam <- predict(lasso_model,s=best_lambda,newx=as.matrix(X.test))
MSE_lasso_bestlam <- get_mse(lasso.predict_bestlam,as.matrix(Y.test))
MSE_lasso_bestlam

set.seed(600)
out <- glmnet(X.train,Y.train,alpha=1,lambda=grid)
lasso.coef <- predict(out,type='coefficients',s=best_lambda_lasso)
list(rownames(lasso.coef)[lasso.coef[,1]!=0])
