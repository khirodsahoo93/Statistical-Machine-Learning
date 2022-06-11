
df_q3 <- read.csv('superconductor.csv')

#df_q3[df_q3=='?'] <- NA
#miss <- c()
#for(i in 1:ncol(df_q3)) {
#  if(length(which(is.na(df_q3[,i]))) >=800000) miss <- append(miss,i) 
#}
#df_q3_temp <- df_q3[,-miss]
sum(is.na(df_q3))
#colSums(is.na(df_q3_temp))
#ncol(df_q3_temp)

###########
#b
set.seed(2037)
train=sample(1:nrow(df_q3),2*nrow(df_q3)/3)
model_q3 <- lm(critical_temp ~ .,data=df_q3[train,])
summary(model_q3)
crit_predict <- predict(model_q3,df_q3[-train,])
MSE_test_q3 <- get_mse(crit_predict,df_q3[-train,'critical_temp'])
MSE_test_q3

################
#c
X <- df_q3[, !names(df_q3) %in% c('critical_temp')] 
Y <- df_q3[,'critical_temp']
#standardize variables
#for (col in colnames(X)){
#  sd_x <- sd(X[,col])
#  mean_x <- mean(X[,col])
#  X[,col] <- (X[,col]-mean_x)/sd_x
#}
library(glmnet)
X.train <- X[train,]
Y.train <- Y[train]
X.test <- X[-train,]
Y.test <- Y[-train]
grid <- 10^seq(6,-2,length=100)
#########
set.seed(600)
ridge_model <- glmnet(X.train,Y.train,alpha=0,lambda=grid,thresh=1e-12)
dim(coef(ridge_model))
coef_mat <- as.matrix(ridge_model$beta)
matplot(x = ridge_model$lambda, y = t(ridge_model$beta)[,1:5], type = "l", main="Ridge regression", xlab="λ", ylab="Coefficient-value", log = "x")
nr = 5
legend("bottomright", rownames(ridge_model$beta)[1:5], col=seq_len(nr), cex=0.5, lty=seq_len(nr), lwd=1)

#row.names(coef_mat) <- colnames(X)
#colnames(coef_mat) <- ridge_model$lambda
#xtick<-seq(10^(-2), 10, length.out=5)
#axis(side=1,at=xtick,labels=FALSE)
ridge.predict <- predict(ridge_model,s=2,newx=as.matrix(X.test))
MSE_ridge <- get_mse(ridge.predict,as.matrix(Y.test))
MSE_ridge
###########
#d

set.seed(5400)
cv.out.ridge <- cv.glmnet(as.matrix(X.train),as.matrix(Y.train),alpha=0,lambda=grid,nfolds=5)
plot(cv.out.ridge)
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
matplot(x = lasso_model$lambda, y = t(lasso_model$beta)[,1:10], type = "l", main="Lasso regression", xlab="λ", ylab="Coefficient-value", log = "x")
nr = 10
legend("bottomright", rownames(ridge_model$beta)[1:10], col=seq_len(nr), cex=0.5, lty=seq_len(nr), lwd=1)
lasso.predict <- predict(lasso_model,s=2,newx=as.matrix(X.test))
MSE_lasso <- get_mse(lasso.predict,as.matrix(Y.test))
MSE_lasso
#f
set.seed(5400)
cv.out.lasso <- cv.glmnet(as.matrix(X.train),as.matrix(Y.train),alpha=1,lambda=grid,nfolds=5)
plot(cv.out.lasso)
best_lambda_lasso <- cv.out.lasso$lambda.min
best_lambda_lasso
cv.out.lasso

lasso.predict_bestlam <- predict(lasso_model,s=best_lambda,newx=as.matrix(X.test))
MSE_lasso_bestlam <- get_mse(lasso.predict_bestlam,as.matrix(Y.test))
MSE_lasso_bestlam

out <- glmnet(X.train,Y.train,alpha=1,lambda=grid)
lasso.coef <- predict(out,type='coefficients',s=best_lambda_lasso)[1:20,]
lasso.coef[2]
