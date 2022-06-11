library(ISLR2)
attach(Wage)

par(mfrow = c(1,1))


get_mse = function(estimate, truth) {
  mean(mean((estimate - truth) ^ 2))
}


#set.seed(200)
df <- data.frame(Wage)
set.seed(2037)
train=sample(1:nrow(df),2*nrow(df)/3)
set.seed(291)
#set.seed(100)
valid=sample(1:length(train),length(train)/2)
X <- data.frame(df[, 'age'])
colnames(X) <- 'age'
Y <- data.frame(df[,'wage'])
colnames(Y) <- 'wage'
df_train <- df[train,c('age','wage')]
df_test <- df[-train,c('age','wage')]



## a

poly.MSE.arr <- c()
n.degree <- c(2,3,4,5,6,7,8,9,10,11)
for(i in n.degree){
  poly.fit <- lm(wage ~ poly(age , i),data=df_train)
  yhat <- predict(poly.fit,newdata=df_test,se=TRUE)
  MSE<-get_mse(yhat$fit,df_test$wage)
  poly.MSE.arr<-append(poly.MSE.arr,MSE)
}
print('the degree with least MSE is: ')
print(n.degree[which.min(poly.MSE.arr)])

plot(n.degree,poly.MSE.arr,type='b',col='red',xlab='degrees',ylab='validation MSE')
title('MSE for different degrees')

#fitting with the best degree of polynomial
poly.fit <- lm(wage ~ poly(age , 3),data=df_train)
yhat <- predict(poly.fit,newdata=df_test,se=TRUE)
poly.MSE<-get_mse(yhat$fit,df_test$wage)


agelims <- range (df_test$age)
age.grid <- seq (from = agelims[1], to = agelims [2])

#plot the polynomial function
poly.predict <- predict(poly.fit,newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind (poly.predict$fit + 2 * poly.predict$se.fit ,
                   poly.predict$fit - 2 * poly.predict$se.fit)

plot (df_test$age , df_test$wage , xlim = agelims , cex = .5, col = " darkgrey ",xlab='age',ylab='wage')
title (" Degree -3 Polynomial ")
lines (age.grid, poly.predict$fit , lwd = 2, col = " blue ")
matlines (age.grid , se.bands, lwd = 1, col = " blue ", lty = 3)

### b
table(cut(df_train$age , 4))

n.cuts <- seq(2,20,1)
step.MSE.arr<- c()
for(i in n.cuts){
  step.fit <- lm(wage ~cut (age , i),data=df_train[valid,])
  yhat <- predict(step.fit,newdata=df_train[-valid,],se=TRUE)
  MSE<-get_mse(yhat$fit,df_train[-valid,]$wage)
  step.MSE.arr<-append(step.MSE.arr,MSE)
}
print('the number of cuts with least MSE is: ')
print(n.knots[which.min(step.MSE.arr)])

plot(n.knots,step.MSE.arr,type='b',col='red',xlab='knots',ylab='validation MSE')
title('MSE for different cuts')

#fitting step function with best cut value
step.fit <- lm(wage ~ cut (age , 9), data = df_train)
yhat <- predict(step.fit,newdata=df_test,se=TRUE)
step.MSE<-get_mse(yhat$fit,df_test$wage)
summary(step.fit)


#plot the step function
step.predict <- predict(step.fit,newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind (step.predict$fit + 2 * step.predict$se.fit ,
                   step.predict$fit - 2 * step.predict$se.fit)

plot (df_test$age , df_test$wage , xlim = agelims , cex = .5, col = " darkgrey ",xlab='age',ylab='wage')
title (" step function with cuts=9 ")
lines (age.grid, step.predict$fit , lwd = 2, col = " blue ")
matlines (age.grid , se.bands, lwd = 1, col = " blue ", lty = 3)

###c

piece.MSE.arr<- c()
piece.var <- c()
for (i in n.degree){
  for(j in n.cuts){
    piece.fit <- lm(wage ~ cut(age,j):poly(age , i),data=df_train[valid,])
    yhat <- predict(piece.fit , newdata = df_train[-valid,])
    MSE<-get_mse(yhat,df_train[-valid,]$wage)
    piece.MSE.arr<-append(piece.MSE.arr,MSE)
    piece.var <- append(piece.var,paste(i,j))
    
  }
}
print('the best combination of degrees and cuts is: ')
print(piece.var[which.min(piece.MSE.arr)])


#fitting piecewise polynomial function with best degree and cut value
piece.fit <- lm(wage ~ cut(age,3):poly(age , 2), data = df_train)
yhat <- predict(piece.fit,newdata=df_test,se=TRUE)
piece.MSE<-get_mse(yhat$fit,df_test$wage)
summary(piece.fit)


#plot the step function
step.predict <- predict(piece.fit,newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind (step.predict$fit + 2 * step.predict$se.fit ,
                   step.predict$fit - 2 * step.predict$se.fit)

plot (df_test$age , df_test$wage , xlim = agelims , cex = .5, col = " darkgrey ",xlab='age',ylab='wage')
title (" piecewise polynomial with degree=2,cuts=3 ")
lines (age.grid, step.predict$fit , lwd = 2, col = " blue ")
matlines (age.grid , se.bands, lwd = 1, col = " blue ", lty = 3)
abline(v=38.7)
abline(v=59.3)



### d

library(splines)
spline.MSE.arr <- c()
n.dof <- seq(3,30,1)
for(i in n.dof){
  spline.fit <- lm(wage ~ bs(age , df=i,degree=3), data = df_train[valid,])
  yhat <- predict(spline.fit,newdata=df_train[-valid,],se=TRUE)
  MSE<-get_mse(yhat$fit,df_train[-valid,]$wage)
  spline.MSE.arr<-append(spline.MSE.arr,MSE)
}
print('the dof with least MSE is: ')
print(n.dof[which.min(spline.MSE.arr)])

plot(n.dof,spline.MSE.arr,type='b',col='red',xlab='degrees',ylab='validation MSE')
title('MSE for different degree of freedom')


spline.fit <- lm(wage ~ bs(age , df=4,degree=3), data = df_train)
yhat <- predict(spline.fit,newdata=df_test,se=TRUE)
spline.MSE<-get_mse(yhat$fit,df_test$wage)
summary(spline.fit)


splines.predict <- predict (spline.fit , newdata = list (age = age.grid), se = T)
#spline_MSE <- get_mse(splines.predict$fit,age.grid)
se.bands <- cbind (splines.predict$fit + 2 * splines.predict$se.fit ,
                   splines.predict$fit - 2 * splines.predict$se.fit)
plot (df_test$age , df_test$wage , xlim = agelims , cex = .5, col = " darkgrey ",xlab='age',ylab='wage')
title (" cubic spline with dof=4 ")
lines (age.grid, splines.predict$fit , lwd = 2, col = " red ")
matlines (age.grid , se.bands, lwd = 1, col = " red ", lty = 3)


### e

#xx <- unique(sort(c(seq(0, 30, by = .2),  unique(df_test$age))))
smooth.fit <- smooth.spline(df_train$age ,df_train$wage ,cv=TRUE)
smooth.fit$df
smooth.fit <- smooth.spline(df_train$age,df_train$wage, df=10 )


yhat <- predict (smooth.fit , df_test$age)

#y_actual <- df_test['wage'][df_test['age']== xx]
smooth_MSE <- get_mse(yhat$y,df_test$wage)

smooth.predict <- predict(smooth.fit , age.grid)
plot (df_test$age , df_test$wage , xlim = agelims , cex = .5, col = " darkgrey ",xlab='age',ylab='wage')
title (" Smoothing spline with dof=5.34 ")
lines (age.grid, smooth.predict$y , lwd = 2, col = " red ")



library(segmented)

#fit simple linear regression model
fit <- lm(wage ~  cbind (age , age^2, age^3, age^4),data=df_train)

#fit piecewise regression model to original model, estimating a breakpoint at x=9
segmented.fit <- segmented(fit, seg.Z = ~ cbind (age , age^2, age^3, age^4) )

#view summary of segmented model
summary(segmented.fit)

