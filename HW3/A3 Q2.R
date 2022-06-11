set.seed(100)
y <- rnorm(100)

set.seed(101)
x <- matrix(rnorm(100*10000),ncol=10000)
df <- data.frame(x,y)

#a
cor_mat <- cor(df[,1:10000],df[,c('y')])
abs_corr <- abs(cor_mat)
cor_mat_sort <- sort(abs_corr,decreasing=TRUE,index.return=TRUE)
hist(cor_mat,breaks=20,col='blue',xlab='correlation bins',ylab='freq',title='histogram plot showing the freq of correlation between each response and predictor variable')
top_10_corr <- cor_mat_sort$x[1:10]
top_10_idx <- cor_mat_sort$ix[1:10]

#train test split
library(caTools)

#(b) Now try out “Option 1” with q = 10. What is the estimated test error?
#option 1 - best subset selection
df_top10 <- df[,c(top_10_idx,10001)]
set.seed(2037)
spl = sample.split(df_top10$y, SplitRatio = 0.6)
train = subset(df_top10, spl == TRUE)
test = subset(df_top10, spl == FALSE)
set.seed(2999)
model_opt1 <- lm(train$y ~. ,data=train )
summary(model_opt1)
predict_test_opt1 <- predict(model_opt1,test)
MSE_test_opt1 <- mean((predict_test_opt1-test$y)^2)
MSE_test_opt1
#(c)Now try out “Option 2” with q = 10. What is the estimated test error?
set.seed(2037)
spl = sample.split(df$y, SplitRatio = 0.6)
train = subset(df, spl == TRUE)
test = subset(df, spl == FALSE)
cor_mat_opt2 <- cor(train[,1:10000],train[,c('y')])
abs_corr_opt2 <- abs(cor_mat_opt2)
cor_mat_sort_opt2 <- sort(abs_corr_opt2,decreasing=TRUE,index.return=TRUE)
top_10_idx_opt2 <- cor_mat_sort_opt2$ix[1:10]
train_top10_opt2 <- train[,c(top_10_idx_opt2,10001)]
set.seed(2999)
model_opt2 <- lm(train_top10_opt2$y ~. ,data=train_top10_opt2 )
summary(model_opt2)
predict_test_opt2 <- predict(model_opt2,test)
MSE_test_opt2 <- mean((predict_test_opt2-test$y)^2)
MSE_test_opt2