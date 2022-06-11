set.seed(1000)
model_QDA <- qda(factor(label)~., data = data_training)

predictions_QDA <- model_QDA %>% predict(data_training)
p_train_QDA <- predictions_QDA$class
tab1_QDA <- table(Predicted = p_train_QDA, Actual = data_training$label)
tab1_QDA

library(klaR)
drawparti(data_training$label, data_training$X1, data_training$X2, method="qda", xlab="X1", ylab="X2"
          , col.wrong = "pink", imageplot = FALSE, 
          legend.err= FALSE,
          col.mean = "pink")
text(-1, 2.5, label="class 1",col='red',cex=1)
text(3, 1, label="class 2",col='blue',cex=1)
text(6, 0, label="class 3",col='green',cex=1)
title(main = "Decision boundary of QDA")


predictions_test_QDA <- model_QDA %>% predict(data_test)
p_test_QDA <- predictions_test_QDA$class
tab2_QDA <- table(Predicted = p_test_QDA, Actual = data_test$label)
tab2_QDA