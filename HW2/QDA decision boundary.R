sigma_est_k1 <- matrix(cov(subset(data_training,label=='1')[,c('X1','X2')]),2)
sigma_est_k2 <- matrix(cov(subset(data_training,label=='2')[,c('X1','X2')]),2)
sigma_est_k3 <- matrix(cov(subset(data_training,label=='3')[,c('X1','X2')]),2)
sigma_est <- (sigma_est_k1*50 + sigma_est_k2* 50 + sigma_est_k3*50)/(150-3)

#decision boundary between k=1 and k=2
plot_dec_QDA <- function(sigma_est_k1,sigma_est_k2,mu1_est,mu2_est,color){
A_mat <- solve(sigma_est_k1-sigma_est_k2)
B_mat <- t((solve(sigma_est_k2) %*% mu2_est) - (solve(sigma_est_k1) %*% mu1_est))
C_mat <- (t(mu1_est) %*% solve(sigma_est_k1) %*% mu1_est) - (t(mu2_est) %*% solve(sigma_est_k2) %*% mu2_est) + log(det(sigma_est_k1)/det(sigma_est_k2))

a <- A_mat[1,1]
#b <- A_mat[1,2]
b <-0
c <- A_mat[2,2]
#d <- B_mat[1,1]
d <-0
#e <- B_mat[1,2]
e <-0
f <- C_mat[1,1]

X1_sim <- seq(-2,8,0.01)
X2_sim <- seq(-2,8,0.01)
X1_new <-c()
X2_new <-c()
for (x1 in X1_sim){
  #print('Hi')
  for (x2 in X2_sim){
    #print(round((a*(x1^2) +2*x1*x2+c*(x2^2)+2*d*x1+2*e*x2+f),0))
    if (round((a*(x1^2) +2*b*x1*x2+c*(x2^2)+2*d*x1+2*e*x2+f),2)==0){
      #print('yes')
      X2_new <- append(X2_new,x2)
      X1_new <- append(X1_new,x1)
      }
    
  }
}
data_new <- data.frame(X1_new,X2_new)
return(geom_point(data=data_new,aes(x = X1_new, y = X2_new,color=color)))
}

#plot_dec_QDA(sigma_est_k2,sigma_est_k3,mu2_est,mu3_est)
plot_new2<-plot_dec_QDA(sigma_est_k1,sigma_est_k2,mu1_est,mu2_est,'red')
plot_new1<-plot_dec_QDA(sigma_est_k2,sigma_est_k3,mu2_est,mu3_est,'blue') 
plot_new3<-plot_dec_QDA(sigma_est_k3,sigma_est_k1,mu3_est,mu1_est,'blue') 
#training data plus QDA
plot1= ggplot(data=data_training,aes(X1,X2,color=label)) + 
  geom_point()+scale_color_manual(values = c("1" = "red", "2" = "blue","3"="green"))
plot1 + ggtitle('Plot of training data') 
plot1 + plot_new2

  
       