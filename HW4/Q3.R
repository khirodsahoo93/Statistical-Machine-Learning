b1 <- function(X){
  i1 <- ifelse(0<=X & X<=2,1,0)
  i2 <- ifelse(1<=X & X<=2,1,0)
  return(i1 - (X+1)*i2)
}

b2 <- function(X){
  i1 <- ifelse(3<=X & X<=4,1,0)
  i2 <- ifelse(4<X & X<=5,1,0)
  return((2*X-2)*i1-i2)
}


X<-seq(-2,6,0.1)
set.seed(100)
e <- rnorm(length(X),0,1)

Y <- 2 + 3*b1(X) -2*b2(X) + e

plot(X,Y,type='b')
