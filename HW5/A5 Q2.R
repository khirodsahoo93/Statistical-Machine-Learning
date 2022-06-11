
library(cluster)
library(rdist)
set.seed(100)
x <- matrix(rnorm(100*2),ncol=2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4

x[26:50, 1] <- x[26:50, 1] + 6
x[26:50, 1] <- x[26:50, 1] + 7

x[51:75, 1] <- x[51:75, 1] + 9
x[51:75, 1] <- x[51:75, 1] + 10

plot(x)

#Compute LHS

LHS.WSS <- function(x){
    n.Ck <- nrow(x)
    sum <- 0
    for (i in 1:n.Ck)
      
      for(j in 1:n.Ck){
        #if(j<=n.Ck)
        {
      for(k in 1:ncol(x)){
        
        sum<- sum + (x[i,k]-x[j,k])^2

      }
      }
      }
    
    sum <- sum / n.Ck
    return(sum)
}

RHS.centroid.dist <- function(x){
  n.Ck <- nrow(x)
  sum <- 0
  for (i in 1:n.Ck){
      for(k in 1:ncol(x)){
        
        sum<- sum + (x[i,k]-mean(x[,k]))^2
    }
  }
  return(sum)
}


LHS<- LHS.WSS(x[1:25,]) + LHS.WSS(x[26:50,]) + LHS.WSS(x[51:75,]) + LHS.WSS(x[76:100,])
LHS
RHS<- 2*(RHS.centroid.dist(x[1:25,]) + RHS.centroid.dist(x[26:50, ]) + RHS.centroid.dist(x[51:75, ]) + RHS.centroid.dist(x[76:100, ]))
RHS


