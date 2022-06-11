

library(cluster)
library(rdist)
set.seed(100)
x <- matrix(rnorm(60*50),ncol=50)
x[1:20, ] <- x[1:20, ] + 2


x[21:40, ] <- x[21:40, ] + 2.5


x[41:60, ] <- x[41:60, ] + 3


plot(x)


df_k1 <- data.frame(x[1:20,])
df_k2 <- data.frame(x[21:40, ])
df_k3 <- data.frame(x[41:60, ])

df_k1$label <- as.factor('1')
df_k2$label <- as.factor('2')
df_k3$label <- as.factor('3')

data <- rbind(df_k1,df_k2,df_k3)

pr.out <- prcomp (data[,c(1:50)] , scale = TRUE)
dim (pr.out$x)
biplot (pr.out , scale = 0)
#pr.out$rotation = -pr.out$rotation
#pr.out$x = -pr.out$x
#biplot (pr.out , scale = 0)

library(ggplot2)

pc_data <- data.frame(pr.out$x)
pc_data$label <- 1
pc_data[1:20,]$label <- 1
pc_data[21:40,]$label <- 2
pc_data[41:60,]$label <- 3
pc_data$label <- as.factor(pc_data$label)
plot1= ggplot(data=pc_data,aes(PC1,PC2,color=label)) + 
  geom_point()+scale_color_manual(values = c("1" = "red", "2" = "blue","3"="green"))
plot1 + ggtitle('Plot of 1st two PC') 

set.seed(2)
km3.out <- kmeans (data[,c(1:50)], 3, nstart = 20)
km3.out$cluster
#ss <- function(x) sum(scale(x, scale = FALSE)^2)

data$clustersK3 <- km3.out$cluster

km3.out
table(data[,c('label','clustersK3')])


#d
set.seed(2)
km2.out <- kmeans (data[,c(1:50)], 2, nstart = 20)
data$clustersK2 <- km2.out$cluster

data$clustersK2 <- as.factor(data$clustersK2)

km2.out
table(data[,c('label','clustersK2')])


#e
set.seed(2)
km4.out <- kmeans (data[,c(1:50)], 4, nstart = 20)
km4.out$cluster
data$clustersK4 <- km4.out$cluster

data$clustersK4 <- as.factor(data$clustersK4)

km4.out
table(data[,c('label','clustersK4')])

#f
set.seed(2)
km3PCA.out <- kmeans (pc_data[,c(1:2)], 3, nstart = 20)
km3PCA.out$cluster
pc_data$clustersK3 <- km3PCA.out$cluster

pc_data$clustersK3 <- as.factor(pc_data$clustersK3)

km3PCA.out
table(pc_data$label,pc_data$clustersK3)

par(mfrow=c(1,1))

## Visualizing clusters
y_kmeans <- km3PCA.out$cluster
clusplot(pc_data[,c(1:2)],
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste("Cluster PCA"),
         xlab = 'PC1',
         ylab = 'PC2')


pc_data$clustersK3 <- ifelse(pc_data$clustersK3== 1,3,ifelse(pc_data$clustersK3== 2,1,2))
pc_data$clustersK3 <- as.factor(pc_data$clustersK3)

plot2= ggplot(data=pc_data,aes(PC1,PC2,color=label,shape=clustersK3)) + 
  geom_point()+scale_color_manual(values = c("1" = "red", "2" = "blue","3"="green"))
plot2 + ggtitle('Cluster with PCA') 

#g
set.seed(2)
km3Scale.out <- kmeans (scale(data[,c(1:50)],center = TRUE,scale=TRUE), 3, nstart = 20)
km3Scale.out$cluster
data$clustersK3_scale <- km3Scale.out$cluster
data$clustersK3_scale <- as.factor(data$clustersK3_scale)
km3Scale.out
table(data$label,data$clustersK3_scale)
table(data$label,data$clustersK3)






pc_data$clustersK3_scale <- km3Scale.out$cluster
pc_data$clustersK3_scale <- as.factor(pc_data$clustersK3_scale)

#pc_data$clustersK3_scale <- ifelse(pc_data$clustersK3_scale== 1,3,ifelse(pc_data$clustersK3_scale== 3,2,1))
#pc_data$clustersK3_scale <- as.factor(pc_data$clustersK3_scale)

plot2= ggplot(data=pc_data,aes(PC1,PC2,color=clustersK3_scale,shape=clustersK3)) + 
  geom_point()+scale_color_manual(values = c("1" = "red", "2" = "blue","3"="green"))
plot2 + ggtitle('Cluster with PCA') 
