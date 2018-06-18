#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#script will take args: 
#data file (csv), x column, y column, k, tolerance, output csv filename.

if (length(args)<6) {
  stop("Too few arguments, try again", call.=FALSE)
} 

data = args[1]
x_col = as.numeric(args[2])
y_col = as.numeric(args[3])
ind = c(x_col, y_col)
k = as.numeric(args[4])
tolerance = as.numeric(args[5])
output = args[6]

print(paste("File used: ", data))
print(paste("Columns for k means: ", x_col, y_col))
print(paste("K means with parameters: ", k, tolerance))
print(paste("Outputting files with path: ", output))

library(datasets)
library(ggplot2)


setwd("/Users/stevenhurwitt/Downloads/Upwork/k means/")

#iris = iris
#write.csv(iris, file = "iris.csv", col.names = T, row.names = F)
input = read.csv(data, header = T)
X = input[,ind]

#compare with R function
#irisCluster <- kmeans(X, 3, nstart = 20)
#irisCluster

#table(irisCluster$cluster, iris$Species)
#iris$cluster <- as.factor(irisCluster$cluster)

#ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$cluster)) + geom_point()


##########################
##########################

##header functions:

#assign clusters to data points
#for each data point, compute the distance between point i and mean of cluster j
#assign point i to cluster j based on min euclidean distance
Clustering <- function(m, means)
{
  clusters = c()
  n <- nrow(m)
  for(i in 1:n)
  {
    distances = c()
    k <- nrow(means)
    for(j in 1:k)
    {
      di <- m[i,] - means[j,]
      ds<-euclid2(di)
      distances <- c(distances, ds)
    }
    minDist <- min(distances)
    cl <- match(minDist, distances)
    clusters <- c(clusters, cl)    
  }
  return (clusters)
}

#update means for clusters
#calc new mean vector (length k) based on new cluster assignments
UpdateMeans <- function(m, cl, k)
{
  means <- c()
  for(c in 1:k)
  {
    # get the point of cluster c
    group <- which(cl == c)
    
    # compute the mean point of all points in cluster c
    mt1 <- mean(m[group,1])
    mt2 <- mean(m[group,2])
    vMean <- c(mt1, mt2)
    means <- c(means, vMean)
  }
  means <- createMeanMatrix(means)
  means[is.na(means)] <- 0
  return(means)
}

#distance function for euclidean dist b/t two points
dist <- function(x,y)
{
  d<-sqrt( sum((x - y) **2 ))
}

#create matrix for distances
createMeanMatrix <- function(d)
{
  matrix(d, ncol=2, byrow=TRUE)
}

# compute euclidean distance
euclid <- function(a,b){
  d<-sqrt(a**2 + b**2)
}
euclid2 <- function(a){
  d<-sqrt(sum(a**2))
}

#compute difference between new means and old means
delta <- function(oldMeans, newMeans)
{
  a <- newMeans - oldMeans
  max(euclid(a[, 1], a[, 2]))
}

#############K Means################


k_means <- function(m, k, tol)
{
  #initialization for k means: k random observations from data
  x <- m[, 1]
  y <- m[, 2]
  d=matrix(data=NA, ncol=0, nrow=0)
  threshold = tol
  
  samp_ind = sample(1:length(x), k, replace = F)
  for(i in 1:k){
    index = samp_ind[i]
    d <-  c(d, c(x[index], y[index]))}
  
  init <- matrix(d, ncol=2, byrow=TRUE)
  oldMeans <- init
  oldMeans 
  cl <- Clustering(m, oldMeans)
  cl
  means <- UpdateMeans(m, cl, k)
  thr <- delta(oldMeans, means)
  itr <- 1
  print("initial centroids calculated")
  
  #main algorithm: cluster points into k groups based on 
  #distance to mean of group, continue until distance between
  #mean vectors of successive runs is less than a tolerance, epsilon
  print("updating centroids...")
  while(thr > threshold)
  {
    cl <- Clustering(m, means)
    oldMeans <- means
    means <- UpdateMeans(m, cl, k)
    thr <- delta(oldMeans, means)
    itr <- itr+1
    print(paste("iteration ", itr))
  }
  print("... done")
  info = list(cl, thr, means, itr)
  return(info)
}


####################
###test function####

#test my function

print("Running k-means algorithm...")
km_output = k_means(X, k, tolerance)
output_data = input
output_data$cluster = as.factor(km_output[[1]])

print("Saving plot...")
png(paste(output, ".png"))
plt = ggplot(output_data, aes(output_data[,x_col], output_data[,y_col], color = output_data$cluster)) + geom_point()
plt = plt + xlab("X Variable") + ylab("Y Variable") 
plt = plt + labs(title = "Plot of X vs. Y colored by Clusters", colour = "Clusters")
print(plt)
dev.off()

print("Saving csv....")
write.csv(output_data, file = paste(output, ".csv"), col.names = T, row.names = F)
