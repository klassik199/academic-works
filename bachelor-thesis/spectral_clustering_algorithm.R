library(ClusterR)
library(cluster)

# Function, which receives matrix or data frame with points to cluster, number of clusters k and 
# logical value normal, indicating whether to carry out a normalize spectral clustering or not. 
# It prepares a similarity matrix S based on Gaussian similarity function (with scaling parameter sigma) 
# and then performs a respective spectral clustering based on S into k clusters and returns them.

SCA <- function(X, sigma,  k, normal){
  # Assertions on proper parameters
  
  stopifnot(is.matrix(X) | is.data.frame(X), 
            !is.na(X),
            all(sapply(X, is.numeric)), 
            is.numeric(sigma),
            sigma > 0,
            k == as.integer(k),
            k >= 2,
            is.logical(normal))
  
  n <- nrow(X)
  
  # Preparing similarity matrix S
  
  S <- exp(-dist(X, diag = T, upper = T)^2 / (2 * (sigma^2)))
  
  # Converting matrix of similarities S into matrix of weights W, in this case W is the same as S
  W <- as.matrix(S)

  # Preparing a respective Graph Laplacian
  
  degs <- numeric(n)
  for(i in 1:n){
    degs[i] <- sum(W[i,])
  }
  D <- diag(degs, nrow = n, ncol = n)
  D_12 <- diag((degs)**(-1/2), nrow = n, ncol = n)
  
  if(!normal){
    L <- D - W
  } else{
    L <- D_12%*%(D - W)%*%D_12
  }
  
  # Computing first k-1 eigenvalues from second smallest one and their corresponding eigenvectors of the Graph Laplacian
  
  eigenvalues <- sort(eigen(L)$values)
  
  eigenvectors <- eigen(L)$vectors[,order(eigen(L)$values)]
  
  if(!normal){
    clusters <- kmeans(eigenvectors[,2:k], k)$cluster
  } else{
    clusters <- kmeans(D_12%*%(eigenvectors[,2:k]), k)$cluster
  }
  
  X_clustered <- cbind(X, clusters)
  colnames(X_clustered)[ncol(X_clustered)] <- "cluster"
  
  attr(X_clustered, "graphlaplacian") <- L
  attr(X_clustered, "degreematrix") <- D_12
  attr(X_clustered, "eigvalues") <- eigenvalues
  attr(X_clustered, "eigvectors") <- eigenvectors
  
  return(X_clustered)
}