PolynomialDesign <- function(x.vector, d)
{
  x_len = length(x.vector)
  X = matrix(0,x_len,d+1)
  for (j in 1:x_len){
    for (i in 0:d){
      X[j,i+1] = x.vector[j]^i
      #print(i)
    }
  }
  return(X)
}

norm_pol_des <-function(x,y2,X,y){ # needed to visualize the contour
  n <- dim(X)[1]
  sol <- matrix(0, length(x), length(y2))
  for (i in 1:length(x)){
    for (j in 1:length(y2)){
      sol[i,j] = norm(y-X%*%c(x[i],y2[j]))/n
    }
  }
  return(sol)
}

SoftThreshholding <- function(z, lambda){
  if (abs(z)<= lambda){
    return (0)
  }
  if(z > lambda){
    return (z - lambda)
  }
  return(z+lambda) 
}

n<- 20; d <- 10; nupdates <- 30
beta_hat = runif(d+1, min = -10, max = 10)
X <- PolynomialDesign(runif(n, min = -1, max = 1), d)
regression.vector <- c(2, 2, rep(0, d-1))
y <- X %*% regression.vector+ rnorm(n)

CreateDesign <-function(n, d){
  X <- PolynomialDesign(runif(n, min = -1, max = 1), d)
  regression.vector <- c(2, 2, rep(0, d-1))
  y <- X %*% regression.vector+ rnorm(n)
  Design <- list(X,y, regression.vector)
  names(Design) <- c("X","y", "beta")
  return(Design)
}

updateCoordinate <- function(beta_hat, coordinate,y,X, lambda){
  xitxi = t(X[,coordinate])%*%X[,coordinate]
  sum_wo_i = 0
  for (i in 1:dim(X)[2]){
    if (i != coordinate){
      sum_wo_i = sum_wo_i + X[,i] * beta_hat[i]
    }
  }
  r_i = y -sum_wo_i
  beta_hat[coordinate] = SoftThreshholding(t(X[,coordinate])%*% r_i / xitxi, lambda/xitxi)
  return(beta_hat)
}

LassoCoordinateDescent <- function(X,y,lambda, max_iter = 30, vis = FALSE){
  beta_hat = rep(0, dim(X)[2]) # runif(dim(X)[2], min = -10, max = 10)
  if (vis){
    list_beta_hat = beta_hat
  }
  #regression.vector <- c(1, 1, 1, rep(0, p-3))
  for (i in 1:max_iter){
   # if(i%%1==0){
    #  print(norm(regression.vector-beta_hat,"2"))
    #}
    index = sample(1:dim(X)[2])
    #index = 1:1:dim(X)[2]
    for (j in 1:dim(X)[2]){
      beta_hat <- updateCoordinate(beta_hat, index[j],y, X, lambda)
      if (vis){
        list_beta_hat = rbind(list_beta_hat, t(beta_hat))
      }
    }
  }
  if (vis){
    return(list_beta_hat)
  }
  return(beta_hat)
}
set.seed(8)
D <- CreateDesign(10,1)
x <- -10:10
y2 <- -10:10
z = norm_pol_des(x,y2,D$X,D$y)
cols <- hcl.colors(15, "Temps")
contour(x, y2, z,drawlabels = FALSE, col=cols)
beta_hat_list<-LassoCoordinateDescent(D$X,D$y, 0.5, 10, TRUE)
points(beta_hat_list[,1],beta_hat_list[,2],"l", col="blue1")
points(beta_hat_list[,1],beta_hat_list[,2], col="blue1")


set.seed(1)
n <- 100; p <- 500
design <- matrix(rnorm(p*n), ncol=p,nrow=n)
regression.vector <- c(1, 1, 1,rep(0, p-3))
outcome <- design %*% regression.vector + rnorm(n,mean = 0, sd=0.01)

beta <- LassoCoordinateDescent(design, outcome, 0.225, 10)
library(glmnet)
lasso.fit <- glmnet(design,outcome, intercept = FALSE, nlambda = 50)
estimators <- lasso.fit$beta
beta_glm <- estimators[,20]
