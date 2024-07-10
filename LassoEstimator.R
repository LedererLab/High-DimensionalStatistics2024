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

grad_ls <- function(X,y, beta){ # calculate gradienr
  return(-2*t(X)%*%(y-X%*%beta))
}


set.seed(1)
update_beta_n <- function(beta_hat, lr, adapting_lr=FALSE){
  list_beta_hat = beta_hat
  #print(norm(y-X%*%beta_hat))
  for (i in 1:nupdates){
    update <- grad_ls(X,y,beta_hat)*lr
    if (adapting_lr){
      lr = lr*0.8
    }
    beta_hat <- beta_hat - update
    list_beta_hat = rbind(list_beta_hat, t(beta_hat))
    #print(norm(y-X%*%beta_hat))
  }
  return(list_beta_hat)
}

n<- 10; d <- 1; nupdates <- 30
beta_hat = runif(d+1, min = -10, max = 10)
X <- PolynomialDesign(runif(n, min = -1, max = 1), d)
regression.vector <- c(2, 2)
y <- X %*% regression.vector #+ rnorm(n)

adapting_lr <- FALSE
lr=0.01
lr_good <- update_beta_n(beta_hat,lr)
lr=0.001
beta_hat <- runif(d+1, min = -10, max = 10)
lr_small <- update_beta_n(beta_hat,lr)
lr=0.1
beta_hat <- runif(d+1, min = -10, max = 10)
lr_big <- update_beta_n(beta_hat,lr)
lr=0.1
beta_hat <- runif(d+1, min = -10, max = 10)
lr_adap <- update_beta_n(beta_hat,lr,TRUE)
x <- -10:10
y2 <- -10:10
z = norm_pol_des(x,y2,X,y)
cols <- hcl.colors(15, "Temps")
contour(x, y2, z,drawlabels = FALSE, col=cols)

points(lr_good[,1],lr_good[,2],"l", col="blue1")
points(lr_good[,1],lr_good[,2], col="blue1")
points(lr_small[,1],lr_small[,2],"l", col="red")
points(lr_small[,1],lr_small[,2], col="red")
points(lr_big[,1],lr_big[,2],"l", col = "green")
points(lr_big[,1],lr_big[,2], col="green")
points(lr_adap[,1],lr_adap[,2],"l", col ="violet")
points(lr_adap[,1],lr_adap[,2], col="violet")

