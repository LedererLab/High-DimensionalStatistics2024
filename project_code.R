# Code zusammengefasst:

# Soft-Thresholding Operator
ST <- function(lambda, z)
{
  if (z > lambda) {
    return(z - lambda)
  }
  else if (z < -lambda) {
    return(z + lambda)
  }
  else {
    return(0)
  }
}

# Coordinate Descent for LASSO
CDLasso <- function(X, y, beta1, lambda, rep)
{
  beta = beta1
  for (j in 1:rep) {
    for (i in 1:ncol(X)) {
      norm2 = norm(X[,i], type = "2")^2
      
      a =  lambda / norm2
      
      b = ( t(X[,i]) %*% (y - X[,-i] %*% beta[-i]) ) / norm2
      
      beta[i] = ST(a,b)
    }
  }
  return(beta)
}

# Lade die Daten
file_path <- #file paht zu day.csv

data <- read.csv(file_path); head(data)

columns_of_interest <- c("workingday", "weathersit", "temp", "atemp", "hum", "windspeed")

# Winter
y_train_w <- as.matrix(data[1:79,"cnt"]) 
y_test_w <- as.matrix(data[355:445,"cnt"])[1:10]

X_train_w <- as.matrix(data[1:79, columns_of_interest])
X_test_w <- as.matrix(data[355:445, columns_of_interest])[1:10,]

X_test_w <- cbind(1, X_test_w, X_test_w^2, X_test_w[,"temp"]*X_test_w[,"atemp"])
X_train_w <- cbind(1, X_train_w, X_train_w^2, X_train_w[,"temp"]*X_train_w[,"atemp"])

# Frühling
y_train_f <- as.matrix(data[80:171,"cnt"]) 
y_test_f <- as.matrix(data[446:537,"cnt"])[1:10]

X_train_f <- as.matrix(data[80:171, columns_of_interest])
X_test_f <- as.matrix(data[446:537, columns_of_interest])[1:10,]

X_test_f <- cbind(1, X_test_f, X_test_f^2, X_test_f[,"temp"]*X_test_f[,"atemp"])
X_train_f <- cbind(1, X_train_f, X_train_f^2, X_train_f[,"temp"]*X_train_f[,"atemp"])

# Sommer
y_train_s <- as.matrix(data[172:265,"cnt"]) 
y_test_s <- as.matrix(data[538:631,"cnt"])[1:10]

X_train_s <- as.matrix(data[172:265, columns_of_interest])
X_test_s <- as.matrix(data[538:631, columns_of_interest])[1:10,]

X_test_s <- cbind(1, X_test_s, X_test_s^2, X_test_s[,"temp"]*X_test_s[,"atemp"])
X_train_s <- cbind(1, X_train_s, X_train_s^2, X_train_s[,"temp"]*X_train_s[,"atemp"])

# Herbst
y_train_h <- as.matrix(data[266:354,"cnt"]) 
y_test_h <- as.matrix(data[632:720,"cnt"])[1:10]

X_train_h <- as.matrix(data[266:354, columns_of_interest])
X_test_h <- as.matrix(data[632:720, columns_of_interest])[1:10,]

X_test_h <- cbind(1, X_test_h, X_test_h^2, X_test_h[,"temp"]*X_test_h[,"atemp"])
X_train_h <- cbind(1, X_train_h, X_train_h^2, X_train_h[,"temp"]*X_train_h[,"atemp"])

# Data analysis
p = dim(X_train_w)[2]

beta_hat_w = CDLasso(X_train_w, y_train_w, beta1=rep(1, p), lambda=1, rep=200)
beta_hat_f = CDLasso(X_train_f, y_train_f, beta1=rep(1, p), lambda=1, rep=200)
beta_hat_s = CDLasso(X_train_s, y_train_s, beta1=rep(1, p), lambda=1, rep=200)
beta_hat_h = CDLasso(X_train_h, y_train_h, beta1=rep(1, p), lambda=1, rep=200)

w_hat = cbind(y_test_w, floor(X_test_w %*% beta_hat_w))
f_hat = cbind(y_test_f, floor(X_test_f %*% beta_hat_f))
s_hat = cbind(y_test_s, floor(X_test_s %*% beta_hat_s))
h_hat = cbind(y_test_h, floor(X_test_h %*% beta_hat_h))

l = length(y_test_w)

data.frame(Winter = y_test_w, W_hat = floor(X_test_w %*% beta_hat_w), X = rep("|",l),
           Frühling = y_test_f, F_hat = floor(X_test_f %*% beta_hat_f), . = rep("|",l),
           Sommer = y_test_s, S_hat = floor(X_test_s %*% beta_hat_s),  x = rep("|",l),
           Herbst = y_test_h, H_hat = floor(X_test_h %*% beta_hat_h)
)


