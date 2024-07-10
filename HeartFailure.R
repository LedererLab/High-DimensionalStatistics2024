data <- read.csv("heart_failure_clinical_records_dataset.csv", header =TRUE)
X_raw <-data[,-dim(data)[2]]
y_data <-data[,dim(data)[2]]
X_data <- X_raw
colN <- colnames(X_raw)
i_counter = 1
for (i in 1:dim(X_raw)[2]){
  for (j in i_counter:dim(X_raw)[2]){
    columnName = paste(colN[i], "*", colN[j])
    X_data <- cbind(X_data, X_raw[,i] * X_raw[,j])
    colnames(X_data)[ncol(X_data)] <- columnName
  }
  i_counter = i_counter + 1
}
for (i in 1:dim(X_data)[2]){
  X_data[,i] = scale(X_data[,i], scale = FALSE)
  X_data[,i] = X_data[,i]/norm(X_data[,i], type = '2')* sqrt(dim(X)[1])
}
colN2 <- colnames(X_data)
beta <- LassoCoordinateDescent(X_data, y_data, 1, 150)
beta




test_beta <- function(test_data, beta, result){
  y_hat <- as.matrix(test_data) %*% beta 
  return(norm(result-y_hat, "2"))
}

k_fold <- function(data, splits = 5, tuning_parameter = 0.5){
  nSamples = dim(data)[1]
  index = sample(1:nSamples)
  X_raw <-data[,-dim(data)[2]]
  y_raw <-data[,dim(data)[2]]
  colN <- colnames(X_raw)
  j_stop = dim(X_raw)[2]
  i_counter = 1
  for (i in 1:dim(X_raw)[2]){
    for (j in i_counter:j_stop){
      columnName = paste(colN[i], "*", colN[j])
      X_raw <- cbind(X_raw, X_raw[,i] * X_raw[,j])
      colnames(X_raw)[ncol(X_raw)] <- columnName
    }
    i_counter = i_counter + 1
  }
  for (i in 1:dim(X_raw)[2]){
    X_raw[,i] = scale(X_raw[,i], scale = FALSE)
    X_raw[,i] = X_raw[,i]/norm(X_raw[,i], type = '2')* sqrt(dim(X_raw)[1])
  }
  y_raw = scale(y_raw, scale = FALSE)
  y_raw = y_raw/norm(y_raw, type = '2')* sqrt(length(y_raw))
  beta_err = rep(0, splits)
  for (i in 1:splits){
    if (i != splits){
      X_test = X_raw[index[((i-1)*floor(nSamples/splits)+1):(i*floor(nSamples/splits))],]
      y_test = y_raw[index[((i-1)*floor(nSamples/splits)+1):(i*floor(nSamples/splits))]]
      X_data = X_raw[-(index[((i-1)*floor(nSamples/splits)+1):(i*floor(nSamples/splits))]),]
      y_data = y_raw[-(index[((i-1)*floor(nSamples/splits)+1):(i*floor(nSamples/splits))])]
    } else{
      X_test = X_raw[index[((i-1)*floor(nSamples/splits)+1):nSamples],]
      y_test = y_raw[index[((i-1)*floor(nSamples/splits)+1):nSamples]]
      X_data = X_raw[-(index[((i-1)*floor(nSamples/splits)+1):nSamples]),]
      y_data = y_raw[-(index[((i-1)*floor(nSamples/splits)+1):nSamples])]
    }
    beta <- LassoCoordinateDescent(X_data, y_data, tuning_parameter, 150)
    beta_err[i] <- test_beta(X_test, beta, y_test)
  }
  return(mean(beta_err))
}

number_tuning_parameters <- 50
tuning_parameter <- seq(0,2, by = 2/number_tuning_parameters)
avg_err = rep(0, number_tuning_parameters)
for (i in 1:number_tuning_parameters){
  avg_err[i] = k_fold(data, tuning_parameter = tuning_parameter[i])
}

number_tuning_parameters <- 50
tuning_parameter <- seq(5,15, by = 2/number_tuning_parameters)
avg_err3 = rep(0, number_tuning_parameters)
for (i in 1:number_tuning_parameters){
  avg_err3[i] = k_fold(data, tuning_parameter = tuning_parameter[i])
}

idx_small <- which.min(avg_err)
best_tuning_parameter <- tuning_parameter[idx_small]


X_raw <-data[,-dim(data)[2]]
y_data <-data[,dim(data)[2]]
colN <- colnames(X_raw)
j_stop = dim(X_raw)[2]
i_counter = 1
for (i in 1:dim(X_raw)[2]){
  for (j in i_counter:j_stop){
    columnName = paste(colN[i], "*", colN[j])
    X_raw <- cbind(X_raw, X_raw[,i] * X_raw[,j])
    colnames(X_raw)[ncol(X_raw)] <- columnName
  }
  i_counter = i_counter + 1
}
for (i in 1:dim(X_raw)[2]){
  X_raw[,i] = scale(X_raw[,i], scale = FALSE)
  X_raw[,i] = X_raw[,i]/norm(X_raw[,i], type = '2')* sqrt(dim(X_raw)[1])
}
y_raw = scale(y_raw, scale = FALSE)
y_raw = y_raw/norm(y_raw, type = '2')* sqrt(length(y_raw))
beta <- LassoCoordinateDescent(X_raw, y_raw, lambda = best_tuning_parameter, max_iter = 150)
## Datensatz aufteilen -> testen bei wie vielen Patienten Tod vorrausgesagt werden kann...
