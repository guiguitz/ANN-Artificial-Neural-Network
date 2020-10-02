# Calculating the standard deviation
calcMeanVarianceStandDev <- function (data){
  mean<-sum(data)/length(data)
  squared_difference<-data-mean
  squared_difference<-squared_difference*squared_difference
  variance<-sum(squared_difference)/length(squared_difference)
  standard_deviation<-sqrt(variance)
  return(list(mean, variance, standard_deviation))
}

## Split the data in test and training groups
splitDataTrainTest <- function(X, Y, train_percent){
  
  # data: matrix with all data
  # train_percent: The percentage of data that will be in training group
  # Returns a list with data to be trained and data to be tested
  
  n_data <- nrow(X)
  n_data_train <- as.integer(n_data * train_percent)
  
  index <- sample(1:n_data, size = n_data_train, replace = FALSE)
  XTraining <- as.matrix(X[index,])
  YTraining <- as.matrix(Y[index,])
  XTest <- as.matrix(X[-index,])
  YTest <- as.matrix(Y[-index,])
  
  return(list(XTraining, YTraining, XTest, YTest))
}