rm(list = ls())
library('corpcor')
library(VarSelLCM)

source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/perceptron.R")
source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/ELM.R")
source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/usefulroutines.R")

#----------------------------------------------------------------------

### Data

data(heart)

# Removing NA data
heart<-heart[complete.cases(heart),]
dt<-data.matrix(heart, rownames.force = NA)

X<-as.matrix(dt[1:dim(dt)[1], 1:12])
Y<-as.matrix(dt[1:dim(dt)[1], 13])

for(i in 1:length(Y)){
  if(Y[i] == 2){
    Y[i] <- -1
  }
}

#----------------------------------------------------------------------

### Run

## ELM
n_max_neurons<-100
n_interaction_per_neurons<-20

error_vec_per_neurons_train<-matrix(nrow=2,ncol=n_max_neurons)
error_vec_per_neurons_test<-matrix(nrow=2,ncol=n_max_neurons)
for(p in 1:n_max_neurons){
  error_vec_train<-matrix(nrow=1,ncol=n_interaction_per_neurons)
  error_vec_test<-matrix(nrow=1,ncol=n_interaction_per_neurons)
  for(i in 1:n_interaction_per_neurons){
    
    # 30% data for testing and 70% for training
    return<-splitDataTrainTest(X, Y, 0.7)
    XTraining<-return[[1]]
    YTraining<-return[[2]]
    XTest<-return[[3]]
    YTest<-return[[4]]
    
    # Calc ELM parameters
    return<-extremeLearningMachineParameters(XTraining, YTraining, p)
    Z<-return[[1]]
    W<-return[[2]]
    H<-return[[3]]
    
    # Calc Yhat of Training data
    return<-extremeLearningMachineOutput(H,W)
    YHatTrain<-return
    
    # Calc Error of Training data
    acumulated_error<-sum((YTraining-YHatTrain)^2)/4
    error_train<-acumulated_error/nrow(YHatTrain)
    
    # Calc Yhat of Test data
    H<-tanh(cbind(1, XTest) %*% Z)
    W<-pseudoinverse(H) %*% YTest
    return<-extremeLearningMachineOutput(H,W)
    YHatTest<-return
    
    # Calc Error of Test data
    acumulated_error<-sum((YTest-YHatTest)^2)/4
    error_test<-acumulated_error/nrow(YHatTest)
    
    error_vec_train[1,i]<-error_train
    error_vec_test[1,i]<-error_test
  }
  return<-calcMeanVarianceStandDev(error_vec_train)
  error_vec_per_neurons_train[1,p]<-return[[1]]
  error_vec_per_neurons_train[2,p]<-return[[3]]
  return<-calcMeanVarianceStandDev(error_vec_test)
  error_vec_per_neurons_test[1,p]<-return[[1]]
  error_vec_per_neurons_test[2,p]<-return[[3]]
}

## Perceptron

for(i in 1:length(Y)){
  if(Y[i] == -1){
    Y[i] <- 0
  }
}

# 30% data for testing and 70% for training
return<-splitDataTrainTest(X, Y, 0.7)
XTraining<-return[[1]]
YTraining<-return[[2]]
XTest<-return[[3]]
YTest<-return[[4]]

# Calc Perceptron
return<-trainperceptron(XTraining,YTraining,0.01,0.0001,10000,1)
Wp<-return[[1]]
error_per_epoch_p<-return[[2]]

# Calc Yhat of Training data
return<-yperceptron(XTraining,Wp,1)
YHatTrain<-return

# Calc Error of Training data
acumulated_error<-sum(abs(YTraining-YHatTrain))
error_train<-acumulated_error/nrow(YHatTrain)

# Calc Yhat of Test data
return<-yperceptron(XTest,Wp,1)
YHatTest<-return

# Calc Error of Test data
acumulated_error<-sum(abs(YTest-YHatTest))
error_test<-acumulated_error/nrow(YHatTest)


#----------------------------------------------------------------------

### Plotting Results

## Plotting [error x p(1:100)] of Test and Training data ELM
max_error<-max(error_vec_per_neurons_train[1,], error_vec_per_neurons_test[1,])
min_error<-min(error_vec_per_neurons_test[1,])
p_min_error<-which.min(error_vec_per_neurons_test[1,])

plot(1:n_max_neurons, error_vec_per_neurons_train[1,], main = "error x p(1:100) ELM", ylim=c(0,max_error), xlab='p', ylab = 'error', type = 'l', col='blue')
par(new=TRUE)
plot(1:n_max_neurons, error_vec_per_neurons_test[1,], ylim=c(0,max_error), xlab='', ylab = '', type = 'l', col='red')
legend(x = "bottomleft", 
       legend = c(paste("p Min Error:", p_min_error), paste("Min Error:", round(min_error, 6)), paste("Standard Deviation:", round(error_vec_per_neurons_test[2,p_min_error], 6)), "Test Error", "Training Error"),
       text.col = "black",
       bg = "gray90",
       xpd = TRUE,
       pch = c(NA,NA,NA,1,1),
       col = c(NA,NA,NA,'red','blue'))

## Plotting [error x epoch] of Percepton Algorithm
plot(1:10000,error_per_epoch_p, type = 'l', main = "error x epoch - Perceptron")

## Comparing Perceptron Training and Test errors
barplot(c(error_test,error_train),
        main = "Error of Test data and Error of Training Data - Perceptron", 
        xlab = "Data",
        ylab = "Error",
        col = c("darkred","blue"),
        horiz = FALSE,
        names.arg = c("Test", "Training")
)
legend("bottomright",
       c(paste("Error of Test data:",round(error_test,6)),paste("Error of Training Data:", round(error_train,6))),
       fill = c("darkred","blue",NA,NA,NA,NA))

## Comparing ELM and Perceptron results
barplot(c(min_error,error_test),
        main = "Error of ELM and Error of Perceptron", 
        xlab = "Algorithm",
        ylab = "Error",
        col = c("yellow","darkgreen"),
        horiz = FALSE,
        names.arg = c("ELM", "Perceptron")
)
legend("bottomright",
       c(paste("Error of ELM Algorithm:",round(min_error,6)),paste("Error of Perceptron Algorithm:", round(error_test,6))),
       fill = c("yellow","darkgreen",NA,NA,NA,NA))
