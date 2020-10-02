rm(list=ls())

source("C:/Users/gvamorim/Google Drive/UFMG/Redes Neurais Artificiais/Algorithms/perceptron.R")

data_iris<-iris

#----------------------------------------------------------------------

# Creating data for learning.
# X1: data type 0
# X2: data type 1

X1<-as.matrix(data_iris[1:50,1:4])
X2<-as.matrix(data_iris[51:150,1:4])

#----------------------------------------------------------------------

# Separating 70% of the data for training and 30% for test
index <- sample(1:50, size = 35, replace = FALSE)
X1Training <- as.matrix(X1[index,])
X1Test <- as.matrix(X1[-index,])

index <- sample(1:100, size = 70, replace = FALSE)
X2Training <- as.matrix(X2[index,])
X2Test <- as.matrix(X2[-index,])

#----------------------------------------------------------------------

# Perceptron algorithm.
# xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
# yd: Output (0 or 1).
# eta: Learning rate.
# tol: Error tolerance.
# maxepochs: Max interation number
# par: xin include or not the polarizarion term.

# Creating xin:
numberOfSamples<-105
numberOfDimensions<-4
xin<-matrix(rbind(X1Training,X2Training), nrow = numberOfSamples, ncol = numberOfDimensions)

# Creating yd:
yd<-c(rep(0,35),rep(1,70))

# Parameters:
eta<-0.1
tol<-0.0000001
maxepochs<-3000
par<-1
returnlist<-trainperceptron(xin,yd,eta,tol,maxepochs,par)

# Return Parameters:
W<-returnlist[[1]]
error<-returnlist[[2]]

#----------------------------------------------------------------------

# Calculating results with test data:

# Xtest
numberOfSamples<-45
numberOfDimensions<-4
Xtest<-matrix(rbind(X1Test,X2Test), nrow = numberOfSamples, ncol = numberOfDimensions)

# Creating y:
y<-c(rep(0,15),rep(1,30))

# Calculating yhat
yhat<-as.double((cbind(1,Xtest) %*% W) >= 0)

# Calculating Percentual error
percentual_error <- 100*sum(abs(yhat-y))/45

# Calculating the confusion Matrix
Confusion<-table(y,yhat)

#----------------------------------------------------------------------

# Calculating 100 times the percentual error using the same Algorithm

percentual_error_vec<-matrix(nrow=1,ncol=100)
for (i in 1:100) {
  
  # Separating 70% of the data for training and 30% for test
  index <- sample(1:50, size = 35, replace = FALSE)
  X1Training <- as.matrix(X1[index,])
  X1Test <- as.matrix(X1[-index,])
  
  index <- sample(1:100, size = 70, replace = FALSE)
  X2Training <- as.matrix(X2[index,])
  X2Test <- as.matrix(X2[-index,])
  
  # Creating xin:
  numberOfSamples<-105
  numberOfDimensions<-4
  xin<-matrix(rbind(X1Training,X2Training), nrow = numberOfSamples, ncol = numberOfDimensions)
  
  # Creating yd:
  yd<-c(rep(0,35),rep(1,70))
  
  # Running Perceptron:
  eta<-0.1
  tol<-0.0000001
  maxepochs<-3000
  par<-1
  returnlist<-trainperceptron(xin,yd,eta,tol,maxepochs,par)
  W<-returnlist[[1]]
  error<-returnlist[[2]]
  
  # Xtest
  numberOfSamples<-45
  numberOfDimensions<-4
  Xtest<-matrix(rbind(X1Test,X2Test), nrow = numberOfSamples, ncol = numberOfDimensions)
  
  # Creating y:
  y<-c(rep(0,15),rep(1,30))
  
  # Calculating yhat
  yhat<-as.double((cbind(1,Xtest) %*% W) >= 0)
  
  # Calculating Percentual error
  percentual_error <- 100*sum(abs(yhat-y))/45
  
  percentual_error_vec[1,i]<-percentual_error
}

# Calculating the variance
calcVariance <- function (data){
  mean<-sum(data)/length(data)
  squared_difference<-data-mean
  squared_difference<-squared_difference*squared_difference
  variance<-sum(squared_difference)/length(squared_difference)
  return(variance)
}

variance<-calcVariance(percentual_error_vec)

#----------------------------------------------------------------------

# Printing Results

plot(1:100,percentual_error_vec, type = "b", xlim = c(0,100), xlab = "Iteration", ylab = "Error (%)" )
legend(x = "topright", 
       legend = c(paste("Variance:", round(variance, 4))),
       text.col = "black",
       bg = "gray90",
       xpd = TRUE,
       lty = c(0))


Confusion
variance
