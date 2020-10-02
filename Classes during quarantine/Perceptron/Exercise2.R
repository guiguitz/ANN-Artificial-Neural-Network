# Exercise 2:
# Uses the Iris data to separate two classes with Perceptron algorithm

#----------------------------------------------------------------------

#rm(list=ls()) # Removing all obects from the current workspace
data_iris<-iris

#----------------------------------------------------------------------

# Creating data for learning.
# xc1: data type 0
# xc2: data type 1

xc1<-as.matrix(data_iris[1:50,1:4])
xc2<-as.matrix(data_iris[51:150,1:4])

#----------------------------------------------------------------------

# Perceptron algorithm.
# xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
# yd: Output (0 or 1).
# eta: Learning rate.
# tol: Error tolerance.
# maxepochs: Max interation number
# par: xin include or not the polarizarion term.

# Creating xin:
numberOfSamples<-150
numberOfDimensions<-4
xin<-matrix(rbind(xc1,xc2), nrow = numberOfSamples, ncol = numberOfDimensions)

# Creating yd:
yd<-c(rep(0,50),rep(1,100))

# Parameters:
eta<-0.1
tol<-0.0000001
maxepochs<-3000
par<-1
returnlist<-trainingpsimple(xin,yd,eta,tol,maxepochs,par)

# Return Parameters:
w<-returnlist[[1]]
error<-tail(returnlist[[2]],1)

#----------------------------------------------------------------------

# Checking output values:
test_output<-1.0*((cbind(-1,xin) %*% w) >= 0)

#----------------------------------------------------------------------

