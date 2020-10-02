# Exercise 3:
# Uses the Iris data to separate two classes with Perceptron algorithm
# removing 20 sample in each class 

#----------------------------------------------------------------------

#rm(list=ls()) # Removing all obects from the current workspace
data_iris<-iris

#----------------------------------------------------------------------

# Creating data for learning.
# xc1: data type 0
# xc2: data type 1

removed_index_xc1<-sample(1:50,10,replace=FALSE)
xc1<-as.matrix(data_iris[1:50,1:4])
xc1<-xc1[-removed_index_xc1,]

removed_index_xc2<-sample(51:150,10,replace=FALSE)
xc2<-as.matrix(data_iris[51:150,1:4])
xc2<-xc2[-(removed_index_xc2-50),]

#----------------------------------------------------------------------

# Perceptron algorithm.
# xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
# yd: Output (0 or 1).
# eta: Learning rate.
# tol: Error tolerance.
# maxepochs: Max interation number
# par: xin include or not the polarizarion term.

# Creating xin:
numberOfSamples<-130
numberOfDimensions<-4
xin<-matrix(rbind(xc1,xc2), nrow = numberOfSamples, ncol = numberOfDimensions)

# Creating yd:
yd<-c(rep(0,40),rep(1,90))

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

# Calculating output of removed values:

removed_matrix_xc1<-as.matrix(data_iris[1:50,1:4])
removed_matrix_xc1<-removed_matrix_xc1[+removed_index_xc1,]
output_removed_xc1<-1.0*((cbind(-1,removed_matrix_xc1) %*% w) >= 0)

removed_matrix_xc2<-as.matrix(data_iris[51:150,1:4])
removed_matrix_xc2<-removed_matrix_xc2[+(removed_index_xc2-50),]
output_removed_xc2<-1.0*((cbind(-1,removed_matrix_xc2) %*% w) >= 0)

#----------------------------------------------------------------------

# Checking output values:

cat("The removed index are:", removed_index_xc1, removed_index_xc2)
n_miss_classifications<-0
n_miss_classifications<-length(which(output_removed_xc1==1))+length(which(output_removed_xc2==0))
print(paste("The number of miss classifiction is:",n_miss_classifications))
