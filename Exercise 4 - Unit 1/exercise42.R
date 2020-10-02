rm(list=ls()) # Removing all obects from the current Workspace

source("C:/Users/gvamorim/Google Drive/UFMG/Redes Neurais Artificiais/Algorithms/trainperceptron.R")
source("C:/Users/gvamorim/Google Drive/UFMG/Redes Neurais Artificiais/Algorithms/yperceptron.R")

library('plot3D')

#----------------------------------------------------------------------

# Creating data for learning.
# Xc1: data type 'blue'
# Xc2: data type 'red'
# nc: Number of points of each class
# s1, s2: HoW these points are dispersed

s1<-0.4
s2<-0.4
nc<-200
Xc1<-matrix(s1*rnorm(nc*2)+2,ncol=2)
Xc2<-matrix(s2*rnorm(nc*2)+4,ncol=2)

#----------------------------------------------------------------------

# Separating 70% of the data for training and 30% for test
index <- sample(1:200, size = 140, replace = FALSE)
X1Training <- as.matrix(Xc1[index,])
X1Test <- as.matrix(Xc1[-index,])

index <- sample(1:200, size = 140, replace = FALSE)
X2Training <- as.matrix(Xc2[index,])
X2Test <- as.matrix(Xc2[-index,])

#----------------------------------------------------------------------

# Perceptron algorithm.
# Xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
# y: Output (0 or 1).
# eta: Learning rate.
# tol: Error tolerance.
# maxepochs: Max interation number
# par: Xin include or not the polarizarion term.

# Creating Xin:
numberOfSamples<-280
numberOfDimensions<-2
Xin<-matrix(rbind(X1Training,X2Training), nrow = numberOfSamples, ncol = numberOfDimensions)

# Creating y:
y<-c(rep(0,140),rep(1,140))

# Parameters:
eta<-0.001
tol<-0.00000001
maxepochs<-3000
par<-1
returnlist<-trainperceptron(Xin,y,eta,tol,maxepochs,par)

# Return Parameters:
W<-returnlist[[1]]
error<-returnlist[[2]]

#----------------------------------------------------------------------

# Calculating acuracy

# Xtest
numberOfSamples<-120
numberOfDimensions<-2
Xtest<-matrix(rbind(X1Test,X2Test), nrow = numberOfSamples, ncol = numberOfDimensions)

# Creating y:
y<-c(rep(0,60),rep(1,60))

# Calculating yhat
yhat<-as.double((cbind(1,Xtest) %*% W) >= 0)

# Calculating the confusion Matrix
Confusion<-table(y,yhat)

# Calculating Accuracy
accuracy<-(Confusion[1]+Confusion[4])/sum(Confusion)

# Calculating Percentual Error
percentual_error<-(1-accuracy)*100

#----------------------------------------------------------------------

# Plotting Results

plot(error, type = "l", xlab = "Epoch", ylab = "Square Error" )
legend(x = "topright", 
       legend = c(paste("Accuracy:", accuracy), paste("Number of Interations:", length(error)), paste("Square Error:", error[length(error)]), paste("Percentual Error:", percentual_error)),
       text.col = "black",
       bg = "gray90",
       xpd = TRUE,
       lty = c(0,0,0))

Confusion
