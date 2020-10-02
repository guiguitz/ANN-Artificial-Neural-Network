# Exercise 1 :
# Create two different types of data and classify them with the Perceptron algorithm

#----------------------------------------------------------------------

#rm(list=ls()) # Removing all obects from the current workspace
library('plot3D')
#----------------------------------------------------------------------

# Creating data for learning.
# xc1: data type 'blue'
# xc2: data type 'red'

xc1<-matrix(0.3*rnorm(60)+2,ncol=2)
xc2<-matrix(0.3*rnorm(60)+4,ncol=2)

plot(xc1[,1],xc1[,2],xlim=c(0,6),ylim=c(0,6),xlab='x1',ylab='x2',col='blue')
par(new=TRUE)
plot(xc2[,1],xc2[,2],xlim=c(0,6),ylim=c(0,6),xlab='x1',ylab='x2',col='red')

#----------------------------------------------------------------------

# Perceptron algorithm.
# xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
# yd: Output (0 or 1).
# eta: Learning rate.
# tol: Error tolerance.
# maxepochs: Max interation number
# par: xin include or not the polarizarion term.

# Creating xin:
numberOfSamples<-(length(xc1)+length(xc2))/2
numberOfDimensions<-2
xin<-matrix(rbind(xc1,xc2), nrow = numberOfSamples, ncol = numberOfDimensions)

# Creating yd:
yd<-c(rep(0,30),rep(1,30))

# Parameters:
eta<-0.1
tol<-0.0000001
maxepochs<-3000
par<-1
returnlist<-trainingpsimple(xin,yd,eta,tol,maxepochs,par)

# Return Parameters:
w<-returnlist[[1]]
# w<-as.matrix(c(6,1,1))
error<-tail(returnlist[[2]],1)

# Theory ?:
# w parameters: -theta,w1, w2
# Line equation: y = ax + b (a=w1/w2, b=theta/w2)

#----------------------------------------------------------------------

# Creating a grid:
# seqx1x2 x seqx1x2 will be the grid
# M: matrix with the reponse value of all points of the grid

seqx1x2<-seq(0,6,0.2)
npgrid<-length(seqx1x2)
M<-matrix(nrow=npgrid, ncol=npgrid)

ci<-0
for(x1 in seqx1x2){
  ci<-ci+1
  cj<-0
  for(x2 in seqx1x2){
    cj<-cj+1
    xin<-as.matrix(cbind(-1,x1,x2))
    M[ci,cj]<-1.0*((xin %*% w) >= 0)
  }
}

#----------------------------------------------------------------------

ribbon3D(x = seqx1x2, y = seqx1x2, z = M, xlim=c(0,6),ylim=c(0,6),colkey=F)
scatter3D(x = xc1[,1], y = xc1[,2], z = matrix(0,nrow=dim(xc1)[1]), add=T, col='blue', colkey=F)
scatter3D(x = xc2[,1], y = xc2[,2], z = matrix(0,nrow=dim(xc1)[1]),add=T,col='red',colkey=F)
