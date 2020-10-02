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

# Perceptron algorithm.
# Xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
# y: Output (0 or 1).
# eta: Learning rate.
# tol: Error tolerance.
# maxepochs: Max interation number
# par: Xin include or not the polarizarion term.

# Creating Xin:
numberOfSamples<-nc*2
numberOfDimensions<-2
Xin<-matrix(rbind(Xc1,Xc2), nrow = numberOfSamples, ncol = numberOfDimensions)

# Creating y:
y<-c(rep(0,nc),rep(1,nc))

# Parameters:
eta<-0.001
tol<-0.00000001
maxepochs<-3000
par<-1
returnlist<-trainperceptron(Xin,y,eta,tol,maxepochs,par)

# Return Parameters:
W<-returnlist[[1]]
error<-returnlist[[2]]
# W<-as.matrix(c(6,1,1))

#----------------------------------------------------------------------

# Creating a grid:
# seqi x seqj Will be the grid
# M: matrix with the reponse value of all points of the grid

seqi<-seq(0,6,0.1)
seqj<-seq(0,6,0.1)
M<-matrix(0, nrow=length(seqi), ncol=length(seqj))

ci<-0
for(i in seqi){
  ci<-ci+1
  cj<-0
  for(j in seqj){
    cj<-cj+1
    x<-c(i,j)
    M[ci,cj]<-yperceptron(x,W,1)
  }
}

#----------------------------------------------------------------------

# Plotting

plot(Xc1[,1],Xc1[,2],xlim=c(0,6),ylim=c(0,6),xlab='x1',ylab='x2',col='blue')
par(new=TRUE)
plot(Xc2[,1],Xc2[,2],xlim=c(0,6),ylim=c(0,6),xlab='',ylab='',col='red')

theta<--W[1]
w1<-W[2]
w2<-W[3]
a<--w1/w2
b<-theta/w2
x1Line <- seq(0, 6, 0.1)
x2Line <- a*x1Line + b

par(new=TRUE)
plot(x1Line,x2Line,type='l',xlim=c(0,6),ylim=c(0,6),xlab='',ylab='',col='orange')

ribbon3D(x = seqi, y = seqj, z = M, xlim=c(0,6),ylim=c(0,6),colkey=F)
scatter3D(x = Xc1[,1], y = Xc1[,2], z = matrix(0,nrow=dim(Xc1)[1]), add=T, col='blue', colkey=F)
scatter3D(x = Xc2[,1], y = Xc2[,2], z = matrix(0,nrow=dim(Xc1)[1]),add=T,col='red',colkey=F)
