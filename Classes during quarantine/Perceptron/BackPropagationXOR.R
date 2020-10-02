rm(list=ls())

#----------------------------------------------------------------------
### Functions

## f(u) = sech2(u)
sech2 <- function(u)
{
  return (((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u)))))
}

## BackPropagation Algorithm (XOR)
trainingbpxor <- function(X,Y,eta,tol,maxepochs)
{
  # Perceptron algorithm
  # X: Matrix Nxn with input data (N = Number of samples, n = Input dimension)
  # Y: Output (-1 or 1)
  # eta: Learning rate
  # tol: Error tolerance
  # maxepochs: Max iteration number
  
  dimx<-dim(X)
  N<-dimx[1] # Number of samples;
  n<-dimx[2] # Input dimension;
  
  # Initialize nepochs, eepoch and error vector
  nepochs<-0
  eepoch<-tol+1
  evec<-matrix(nrow=1,ncol=maxepochs)
  
  ## Weight init
  w95<-runif(1)-0.5
  w96<-runif(1)-0.5
  w97<-runif(1)-0.5
  
  w106<-runif(1)-0.5
  w107<-runif(1)-0.5
  w108<-runif(1)-0.5
  
  w61<-runif(1)-0.5
  w62<-runif(1)-0.5
  w63<-runif(1)-0.5
  
  w72<-runif(1)-0.5
  w73<-runif(1)-0.5
  w74<-runif(1)-0.5
  
  ## Bias input init
  i1<-1
  i4<-1
  i5<-1
  i8<-1
  
  ## Yhat
  Yhat<-matrix(rep(0, 8), ncol = 2, byrow = T)
  
  ## Algorithm
  while ((nepochs < maxepochs) && (eepoch>tol))
  {
    ei2<-0
    # Random sequence for training.
    xseq<-sample(N)
    
    for (i in 1:N)
    {
      # Pattern from random sequence:
      irand<-xseq[i]
      
      # Input data used on learning
      i2<-X[irand,1]
      i3<-X[irand,2]
      
      # Output data used on learning
      y9<-Y[irand,1]
      y10<-Y[irand,2]
      
      u6<-i1*w61+i2*w62+i3*w63
      u7<-i2*w72+i3*w73+i4*w74
      
      # Calculating output of hidden layer
      i6<-tanh(u6)
      i7<-tanh(u7)
      
      u9<-i5*w95+i6*w96+i7*w97
      u10<-i6*w106+i7*w107+i8*w108
      
      # Calculating output of output layer
      i9<-tanh(u9)
      i10<-tanh(u10)
      
      # Calculating error on output layer
      e9<-y9-i9
      e10<-y10-i10
      
      # Calculating the deriv of output and hidden layers
      d9<-e9*sech2(u9)
      d10<-e10*sech2(e10)
      
      d6<-sech2(u6)*(d9*w96+d10*w106)
      d7<-sech2(u7)*(d9*w97+d10*w107)
      
      # Calculating delta for weight update
      dw95<-eta*d9*i5
      dw96<-eta*d9*i6
      dw97<-eta*d9*i7
      
      dw106<-eta*d10*i6
      dw107<-eta*d10*i7
      dw108<-eta*d10*i8
      
      dw61<-eta*d6*i1
      dw62<-eta*d6*i2
      dw63<-eta*d6*i3
      
      dw72<-eta*d7*i2
      dw73<-eta*d7*i3
      dw74<-eta*d7*i4
      
      # weight update
      w95<-w95+dw95
      w96<-w96+dw96
      w97<-w97+dw97
      
      w106<-w106+dw106
      w107<-w107+dw107
      w108<-w108+dw108
      
      w61<-w61+dw61
      w62<-w62+dw62
      w63<-w63+dw63
      
      w72<-w72+dw72
      w73<-w73+dw73
      w74<-w74+dw74
      
      # Accumulated square error
      ei2<-ei2+(e9*e9+e10*e10)
      
      # Updating Yhat
      Yhat[irand,1]<-i9
      Yhat[irand,2]<-i10
    }
    
    # Number of epochs update
    nepochs<-nepochs+1
    
    # Updating the error vec with the avarage error of this last epoch
    evec[nepochs]<-ei2/N
    
    # Getting the error of the last epoch for the next interation
    eepoch<-evec[nepochs]
    
  }
  
  list(nepochs, evec, Yhat)
}

#----------------------------------------------------------------------

# Input and Output init
X<-matrix(c(0,0,0,1,1,0,1,1), ncol = 2, byrow = T)
Y<-matrix(c(-1,+1,+1,-1,+1,-1,-1,+1), ncol = 2, byrow = T)
eta<-0.001
tol<-0.01
maxepochs<-30000
list<-trainingbpxor(X,Y,eta,tol,maxepochs)
nepochs<-list[[1]]
evec<-list[[2]]
Yhat<-list[[3]]

#----------------------------------------------------------------------

# Ploting:
Y
sign(Yhat)
evec[nepochs]
plot(seq(1:nepochs),evec,type='l')
