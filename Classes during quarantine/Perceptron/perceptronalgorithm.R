### Simple Perceptron Algorithm

trainingpsimple <- function(xin,yd,eta,tol,maxepochs,par)
{
  # Perceptron algorithm
  # xin: Matrix Nxn with input data (N = Number of samples, n = Input dimension)
  # yd: Output (0 or 1)
  # eta: Learning rate
  # tol: Error tolerance
  # maxepochs: Max iteration number
  # par: xin include or not the polarization term (0 or 1)
  
  dimxin<-dim(xin)
  N<-dimxin[1] # Number of samples
  n<-dimxin[2] # Input dimension.
  
  # Adding or not the polarization term
  if (par==1){
    wt<-as.matrix(runif(n+1)-0.5)
    xin<-cbind(-1,xin)
  }
  else wt<-as.matrix(runif(n)-0.5)
  
  # Initialize nepochs, eepoch and error vector
  nepochs<-0
  eepoch<-tol+1
  evec<-matrix(nrow=1,ncol=maxepochs)
  
  # Algorithm
  while ((nepochs < maxepochs) && (eepoch>tol))
  {
    ei2<-0
    # Random sequence for training.
    xseq<-sample(N)
    
    for (i in 1:N)
    {
      # Pattern from random sequence.
      irand<-xseq[i]
      
      # Activation Function
      yhat<-as.double((xin[irand,] %*% wt) >= 0)
      
      # Calculating the error
      ei<-yd[irand]-yhat
      
      # Updating weight with gradient: wt' = wt + eta*gradient*xin
      dw<-eta*ei*xin[irand,]
      wt<-wt+dw
      
      # Accumulate square error
      ei2<-ei2+ei*ei
    }
    # Number of epochs update
    nepochs<-nepochs+1
    
    # Updating the error vec with the avarage error of this last epoch
    evec[nepochs]<-ei2/N
    
    # Getting the error of the last epoch for the next interation
    eepoch<-evec[nepochs]
  }
  
  # Return the coeficients vector and the error vector
  retlist<-list(wt,evec[1:nepochs])
  return(retlist)
}

#----------------------------------------------------------------------

### Perceptron Algorithm with sigmoid as activation function

trainingpsigmoid <- function(xin,yd,eta,tol,maxepochs,par)
{
  # Perceptron algorithm.
  # xin: Matrix Nxn with input data (N = Number of samples, n = Input dimension)
  # yd: Output (0 or 1).
  # eta: Learning rate.
  # tol: Error tolerance.
  # maxepochs: Max iteration number
  # par: xin include or not the polarization term (0 or 1)
  
  dimxin<-dim(xin)
  N<-dimxin[1] # Number of samples
  n<-dimxin[2] # Input dimension.
  
  # Adding or not the polarization term
  if (par==1){
    wt<-as.matrix(runif(n+1)-0.5)
    xin<-cbind(-1,xin)
  }
  else wt<-as.matrix(runif(n)-0.5)
  
  # Initialize nepochs, eepoch and error vector
  nepochs<-0
  eepoch<-tol+1
  evec<-matrix(nrow=1,ncol=maxepochs)
  
  # Algorithm
  while ((nepochs < maxepochs) && (eepoch>tol))
  {
    ei2<-0
    # Random sequence for training.
    xseq<-sample(N)
    
    for (i in 1:N)
    {
      # Pattern from random sequence:
      irand<-xseq[i]
      
      # Sigmoid form:
      u<-xin[irand,] %*% wt
      yhat<-(1/(1+exp(-u)))
      
      # Calculating the error:
      ei<-(yd[irand]-yhat)
      
      # Updating weight with gradient: wt' = wt + eta*gradient*xin
      dw<- eta * ei * xin[irand,] * (exp(u)/((1+exp(u))^2))
      wt<-wt+dw
      
      # Accumulate square error
      ei2<-ei2+ei*ei
    }
    # Number of epochs update
    nepochs<-nepochs+1
    
    # Updating the error vec with the avarage error of this last epoch
    evec[nepochs]<-ei2/N
    
    # Getting the error of the last epoch for the next interation
    eepoch<-evec[nepochs]
  }
  
  # Return the coeficients vector and the error vector
  retlist<-list(wt,evec[1:nepochs])
  return(retlist)
}
