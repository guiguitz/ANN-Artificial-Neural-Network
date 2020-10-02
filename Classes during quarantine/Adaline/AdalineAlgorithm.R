## ADALINE Alhorithm (Adaptive Linear Neuron or later Adaptive Linear Element)
trainingADALINE <- function(xin,yd,eta,tol,maxepochs,par,lamda)
{
  # Adaline algorithm
  # xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
  # yd: Output (0 or 1)
  # eta: Learning rate
  # tol: Error tolerance
  # maxepochs: Max interation number
  # par: xin include or not the polarizarion term (0 or 1)
  # lambda: Controls the importance of the regularization term
  
  dimxin<-dim(xin)
  N<-dimxin[1] # Number of samples
  n<-dimxin[2] # Input dimenssion.
  
  # Adding or not the polarization term
  if (par==1){
    wt<-as.matrix(runif(n+1, -0.5, 0.5))
    xin<-cbind(-1,xin)
  }
  else wt<-as.matrix(runif(n+1, -0.5, 0.5))
  
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
      # Pattern from random sequence
      irand<-xseq[i]
      
      # Calculating yhat
      yhat<-(xin[irand,] %*% wt)
      
      # Calculating the error
      ei<-(yd[irand]-yhat)
      
      # Updating weight: wt' = lambda*wt + eta*ei*xin
      wt<-lamda*wt+(eta*ei*xin[irand,])
      
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