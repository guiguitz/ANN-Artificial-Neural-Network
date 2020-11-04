sech2 <- function(u)
{
  return(((2/(exp(u)+exp(-u)))*(2/(exp(u)+exp(-u)))))
}

trainMLP <- function(X,Y,p,eta,tol,maxepocas,func_ati){
  
  xin<-as.matrix(X)
  yd<-as.matrix(Y)
  
  N<-dim(xin)[1] # num de dados)
  n<-dim(xin)[2] # num dimensões xin
  m<-dim(yd)[2] # num de dimensões yd
  
  Z<-matrix(runif((n+1)*p)-0.5, nrow = n+1, ncol = p) # dim (n+1 x p)
  W<-matrix(runif((p+1)*m)-0.5, nrow = p+1, ncol = m) # dim (p+1 x m)
  
  xin<-cbind(xin,1) # dim (N, n+1)
  
  nepocas<-0
  eepoca<-tol+1
  #inicializa vetor erro evec, 
  evec<-matrix(nrow=1,ncol=maxepocas)
  while ((nepocas < maxepocas) && (eepoca>tol))#eepocas erro da epoca e tol tolerancia
  {
    ei2<-0
    #sequencia aleatoria para treinamento
    xseq<-sample(N)
    for (i in 1:N)
    {
      #padrao para sequencia aleatoria
      irand<-xseq[i]
      
      U<-xin[irand,]%*%Z # xin (1, n+1) %*% Z (n+1 x p) = U (1 x p)
      H<-tanh(U) # dim (1 x p)
      Haug<-cbind(H,1) # dim (1 x p+1)
      
      O<-Haug%*%W # Haug (1, p+1) %*% W (p+1 x m) = O (1 x m)
      
      if (func_ati == 0){
        yhat<-O # dim (1 x m) - Função de ativação linear
      } else {
        yhat<-tanh(O) # dim (1 x m) - Função de ativação tanh
      }
      
      ## Backpropagation
      
      ei<-yd[irand,]-yhat # ei (1 x m)
      if (func_ati == 0){
        flinhaO<-1 # Função de ativação linear
      } else {
        flinhaO<-sech2(O) # flinhaO (1 x m) - Função de ativação tanh
      }
      dO<-ei*flinhaO # dO (1 x m) - produto ei x (elemento a elemento) 
      
      Wminus<-W[-(p+1),] # Wminus (p x m) - Removendo o term de polarização (Não se propaga)
      ehidden<-dO %*% t(Wminus) # dO (1 x m) %*% t(Wminus) (m x p) = ehidden (1 x p)
      flinhaU<-sech2(U) # flinhaU (1 x p)
      dU<-ehidden*flinhaU # dU (1 x p) - produto ehidden x (elemento a elemento) 
      
      W<-W+eta*(t(Haug) %*% dO) # (p+1 x m) + eta * [(p+1 x 1) %*% (1 x m)] = (p+1 x m)
      xint<-t(xin)
      Z<-Z+eta*(xint[,irand] %*% dU) # (n+1 x p) + eta * [(n+1 x 1) %*% (1 x p))] = (n+1 x p)
      
      ei2<-ei2+(ei*t(ei))
    }
    #numero de epocas
    nepocas<-nepocas+1
    evec[nepocas]<-ei2/N
    #erro por epoca
    eepoca<-evec[nepocas]
  }
  retlist<-list(W,Z,evec[1:nepocas])
  return(retlist)
}

yMLP <- function(X, MLP, func_ati)
{
  Xin<-as.matrix(X)
  Xin<-cbind(Xin, 1)
  W<-MLP[[1]]
  Z<-MLP[[2]]
  
  H <- tanh(Xin %*% Z)
  Haug<-cbind(H,1)
  O <- Haug %*% W
  if (func_ati == 0){
    YTestHat<-O # dim (1 x m) - Função de ativação linear
  } else {
    YTestHat<-tanh(O) # dim (1 x m) - Função de ativação tanh
  }
  return (YTestHat)
}

errorMLP <- function(Y, YHat)
{
  error <- (sum(abs(Y-YHat)))/(2*length(Y))
  return(error)
}