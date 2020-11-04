library('corpcor') # pseudoinverse

treinaRBF <- function (xin,yin,p)
{

  ####### Função Radial Gaussiana
  pdfnvar<-function (x,m,K,n)
  {
    if (n == 1)
    {
      r<-sqrt(as.numeric(K))
      px<-(1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/(r))^2)
    }
    else px<-((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))
    return(px)
  }
  #######

  N<-dim(xin)[1] # número de amostras
  n<-dim(xin)[2] # dimensão de entrada (deve ser maior que 1)

  xin<-as.matrix(xin) # garante que xin seja matrix
  yin<-as.matrix(yin) #garante que yin seja matriz
  
  # if(par == 1){
  #   xin<-cbind(1,xin) # Adicionando o termo de polarização na entrada 
  # }

  xclust<-kmeans(xin,p)

  # Armazena vetores de centros das funções
  m<-as.matrix(xclust$centers)
  covlist<-list()

  # Estima matrizes de covariância para todos os centros
  for (i in 1:p)
  {
    ici<-which(xclust$cluster==i)
    xci<-xin[ici,]
    # xci<-as.matrix(xin[ici,])
    print(typeof(xci))
    if (n == 1){
      covi<-var(xci) 
    }
    else{
      covi<-cov(xci) 
    }
    covlist[[i]]<-covi
  }

  H<-matrix(nrow=N,ncol=p)
  # calcula mariz H
  for (j in 1:N)
  {
    for (i in 1:p)
    {
      mi<-m[i,]
      covi<-covlist[i]
      covi<-matrix(unlist(covlist[i]),ncol=n,byrow = T) + 0.001*diag(n)
      H[j,i]<-pdfnvar(xin[j,], mi, covi+1e-3*diag(nrow(covi)), n)
    }
  }

  Haug<-cbind(1,H)
  W<-pseudoinverse(Haug) %*% yin
  # W<-(solve(t(Haug) %*% Haug) %*% t(Haug)) %*% yin

  return(list(m,covlist,W,H))
}

YRBF<-function (xin, modRBF, func_ativ)
{

  ####### Funcão radial Gaussiana
  pdfnvar<-function (x,m,K,n)
  {
    if (n == 1)
    {
      r<-sqrt(as.numeric(K))
      px<-(1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(r))^2)
    }
    else px<-((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))
    return(px)
  }
  ##########################

  N<-dim(xin)[1] # núumero de amostras
  n<-dim(xin)[2] # dimensão de entrada (deve ser maior que 1)
  m<-as.matrix(modRBF[[1]])
  covlist<-modRBF[[2]]
  p<-length(covlist) # Número de funções radiais
  W<-modRBF[[3]]

  xin<-as.matrix(xin) # garante que xin seja matriz
  
  # if(par == 1){
  #   xin<-cbind(1,xin) # Adicionando o termo de polarização na entrada 
  # }

  H<-matrix(nrow = N, ncol = p)
  # Calcula matriz H
  for (j in 1 :N)
  {
    for (i in 1 : p)
    {
      mi<-m[i,]
      covi<-covlist[i]
      covi<-matrix(unlist(covlist[i]),ncol=n,byrow=T)#+0.001*diag(n)
      # H[j,i]<-pdfnvar(xin[j,],mi,covi,n)
      H[j,i]<-pdfnvar(xin[j,], mi, covi+1e-3*diag(nrow(covi)), n)
    }
  }

  Haug<-cbind(1,H)
  Yhat<-Haug %*% W

  if(func_ativ == 0){
    result<-Yhat
  } else {
    result<-sign(Yhat)
  }
  return (result)
}

ErrorRBF <- function (Y, YHat){
  error <- (sum(abs(Y-YHat)))/(2*length(Y))
  return(error)
}

treinaRBFELM <- function (xin,yin,p)
{

  ####### Função Radial Gaussiana
  pdfnvar<-function (x,m,K,n)
  {
    if (n == 1)
    {
      r<-sqrt(as.numeric(K))
      px<-(1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/(r))^2)
    }
    else px<-((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))
    return(px)
  }
  #######

  N<-dim(xin)[1] # número de amostras
  n<-dim(xin)[2] # dimensão de entrada (deve ser maior que 1)

  xin<-as.matrix(xin) # garante que xin seja matrix
  yin<-as.matrix(yin) #garante que yin seja matriz

  xcenters <- matrix(0, nrow = p, ncol = n)
  for (i in seq(1, p, 1)) {
    xcenters[i,] <- xin[runif(1, 1, N),]
  }
  xcluster <- matrix(0, ncol = 1, nrow = N)
  for (i in seq(1, N, 1)) {
    xcluster[i,] <- sample(1:p, size = 1)
  }

  # Armazena vetores de centros das funções
  m <- xcenters
  covlist<-list()

  # Estima matrizes de covariância para todos os centros
  for (i in 1:p)
  {
    ici<-which(xcluster==i)
    xci<-as.matrix(xin[ici,])
    if (n == 1){
      covi<-var(xci) 
    }
    else{
      covi<-cov(xci) 
    }
    covlist[[i]]<-covi
  }

  H<-matrix(nrow=N,ncol=p)
  # calcula mariz H
  for (j in 1:N)
  {
    for (i in 1:p)
    {
      mi<-m[i,]
      covi<-covlist[i]
      covi<-matrix(unlist(covlist[i]),ncol=n,byrow = T) + 0.001*diag(n)
      H[j,i]<-pdfnvar(xin[j,], mi, covi+1e-3*diag(nrow(covi)), n)
    }
  }

  Haug<-cbind(1,H)
  W<-pseudoinverse(Haug) %*% yin
  # W<-(solve(t(Haug) %*% Haug) %*% t(Haug)) %*% yin

  return(list(m,covlist,W,H))
}
