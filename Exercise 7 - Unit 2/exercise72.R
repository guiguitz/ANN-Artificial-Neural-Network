rm(list=ls())
library("mlbench")
source("C:/Users/gvamorim/Google Drive/UFMG/Redes Neurais Artificiais/Algorithms/RBF.R")

#----------------------------------------------------------------------

## Data
X <- matrix(runif(100, -15, 15), ncol = 1, nrow = 100)
Y <- matrix((sin(X)/X + rnorm(100, 0, 0.05)), ncol = 1, nrow = 100)

Xtest <- matrix(runif(50, -15, 15), ncol = 1, nrow = 50)
Ytest <- matrix((sin(Xtest)/Xtest + rnorm(50, 0, 0.05)), ncol = 1, nrow = 50)

error_vec<-c()
for (p in 1:10) {
  
  modRBF <- treinaRBF(X, Y, p)
  
  Yhat <- YRBF(Xtest, modRBF)
  error <- sum((Ytest - Yhat)^2)/length(Ytest)
  
  error_vec<-c(error_vec,error)
  
  plot(Xtest, Yhat, main = paste(p, "centers"), col='red')
}

plot(1:10, error_vec, type = 'b')
