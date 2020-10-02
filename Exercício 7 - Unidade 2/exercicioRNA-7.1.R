rm(list=ls())
library("mlbench")
source("C:/Users/gvamorim/Google Drive/UFMG/Redes Neurais Artificiais/Algorithms/RBF.R")

#gera a entrada para treinamento 
x <- matrix(runif(100, -15, 15), ncol = 1, nrow = 100)
y <- matrix((sin(x)/x + rnorm(100, 0, 0.05)), ncol = 1, nrow = 100)
#gera a entrada para treinamento 
xtest <- matrix(runif(50, -15, 15), ncol = 1, nrow = 50)
ytest <- matrix((sin(xtest)/xtest +rnorm(50, 0, 0.05)), ncol = 1, nrow = 50)
#loop para gerar 3 redes com 1, 4 e 9 neuronios na camada intermediaria, respectivamente
for (i in seq(1, 3, 1)) {
  n <- i*i #numero de neuronios
  pRBF <- treinaRBF(x, y, n) #treinamento da rede
  
  Yhat <- YRBF(xtest, pRBF)
  error <- sum((ytest - Yhat)^2)/length(ytest)
  
  cat("erro: ", error, "para ", n, " neuronios", "\n")
}