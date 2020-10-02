rm(list=ls())
library("mlbench")
source("C:/Users/gvamorim/Google Drive/UFMG/Redes Neurais Artificiais/Algorithms/RBF.R")
library('RSNNS') #Fornece a rotina para validação 

ams1 <- mlbench.2dnormals(200)
ams2 <- mlbench.xor(100)
ams3 <- mlbench.circle(100)
ams4 <- mlbench.spirals(100, sd = 0.5)

X1 <- as.matrix(ams1$x) 
Y1 <- as.matrix(c(ams1$classes)) 
X2 <- as.matrix(ams2$x) 
Y2 <- as.matrix(c(ams2$classes))
X3 <- as.matrix(ams3$x) 
Y3 <- as.matrix(c(ams3$classes)) 
X4 <- as.matrix(ams4$x) 
Y4 <- as.matrix(c(ams4$classes)) 

for(i in seq(1, length(Y1), 1)){
  if(Y1[i] == 2)
    Y1[i] <- -1
}

for(i in seq(1, length(Y2), 1)){
  if(Y2[i] == 2)
    Y2[i] <- -1
}

for(i in seq(1, length(Y3), 1)){
  if(Y3[i] == 2)
    Y3[i] <- -1
}

for(i in seq(1, length(Y4), 1)){
  if(Y4[i] == 2)
    Y4[i] <- -1
}

XY1 <- splitForTrainingAndTest(X1, Y1, ratio = 0.3)
x1train <- as.matrix(XY1$inputsTrain)
y1train <- as.matrix(XY1$targetsTrain)
x1test <- as.matrix(XY1$inputsTest)
y1test <- as.matrix(XY1$targetsTest)

XY2 <- splitForTrainingAndTest(X2, Y2, ratio = 0.3)
x2train <- as.matrix(XY2$inputsTrain)
y2train <- as.matrix(XY2$targetsTrain)
x2test <- as.matrix(XY2$inputsTest)
y2test <- as.matrix(XY2$targetsTest)

XY3 <- splitForTrainingAndTest(X3, Y3, ratio = 0.3)
x3train <- as.matrix(XY3$inputsTrain)
y3train <- as.matrix(XY3$targetsTrain)
x3test <- as.matrix(XY3$inputsTest)
y3test <- as.matrix(XY3$targetsTest)

XY4 <- splitForTrainingAndTest(X4, Y4, ratio = 0.3)
x4train <- as.matrix(XY4$inputsTrain)
y4train <- as.matrix(XY4$targetsTrain)
x4test <- as.matrix(XY4$inputsTest)
y4test <- as.matrix(XY4$targetsTest)

for(p in c(1,4,9))
{
  pRBF <- treinaRBF(x1train,y1train, p)
  Y1hat<-tanh(YRBF(x1test, pRBF))
  error1 <- sum((y1test - Y1hat)^2)/length(y1test)*4
  
  cat("Error: ", error1, "using", p, "centers", "\n")
  
  seq1x1x2 <- seq(min(X1), max(X1), 0.1)
  lseq1 <- length(seq1x1x2)
  MZ1 <- matrix(nrow = lseq1, ncol = lseq1)
  for (i in 1:lseq1) {
    for (j in 1:lseq1) {
      x1 <- seq1x1x2[i]
      x2 <- seq1x1x2[j]
      x1x2 <- matrix((cbind(x1,x2)), nrow = 1)
      MZ1[i,j] <- tanh(YRBF(x1x2, pRBF))
    }
  }

  contour(seq1x1x2, seq1x1x2, MZ1, nlevels = 1, ylim = c(max(X1), min(X1)),  xlim = c(max(X1), min(X1)))
  par(new =T)
  plot(ams1, ylim = c(max(X1), min(X1)), xlim = c(max(X1), min(X1)), main = paste(p, "centers"))
}


cat("\n")
for(p in c(1,9,16))
{
  pRBF <- treinaRBF(x2train,y2train, p)
  Y2hat<-tanh(YRBF(x2test, pRBF))
  error2 <- sum(abs(y2test - Y2hat))/length(y2test)*4
  
  cat("Error: ", error2, "using", p, "centers", "\n")
  
  seq2x1x2 <- seq(min(X2), max(X2), 0.1)
  lseq2 <- length(seq2x1x2)
  MZ2 <- matrix(nrow = lseq2, ncol = lseq2)
  
  for (i in 1:lseq2) {
    for (j in 1:lseq2) {
      x1 <- seq2x1x2[i]
      x2 <- seq2x1x2[j]
      x1x2 <- matrix((cbind(x1,x2)), nrow = 1)
   
      MZ2[i,j] <- tanh(YRBF(x1x2, pRBF))
    }
  }
  
  contour(seq2x1x2, seq2x1x2, MZ2, nlevels = 1, ylim = c(max(X2), min(X2)),  xlim = c(max(X2), min(X2)))
  par(new =T)
  plot(ams2, ylim = c(max(X2), min(X2)),  xlim = c(max(X2), min(X2)), main = paste(p, "centers"))
}

for(p in c(4,9,16))
{
  pRBF <- treinaRBF(x3train,y3train, p)
  Y3hat<-tanh(YRBF(x3test, pRBF))
  error3 <- sum(abs(y3test - Y3hat))/length(y3test)*4
  
  cat("Error: ", error3, "using", p, "centers", "\n")
  
  seq3x1x2 <- seq(min(X3), max(X3), 0.1)
  lseq3 <- length(seq3x1x2)
  MZ3 <- matrix(nrow = lseq3, ncol = lseq3)
  
  for (i in 1:lseq3) {
    for (j in 1:lseq3) {
      x1 <- seq3x1x2[i]
      x2 <- seq3x1x2[j]
      x1x2 <- matrix((cbind(x1,x2)), nrow = 1)
      MZ3[i,j] <- tanh(YRBF(x1x2, pRBF))
    }
  }
  
  contour(seq3x1x2, seq3x1x2, MZ3, nlevels = 1, ylim = c(max(X3), min(X3)),  xlim = c(max(X3), min(X3)))
  par(new =T)
  plot(ams3, ylim = c(max(X3), min(X3)),  xlim = c(max(X3), min(X3)), main = paste(p, "centers"))
  
}

for(p in c(4,9,16))
{
  p<-16
  pRBF <- treinaRBF(x4train,y4train, p)
  Y4hat<-tanh(YRBF(x4test, pRBF))
  error4 <- sum(abs(y4test - Y4hat))/length(y4test)*4
  
  cat("Error: ", error4, "using", p, "centers", "\n")
  
  seq4x1x2 <- seq(min(X4), max(X4), 0.1)
  lseq4 <- length(seq4x1x2)
  MZ4 <- matrix(nrow = lseq4, ncol = lseq4)
  
  for (i in 1:lseq4) {
    for (j in 1:lseq4) {
      x1 <- seq4x1x2[i]
      x2 <- seq4x1x2[j]
      x1x2 <- matrix((cbind(x1,x2)), nrow = 1)
      MZ4[i,j] <- tanh(YRBF(x1x2, pRBF))
    }
    
  }
  
  contour(seq4x1x2, seq4x1x2, MZ4, nlevels = 1, ylim = c(max(X4), min(X4)),  xlim = c(max(X4), min(X4)))
  par(new =T)
  plot(ams4, ylim = c(max(X4), min(X4)),  xlim = c(max(X4), min(X4)), main = paste(p, "centers"))
}
