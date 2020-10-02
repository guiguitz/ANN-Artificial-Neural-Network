rm(list=ls())
library('corpcor')# biblioteca para uso da pseudoinversa 
library('mlbench')# biblioteca para usar as bases de dados 

#gerando as bases de dados requisitadas
ams1 <- mlbench.2dnormals(200)
ams2 <- mlbench.xor(100)
ams3 <- mlbench.circle(100)
ams4 <- mlbench.spirals(100,sd = 0.05)

#obtencao dos valores de entrada
X1 <- ams1$x 
Y1 <- c(ams1$classes) 
X2 <- ams2$x 
Y2 <- c(ams2$classes) 
X3 <- ams3$x 
Y3 <- c(ams3$classes) 
X4 <- ams4$x 
Y4 <- c(ams4$classes) 

n <- 30 #numero de neuronios da camada escondida


Z <- replicate(n, runif(3, -0.5, 0.5)) #gera valores de pesos aleatórios para a camada escondida

# adicao do termo de polarizacao
X1ext <- (cbind(1, X1)) 
X2ext <- (cbind(1, X2)) 
X3ext <- (cbind(1, X3)) 
X4ext <- (cbind(1, X4))

# projecao da entrada na camada escondida
H1 <- tanh(X1ext %*% Z)
H2 <- tanh(X2ext %*% Z)
H3 <- tanh(X3ext %*% Z)
H4 <- tanh(X4ext %*% Z)

#ajuste dos valores de saida para -1 e 1 
for(i in seq(1, length(Y1), 1)){
  if(Y1[i] == 2)
    Y1[i] <- -1
  if(Y1[i] == 1)
    Y1[i] <- 1
}

for(i in seq(1, length(Y2), 1)){
  if(Y2[i] == 2)
    Y2[i] <- -1
  if(Y2[i] == 1)
    Y2[i] <- 1
}

for(i in seq(1, length(Y3), 1)){
  if(Y3[i] == 2)
    Y3[i] <- -1
  if(Y3[i] == 1)
    Y3[i] <- 1
}

for(i in seq(1, length(Y4), 1)){
  if(Y4[i] == 2)
    Y4[i] <- -1
  if(Y4[i] == 1)
    Y4[i] <- 1
}

# obtencao da matriz de pesos de saida
W1 <- pseudoinverse(H1) %*% Y1
W2 <- pseudoinverse(H2) %*% Y2
W3 <- pseudoinverse(H3) %*% Y3
W4 <- pseudoinverse(H4) %*% Y4

# obtencao da saida para cada entrada com base na projecao da saida 
Yo1 <- sign(H1 %*% W1)
Yo2 <- sign(H2 %*% W2)
Yo3 <- sign(H3 %*% W3)
Yo4 <- sign(H4 %*% W4)

#erro medio quadratico 
erro1 <- sum((Y1 - Yo1)^2)/length(Y1)
erro2 <- sum((Y2 - Yo2)^2)/length(Y2)
erro3 <- sum((Y3 - Yo3)^2)/length(Y3)
erro4 <- sum((Y4 - Yo4)^2)/length(Y4)

print("erro 1")
print(erro1)
print("erro 2")
print(erro2)
print("erro 3")
print(erro3)
print("erro 4")
print(erro4)

#valores base para para varrer o plano
seq1x1x2 <- seq(min(X1), max(X1), 0.1)
seq2x1x2 <- seq(min(X2), max(X2), 0.1)
seq3x1x2 <- seq(min(X3), max(X3), 0.1)
seq4x1x2 <- seq(min(X4), max(X4), 0.1)

lseq1 <- length(seq1x1x2)
lseq2 <- length(seq2x1x2)
lseq3 <- length(seq3x1x2)
lseq4 <- length(seq4x1x2)

MZ1 <- matrix(nrow = lseq1, ncol = lseq1)
MZ2 <- matrix(nrow = lseq2, ncol = lseq2)
MZ3 <- matrix(nrow = lseq3, ncol = lseq3)
MZ4 <- matrix(nrow = lseq4, ncol = lseq4)

cr1 <- 0
cr2 <- 0
cr3 <- 0
cr4 <- 0

#montar matriz com os pontos da superficie de separacao 
for (i in 1:lseq1) {
  for (j in 1:lseq1) {
    cr1 <- cr1+1
    x1 <- seq1x1x2[i]
    x2 <- seq1x1x2[j]
    x1x2 <- matrix((cbind(x1,x2)), nrow = 1)
    x1x2ext <- cbind(1, x1x2)
    Ha <- tanh(x1x2ext %*% Z)
    MZ1[i,j] <- Ha %*%  W1
  }
}

for (i in 1:lseq2) {
  for (j in 1:lseq2) {
    cr2 <- cr2+1
    x1 <- seq2x1x2[i]
    x2 <- seq2x1x2[j]
    x1x2 <- matrix((cbind(x1,x2)), nrow = 1)
    x1x2ext <- cbind(1, x1x2)
    Ha <- tanh(x1x2ext %*% Z)
    MZ2[i,j] <- Ha %*%  W2
  }
}

for (i in 1:lseq3) {
  for (j in 1:lseq3) {
    cr3 <- cr3+1
    x1 <- seq3x1x2[i]
    x2 <- seq3x1x2[j]
    x1x2 <- matrix((cbind(x1,x2)), nrow = 1)
    x1x2ext <- cbind(1, x1x2)
    Ha <- tanh(x1x2ext %*% Z)
    MZ3[i,j] <- Ha %*%  W3
  }
}

for (i in 1:lseq4) {
  for (j in 1:lseq4) {
    cr4 <- cr4+1
    x1 <- seq4x1x2[i]
    x2 <- seq4x1x2[j]
    x1x2 <- matrix((cbind(x1,x2)), nrow = 1)
    x1x2ext <- cbind(1, x1x2)
    Ha <- tanh(x1x2ext %*% Z)
    MZ4[i,j] <- Ha %*%  W4
  }
}

#plot das superfícies de separacao
contour(seq1x1x2, seq1x1x2, MZ1, nlevels = 1, ylim = c(max(X1), min(X1)),  xlim = c(max(X1), min(X1)))
par(new =T)
plot(ams1, ylim = c(max(X1), min(X1)),  xlim = c(max(X1), min(X1)))

contour(seq2x1x2, seq2x1x2, MZ2, nlevels = 1, ylim = c(max(X2), min(X2)),  xlim = c(max(X2), min(X2)))
par(new =T)
plot(ams2, ylim = c(max(X2), min(X2)),  xlim = c(max(X2), min(X2)))

contour(seq3x1x2, seq3x1x2, MZ3, nlevels = 1, ylim = c(max(X3), min(X3)),  xlim = c(max(X3), min(X3)))
par(new =T)
plot(ams3, ylim = c(max(X3), min(X3)),  xlim = c(max(X3), min(X3)))

contour(seq4x1x2, seq4x1x2, MZ4, nlevels = 1, ylim = c(max(X4), min(X4)),  xlim = c(max(X4), min(X4)))
par(new =T)
plot(ams4, ylim = c(max(X4), min(X4)),  xlim = c(max(X4), min(X4)))
