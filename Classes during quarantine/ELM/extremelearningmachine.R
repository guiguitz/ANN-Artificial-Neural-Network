# Extreme Learning Machine:

#----------------------------------------------------------------------

rm(list = ls())
library(package="corpcor")
library('ggplot2')
library('plot3D')

#----------------------------------------------------------------------

# Custom data:
n_samp_learn_xc1<-70
n_samp_test_xc1<-70
n_samp_learn_xc2<-70
n_samp_test_xc2<-70

n_samp_learn<-n_samp_learn_xc1+n_samp_learn_xc2
n_samp_test<-n_samp_test_xc1+n_samp_test_xc2

#----------------------------------------------------------------------

# Creating data for learning
# xc1: data type 0
# xc2: data type 1
# xc1_test: data type 0 for learning
# xc2_test: data type 1 for learning

# Creating all xc1 data: For learning and test
xc1<-rbind(matrix(0.3*rnorm(n_samp_learn_xc1+n_samp_test_xc1)+2,ncol=2), matrix(0.3*rnorm(n_samp_learn_xc1+n_samp_test_xc1)+4,ncol=2))
removed_idx_xc1<-sample(1:(n_samp_learn_xc1+n_samp_test_xc1),n_samp_test_xc1,replace=FALSE)
xc1_test<-xc1[+removed_idx_xc1,]
xc1<-xc1[-removed_idx_xc1,]

# Creating all xc2 data: For learning and test
xc2<-rbind(cbind(0.3*rnorm((n_samp_learn_xc2+n_samp_test_xc2)/2)+2,0.4*rnorm((n_samp_learn_xc2+n_samp_test_xc2)/2)+4), cbind(0.3*rnorm((n_samp_learn_xc2+n_samp_test_xc2)/2)+4,0.4*rnorm((n_samp_learn_xc2+n_samp_test_xc2)/2)+2))
removed_idx_xc2<-sample(1:(n_samp_learn_xc2+n_samp_test_xc2),n_samp_test_xc2,replace=FALSE)
xc2_test<-xc2[+removed_idx_xc2,]
xc2<-xc2[-removed_idx_xc2,]

# Data for ELM algorithm
X<-cbind(rbind(xc1,xc2),1)
X_test<-cbind(rbind(xc1_test,xc2_test),1)
Y<-matrix(c(rep(-1,n_samp_learn_xc1), rep(1,n_samp_learn_xc2)), ncol = 1)
Y_test<-matrix(c(rep(-1,n_samp_test_xc1), rep(1,n_samp_test_xc2)), ncol = 1)

#----------------------------------------------------------------------

# p: Number of neurons in the hidden layer
# m: Number of neurons in the input layer
# X: Matrix with learning data
# Y: Vector with classes (0 or 1)
# Z: Matrix with input weights (random weights)
# W: Matrix with hidden layer connection weights (features matrix)
# H: Stores the result of all hidden layer neurons 
# obtained from the X and W multiplication

# Deciding the number of neurons and creating X and Y
p<-20

# Starting the input weights randomly (W)
Z<-replicate(p, runif(dim(X)[2], -0.5, 0.5))

# Calculating matrix H
H<-tanh(X %*% Z)

# Calculating the features matrix (W)
W<-pseudoinverse(H) %*% Y

#----------------------------------------------------------------------

# Checking results with data used on learning
# error: Mean square error
# Yhat_training: Data output obtained with H and Z

Y_training<-sign(H %*% W)
error_training<-sum((Y-Y_training)^2)/4

Y_ttest<-sign(tanh(X_test %*% Z) %*% W)
error_test<-sum((Y_test-Y_ttest)^2)/4

#----------------------------------------------------------------------

# Creating a grid to expose results

seqx1x2<-seq(-2,10,0.1)
lseq<-length(seqx1x2)
M<-matrix(nrow=lseq, ncol=lseq)
for(i in 1:lseq){
  for(j in 1:lseq){
    x_1 <- seqx1x2[i]
    x_2 <- seqx1x2[j]
    x1x2 <- as.matrix(cbind(x_1,x_2,1))
    H <- tanh(x1x2%*%Z)
    M[i,j] <- sign(H %*% W)
  }
}

#----------------------------------------------------------------------

# Ploting Results

plot(xc1[,1],xc1[,2],xlim=c(-2,10),ylim=c(-2,10),xlab='x1',ylab='x2',col='blue')
par(new=TRUE)
plot(xc2[,1],xc2[,2],xlim=c(-2,10),ylim=c(-2,10),xlab='x1',ylab='x2',col='red')
par(new= TRUE)
contour(x= seqx1x2, y = seqx1x2, z = M, nlevels = 1, xlim = c(-2,10), ylim = c(-2,10), xlab= "x1", ylab = "x2")

persp3D(seqx1x2, seqx1x2, M)
#image(M, col = blues9)

cat("error_training =", error_training)
cat("error_test =", error_test)

### References: # http://computacaointeligente.com.br/algoritmos/maquina-de-aprendizado-extremo/
