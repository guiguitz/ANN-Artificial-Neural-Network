rm(list = ls())

library("RSNNS")
source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/MLP.R")

#----------------------------------------------------------------------

xtrain = seq(from = 0, to = 2 * pi, by = 0.15)
xtrain = xtrain + (runif(length(xtrain)) - 0.5)/5
ytrain = sin(xtrain)
ytrain = ytrain + (runif(length(ytrain)) - 0.5)/5
xtest = seq(from = 0, to = 2 * pi, by = 0.01)
ytest = sin(xtest)

#----------------------------------------------------------------------

MLP_vec<-c()

for (i in 1:5){
  
  # MLP Training
  MLP<-trainMLP(xtrain, ytrain, 3, 0.01, 0.1, 3000, 0)
  # ytest
  YTestHat<-yMLP(xtest, MLP, 0)
  # Calc percentage mean squared error (MSE)
  MSE<-(sum(YTestHat-ytest)^2)/length(YTestHat)
  
  MLP_vec<-c(MLP_vec, MSE)
}

mean_MLP <- mean(MLP_vec)
sd_MLP <-sd(MLP_vec)

#----------------------------------------------------------------------

# Plotting training error
error_training<-MLP[[3]]
plot(1:length(error_training), error_training[1:length(error_training)], type="l", ylab="Error", xlab="Epoch")

# Plotting sin
xlim<-max(xtrain, xtest)
plot(xtrain, ytrain, type='p', xlim=c(0,xlim), ylim=c(-1,1), col='black', xlab="x", ylab="sin(x)")
par(new =T)
plot(xtest, ytest, type='l', xlim=c(0,xlim), ylim=c(-1,1), col='red', xlab="", ylab="")
par(new =T)
plot(xtest, YTestHat, type='l', xlim=c(0,xlim), ylim=c(-1,1), col='blue', xlab="", ylab="")

MLP_vec
mean_MLP
sd_MLP