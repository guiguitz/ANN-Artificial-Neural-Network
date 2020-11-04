rm(list = ls())
library('RSNNS')

#----------------------------------------------------------------------

# Reading data

Wine.data <- as.matrix(read.csv("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Final Article/winequality-red.csv", 
                                header = TRUE, sep = ";"))

X<-as.matrix(Wine.data[1:dim(Wine.data)[1], 1:11])
Y<-as.matrix(Wine.data[1:dim(Wine.data)[1], 12])
for(i in 1:length(Y)){
  if(Y[i] < 6){
    Y[i]<-0
  } else {
    Y[i]<-1
  }
}

#----------------------------------------------------------------------

# Splitting data for training and testing 

XY <- splitForTrainingAndTest(X, Y, ratio = 0.3)
xtrain <- XY$inputsTrain
ytrain <- XY$targetsTrain
xtest <- XY$inputsTest
ytest <- XY$targetsTest

#----------------------------------------------------------------------

# Perceptron Simples

source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/perceptron.R")

error_train_vec<-c()
error_test_vec<-c()
for(i in 1:5){
  perceptron<-trainperceptron(xtrain,ytrain,0.1, 0.01, 10000, 1)
  W<-perceptron[[1]]
  error_train<-min(perceptron[[2]])
  error_train_vec<-c(error_train_vec, error_train)
  
  YhatTest<-yperceptron(xtest, W, 1)
  error_test<-sum(abs(ytest-YhatTest))/length(ytest)
  error_test_vec<-c(error_test_vec, error_test)
}

cat("Perceptron: ",
    "\n  Training Error: ", mean(error_train_vec), "+/-", sd(error_train_vec),
    "\n  Testing Error:  ", mean(error_test_vec), "+/-", sd(error_test_vec))

#----------------------------------------------------------------------

# ELM

source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/ELM.R")

for(i in 1:length(ytrain)){
  if(ytrain[i]==0){
    ytrain[i]<--1
  }
}
for(i in 1:length(ytest)){
  if(ytest[i]==0){
    ytest[i]<--1
  }
}

error_train_avg_vec<-c()
error_test_avg_vec<-c()
for(i in 1:20){
  error_train_vec<-c()
  error_test_vec<-c()
  for(j in 1:50){
    ELM<-extremeLearningMachineParameters(xtrain, ytrain, 2*j)
    Z<-ELM[[1]]
    W<-ELM[[2]]
    
    # Training data
    H<-ELM[[3]] 
    YhatTrain<-extremeLearningMachineOutput(H,W)
    error_train<-extremeLearningMachineError(ytrain, YhatTrain)
    error_train_vec<-c(error_train_vec, error_train)
    
    # Testing data
    H<-extremeLearningMachineH(xtest, Z)
    YhatTest<-extremeLearningMachineOutput(H,W)
    error_test<-extremeLearningMachineError(ytest, YhatTest)
    error_test_vec<-c(error_test_vec, error_test)
  }
  error_train_avg_vec<-c(error_train_avg_vec, min(error_train_vec))
  error_test_avg_vec<-c(error_test_avg_vec, min(error_test_vec))
}

cat("ELM: ",
    "\n  Training Error: ", mean(error_train_avg_vec), "+/-", sd(error_train_avg_vec),
    "\n  Testing Error:  ", mean(error_test_avg_vec), "+/-", sd(error_test_avg_vec))

#----------------------------------------------------------------------

# RBF

source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/RBF.R")

error_train_avg_vec<-c()
error_test_avg_vec<-c()
for(i in 1:5){
  error_train_vec<-c()
  error_test_vec<-c()
  for(j in 1:5){
    RBF<-rbf(xtrain, ytrain, size = 3*j, maxit = 500,initFunc = "RBF_Weights",
             initFuncParams = c(0,1,0,0.02,0.04),learnFunc ="RadialBasisLearning",learnFuncParams=c(1e-05,0,1e-05,0.1,0.8),
             updateFunc = "Topological_Order",updadeFuncParams = c(0),
             shufflePatterns = TRUE,linOut = FALSE)
    
    # Training data
    YhatTrain <- predict(RBF,as.matrix(xtrain))
    error_train<-ErrorRBF(ytrain, YhatTrain)
    error_train_vec<-c(error_train_vec, error_train)
    
    # Testing data
    YhatTest <- predict(RBF,as.matrix(xtest))
    error_test<-ErrorRBF(ytest, YhatTest)
    error_test_vec<-c(error_test_vec, error_test)
  }
  error_train_avg_vec<-c(error_train_avg_vec, min(error_train_vec))
  error_test_avg_vec<-c(error_test_avg_vec, min(error_test_vec))
}

cat("RBF: ",
    "\n  Training Error: ", mean(error_train_avg_vec), "+/-", sd(error_train_avg_vec),
    "\n  Testing Error:  ", mean(error_test_avg_vec), "+/-", sd(error_test_avg_vec))

#----------------------------------------------------------------------


# MLP

source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/MLP.R")

error_train_avg_vec<-c()
error_test_avg_vec<-c()
for(i in 1:2){
  error_train_vec<-c()
  error_test_vec<-c()
  for(j in 1:5){
    MLP<-trainMLP(xtrain,ytrain,3*j,0.1,0.01,2000,1)
    
    # Training data
    YhatTrain<-yMLP(xtrain, MLP, 1)
    error_train<-errorMLP(ytrain, YhatTrain)
    error_train_vec<-c(error_train_vec, error_train)
    
    # Testing data
    YhatTest<-yMLP(xtest, MLP, 1)
    error_test<-errorMLP(ytest, YhatTest)
    error_test_vec<-c(error_test_vec, error_test)
  }
  error_train_avg_vec<-c(error_train_avg_vec, min(error_train_vec))
  error_test_avg_vec<-c(error_test_avg_vec, min(error_test_vec))
}

cat("MLP: ",
    "\n  Training Error: ", mean(error_train_avg_vec), "+/-", sd(error_train_avg_vec),
    "\n  Testing Error:  ", mean(error_test_avg_vec), "+/-", sd(error_test_avg_vec))

#----------------------------------------------------------------------
