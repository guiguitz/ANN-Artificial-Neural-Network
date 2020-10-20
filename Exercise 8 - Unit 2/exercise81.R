# Breast Cancer:
# The class performace with Perceptron algorithm will be study
# with breast cancer data from mlbench.

#----------------------------------------------------------------------

rm(list=ls())
library('corpcor')
library(mlbench)
library('RSNNS')

source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/RBF.R")

#----------------------------------------------------------------------

### Data

data("BreastCancer")
BreastCancer$Class<-1*(BreastCancer$Class=='benign')
for(i in 1:length(BreastCancer$Class)){
  if(BreastCancer[[11]][i] == 0){
    BreastCancer[[11]][i] <- -1
  }
}
# Removing NA data
BreastCancer<-BreastCancer[complete.cases(BreastCancer),]
dt<-data.matrix(BreastCancer, rownames.force = NA)

X<-as.matrix(dt[1:dim(dt)[1], 2:10])
Y<-as.matrix(dt[1:dim(dt)[1], 11])

for(i in 1:dim(X)[2]){
  max<-max(X[,i])
  min<-min(X[,i])
  for(j in 1:dim(X)[1]){
    X[j,i]<-(X[j,i]-min)/(max-min)
  }
}

#----------------------------------------------------------------------

XY <- splitForTrainingAndTest(X, Y, ratio = 0.3)
xtrain <- XY$inputsTrain
ytrain <- XY$targetsTrain
xtest <- XY$inputsTest
ytest <- XY$targetsTest

#----------------------------------------------------------------------

# RBF network with randomly assigned centers to neurons
print("Error using randomly assigned centers ")
for (p in 1:20){
  modRBF <- treinaRBF(xtrain, ytrain, p)
  yhat <- YRBF(xtest, modRBF)
  yhat <- sign(yhat)
 
  # Error calc
  # Calc Error of Test data
  error<-sum((ytest-yhat)^2)/(4*nrow(yhat))
  vr <- var(ytest-yhat)
  cat("Error : ", error,"+/-", vr,  "using ", p, " neurons", "\n")
}


# RBF network with k-means centers
print("Error using k-means")
for (p in 1:20){
  modRBF <- treinaRBF(xtrain, ytrain, p)
  yhat <- YRBF(xtest, modRBF)
  yhat <- sign(yhat)
  
  # Error calc
  # Calc Error of Test data
  error<-sum((ytest-yhat)^2)/(4*nrow(yhat))
  vr <- var(ytest-yhat)
  cat("Error : ", error,"+/-", vr,  "using ", p, " neurons", "\n")
}
