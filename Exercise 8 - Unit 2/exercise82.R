rm(list = ls())
library('corpcor')
library(VarSelLCM)

source("C:/Users/gvamorim/Documents/Personal/Repositories/ANN-Artificial-Neural-Network/Algorithms/RBF.R")

#----------------------------------------------------------------------

### Data

data(heart)

# Removing NA data
heart<-heart[complete.cases(heart),]
dt<-data.matrix(heart, rownames.force = NA)

X<-as.matrix(dt[1:dim(dt)[1], 1:12])
Y<-as.matrix(dt[1:dim(dt)[1], 13])

for(i in 1:length(Y)){
  if(Y[i] == 2){
    Y[i] <- -1
  }
}

#----------------------------------------------------------------------

XY <- splitForTrainingAndTest(X, Y, ratio = 0.3)
xtrain <- as.matrix(XY$inputsTrain)
ytrain <- as.matrix(XY$targetsTrain)
xtest <- as.matrix(XY$inputsTest)
ytest <- as.matrix(XY$targetsTest)

for(i in 1:dim(X)[2]){
  max<-max(X[,i])
  min<-min(X[,i])
  for(j in 1:dim(X)[1]){
    X[j,i]<-(X[j,i]-min)/(max-min)
  }
}

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
