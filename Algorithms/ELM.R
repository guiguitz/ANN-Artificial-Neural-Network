library('corpcor') # pseudoinverse

## Extreme Learning Machine Parameters
extremeLearningMachineParameters <- function(X, Y, p){
  
  # X: Matrix with learning data
  # Y: Vector with classes (-1 or 1)
  # p: Number of neurons in the hidden layer
  # returns: 
  # [[1]] -> Z
  # [[2]] -> W
  # [[3]] -> H
  
  n_dim<-dim(X)[2]
  Z <- replicate(p, runif(n_dim+1, -0.5, 0.5))
  H <- tanh(cbind(1, X) %*% Z)
  W <- pseudoinverse(H) %*% Y
  l <- list(Z,W,H)
  return(l)
}

extremeLearningMachineOutput <- function(H, W){
  return(sign(H %*% W))
}

extremeLearningMachineZ <- function (X, p){
  n_dim<-dim(X)[2]
  Z <- replicate(p, runif(n_dim+1, -0.5, 0.5))
  return(Z)
}

extremeLearningMachineH <- function (X, Z){
  H <- tanh(cbind(1, X) %*% Z)
  return(H)
}

extremeLearningMachineW <- function (H, Y){
  W <- pseudoinverse(H) %*% Y
  return(W)
}