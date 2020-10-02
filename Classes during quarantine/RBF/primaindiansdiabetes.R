#### Pima Indians Diabetes 
## RFB: Radial Basis Functions Neural Networks

#----------------------------------------------------------------------

### libraries 

rm(list = ls())
library(mlbench)
library('corpcor')

#----------------------------------------------------------------------

### Data

## Data classes: ('neg' -> -1) and ('pos' -> 1)
## The class is on the last column of dt

data("PimaIndiansDiabetes")
PimaIndiansDiabetes$diabetes <- 1*(PimaIndiansDiabetes$diabetes == 'pos')
for(i in 1:length(PimaIndiansDiabetes$diabetes)){
  if(PimaIndiansDiabetes[[9]][i] == 0){
    PimaIndiansDiabetes[[9]][i] <- -1
  }
}
dt <- data.matrix(PimaIndiansDiabetes, rownames.force = NA)

#----------------------------------------------------------------------

### Functions

## Build identity matrix
buildIdentityMatrix <- function(n, lambda){
  
  # n: Identity matrix nxn
  # return: lambda*(Identity matrix nxn)
  
  m <- diag(rep(1,n))
  return(lambda*m)
}

## Creating Z and H by xin and p
calcZH <- function(xin, p){
  
  # xin: input data
  # p: Neurons number
  # Returns:
  # [[1]] -> z
  # [[2]] -> h
  
  x <- cbind(xin, 1)
  z <- replicate(p, runif(ncol(x), -0.5, 0.5))
  h <- tanh(x%*%z)
  l <- list(z, h)
  return(l)
}

## Calculating w and A coefficients of an RFB
calcCoefficientsRFB <- function(H, parameter_matrix, y){
  
  # returns:
  # [[1]] -> w
  # [[2]] -> A
  
  A <- (t(H) %*% H) + parameter_matrix
  w <- pseudoinverse(A) %*% t(H) %*% y 
  return_list <- list(w, A)
  
  return(return_list)
}

## Calculating Sum square error and hit percentage
calcErrorPercentage <- function(y, yhat){
  
  # y: Expected output
  # yhat: Calculated output
  # Returns:
  # [[1]] -> Sum square error
  # [[2]] -> Hit percentage
  
  sum_error <- sum((y-yhat)^2)/4
  per <- 1-(sum_error/length(y))
  l <- list(sum_error, per)
  return(l)
}

## Calculating the mean square error using LOO
calcLOO <- function(y,N,A,H) {
  
  # Calculating P
  IN <- buildIdentityMatrix(N, 1)
  P <- IN - (H %*% pseudoinverse(A) %*% t(H)) 
  
  # Calculating the mean square error of the LOO
  error<-(t(y) %*% P %*% (t(solve(diag(diag(P)))) %*% solve(diag(diag(P)))) %*% P %*% y)/N
  
  return(error)
}

## RFB algorithm with LOO 
calcRFB <- function(data, p, lambda, calc_LOO){
  
  # data: All data
  # p: neurons number
  # lambda: lambda parameter for regularization
  # calc_LOO: 1 to calc LOO
  # Returns:
  # [[1]] -> LOO
  # [[2]] -> Sum mean square error
  # [[3]] -> Hit percentage
  
  X<-data[,-ncol(data)]
  Y<-data[,+ncol(data)]
  parameter_matrix<-buildIdentityMatrix(p, lambda)
  list<-calcZH(X, p)
  H<-list[[2]]
  Z<-list[[1]]
  list<-calcCoefficientsRFB(H, parameter_matrix, Y)
  W<-list[[1]]
  A<-list[[2]]
  
  # Calculating yhat
  Yhat<-sign(H %*% W)
  
  # Calculating LOO
  LOO<-0
  if(calc_LOO == 1){
    LOO<-calcLOO(Y, length(Y), A, H)
  }
  
  # Calculating error and hit percentage
  error_hit<-calcErrorPercentage(Y, Yhat)
  
  # Return list
  return_list<-list(LOO, error_hit[[1]], error_hit[[2]], W)
  
  # Returns LOO
  return(return_list)
}

## Calculating optimized Lambda and Neuron of a RFB with LOO
optimizedParametersRFB <- function(dt, n_max_neurons, lambda_highest_denominator, lambda_update_rate, method){
  
  # dt: All data
  # n_max_neurons: Max number of neurons
  # lambda_highest_denominator: The shortest lambda value = 1/lambda_highest_denominator
  # lambda_update_rate: Rate to update lambda
  # method: The method to decide what is the best parameters configuration
  # method<-1 -> LOO
  # method<-2 -> Sum square error
  # method<-3 -> Hit percentage
  # Returns:
  # [[1]] -> p_optimized
  # [[2]] -> lambda_optimized, method_type_optimized
  # [[3]] -> method_type_optimized
  # [[4]] -> n_iterations
  
  # Initialize parameters
  lambda_optimized<-0
  p_optimized<-1
  n_iterations<-0
  
  if(method == 1){
    method_type_optimized<-calcRFB(dt, p_optimized, lambda_optimized, method)[[1]]
  }else if(method == 2){
    method_type_optimized<-calcRFB(dt, p_optimized, lambda_optimized, method)[[2]]
  }else if(method == 3){
    method_type_optimized<-calcRFB(dt, p_optimized, lambda_optimized, method)[[3]]
  }
  
  for(i in 1:n_max_neurons){
    
    lambda_actual<-1/lambda_highest_denominator
    
    while(lambda_actual < 0.01){
      
      # Calculating the RFB with i neurons number and lambda_actual lambda
      if(method == 1){
        method_type_actual<-calcRFB(dt, i, lambda_actual, method)[[1]]
      }else if(method == 2){
        method_type_actual<-calcRFB(dt, i, lambda_actual, method)[[2]]
      }else if(method == 3){
        method_type_actual<-calcRFB(dt, i, lambda_actual, method)[[3]]
      }
      
      # If a best configuration of lambda and p was found
      if(method == 1 || method == 2){
        if(method_type_actual < method_type_optimized){
          method_type_optimized<-method_type_actual
          lambda_optimized<-lambda_actual
          p_optimized<-i
        }
      }else{
        if(method_type_actual > method_type_optimized){
          method_type_optimized<-method_type_actual
          lambda_optimized<-lambda_actual
          p_optimized<-i
        }
      }
      
      # Updating the lambda_actual value with the lambda_update_rate
      lambda_actual<-lambda_actual*lambda_update_rate
      
      # Updating the iterations number
      n_iterations<-n_iterations+1
      
    }
  }
  
  return_list<-list(p_optimized, lambda_optimized, method_type_optimized, n_iterations)
  return(return_list)
  
}

#----------------------------------------------------------------------

### Run

## Custom data
# method<-1 -> LOO
# method<-2 -> Sum square error
# method<-3 -> Hit percentage

n_max_neurons<-20
lambda_highest_denominator<-1000
lambda_update_rate<-1.1
method<-1

## Using LOO to find the best lambda and p configuration
list<-optimizedParametersRFB(dt, n_max_neurons, lambda_highest_denominator, lambda_update_rate, method)
p_optimized<-list[[1]]
lambda_optimized<-list[[2]]
method_type_optimized<-list[[3]]
n_iterations<-list[[4]]


#----------------------------------------------------------------------

### Printing results

if(method == 1){
  method_string<-{"LOO"}
}else if(method == 2){
  method_string<-{"Sum square error"}
}else if(method == 3){
  method_string<-{"Hit Percetege"}
}

cat("\nMethod used:", method_string, "\nIterations number:", n_iterations,"\nOptimized lambda:", lambda_optimized, "\nOptimized Neurons number:", p_optimized, paste("\nOptimized ", method_string, ":", sep = ""), method_type_optimized, "\n") 

# Test
#calcRFB(dt, 20, 0.001, 1)
