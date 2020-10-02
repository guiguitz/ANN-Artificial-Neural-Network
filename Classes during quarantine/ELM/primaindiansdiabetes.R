#### Pima Indians Diabetes 

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

### Funcitons

## Extreme Learning Machine 
extremeLearningMAchine <- function(X, Y, p, n_dimension){
  
  # X: Matrix with learning data
  # Y: Vector with classes (-1 or 1)
  # p: Number of neurons in the hidden layer
  # returns: 
  # [[1]] -> Z
  # [[2]] -> W
  # [[3]] -> H
  
  X <- cbind(X, 1)
  Z <- replicate(p, runif((n_dimension+1), -0.5, 0.5))
  H <- tanh(X %*% Z)
  W <- pseudoinverse(H) %*% Y
  l <- list(Z,W,H)
  return(l)
}

## Split the data in n_pieces pieces
splitData <- function(data, n_pieces){
  
  # data: matrix with all data
  # n_pieces: How many times the data will be splited
  # Returns a list with data splited in n_pieces pieces randomly
  
  n_data <- nrow(data)
  n <- (n_data %/% n_pieces)
  return_list <- list()
  
  for(i in 1:n_pieces){
    index <- sample(1:n, size = n, replace = FALSE)
    return_list[[i]] <- data[index,]
    data <- data[-index,]
  }
  return(return_list)
}

## Houldout Validation of an ELM 
houldoutValidation <- function(data, p){
  
  # data: Matrix with all data
  # p: Number of neurons in the hidden layer
  # The output must be the last column of data
  # Returns the hit percentage
  
  # The data is splited randomly in 75% for learning and 25% for test 
  splited_data_all<-splitData(data, 4)
  splited_data_train<-rbind(splited_data_all[[1]], splited_data_all[[2]], splited_data_all[[3]])
  splited_data_test<-splited_data_all[[4]]
  
  # Training the data separated for training
  x_train<-splited_data_train[ ,-ncol(splited_data_train)]
  y_train<-splited_data_train[ ,+ncol(splited_data_train)]
  elm_return_list<-extremeLearningMAchine(x_train, y_train, p, ncol(x_train))
  Z<-elm_return_list[[1]]
  W<-elm_return_list[[2]]
  
  # Calculating the output from data separated to be tested
  x_test<-cbind(splited_data_test[ ,-ncol(splited_data_test)],1)
  y_test<-splited_data_test[ ,+ncol(splited_data_test)]
  Y_hat<-sign(x_test %*% Z %*% W)
  
  # Calculating errors and hit percentage
  acumulated_error<-sum((y_test-Y_hat)^2)/4
  hit_percentage<-1-(acumulated_error/length(Y_hat))
  
  return(hit_percentage)
}

## Cross Validation of an ELM 
crossValidation <- function(data, p, n_folds){
  
  # data: Matrix with all data
  # p: Number of neurons in the hidden layer
  # n_folds: Number of folds that the data was separated
  # The output must be the last column of data
  # Returns the avarage hit percentage of each intaraction
  
  return_vector<-c()
  
  # The data is splited randomly in n_folds folds 
  splited_data_all<-splitData(data, n_folds)
  
  for (i in 1:n_folds) {
    train <- c()
    train <- matrix(ncol = ncol(splited_data_all[[1]]))
    
    # Spliting the data for learning and for test
    for (j in 1:n_folds) {
      if(i==j){
        test <- splited_data_all[[j]]
      }
      else{
        train <-rbind(train,splited_data_all[[j]])  
      }
    }
    
    # Training the data separated for training
    train<-train[-1,]
    x_train<-train[,-ncol(train)]
    y_train<-train[,+ncol(train)]
    elm_return_list<-extremeLearningMAchine(x_train, y_train, p, ncol(x_train))
    Z<-elm_return_list[[1]]
    W<-elm_return_list[[2]]
    
    # Calculating the output from data separated to be tested
    x_test<-test[,-ncol(test)]
    y_test<-test[,+ncol(test)]
    x_test<-cbind(x_test,1)
    Y_hat<-sign(x_test %*% Z %*% W)
    
    # Calculating errors and hit percentage
    acumulated_error<-sum((y_test-Y_hat)^2)/4
    hit_percentage<-1-(acumulated_error/length(Y_hat))

    # Updating the return vector with hit percentage
    return_vector<-c(return_vector, hit_percentage)
    
  }
  return(mean(return_vector))
}

## ADALINE Alhorithm (Adaptive Linear Neuron or later Adaptive Linear Element)
trainingADALINE <- function(xin,yd,eta,tol,maxepochs,par,lambda)
{
  # Adaline algorithm
  # xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
  # yd: Output (0 or 1)
  # eta: Learning rate
  # tol: Error tolerance
  # maxepochs: Max interation number
  # par: xin include or not the polarizarion term (0 or 1)
  
  dimxin<-dim(xin)
  N<-dimxin[1] # Number of samples
  n<-dimxin[2] # Input dimenssion.
  
  # Adding or not the polarization term
  if (par==1){
    wt<-as.matrix(runif(n+1, -0.5, 0.5))
    xin<-cbind(-1,xin)
  }
  else wt<-as.matrix(runif(n, -0.5, 0.5))
  
  # Initialize nepochs, eepoch and error vector
  nepochs<-0
  eepoch<-tol+1
  evec<-matrix(nrow=1,ncol=maxepochs)
  
  # Training loop
  while ((nepochs < maxepochs) && (eepoch>tol))
  {
    ei2<-0
    # Random sequence for training.
    xseq<-sample(N)
    
    for (i in 1:N)
    {
      # Pattern from random sequence
      irand<-xseq[i]
      
      # Calculating yhat
      yhat<-(xin[irand,] %*% wt)
      
      # Calculating the error
      ei<-(yd[irand]-yhat)
      
      # Calculatong delta and dw
      delta<-ei*xin[irand,]
      dw<-eta*delta + lambda*wt
      
      # Updating weight
      wt<-wt+dw
      
      # Accumulate square error
      ei2<-ei2+ei*ei
    }
    # Number of epochs update
    nepochs<-nepochs+1
    
    # Updating the error vec with the avarage error of this last epoch
    evec[nepochs]<-ei2/N
    
    # Getting the error of the last epoch for the next interation
    eepoch<-evec[nepochs]
  }
  
  # Return the coeficients vector and the error vector
  retlist<-list(wt,evec[1:nepochs])
  return(retlist)
}

#----------------------------------------------------------------------

### Run
n_max_neurons<-20
n_interaction_per_neurons<-20
n_folds_CV<-10

## Houldout Validation
hit_percentage_HV<-c()
for(i in 1:n_max_neurons){
  aux<-0
  for(j in 1:n_interaction_per_neurons){
    aux<-aux+houldoutValidation(dt, i)
  }
  hit_percentage_HV<-c(hit_percentage_HV, aux/n_interaction_per_neurons)
}
max_hit_percentage_HV<-matrix(data = rbind(which.max(hit_percentage_HV), max(hit_percentage_HV)))


## Cross Validation
hit_percentage_CV<-c()
for(i in 1:n_max_neurons){
  aux<-0
  for(j in 1:n_interaction_per_neurons){
    aux<-aux+crossValidation(dt, i, n_folds_CV)
  }
  hit_percentage_CV<-c(hit_percentage_CV, aux/n_interaction_per_neurons) 
}
max_hit_percentage_CV<-matrix(data = rbind(which.max(hit_percentage_CV), max(hit_percentage_CV)))

#----------------------------------------------------------------------

### Using ADALINE

## Using the best neurons number
p_ada<-max_hit_percentage_CV[1]

## The data is splited randomly in 75% for learning and 25% for test 
splited_data_all_ada<-splitData(dt, 4)
splited_data_train_ada<-rbind(splited_data_all_ada[[1]], splited_data_all_ada[[2]], splited_data_all_ada[[3]])
splited_data_test_ada<-splited_data_all_ada[[4]]

## Data separated for training
x_train_ada<-splited_data_train_ada[ ,-ncol(splited_data_train_ada)]
y_train_ada<-splited_data_train_ada[ ,+ncol(splited_data_train_ada)]
elm_return_ada<-extremeLearningMAchine(x_train_ada, y_train_ada, p_ada, ncol(x_train_ada))
z_train_ada<-elm_return_ada[[1]]
w_train_ada<-elm_return_ada[[2]]
h_train_ada<-elm_return_ada[[3]]

## ADALINE algorithm
return_ada<-trainingADALINE(h_train_ada, y_train_ada, 0.00001, 0.0000001, 100, 0, 0.000001)
wt_ada<-return_ada[[1]]

## Calculating the output from data separated to be tested
x_test_ada<-splited_data_test_ada[ ,-ncol(splited_data_test_ada)]
y_test_ada<-splited_data_test_ada[ ,+ncol(splited_data_test_ada)]
elm_return_ada<-extremeLearningMAchine(x_test_ada, y_test_ada, p_ada, ncol(x_test_ada))
z_test_ada<-elm_return_ada[[1]]
w_test_ada<-elm_return_ada[[2]]
h_test_ada<-elm_return_ada[[3]]
yhat_test_ada<-sign(h_test_ada %*% wt_ada)

## Calcuating error and hit percentage of data separated to be tested
sum_error_test_ada<-sum((y_test_ada-yhat_test_ada)^2)/4
hit_percentage_test_ada<-1-(sum_error_test_ada/length(y_test_ada))

#----------------------------------------------------------------------

### Printing results

## Ploting Hit percentage x Neurons number (Houldout Validation)
barplot(hit_percentage_HV, 
        main = "Hit percentage Houdout Validation",
        xlab = "Neurons Number",
        ylab = "Hit Percentage",
        col = "darkblue", 
        horiz = FALSE)

## Ploting Hit percentage x Neurons number (Cross Validation)
barplot(hit_percentage_CV, 
        main = "Hit percentage Cross Validation",
        xlab = "Neurons Number",
        ylab = "Hit Percentage",
        col = "darkred", 
        horiz = FALSE)

## Ploting max Hit percentage x Neurons number (Houldout and Cross Validations)
barplot(cbind(max_hit_percentage_HV[2], max_hit_percentage_CV[2], hit_percentage_test_ada), 
        main = "Hit percentage HV, CV and ADALINE",
        xlab = "Validation Method",
        ylab = "Hit Percentage",
        names.arg = c("Houdout", "Cross", "ADALINE"),
        col = c("darkred", "darkblue", "yellow"), 
        horiz = FALSE)

cat("\nMax hit percentage with Holdout Validation:", max_hit_percentage_HV, "\nMax hit percentage with Cross validation:", max_hit_percentage_CV, "\nHit percentage with ADALINE:", hit_percentage_test_ada)


