rm(list=ls()) # Removing all obects from the current workspace
source("C:/Users/gvamorim/Google Drive/UFMG/Redes Neurais Artificiais/Algorithms/adalinetraining.R")

# Reading the sampling time, Input data, and output data 
t <- as.matrix(read.table("Ex1_t", header = TRUE))
X <- as.matrix(read.table("Ex1_x", header = TRUE))
Y <- as.matrix(read.table("Ex1_y", header = TRUE))

# Separating 70% of the data for training and 30% for test
index <- sample(1:20, size = 14, replace = FALSE)
Xtraining <- as.matrix(X[index,])
Ytraining <- as.matrix(Y[index,])
Xtest <- as.matrix(X[-index,])
Ytest <- as.matrix(Y[-index,])

# Running adaline algorithm
return<-trainadaline(Xtraining, Ytraining, 0.01, 0.0001, 3000, 1)
W <- return[[1]]
errorPerEpoch <- return[[2]]

# Calculating the mean square error of data separated to be tested
YhatTest <- cbind(1, Xtest) %*% W 
meanSquareError <- sum((YhatTest-Ytest)^2)/6

# Plotting data and results
Yhat <- cbind(1, X) %*% W 
plot(t, Y, type = "b", col = "red", xlab = "t", ylab = "Y", xlim = c(min(t), max(t)), ylim = c(min(Y, Yhat), max(Y, Yhat)))
par(new=T)
plot(t, Yhat, type = "b", col = "green", xlab = "", ylab = "", xlim = c(min(t), max(t)), ylim = c(min(Y, Yhat), max(Y, Yhat)))
legend(x = "topright", 
       legend = c("Saída aquisitada", "Saída aproximada", paste("Mean Square Error:", round(meanSquareError, 7))),
       col = c("red", "green"),
       text.col = "black",
       bg = "gray90",
       xpd = TRUE,
       lty = c(1,1,0))

plot(return[[2]], type = "l", xlab = "Epoch", ylab = "Square Error" )