# Breast Cancer:
# The class performace with Perceptron algorithm will be study
# with breast cancer data from mlbench.

#----------------------------------------------------------------------

#rm(list=ls()) # Removing all obects from the current workspace
library(mlbench)
data("BreastCancer")

#----------------------------------------------------------------------

# Perceptron algorithm.
# xin: Matrix Nxn with input data (N = Number of samples, n = Input dimenssion)
# yd: Output (0 or 1).
# eta: Learning rate.
# tol: Error tolerance.
# maxepochs: Max interation number
# par: xin include or not the polarizarion term.

# Creating xin for learning (Removing the ID column):
xin<-as.matrix(BreastCancer[1:699, 2:10])
xin<-mapply(xin, FUN = as.numeric)
xin<-matrix(xin, nrow = 699, ncol = 9)
xin<-xin[,-(6),drop=FALSE]                 # Removing 6th column
xin<-xin[complete.cases(xin),]             # Removing lines with NA data

# Creating yd ("malignant" -> 1 and "benignant" -> 0):
yd<-1*(BreastCancer$Class == "malignant")

# Parameters:
eta<-0.1
tol<-0.000001
maxepochs<-3000
par<-1
returnlist<-trainingpsimple(xin,yd,eta,tol,maxepochs,par)

# Return Parameters:
w<-returnlist[[1]]
error<-tail(returnlist[[2]],1)

#----------------------------------------------------------------------

# Checking output values:

test_output<-1.0*((cbind(-1,xin) %*% w) >= 0)
real_output<-1*(BreastCancer$Class == "malignant")
missclassifications<-1*(real_output != test_output)
malignant_missclassifications<-sum(missclassifications*test_output)
benign_missclassifications<-sum(missclassifications*real_output)
number_missclassifications<-malignant_missclassifications+benign_missclassifications

#----------------------------------------------------------------------

# Printing results:

# Open a bigger device window:
dev.new(width=10, height=10)
barplot(c(malignant_missclassifications,benign_missclassifications),
main = "Missclassifications Breast Cancer", 
xlab = "Classes",
ylab = "Number of miss classifications",
col = c("darkred","red"),
horiz = FALSE,
names.arg = c("Malignant missclassifications", "Benign missclassifications")
)

malignant_number_of_cases<-sum(1*(BreastCancer$Class == "malignant"))
benign_number_of_cases<-sum(1*(BreastCancer$Class == "benign"))

legend("topright",
c(paste("Malignant missclassifications:",malignant_missclassifications),paste("Benign missclassifications:", benign_missclassifications), paste("Total missclassifications:", number_missclassifications), paste("Error:", round(error,7)), paste("Benign real cases:", benign_number_of_cases), paste("Malignant real cases:", malignant_number_of_cases)),
fill = c("darkred","red",NA,NA,NA,NA))

print(w)

