#rm(list=ls()) # Removing all obects from the current workspace
library(readxl)
library(httr)

#----------------------------------------------------------------------

# Receiving updateted data from a website:

# Create the URL where the dataset is stored with automatic updates every day
url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

# Download the dataset from the website to a local temporary file
GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

# Read the Dataset sheet into R
data <- read_excel(tf)

# Converting data formtting
data$dateRep <- as.Date(data$dateRep)

# Sorting by date
data <- data[order(data$dateRep),]

# Converting from variables to factor
data$countriesAndTerritories = as.factor(data$countriesAndTerritories)

#----------------------------------------------------------------------

# General Numbers:

totalWorldCases<-sum(data$cases)
totalWorldDeaths<-sum(data$deaths)

#----------------------------------------------------------------------

# Brazil Numbers:

dataBrazil<-data[which(data$countriesAndTerritories == "Brazil"),]
numberDays = length(dataBrazil$cases)
newCasesBrazil = dataBrazil$cases
sumCasesBrazil = rep(0,numberDays)

# Calculating the cumulative number of cases
sumCasesBrazil[1] <- newCasesBrazil[1]
for(i in 1 : (numberDays-1)){
  sumCasesBrazil[i+1] <- sumCasesBrazil[i]+newCasesBrazil[i+1]
}
totCasesBrazil<-sum(dataBrazil$cases)

# Removing zero cases days:
nonZeroSumCasesBrazil <- sumCasesBrazil[sumCasesBrazil!=0]
nonZeroXDays <- seq(1,length(nonZeroSumCasesBrazil),1)

plot(nonZeroXDays, nonZeroSumCasesBrazil, type = 'line')

#----------------------------------------------------------------------

# Perceptron Algorithm

# Parameters:

# Max cases: choosen by user
max_cases <- 280000

# Matrix with days adjusted to values between 0 and 1 and the dimension number (1):
xin <- (as.matrix(nonZeroXDays)) / max(nonZeroXDays)

# Vector with cases number after the first case adjusted to values between 0 and 1:
yd <- (nonZeroSumCasesBrazil) / max_cases

# Learning rate:
eta <- 0.1

# Error tolerance:
tol <- 0.0001

# Max interation number
maxepocas <- 10000

# xin doesn't include the polarizarion term:
par <- 1

# Calling Perceptron algorithm
lista_treinada <- trainingpsigmoid(xin,yd,eta,tol,maxepocas,par)

#----------------------------------------------------------------------

# Sigmoid:
x_extrapol <- seq(0,3,0.01)
aux_extrapol <- cbind(-1,x_extrapol) %*% lista_treinada[[1]]
y_extrapol <- max_cases * exp(aux_extrapol)/(exp(aux_extrapol)+1)
x_real <- max(nonZeroXDays) * x_extrapol

#Calculating the final interaction error:
erro_ajuste <- tail(lista_treinada[[2]],1)
erro_ajuste <- round(x = erro_ajuste, digits = 7)

#----------------------------------------------------------------------

# Ploting:

# Open a bigger device window:
dev.new(width=10, height=10)

# Ploting the extrapolation:
plot(x_real, y_extrapol, type='l', col="blue", main="Extrapol coronavirus cases: max 280000 cases", xlim=c(0,max(x_real)), ylim = c(0,max_cases), xlab = "Days after the first case", ylab = "Cases number after the first case")

#Ploting the real cases:
par(new=T)
plot(nonZeroXDays,nonZeroSumCasesBrazil,xlim=c(0,max(x_real)),ylim=c(0,max_cases),xlab="",ylab="", col="red")

# Legend:
legend(x = 70, y = 13000,
       legend = c("Real cases", "Approximation", "Sigmoid behavior change day: ", paste("Error:", erro_ajuste, sep = " ", collapse = NULL)),
       col = c("red", "blue", "black", "black"),
       text.col = "black",
       bg = "gray90",
       xpd = TRUE,
       lty = c(0,1,NA,NA),
       pch = c(1, NA, NA, NA)
       )
