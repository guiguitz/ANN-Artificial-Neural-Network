
# Checking wich variable better separe benign and malignant
# breast cancer with score Fisher.

#----------------------------------------------------------------------

rm(list=ls()) # Removing all obects from the current workspace
library(mlbench)
data("BreastCancer")

#----------------------------------------------------------------------

# Dealing with data:
# The class malignant is class 1
# The class benign is class 2

# Number of malignant cases, benign cases and samples:
malignant_number_of_cases<-sum(1*(BreastCancer$Class == "malignant"))
benign_number_of_cases<-sum(1*(BreastCancer$Class == "benign"))
sample_number<-malignant_number_of_cases+benign_number_of_cases

# Creating a matrix olnly with malignant data (without ID and 6th column):
data_malignant<-BreastCancer[which(BreastCancer$Class == "malignant"),]
data_malignant<-as.matrix(data_malignant[1:699, 2:10])
data_malignant<-mapply(data_malignant, FUN = as.numeric)
data_malignant<-matrix(data_malignant, nrow = 699, ncol = 9)
data_malignant<-data_malignant[,-(6),drop=FALSE]              
data_malignant<-data_malignant[complete.cases(data_malignant),]

# Creating a matrix olnly with benign data (without ID and 6th column):
data_benign<-BreastCancer[which(BreastCancer$Class == "benign"),]
data_benign<-as.matrix(data_benign[1:699, 2:10])
data_benign<-mapply(data_benign, FUN = as.numeric)
data_benign<-matrix(data_benign, nrow = 699, ncol = 9)
data_benign<-data_benign[,-(6),drop=FALSE]              
data_benign<-data_benign[complete.cases(data_benign),]

#----------------------------------------------------------------------

# Calculating score Fisher:
# m1: avarage data of class 1
# m2: avarage data of class 2
# m:  global avarage data
# s1: standard deviation of class 1
# s2: standard deviation of class 2
# sf = (abs(m1-m)+abs(m2-m))/(s1^2+s2^2)

standardDeviation <- function(vec,avarage,number_of_data){
  aux<-0
  for (i in 1:number_of_data) {
    aux<-aux+(vec[i]-avarage)^2
  }
  return(sqrt(aux/number_of_data))
}

scoreFisher <- function(m1,m2,m,s1,s2){
  return((abs(m1-m)+abs(m2-m))/(s1^2+s2^2))
}

# Score Fisher vector:
score_fisher<-matrix(nrow = 1, ncol = 8)
m1<-0
m2<-0
m<-0
s1<-0
s2<-0

# Calculating the score fisher per variable:
for (j in 1:8) {
  m1<-sum(data_malignant[,j])/malignant_number_of_cases
  m2<-sum(data_benign[,j])/benign_number_of_cases
  m<-(sum(data_malignant[,j])+sum(data_benign[,j]))/sample_number
  s1<-standardDeviation(data_malignant[,j],m1,malignant_number_of_cases)
  s2<-standardDeviation(data_benign[,j],m2,benign_number_of_cases)
  score_fisher[1,j]<-scoreFisher(m1,m2,m,s1,s2)
}


#----------------------------------------------------------------------

# Printing results:

# Open a bigger device window:
dev.new(width=10, height=10)
barplot(score_fisher[1,], 
main = "Score Fisher per class",
xlab = "Class",
ylab = "Score Fisher",
names.arg = c("2", "3", "4", "5", "7", "8", "9", "10"),
col = "darkred", 
horiz = FALSE)

cat("\nScore Fisher per class:\n", score_fisher[1,])
