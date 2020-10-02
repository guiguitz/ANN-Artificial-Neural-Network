# Exercise 2:

# Generating Function:
t<-seq(0,2*pi,0.01*pi)
x1<-matrix(sin(t)+cos(t),ncol = 1)
x2<-matrix(tanh(t),ncol = 1)
x3<-matrix(sin(4*t),ncol = 1)
x4<-matrix(abs(sin(t)),ncol = 1)
x<-cbind(x1,x2,x3,x4)
y<-x1+2*x2+0.8*x3+3.2*x4+pi/2

#-- Adaline ----------------------------

# Normalized xin:
xin<-cbind(x1/max(x1),x2/max(x2),x3/max(x3),x4/max(x4))

# Normalized yd
yd<-y/max(y)

# Learning rate:
eta<-0.01

# Error tolerance:
tol<-0.01

# Max interation number
maxepochs<-10000

# xin doesn't include the polarizarion term:
par<-1

# Calling Adaline algorithm
retlist <- treinap(xin,yd,eta,tol,maxepocas,par)

# Coef vector:
w<-retlist[[1]]

#Eror:
error <- round(x = tail(retlist[[2]],1), digits = 7)

#-- Test ----------------------------

# ttest and xtest are acquired values:
ttest<-seq(0,2*pi,0.01*pi)
x1test<-matrix(sin(ttest)+cos(ttest),ncol = 1)
x2test<-matrix(tanh(ttest),ncol = 1)
x3test<-matrix(sin(4*ttest),ncol = 1)
x4test<-matrix(abs(sin(ttest)),ncol = 1)
xtest<-cbind(1,x1test,x2test,x3test,x4test)

# Calculating y from estimeded w:
yestimated<-xtest %*% w

# Calculating the real y values:
yreal<-x1test+2*x2test+0.8*x3test+3.2*x4test+pi/2


#-- Ploting ----------------------------

# Plotting Real points:
plot(t,y,xlim=c(0,6),ylim=c(0,8),col='red',xlab='t',ylab='Saida')

# Plotting estimated values:
par(new=T)
plot(ttest,yestimated,xlim=c(0,6),ylim=c(0,8),col='blue',type='l',xlab='t',ylab='Saida')

# Plotting real curve:
par(new=T)
plot(ttest,yreal,xlim=c(0,6),ylim=c(0,8),col='black',type='l',xlab='t',ylab='Saida')
