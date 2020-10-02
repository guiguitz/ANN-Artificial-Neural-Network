rm(list=ls())
library('corpcor')

source("C:/Users/gvamorim/Google Drive/UFMG/Redes Neurais Artificiais/Algorithms/RBF.R")

xin<-matrix(seq(0,2*pi,0.01*pi),ncol=1)
yin<-matrix(sin(xin),ncol=1)

modRBF<-treinaRBF(xin,yin,10)
xteste<-matrix(seq(0,2*pi,0.001*pi),ncol=1)

yhat_teste<-YRBF(xteste,modRBF)

plot(xin,yin,type='b',xlim=c(0,2*pi),ylim=c(-1,1),xlab="x",ylab="f(x),yhat")
par(new=T)
plot(xteste,yhat_teste,type='l',col='red',xlim=c(0,2*pi),ylim=c(-1,1),xlab="",ylab="")

