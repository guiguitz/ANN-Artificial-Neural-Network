rm(list=ls())
library('corpcor')

fnormal1var<-function(x,m,r) ((1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/(2*r))^2))

xl<--10
xr<-10
st<-1
p<-10
faixa<-(xr-xl)/p
c0<-xl+0.5*faixa
ci<-matrix()

x<-seq(xl,xr,st)+0.01
y<-data.matrix(sin(x)/x)

ci[1]<-c0
for (i in 2:p)
  ci[i]<-ci[i-1]+faixa

## PRojeção H
H<-matrix(nrow=length(x),ncol=p)
for(j in 1:length(x))
  for(i in 1:p)
    H[,i]<-fnormal1var(x,ci[i],0.6)

xrange<-seq(xl,xr,0.05)

## Projeção H range
Hrange<-matrix(nrow=length(xrange),ncol=p)
for(j in 1:length(xrange))
  for(i in 1:p)
    Hrange[,i]<-fnormal1var(xrange,ci[i],0.6)

ph<-pseudoinverse(H)
w<-ph %*% y

yhat<-H %*% w
yhatr<-Hrange %*% w

plot(x,y,xlim=c(xl,xr),ylim=c(-1,1),xlab="x",ylab="f(x),h1(x),h2(x)")
par(new=T)
plot(xrange,yhatr,type='l',col='red',xlim=c(xl,xr),ylim=c(-1,1),xlab="x",ylab="f(x),h1(x),h2(x)")
par(new=T)


## Plot normais
for(i in 1:p)
{
  plot(xrange,Hrange[,i],col='blue',type='l',xlim=c(xl,xr),ylim=c(-1,1),xlab='',ylab='')
  par(new=T)
}

