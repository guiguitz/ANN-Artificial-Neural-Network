rm(list=ls()) # Removing all obects from the current workspace
w<-1
limiar<-1
x<-seq(-2,2,0.1)
yhatlimiar<-1*((x * w) >= limiar)
yhattgh<-tanh(w*x)

plot(x, yhatlimiar, xlim = c(-2,2), ylim = c(-1,1), type = "l", main = "w = 1", col = "blue", xlab = "x", ylab = "yhat")
par(new=T)
plot(x, yhattgh, xlim = c(-2,2), ylim = c(-1,1), type = "l", col = "red", xlab = "", ylab = "")
legend(x = "bottomright", 
       legend = c("f(u) = limiar", "f(u) = tanh"),
       col = c("blue", "red"),
       text.col = "black",
       bg = "gray90",
       xpd = TRUE,
       lty = c(1,1)
)

w<--1
yhatlimiar2<-1*((x * w) >= limiar)
yhattgh2<-tanh(w*x)
plot(x, yhatlimiar2, xlim = c(-2,2), ylim = c(-1,1), type = "l", main = "w = -1", col = "blue", xlab = "x", ylab = "yhat")
par(new=T)
plot(x, yhattgh2, xlim = c(-2,2), ylim = c(-1,1), type = "l", col = "red", xlab = "", ylab = "")
legend(x = "topright", 
       legend = c("f(u) = limiar", "f(u) = tanh"),
       col = c("blue", "red"),
       text.col = "black",
       bg = "gray90",
       xpd = TRUE,
       lty = c(1,1)
)