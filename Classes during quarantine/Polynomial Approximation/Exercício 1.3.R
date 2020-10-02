library(corpcor)


#Funcoes:
geradora <- function(x){
  return(0.5*x^2+3*x+10)
}

poly_fun <- function(x, grau){
  x_vec <- c(1)
  for(p in 1:grau){
    x_vec <- c(x^p, x_vec)
  }
  return(x_vec)
}

hat_calc <- function(x, w){
  poly <- poly_fun(x, length(w) - 1)
  return(poly%*%w)
}



# Grau e número de amostras:
grau <- 3
N_amostras <- 10



# Gerando 10 amostras da função geradora somadas com um ruído gaussiano
x <- runif(N_amostras, min = -15, max = 10)
y = geradora(x) + rnorm(n = N_amostras, sd = 4)
plot(x, y, col = "red") # Plot amostras



# Gerando H:
H <- matrix(0, nrow = N_amostras, ncol = grau + 1)
for(i in 1 : N_amostras){
  H[i,] <- poly_fun(x[i],grau)
}



# Calculando W:
w <- pseudoinverse(H)%*%y # w = H+ y
w2 <- solve(t(H)%*%H)%*%t(H)%*%y



# Gerando o polinomio aproximado:
seq_x <- seq(-15, 10, 0.2)
y_hat <- c()
for(i in 1 : length(seq_x)){
  y_hat[i] <- hat_calc(seq_x[i], w)
}
lines(seq_x, y_hat, col = "blue") # Plot polinômio aproximado



# Recriando funcao geradora
funcao_geradora_x = seq(-15,10,length.out = 1000)
funcao_geradora_y = geradora(funcao_geradora_x)
lines(funcao_geradora_x, funcao_geradora_y, col = "green")  # Plot geradora



# Plot Legenda
legend(3, 15, c("Amostras", "Polinômio Obtido", "Geradora"), xpd = TRUE, col = c("red", "blue", "green"),
       text.col = "black", lty = c(0, 1, 1), pch = c(1, NA, NA),
       merge = TRUE, bg = "gray90")

