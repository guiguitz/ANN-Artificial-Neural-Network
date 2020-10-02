library(plot3D)

x = seq(-1, 1, by = 0.1)
y = seq(-1, 1, by = 0.1)
raio = 0.6
create_grid <- expand.grid(x,y)

#Reconstruindo o gráfico:
circle = function(x,y){
  return(sqrt(x^2+y^2))
}

classe = 1*(circle(create_grid$Var1,create_grid$Var2)>raio)
plot(create_grid, col = classe+1, xlab = "X", ylab = "Y")

#Mapa de calor:
grid_matrix <- matrix(0, nrow = length(x), ncol = length(y))
for(i in 1:length(x)){
  for(j in 1:length(y)){
    grid_matrix[i,j] <- x[i]^2 + y[j]^2
  }
}
image(grid_matrix, axes = F, col = heat.colors(n = 40, alpha=1))
par(new = T)
plot(create_grid,
     pch = classe + 2,
     col = 1,
     xlim = c(-1,1),
     ylim = c(-1,1),
     xlab = "X",
     ylab = "Y")

#Projeção não linear:
mudancaDeEspaco = function(x,y){
  return(x^2 + y^2)
}

Resp = mudancaDeEspaco(create_grid$Var1,create_grid$Var2)
plot(Resp, col = classe+1, xlab = "X", ylab = "Y")
abline(h = raio^2)


#Plot 3D
maux = matrix(Resp, length(x), length(y))
persp3D(x, y, maux)
