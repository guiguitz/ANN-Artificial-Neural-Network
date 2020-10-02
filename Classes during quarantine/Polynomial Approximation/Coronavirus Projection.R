library(corpcor)


#Função Geradora obtida no site: https://www.worldometers.info/coronavirus/country/brazil/
n_amostras <- 32

#Casos no Brasil
geradora_eixo_x = seq(1, n_amostras, 1)
geradora_eixo_y = c(1,1,2,2,2,2,3,8,13,19,25,25,34,52,77,151,151,200,234,346,529,640,970,1178,1546,1924,2247,2554,2985,3417,3904,4256)

#Comparacao com a Itália
italia_eixo_x = seq(1, n_amostras, 1)
italia_eixo_y = c(3,3,3,3,3,4,21,79,157,229,323,470,655,889,1128,1701,2036,2502,3089,3858,4636,5883,7375,9172,10149,12462,15113,17660,21157,24747,27980,31506)

#Comparacao com a India
india_eixo_x = seq(1, n_amostras, 1)
india_eixo_y = c(3,3,3,3,6,7,29,30,31,34,40,47,62,62,74,82,100,114,129,143,169,194,249,332,396,499,536,657,727,887,987,1024)

#Comparacao com a Espanha
espanha_eixo_x = seq(1, n_amostras, 1)
espanha_eixo_y = c(2,2,2,2,2,2,2,2,2,3,9,13,25,33,58,84,120,165,228,282,401,525,674,1231,1695,2277,3146,5232,6391,7988,9942,11826)

#Comparacao com Israel
israel_eixo_x = seq(1, n_amostras, 1)
israel_eixo_y = c(3,7,7,10,12,12,15,17,21,25,39,50,75,97,109,143,193,213,298,337,433,677,705,883,1071,1442,1930,2369,2693,3035,3619,4247)

#------------------------------------------------------------------------------------------------------------------------------

#Funções:

createH = function(eixo_x, n_lines, n_col){
  matriz = matrix(nrow = n_lines+1, ncol = n_col)
  a <- 1
  for(t in 1 : n_col){
    for(k in 0 : n_lines){
      matriz[a] <- (eixo_x[t])^(n_lines-k)
      a<- a+1
    }
  }
  matriz = t(matriz)
  return(matriz)
}

calculateY = function(eixo_x, n_pontos, coefficients_vec, grau){
  saida_y <- rep(0, n_pontos)
  for(i in 1 : n_pontos){
    saida_y[i]<- 0
    for(j in 0 : grau){
      saida_y[i] <- saida_y[i] + coefficients_vec[j+1]*(eixo_x[i]^(grau-j))
    }
  }
  return(saida_y)
}

#------------------------------------------------------------------------------------------------------------------------------

#Aproximação:

#Grau do polinômio e número de amostras
grau <- 3
n_pontos_saida <- 40

#Gerando H:
matriz_h = createH(geradora_eixo_x, grau, n_amostras)

#Gerando W
vector_w = pseudoinverse(matriz_h) %*% geradora_eixo_y

#Construindo polinômio aproximado
aprox_eixo_x = seq(1, n_pontos_saida, 1)
aprox_eixo_y = calculateY(aprox_eixo_x, n_pontos_saida, vector_w, grau)

#------------------------------------------------------------------------------------------------------------------------------

#Plotando Geradora, função aproximada e dados de outros países

plot(geradora_eixo_x, geradora_eixo_y, col="black", xlab="Tempo(dias)", ylab="Número de casos", xlim = c(0,n_pontos_saida+5), ylim = c(0,aprox_eixo_y[n_pontos_saida]+100), main="Número de casos x Tempo(dias)") # Plot amostras
#lines(geradora_eixo_x, geradora_eixo_y, col = "green")  # Plot geradora
lines(aprox_eixo_x, aprox_eixo_y, col = "red")  # Plot aproximação
lines(italia_eixo_x, italia_eixo_y, col = "blue")  # Plot casos italia
lines(espanha_eixo_x, espanha_eixo_y, col = "yellow")  # Plot casos Espanha
lines(india_eixo_x, india_eixo_y, col = "green")  # Plot casos India
lines(israel_eixo_x, israel_eixo_y, col = "purple")  # Plot casos Israel

