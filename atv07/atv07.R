# Primeira questao

nivel = 0.05

array_freq_esperadas <- c(dbinom(0, 4, 2/5), dbinom(1, 4, 2/5), dbinom(2, 4, 2/5), dbinom(3, 4, 2/5), dbinom(4, 4, 2/5))

resultados_calculados <- c()

for (i in 1:5) {
  resultados_calculados[i] <- array_freq_esperadas[i] * 625
}

cal_qui_quadrado <- function(observado, esperado) {
  return (((observado - esperado) ^ 2) / esperado)
}

resultados_qui_quadrado <- c()

array_freq_obervadas <- c(72, 204, 228, 101, 20)

for( i in 1:5) {
  
  resultados_qui_quadrado[i] <-  cal_qui_quadrado(array_freq_obervadas[i],resultados_calculados[i])
  
}

soma_qui_quadrado_geral <- 0

for (i in 1:5) {
  soma_qui_quadrado_geral <- soma_qui_quadrado_geral + resultados_qui_quadrado[i]
}
