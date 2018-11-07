# Primeira questao

array_binomias <- c(dbinom(0, 4, 2/5), dbinom(1, 4, 2/5), dbinom(2, 4, 2/5), dbinom(3, 4, 2/5), dbinom(4, 4, 2/5))

resultados_calculados <- c()

for (i in 1:5) {
  resultados_calculados[i] <- array_binomias[i] * 625
}

soma_dos_resultados <- 0

for (i in 1:5) {
  
  soma_dos_resultados <- soma_dos_resultados + array_binomias[i]  
  
}

cal_qui_quadrado <- function(observado, esperado) {
  
  resultado_calculado <- (exp((observado - esperado)) / esperado)
  
  return (resultado_calculado)
}

resultados_qui_quadrado <- c()

array_freq_obervadas <- c(72, 204, 228, 101, 20)

for( i in 1:5) {
  
  resultados_qui_quadrado[i] <-  cal_qui_quadrado(array_freq_obervadas[i],array_binomias[i])
  
}