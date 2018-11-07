# Hiago Natan Fernandes de Sousa
# Atv -07
# Primeira questao

array_binomias <- c(dbinom(0, 4, 2/5), dbinom(1, 4, 2/5), dbinom(2, 4, 2/5), dbinom(3, 4, 2/5), dbinom(4, 4, 2/5))

resultados_calculados <- c()

for (i in 1:5) {
  resultados_calculados[i] <- array_binomias[i] * 625
}

cal_qui_quadrado <- function(observado, esperado) {
  
  resultado_calculado <- exp((observado - esperado)) / esperado
  
  return resultado_calculado
}

soma_dos_resultados <- 0

for (dado in 0:5) {
  
  soma_dos_resultados <-  
    
}





  

