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

soma_qui_quadrado_geral <- sum(resultados_qui_quadrado)

calculo_probabilidade2 <- pchisq(3, soma_qui_quadrado_geral, lower.tail = FALSE, log.p = FALSE)

# Questao 02
#letra B

valores_esperados <- c((11 * 12) / 36, (11 * 18) / 36, 
                     (11 * 6) / 36, (12 * 12) /36, (12 * 18) / 36, 
                     (12 * 6) / 36, (13 * 12) / 36, (13 * 18) / 36, 
                     (13 * 6) / 36)

valores_observados = c(4,5,2,3,7,2,5,6,2)

k = 9
somatorio = 0
for (i in 1:k) {
  somatorio <- somatorio + ((valores_observados[i] - valores_esperados[i])**2) / valores_esperados[i]
}

calculo_probabilidade2 <- pchisq(4, somatorio, lower.tail = FALSE, log.p = FALSE)

#D

vals_esperados <- c(16 * 12 / 36, 16 * 18 / 36, 16 * 6 / 36, 
                    20 * 12 / 36, 20 * 18/ 36, 20 * 6  / 36)

valores_observados = c(7,6,3,5,12,3)

somatorio = 0
for (i in 1:6) {
  somatorio = somatorio + ((valores_observados[i] - vals_esperados[i]) ** 2) / vals_esperados[i]
}

calculo_probabilidade3 <- pchisq(2, somatorio, lower.tail = FALSE, log.p = FALSE)


# letra e

salarios_minimos = c(4.0, 4.56, 5.25, 5.73, 
                     6.26, 6.66, 6.86, 7.39, 
                     7.59, 7.44, 8.12, 8.46, 
                     8.74, 8.95,9.13,9.35, 
                     9.77, 9.8, 10.53, 10.76,
                     11.06,11.59, 12.0,12.79,
                     13.23, 13.6, 13.85, 14.69, 
                     14.71, 15.99, 16.22, 16.61,
                     17.26, 18.75, 19.4, 23.3)

idades_meses = c(315,394,437,250,487,336,492,
                 520,428,282,402,335,449,530,
                 365,464,379,475,308,448,369,
                 410,492,313,389,420,559,356,
                 486,430,377,436,523,403,587,506)

plot(salarios_minimos, idades_meses)

a = cor(salarios_minimos, idades_meses)

b = a * sqrt( 34 / (1 - a ^ 2))
