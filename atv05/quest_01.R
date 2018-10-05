#Estatistica Aplicada - Atv 05

populacao <- c(4, 5, 2, 9, 4, 7, 1, 2, 6, 4, 
               1, 4, 4, 6, 4, 5, 2, 3, 2, 3, 
               7, 2, 2, 4, 6, 8, 2, 4, 5, 6, 
               8, 5, 2, 3, 8, 5, 2, 4, 5, 9,
               4, 1, 6, 3, 4, 2, 5, 6, 4, 3,
               2, 3, 5, 4, 4, 3, 4, 5, 4, 2, 
               9, 8, 18, 22, 8, 9, 7, 7, 9, 
               9, 8, 7, 9, 6, 14, 9, 9, 8, 7,
               12, 14, 8, 9, 8, 8, 15, 8, 9, 
               8, 8)

# Letra "a" 

cal_proportion <- function(data, limit, size){
  
  proportion <- 0
  
  for (i in data) {
    if (i > limit) proportion <- proportion + 1
  }
  
  proportion <- proportion / size
  
  return(proportion)
}

media_comodos <- mean(populacao)
proporcao <- cal_proportion(populacao, 5, 90)

# Letra "b"

amostras <- matrix(NA, nrow = 20, ncol = 10000)

ic_media <- matrix(NA, nrow = 2, ncol = 10000)
ic_proporcao <- matrix(NA, nrow = 2, ncol = 10000)

t_alpha <- qt(0.95, 19)

for (n in 1:10000) {
  amostra <- sample(populacao, size = 20, replace = TRUE)
  amostras[ , n] <- amostra
  
  tamanho <- 20
  media_amostral <- mean(amostra)
  desvio_padrao_amostral <- sd(amostra)
  proporcao_amostral <- cal_proportion(amostra, 5, 20)
  
  ic_media[1, n] <- (media_amostral - (t_alpha * (desvio_padrao_amostral / sqrt(tamanho))))
  ic_media[2, n] <- (media_amostral + (t_alpha * (desvio_padrao_amostral / sqrt(tamanho))))
  
  ic_proporcao[1, n] <- (proporcao_amostral - (t_alpha * sqrt((proporcao_amostral * (1 - proporcao_amostral)) / tamanho)))
  ic_proporcao[2, n] <- (proporcao_amostral + (t_alpha * sqrt((proporcao_amostral * (1 - proporcao_amostral)) / tamanho)))
}

# Letra "c"

#Proporcao de intervalos - Media
pi_media <- 0

#Proporcao de intervalos - Proporcao
pi_proporcao <- 0

for (n in 1:10000) {
  
  if (media_comodos >= ic_media[1, n] & media_comodos <= ic_media[2, n])  pi_media <- pi_media + 1
  
  if (proporcao >= ic_proporcao[1, n] & proporcao <= ic_proporcao[2, n]) pi_proporcao <- pi_proporcao + 1
  
}

pi_media <- pi_media / 10000
pi_proporcao <- pi_proporcao / 10000