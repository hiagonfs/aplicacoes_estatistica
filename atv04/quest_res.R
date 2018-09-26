# Primeira Questao
# n= 10
normal_media1 <- matrix(NA, 10, 10000)
normal_var1 <- matrix(NA, 10, 10000)
median_dist1 <- matrix(NA, 10, 10000)

# n= 20
normal_media2 <- matrix(NA, 20, 10000)
normal_var2 <- matrix(NA, 20, 10000)
median_dist2 <- matrix(NA, 20, 10000)

# n= 30
normal_media3 <- matrix(NA, 30, 10000)
normal_var3 <- matrix(NA, 30, 10000)
median_dist3 <- matrix(NA, 30, 10000)

# n= 50
normal_media4 <- matrix(NA, 50, 10000)
normal_var4 <- matrix(NA, 50, 10000)
median_dist4 <- matrix(NA, 50, 10000)

# n= 100
normal_media5 <- matrix(NA, 100, 10000)
normal_var5 <- matrix(NA, 100, 10000)
median_dist5 <- matrix(NA, 100, 10000)

for (j in 1:10000) {
  
  #n = 10 (tamanho do amostra)
  normal_sample1 <- rnorm(10000, 500, 10)
  normal_media1[, j] <- mean(normal_sample1)
  normal_var1[, j] <- var(normal_sample1)
  median_dist1[, j] <- median(normal_sample1)
  
  #n = 20 (tamanho do amostra)
  normal_sample2 <- rnorm(10000, 500, 20)
  normal_media2[, j] <- mean(normal_sample2)
  normal_var2[, j] <- var(normal_sample2)
  median_dist2[, j] <- median(normal_sample2)
  
  #n = 30 (tamanho do amostra)
  normal_sample3 <- rnorm(10000, 500, 30)
  normal_media3[, j] <- mean(normal_sample3)
  normal_var3[, j] <- var(normal_sample3)
  median_dist3[, j] <- median(normal_sample3)
  
  #n = 50 (tamanho do amostra)
  normal_sample4 <- rnorm(10000, 500, 50)
  normal_media4[, j] <- mean(normal_sample4)
  normal_var4[, j] <- var(normal_sample4)
  median_dist4[, j] <- median(normal_sample4)
  
  #n = 100 (tamanho do amostra)
  normal_sample5 <- rnorm(10000, 500, 100)
  normal_media5[, j] <- mean(normal_sample5)
  normal_var5[, j] <- var(normal_sample5)
  median_dist5[, j] <- median(normal_sample5)
  
}

hist(normal_media1, 
     main="Histograma da Média Amostral da Distribuição Normal", 
     col="aquamarine3", 
     xlab= "Media Amostral", 
     ylab= "Frequencia Relativa")
hist(median_dist1,
     main="Histograma da Mediana da Distribuição Normal", 
     col="aquamarine3", 
     xlab= "Media Amostral", 
     ylab= "Frequencia Relativa")


hist(normal_media2, 
     main="Histograma da Média Amostral da Distribuição Normal",
     col="brown2", 
     xlab= "Media Amostral", 
     ylab= "Frequencia Relativa")
hist(median_dist2,
     main="Histograma da Mediana da Distribuição Normal", 
     col="brown2", 
     xlab= "Media Amostral", 
     ylab= "Frequencia Relativa")


hist(normal_media3, 
     main="Histograma da Média Amostral da Distribuição Normal", 
     col="red", 
     xlab= "Media Amostral", 
     ylab= "Frequencia Relativa")
hist(median_dist3, 
     main="Histograma da Mediana da Distribuição Normal", 
     col="red",
     xlab= "Media Amostral",
     ylab= "Frequencia Relativa")


hist(normal_media4,
     main="Histograma da Média Amostral da Distribuição Normal",
     col="lightcoral", 
     xlab= "Media Amostral",
     ylab= "Frequencia Relativa")
hist(median_dist4,
     main="Histograma da Mediana da Distribuição Normal", 
     col="lightcoral", 
     xlab= "Media Amostral", 
     ylab= "Frequencia Relativa")


hist(normal_media5, 
     main="Histograma da Média Amostral da Distribuição Normal",
     col="lightsteelblue3", 
     xlab= "Media Amostral",
     ylab= "Frequencia Relativa")
hist(median_dist5, 
     main="Histograma da Mediana da Distribuição Normal", 
     col="lightsteelblue3",
     xlab= "Media Amostral",
     ylab= "Frequencia Relativa")

tabela <- data.frame( medias = c(normal_media1, normal_media2, normal_media3, normal_media4, normal_media5), 
    variancias = c(normal_var1, normal_var2, normal_var3, normal_var4, normal_var5)
)

print(tabela)

#Segunda Questao

calculate_estimator<- function(n) {
  
  sample <- c()
  
  for(i in 1:10000) {
    amostra[i] <- runif(n, 0, 500)
  }
  
  estimator1 <- 2 * mean(sample)
  estimator2 <- ((n + 1))
  
}