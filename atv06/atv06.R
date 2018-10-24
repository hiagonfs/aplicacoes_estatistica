
populacao <- c(4, 5, 2, 9, 4, 7, 1, 2, 6, 4, 1, 4, 4, 6, 4, 5, 2, 3, 2,
               3, 7, 2, 2, 4, 6, 8, 2, 4, 5, 6, 8, 5, 2, 3, 8, 5, 2, 4,
               5, 9, 4, 1, 6, 3, 4, 2, 5, 6, 4, 3, 2, 3, 5, 4, 4, 3, 4,
               5, 4, 2, 9, 8, 18, 22, 8, 9, 7, 7, 9, 9, 8, 7, 9, 6, 14,
               9, 9, 8, 7, 12, 14, 8, 9, 8, 8, 15, 8, 9, 8, 8)

# a)

calculoProporcao <- function(dados, limitador, tamanho){
  proporcao <- 0
  
  for (i in dados) 
  {
    if (i > limitador) proporcao <- proporcao + 1
  }
  
  proporcao <- proporcao / tamanho
  
  return(proporcao)
}

media <- mean(populacao)
proporcao <- calculoProporcao(populacao, 5, 90)

# b)

h0 = 7
alpha = 0.05

amostras <- matrix(NA, nrow = 20, ncol = 10000)
pValues <- c()

for (n in 1:10000) 
{
  
  amostraSelecionada <- sample(populacao, size = 20, replace = TRUE)
  amostras[ , n] <- amostraSelecionada
  
  pValues[n] <- t.test(amostraSelecionada, mu = h0, alterative = "two.sided", conf.level = (1 - alpha))$p.value
  
}

pRejeitados <- 1 - calculoProporcao(pValues, alpha, 10000)

# c)

h0 = 7
alpha = 0.4

amostras <- matrix(NA, nrow = 60, ncol = 10000)
pValues <- c()

for (n in 1:10000) 
{
  
  amostraSelecionada <- sample(populacao, size = 60, replace = TRUE)
  amostras[ , n] <- amostraSelecionada
  
  pValues[n] <- t.test(amostraSelecionada, mu = h0, alterative = "two.sided", conf.level = (1 - alpha))$p.value
  
}

pRejeitados <- 1 - calculoProporcao(pValues, alpha, 10000)

# d) 

proporcoes <- c()

for (n in 1:10000) 
{
  proporcoes[n] = calculoProporcao(amostras[n, ], 5, 20)
}

n_sucessos <- length(Filter(function(x) x > 0.5, proporcoes))
binom.test(x = n_sucessos, n = 10000, p = 0.5, conf.level = 1 - alpha)
