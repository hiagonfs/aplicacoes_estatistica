# Atividade 08 -Hiago Natan Fernandes de Sousa
# Mat.: 118210402

# 1 Questao

read.csv("DadosCiaMB.csv", header=TRUE, sep=";", dec=",")
attach(DadosCiaMB)

# Letra "a"

plot(idade~salario, xlab = "Idade (em anos)", ylab = "Salário")

#Letra "b"

# Existe uma certa correlação positiva entre as duas variáveis, no entando
# observa-se que não aparenta ser forte, porque os pontos estão dispersos. 

#Letra "c"

correlacao_idade_e_salario <- cor(idade, salario)

#Letra "d" 

coeficientes_ml <- lm(idade~salario)

# Letra "e" 

regressao_variaveis <- coeficientes_ml
abline(regressao_variaveis)

# 2 Questao

# Letra "a" 

plot(idade, salario, ylab = "Idade", xlab = "Salário")
abline(lm(salario ~ idade))
cor(idade, salario)

# Letra "b"

plot((idade^2), salario, ylab = "Idade²", xlab = "Salário")
abline(lm(salario ~ (idade^2)))
cor((idade^2), salario)

# Letra "c"

plot(log(idade), salario, ylab = "Log(Idade)", xlab = "Salário")
abline(lm(salario ~ log(idade)))
cor(log(idade), salario)

# Letra "d"

plot(sqrt(idade), salario, ylab = "Sqrt(Idade)", xlab = "Salário")
abline(lm(salario ~ sqrt(idade)))
cor(sqrt(idade), salario)

# Letra "e"

plot((1/idade), salario, ylab = "1 / Idade", xlab = "Salário")
abline(lm(salario ~ (1/idade)))
cor((1/idade), salario)

