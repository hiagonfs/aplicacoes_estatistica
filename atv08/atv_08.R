# Atividade 08 -Hiago Natan Fernandes de Sousa
# Mat.: 118210402

# 1 Questao

read.csv("DadosCiaMB.csv", header=TRUE, sep=";", dec=",")
attach(DadosCiaMB)

# Letra "a"

plot(idade~salario, xlab = "Idade (em anos)", ylab = "Salário")