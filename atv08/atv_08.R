# Atividade 08 -Hiago Natan Fernandes de Sousa
# Mat.: 118210402

# 1 Questao

read.csv("DadosCiaMB.csv", header=TRUE, sep=";", dec=",")
attach("DadosCiaMB.csv")

# Letra "a"

plot(idade~salario, ylab = "Idade (em anos)", xlab = "Salário")