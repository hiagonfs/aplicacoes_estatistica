# Hiago Natan Fernandes de Sousa
# Estatistica Aplicada

# Questão 1

dados <- scan("vendas.dat", what = list(telhados = 0, gastos = 0, clientes = 0, marcas = 0, potencial = 0))
attach(dados)

# a)
plot(telhados ~ gastos, ylab = "Telhados (em mil m2)", xlab = "Gastos (em mil US$)")
plot(telhados ~ clientes, ylab = "Telhados (em mil m2)", xlab = "Clientes (em milhares)")
plot(telhados ~ marcas, ylab = "Telhados (em mil m2)", xlab = "Marcas")
plot(telhados ~ potencial, ylab = "Telhados (em mil m2)", xlab = "Potencial")

# b)
correlacao_gastos = cor(telhados, gastos)
correlacao_clientes = cor(telhados, clientes)
correlacao_marcas = cor(telhados, marcas)
correlacao_potencial = cor(telhados, potencial)

# c)
modelo1 <- lm(telhados ~ marcas)
anova(modelo1)
summary(modelo1)

modelo2 <- lm(telhados ~ (marcas + clientes))
anova(modelo2)
summary(modelo2)

modelo3 <- lm(telhados ~ (marcas + clientes + potencial))
anova(modelo3)
summary(modelo3)

modelo4 <- lm(telhados ~ (marcas + clientes + potencial + gastos))
anova(modelo4)
summary(modelo4)

# d) 

# Conforme as tabelas paras os quatro modelos requeridos, nota-se que o quarto modelo (incluindo
# marcas, clientes, potencial e gastos) é aquele que melhor se ajusta aos dados. Essa conclusão se
# deve ao fato que este modelo é aquele com a menor quantidade de resíduos associados, bem como, o
# com menor SQT.

# 2)

# a) 
