# -------------------- Regressão Logística -------------------- #

# Pacotes & Diretório ----
library(tidyverse)
library(car)


# Banco de dados

# n = 10
df10 <- read.csv('df10.csv')
df10 <- df10[,-1]
# n = 12
df12 <- read.csv('df12.csv')
df12 <- df12[,-1]
# n = 15
df15 <- read.csv('df15.csv')
df15 <- df15[,-1]
# n = 17
df17 <- read.csv('df17.csv')
df17 <- df17[,-1]
# n = 20
df20 <- read.csv('df20.csv')
df20 <- df20[,-1]





# Modelo ----
train <- df10


model <- glm(campeao1 ~ vitoria+empate+derrota+GP+GC+SG,
             family = binomial(link='logit'), data=train)
summary(model)
# derrota e SG retornam valor NA


model <- glm(campeao1 ~ vitoria+empate+GP+GC,
             family = binomial(link='logit'), data=train)
summary(model)


anova(model, test = 'Chisq')

# Ausencia de regressao 
library(aod)
for (i in 2:4){
  a <- wald.test(Sigma = vcov(model), b = coef(model), Terms = i)
  print(a)
}

# Coeficientes
coef(model); confint(model)

# Odds ratios
odds = exp(coef(model)); odds

s = summary(model)$coefficients[,2]
li = exp(coef(model) - 1.96*s); ls = exp(coef(model) + 1.96*s)
round(data.frame(li,ls),3)


data.frame(round(coef(model),3),
           odds, li, ls,
           round(summary(model)$coefficients[,4],4)) %>% 
  magrittr::set_colnames(c("Estimativas", "Razões de Chances", "Limite Inferior",
                           "Limite Superior", "P-valor"))



# Ajuste do modelo

# D/(n-k) < 1 : então está adequado
model$deviance / (224 - 4)



# Diagnóstico do Modelo
marginalModelPlots(model)



# Outlier
# H0: não existe ponto influente
outlierTest(model)



# Pontos influentes
influenceIndexPlot(model)



# Multicolinearidade
# < 5 : ok
vif(model)



# Gráfico dos efeitos
plot(effects::allEffects(model))




# Análise de Resíduos
# Estatística Prática para Cientistas de Dados (p. 197)
terms <- predict(model, type = 'terms')
partial_resid <- resid(model) + terms
df_r <- data.frame(vitoria = train[, 'vitoria'],
                   terms = terms[,"vitoria"],
                   partial_resid = partial_resid[, "vitoria"])

p1 <- ggplot(df_r, aes(x=vitoria, y=partial_resid, solid=F)) +
  geom_point(alpha=.4) +
  geom_line(aes(x=vitoria, y=terms), color="red", alpha=.5, size=1.5) +
  labs(y="Partial Residual")


terms <- predict(model, type = 'terms')
partial_resid <- resid(model) + terms
df_r <- data.frame(empate = train[,'empate'],
                   terms = terms[,"empate"],
                   partial_resid = partial_resid[, "empate"])

p2 <- ggplot(df_r, aes(x=empate, y=partial_resid, solid=F)) +
  geom_point(alpha=.4) +
  geom_line(aes(x=empate, y=terms), color="red", alpha=.5, size=1.5) +
  labs(y="Partial Residual")


terms <- predict(model, type = 'terms')
partial_resid <- resid(model) + terms
df_r <- data.frame(GP = train[,'GP'],
                   terms = terms[,"GP"],
                   partial_resid = partial_resid[, "GP"])

p3 <- ggplot(df_r, aes(x=GP, y=partial_resid, solid=F)) +
  geom_point(alpha=.4) +
  geom_line(aes(x=GP, y=terms), color="red", alpha=.5, size=1.5) +
  labs(y="Partial Residual")


terms <- predict(model, type = 'terms')
partial_resid <- resid(model) + terms
df_r <- data.frame(GC = train[,'GC'],
                   terms = terms[,"GC"],
                   partial_resid = partial_resid[, "GC"])

p4 <- ggplot(df_r, aes(x=GC, y=partial_resid, solid=F)) +
  geom_point(alpha=.4) +
  geom_line(aes(x=GC, y=terms), color="red", alpha=.5, size=1.5) +
  labs(y="Partial Residual")

gridExtra::grid.arrange(p1, p2, p3, p4, ncol=2)




# Previsão
new = data.frame(vitoria = train$vitoria,
                 empate = train$empate,
                 GP = train$GP,
                 GC = train$GC)
aux = data.frame(pred = ifelse(predict(model, newdata = new, type='response')>0.5,1,0),
                 real = train$campeao1)

table(aux); round(prop.table(table(aux), 1), 3)








