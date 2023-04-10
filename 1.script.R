# -------------------- Script -------------------- #

# Pacotes & Diretório ----
library(tidyverse)

#setwd("~/FACUL/Semestre 8/TCC 1/dados")




# Banco de dados
df <- read.csv('dados.csv')
df <- df[,-1]




# Organizando o banco de dados -----
df$hora <- lubridate::hm(df$hora)

df <- df %>% 
  mutate(vencedor = (if_else(placar.time1 > placar.time2, mandante.uf, 
                     if_else(placar.time1 == placar.time2, "-", visitante.uf))), 
         .after = 7)




# Arrumando times ----

# MANDANTE - - - - - - -
sort(unique(df$mandante))

# América
df$mandante[df$mandante=='America'] <- 'América'
df$mandante[df$mandante=='America Fc'] <- 'América'
df$mandante[df$mandante=='América Fc Saf'] <- 'América'

# Atlético
df$mandante[df$mandante=='Atlético Paranaense'] <- 'Athletico Paranaense'
df$mandante[df$mandante=='Atletico'] <- 'Athletico Paranaense'
df$mandante[df$mandante=='Atlético' & df$mandante.uf == "PR"]  <- 'Athletico Paranaense'
df$mandante[df$mandante=='Atlético' & df$mandante.uf == "GO"] <- 'Atlético Goianiense'
df$mandante[df$mandante=='Atlético' & df$mandante.uf == "MG"] <- 'Atlético Mineiro'

# Cuiabá
df$mandante[df$mandante=='Cuiabá Saf'] <- 'Cuiabá'



# VISITANTE - - - - - - -
sort(unique(df$visitante))

# América
df$visitante[df$visitante=='America'] <- 'América'
df$visitante[df$visitante=='America Fc'] <- 'América'
df$visitante[df$visitante=='América Fc Saf'] <- 'América'

# Atlético
df$visitante[df$visitante=='Atlético Paranaense'] <- 'Athletico Paranaense'
df$visitante[df$visitante=='Atletico'] <- 'Athletico Paranaense'
df$visitante[df$visitante=='Atlético' & df$visitante.uf == "PR"]  <- 'Athletico Paranaense'
df$visitante[df$visitante=='Atlético' & df$visitante.uf == "GO"] <- 'Atlético Goianiense'
df$visitante[df$visitante=='Atlético' & df$visitante.uf == "MG"] <- 'Atlético Mineiro'

# Cuiabá
df$visitante[df$visitante=='Cuiabá Saf'] <- 'Cuiabá'



write.csv(df, "dados.csv")





# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#
# -----------------------------------------------------------------------------#




# Descritiva -----
str(df)

summary(df[c(6,7,10,13)])


# Hora
sort(table(as.character(df$hora)))


# Dia
sort(table(as.character(df$dia)))


# Dia da Semana
sort(table(as.character(df$dia.semana)))


# Vencedor
sort(table(df$vencedor))


# Mandante Placar
sort(table(df$placar.time1))


# Visitante Placar
sort(table(df$placar.time2))


# Arena
sort(table(df$estadio))


# Estado Jogando
sort(table(df$mandante.uf))


# Cidade em que ocorreu o jogo
sort(table(df$cidade))


# Árbitro
sort(table(df$arbitro))


# UF do Árbitro
sort(table(df$arbitro.uf))





# Gráficos -----

bar.fun <- function(...){
  aux <- df %>% group_by(...) %>% summarise(n = n()) %>% arrange(desc(n))
  barplot(aux$n[1:5], names.arg = aux[1:5, 1] %>% unlist)
}

ggplot.fun <- function(...){
  ggplot(df, aes(...)) +
    geom_histogram(bins=6,fill="#D3D3D3", col=I("black")) + 
    theme_classic() +
    xlab("") + ylab("")
}



# Variáveis quantitativas

png(filename="var_quant_top3.png", height=20, width=25, unit="cm", res=300)
par(mfrow=c(4,1), mar=c(3,3,2,2), oma=c(3,3,2,2))

# Placar Mandante
p1 <- ggplot.fun(placar.time1) + ggtitle("Placar Mandante") +
  theme(plot.title = element_text(hjust = 0.5)) 
# Placar Visitante
p2 <- ggplot.fun(placar.time2)  + ggtitle("Placar Visitante") +
  theme(plot.title = element_text(hjust = 0.5))
# Dia
p3 <- ggplot.fun(dia) + ggtitle("Dia") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2, p3, ncol=1)
dev.off()




png(filename="hora.png", height=10, width=25, unit="cm", res=300)
par(mfrow=c(1,1))

# Hora
aux <- df %>% group_by(hora) %>% count() #%>% arrange(desc(n))
barplot(aux$n, names.arg = aux$hora, main = "Hora")

dev.off()



# Variáveis qualitativas (top 3)

png(filename="var_qual_top5.png", height=25, width=25, unit="cm", res=300)
par(mfrow=c(5,1), mar=c(3,3,2,2), oma=c(3,3,2,2))
# Times
bar.fun(mandante); title(main = "Times")
# UF Times
bar.fun(mandante.uf); title(main = "Times (UF)")
# Time Vencedor
bar.fun(vencedor); title(main = "Times Vencedores")
# Dia da Semana
bar.fun(dia.semana); title(main = "Dia da Semana")
# Arena
bar.fun(estadio); title(main = "Estádio")
# Árbitro
#bar.fun(arbitro); title(main = "Árbitro")
# UF Árbitro
#bar.fun(arbitro.uf); title(main = "Árbitro (UF)")

dev.off()
