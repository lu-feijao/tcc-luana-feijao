# ---------- Script ---------- #

# Pacotes & Diretório ----
library(tidyverse)

setwd("~/FACUL/Semestre 8/TCC 1/dados")




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




# Descritiva -----
str(df)

summary(df[c(6,7,10,13)])


# Hora
sort(table(as.character(df$hora)))


# Dia
sort(table(as.character(df$dia)))


# Vencedor
aux <- df %>% group_by(vencedor) %>% count() %>% arrange(desc(n))
barplot(aux$n[1:5], names.arg = aux$vencedor[1:5])
sort(table(df$vencedor))


# Mandante Placar
ggplot(df, aes(placar.time1)) +
  geom_bar()
sort(table(df$placar.time1))


# Visitante Placar
ggplot(df, aes(placar.time2)) +
  geom_bar()
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


