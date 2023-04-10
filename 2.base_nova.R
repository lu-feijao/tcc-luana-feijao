# -------------------- Nova Base -------------------- #

# Pacotes & Diretório ----
library(tidyverse)


# Banco de dados
df <- read.csv('dados.csv')
df <- df[,-1]




# Base inicial -----

# times mandantes na coluna time1
df_time1 <- df[,c(1,12, 2,4,6,7)] %>%
  `colnames<-`(c("jogo", "ano", "time1", "time2", "placar.time1", "placar.time2"))

# times visitantes na coluna de time1
df_time2 <- df[,c(1,12, 4,2,7,6)] %>%
  `colnames<-`(c("jogo", "ano", "time1", "time2", "placar.time1", "placar.time2"))

df.new <- rbind(df_time1, df_time2)
df.new <- arrange(df.new, ano, jogo)




# Agregado por time (função) ----

agregar_resultados <- function(df, max_jogos) {
  
  # Coluna de índice que conta o número de jogos de cada time em cada ano
  df <- df %>%
    group_by(ano, time1) %>%
    mutate(jogos = row_number())
  
  # Filtra apenas as primeiras partidas (segundo max_jogos)
  df_filtrado <- df %>%
    filter(jogos <= max_jogos) %>%
    ungroup()
  
  # Agregando as informações por time e por ano e adicionando a coluna com resultado
  df_agregado <- df_filtrado %>%
    mutate(resultado = ifelse(placar.time1 > placar.time2, "vitoria",
                              ifelse(placar.time1 == placar.time2, "empate",
                                     "derrota"))) %>%
    group_by(ano, time = time1) %>%
    summarize(vitoria = sum(resultado == "vitoria"),
              empate = sum(resultado == "empate"),
              derrota = sum(resultado == "derrota"))
  
  return(df_agregado)
}


# Base nova ----
df.novo <- agregar_resultados(df.new, 10)
