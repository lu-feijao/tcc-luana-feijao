# ---------- Raspagem de Dados ---------- #

# Pacotes & Diretório ----
library(tidyverse)
library(rvest)

setwd("~/FACUL/Semestre 8/TCC 1/dados")


# Vetores nulos ----
arbitro <- c()
arbitro.uf <- c()
data <- c()
estadio <- c()
hora <- c()
jogo <- c()
placar.time1 <- c()
placar.time2 <- c()
time1 <- c()
time2 <- c()


# Raspagem ----
for(ano in 2017:2022){
  for(num_jogo in 1:380){
    
    link <- paste0("https://www.cbf.com.br/futebol-brasileiro/competicoes/campeonato-brasileiro-serie-a/", ano, "/", num_jogo, "?ref=botao")
    fonte <- read_html(link)
    
  
    # Estadio
    aux.estadio <- fonte %>% 
      html_node(".p-r-20:nth-child(1)") %>% 
      html_text()
    
    aux.estadio <- str_remove_all(aux.estadio, 
                              paste(c("\r\n                \r\n                " , "\r\n              "), 
                                    collapse = "|"))
    
    estadio <- append(estadio, aux.estadio)
    
    # Data
    aux.data <- fonte %>% 
      html_node(".p-r-20:nth-child(2)") %>% 
      html_text()
    
    aux.data <- sub(" ", "", aux.data)
    
    data <- append(data, aux.data)
    
    # Hora
    aux.hora <- fonte %>% 
      html_node(".p-r-20:nth-child(3)") %>% 
      html_text()
    
    aux.hora <- sub(" ", "", aux.hora)
    
    hora <- append(hora, aux.hora)
    
    # Time1
    aux.time1 <- fonte %>% 
      html_node(".time-left .color-white") %>% 
      html_text()
    
    time1 <- append(time1, aux.time1)
    
    # Time2
    aux.time2 <- fonte %>% 
      html_node(".time-right .color-white") %>% 
      html_text()
    
    time2 <- append(time2, aux.time2)
    
    # Placar Time1
    aux.placar.time1 <- fonte %>% 
      html_node(".text-left .time-gols") %>% 
      html_text()
    
    placar.time1 <- append(placar.time1, aux.placar.time1)
    
    # Placar Time2
    aux.placar.time2 <- fonte %>% 
      html_node(".text-right .time-gols") %>% 
      html_text()
    
    placar.time2 <- append(placar.time2, aux.placar.time2)
    
    # Jogo
    aux.jogo <- fonte %>% 
      html_node(".text-1") %>% 
      html_text()
    
    aux.jogo <- str_remove_all(aux.jogo, 
                           paste(c("\r\n                            Jogo: ", "\r\n            "), 
                                 collapse = "|"))
    
    jogo <- append(jogo, aux.jogo)
    
    # Arbitro
    aux.arbitro <- fonte %>% 
      html_node("tr:nth-child(1) th+ td") %>% 
      html_text()
    
    aux.arbitro <- str_remove_all(aux.arbitro, 
                              paste(c("\r\n                \r\n                  ", "\r\n                                  \r\n              ", "\r\n                                      [(]FIFA[])]"), 
                                    collapse = "|"))
  
    arbitro <- append(arbitro, aux.arbitro)
    
    # Arbitro UF
    aux.arbitro.uf <- fonte %>% 
      html_node("tr:nth-child(1) td+ td") %>% 
      html_text()
    
    arbitro.uf <- append(arbitro.uf, aux.arbitro.uf)
    
  }
}


df <- data.frame(jogo, time1, time2, placar.time1, placar.time2,
                 data, hora, estadio, arbitro, arbitro.uf)


# Criando mais colunas ----
df <- separate(df, time1, into = c("mandante","mandante.uf"), sep = " - " )
df <- separate(df, time2, into = c("visitante","visitante.uf"), sep = " - " )
df <- separate(df, estadio, into = c("estadio","cidade", "uf"), sep = " - " )
df <- separate(df, data, into = c("dia.semana","data"), sep = ", " )
df <- separate(df, data, into = c("dia", "mes", "ano"), sep = " de " )


# Organizando os dados ----
str(df)

df$jogo <- as.numeric(df$jogo)
df$placar.time1 <- as.numeric(df$placar.time1)
df$placar.time2 <- as.numeric(df$placar.time2)
df$dia <- as.numeric(df$dia)
df$ano <- as.numeric(df$ano)
#df$hora <- lubridate::hm(df$hora)


# Salvando dados em .csv ----
write.csv(df, "dados.csv")


