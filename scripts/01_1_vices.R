library(tidyverse)
library(lubridate)
library(youtubecaption)
library(reticulate)
library(googlesheets4)

options(scipen = 999)

# 1 - levanto lista de links---------------------------------------------------------------------
df_links<-read_sheet(ss = "https://docs.google.com/spreadsheets/d/1E_UkEt11QqgwfLZLcS0xfyt_rb8brnSuh2ghcyowzpk/edit#gid=0",
                     sheet =  "candidato_topico") |> 
  janitor::clean_names()

# 2 Descargo los subtitulos
link <- "https://www.youtube.com/watch?v=NLb9WkfrvUY" 

tictoc::tic()

# Me traigo los subtitulos del debate
df_subtitulos<- get_caption(url = link, language = "es")

tictoc::tic()

# 3 - filtro los intervalos que me interesan ----------------------------------------------
df_links2 <- df_links %>%
  filter(!is.na(desde))%>%
  filter(!is.na(hasta))%>%
  # Convierto "Desde" "Hasta" en segundos para poder hacer los intervalos
  mutate(desde = as.numeric(seconds(hms(desde))), 
         hasta = as.numeric(seconds(hms(hasta))))


# Convierte "start" a numérico
df_subtitulos$start <- as.numeric(df_subtitulos$start)
## creo df vacío ####
df_final <- data.frame()
id_videos <- unique(df_links$link)

# Creo un loop para crear el df final
for (i in 1:nrow(df_links2)) {
  desde <- df_links2$desde[i]
  hasta <- df_links2$hasta[i]
  
  # Filtra los subtítulos que caen dentro del intervalo de tiempo actual
  df_filtrado <- df_subtitulos %>%
    filter(start >= desde, start <= hasta)
  
  # Agrega las columnas de intervalo y candidato
  df_filtrado$desde <- desde
  df_filtrado$hasta <- hasta
  df_filtrado$candidato <- df_links2$candidato[i]
  df_filtrado$topico <- df_links2$topico[i]
  df_filtrado$link <- df_links2$link[i]
  
  # Combina el DataFrame filtrado con el DataFrame final
  df_final <- bind_rows(df_final, df_filtrado)
}

# ordeno
df_debates_vices <- df_final |> 
  select(-vid) |> 
  select(segment_id,topico, candidato, text, everything())

# Exporto
write.csv(df_debates_vices, "./data/01_debate_vicepresidentes.csv", fileEncoding = "UTF-8", row.names = F)



# df_debates_vices <- read_csv("data/01_debate_vicepresidentes.csv")

print(head(df_debates_vices))

debate_vicepresidentes_colapsado <- df_debates_vices %>%
  arrange(topico) %>% 
  group_by(topico, candidato, desde, hasta) %>%
  summarize(texto_colapsado = paste(text, collapse = " "),
            fecha_debate = "2023-09-20",
            link = first(link)) %>%
  mutate(desde = format(seconds_to_period(desde), format = "%H:%M:%S"),
         hasta = format(seconds_to_period(hasta), format = "%H:%M:%S"))%>%
  ungroup() 



# Exporto
write.csv(debate_vicepresidentes_colapsado, "./data/01_debate_vicepresidentes.csv", fileEncoding = "UTF-8", row.names = F)
