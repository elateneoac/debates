library(readr)
library(tidyverse)
library(tidytext)
library(stringi)

debate_vicepresidentes <- read_csv("data/01_debate_vicepresidentes.csv")

# Me qyuedo solo con los finalistas

df_debate <- debate_vicepresidentes %>% 
  filter(candidato %in% c("Rossi", "Villarruel"))

rm(debate_vicepresidentes)

# Limpio el texto
df_debate<-df_debate%>%
  mutate(text= stri_trans_general(text, "Latin-ASCII"), #una forma rápida de quitar acentos y ñ
         text= str_replace_all(text,"www\\S*", ""), #saco urls con REGEX
         text= str_replace_all(text,"https\\S*", ""), #saco urls con REGEX
         text= str_replace_all(text, "[[:punct:]]", " "), #reemplazo puntuaciones con REGEX
         text= str_replace_all(text, "[[:digit:]]+", " "),#reemplazo numeros con REGEX
         text = str_squish(text),
         text= str_to_lower(text))

filtro_palabras<-stri_trans_general(stopwords("es"), "Latin-ASCII")

# divido por palabra
palabras <- df_debate%>%
  unnest_tokens(input = text, output = word) %>% 
  count(topico,candidato,word) %>% 
  group_by(topico,candidato)

#esquisse::esquisser(palabras)

# Grafico de palabras
palabras %>% 
  filter(!word %in% filtro_palabras) %>% 
  top_n(8) %>% 
  ggplot() +
  aes(x = reorder(word,n), y = n, fill = topico) +
  geom_col() +
  scale_fill_manual(
    values = c("#440154","#22908B","#FDE725")
  ) +
  labs(
    x = " ",
    y = " ",
    title = "Palabras más utilizadas por los candidatos",
    subtitle = "Primer debate vicepresidencial",
    caption = "Fuente: Dicen los medios"
  ) +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(
    plot.title = element_text(size = 16L,
                              face = "bold",
                              hjust = 0.5),
    plot.subtitle = element_text(size = 14L,
                                 hjust = 0.5)
  ) +
  facet_wrap(vars(candidato), scales = "free")

# Diversidad de léxico

diversidad <- palabras |> 
  mutate(n = as.numeric(n)) |> 
  group_by(candidato) |> 
  summarise(N = n(),
            total = sum(n, na.rm = TRUE),
            diversidad = N/sum(n, na.rm =TRUE))

print(diversidad)

