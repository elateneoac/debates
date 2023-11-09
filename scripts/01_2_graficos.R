library(readr)
library(tidyverse)
library(tidytext)
library(stringi)
library(stopwords)

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
filtro_palabras2 <- c("hace","ser","solo","victoria","eh","voy","anos","asi","van","dos","hoy","vos","nlega","aca","luis","va","cuatro")
# divido por palabra
palabras <- df_debate%>%
  unnest_tokens(input = text, output = word) %>% 
  count(topico,candidato,word) %>% 
  group_by(topico,candidato)%>% 
  filter(!word %in% filtro_palabras) %>% 
  filter(!word %in% filtro_palabras2) %>% 
  group_by(candidato,word) %>% 
  mutate(N = sum(n)) %>% 
  ungroup() %>% 
  group_by(topico,candidato) %>% 
  arrange(N)

#esquisse::esquisser(palabras)

# Grafico de palabras
palabras   %>% 
  filter(N > 4) %>% 
  ggplot() +
  aes(x = reorder_within(word, N, candidato), y = n, fill = topico) +
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
  scale_x_reordered()+
  facet_wrap(vars(candidato), scales = "free")  

# Diversidad de léxico

diversidad <- palabras |> 
  mutate(n = as.numeric(n)) |> 
  group_by(candidato) |> 
  summarise(N = n(),
            total = sum(n, na.rm = TRUE),
            diversidad = N/sum(n, na.rm =TRUE))

print(diversidad)

