
# Solidão e conexões sociais ------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ------------------------------------------------------------------------------------------------
# Data: 13/09/22 ------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/social-connections-and-loneliness --------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Pesquisas disponíveis mostram que conexões sociais são importantes para o
### nosso bem-estar. Ter suporte de família e amigos é importante para a nossa
### felicidade e saúde, e é também instrumento para nossa habilidade de compartilhar
### informações, aprender com os outros e obter oportunidades econômicas.

### Nesse artigo, só exploramos dados sobre solidão e conexões sociais em vários
### países ao longo do tempo, e revisamos evidências disponíveis sobre como e 
### porquê solidão e conexões sociais afertam nossa saúde e emocional, assim como
### bem-estar material.

### Apesar do fato de existir uma clara ligação entre conexões sociais e bem-estar
### mais pesquisas são necessárias para entender mecanismos causais, tamanho
### de efeitos e mudanças ao longo do tempo.

### A renda per capita nacional é fortemente correlacionada com agregados familiars
### unipessoais: pessoas mais ricas tendem a viver sozinhas. O número de pessoas 
### que moram sozinhas tende a ser maior em países com maior renda per capita.

### Estas correlações devem-se em parte ao fato de que as pessoas que podem 
### dar-se a esse luxo, optam frequentemente por viver sozinhas. 

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

scs <- read.csv("one-person-households-vs-gdp-per-capita.csv")
view(scs)
names(scs)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

scs <- scs %>%
  select(Entity, Year, Share.of.one.person.households,
         GDP.per.capita..PPP..constant.2017.international...) %>%
  rename(porcentagem = Share.of.one.person.households,
         renda = GDP.per.capita..PPP..constant.2017.international...) %>%
  drop_na() %>%
  view()

scs1 <- scs %>%
  filter(Entity %in% c("Austria", "Belgium", "Bulgaria",
                       "Croatia", "Brazil", "China",
                       "Russia", "Denmark", "Estonia")) %>%
  group_by(Entity) %>%
  summarise(media = mean(porcentagem),
            sd = sd(porcentagem), 
            n = n(),
            se = sd/sqrt(n)) %>%
  view()

scs2 <- scs %>%
  filter(Entity %in% c("Belgium", "Bulgaria",
                       "Denmark", "Estonia")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

### Seleção de cores

c4a_gui()
c4a("paired", 9)

ggplot(scs1, aes(x = fct_reorder(Entity, media), y = media, 
                 fill = Entity)) +
  geom_col() +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.3, size = 0.8) +
  scale_fill_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A",
                               "#33A02C", "#FB9A99", "#E31A1C",
                               "#FDBF6F", "#FF7F00", "#CAB2D6")) +
  labs(x = "Países", y = "Percentagem de lares com uma pessoa") +
  theme_bw() +
  theme(legend.position = "none", 
        axis.text = element_text(color = "black"))  

c4a("paired", 4)

ggplot(scs2, aes(x = Year, y = porcentagem, 
                 group = Entity, col = Entity)) +
  geom_point(size = 3) +
  geom_line(size = 1.8) +
  scale_color_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A",
                               "#33A02C")) +
  labs(x = "Tempo (anos)", y = "Percentagem de lares com uma pessoa",
       color = "Países") 
