source("rdocs/source/packages.R")

# ---------------------------------------------------------------------------- #

#        ______   _____  ________      ________ 
#      |  ____| / ____| |__   __| /\  |__   __|
#     | |__    | (___     | |   /  \    | |   
#    |  __|    \___ \    | |  / /\ \   | |   
#   | |____   ____) |   | |  /____ \  | |   
#  |______   |_____/   |_| /_/    \_\|_|   
#  
#         Consultoria estatística 
#

# ---------------------------------------------------------------------------- #
# ############################## README ###################################### #
# Consultor, favor utilizar este arquivo .R para realizar TODAS as análises
# alocadas a você neste projeto pelo gerente responsável, salvo instrução 
# explícita do gerente para mudança.
#
# Escreva seu código da forma mais clara e legível possível, eliminando códigos
# de teste depreciados, ou ao menos deixando como comentário. Dê preferência
# as funções dos pacotes contidos no Tidyverse para realizar suas análises.
# ---------------------------------------------------------------------------- #


# Importando o banco
df_pablo <- read_excel("banco/questionario_pablo.xlsx")
# Filtrando linhas/colunas indesejadas
df_pablo <- df_pablo[-(1:2), (1:12)]


# ANÁLISE 1
# Gráfico 1
ggplot(df_pablo) +
  aes(x = as.numeric(`1. Qual a sua idade?`)) +
  geom_bar(fill = "#A11D21") +
  scale_x_binned() +
  labs(x = "Questão 1", y = "Frequência") +
  theme_estat()
ggsave("graph1.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 2
# Ajeitando o banco
df_an2 = df_pablo %>%
  group_by(`2. Qual seu nível de escolaridade mais alto?`) %>%
  summarize(n=n())
df_an2$label = c('7 (58,33%)', '5 (41,67%)')

# Gráfico 2
ggplot(df_an2) +
  aes(
    x = `2. Qual seu nível de escolaridade mais alto?`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 2", y = "Frequência") +
  theme_estat()
ggsave("graph2.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 3
# Ajeitando o banco
df_an3 = df_pablo %>%
  group_by(`3. Qual seu cargo efetivo na universidade?`) %>%
  summarize(n=n())
df_an3$label = c('6 (50%)', '6 (50%)')

# Gráfico 3
ggplot(df_an3) +
  aes(
    x = `3. Qual seu cargo efetivo na universidade?`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 3", y = "Frequência") +
  theme_estat()
ggsave("graph3.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 4
# Gráfico 4
ggplot(df_pablo) +
  aes(x = as.numeric(df_pablo$`4. Há quantos anos é servidor da universidade?`)) +
  geom_bar(fill = "#A11D21") +
  scale_x_binned(n.breaks = 6) +
  labs(x = "Questão 4", y = "Frequência") +
  theme_estat()
ggsave("graph4.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 5
# Gráfico 5
ggplot(df_pablo) +
  aes(x = as.numeric(df_pablo$`5. Há quantos anos está lotado no seu setor?`)) +
  geom_bar(fill = "#A11D21") +
  scale_x_binned(n.breaks = 6) +
  labs(x = "Questão 5", y = "Frequência") +
  theme_estat()
ggsave("graph5.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 6
# Ajeitando o banco
df_an6 = df_pablo %>%
  group_by(`6. Tem formação na área de ciência da informação ou gestão da informação?`) %>%
  summarize(n=n())
df_an6$label = c('10 (83,33%)', '2 (16,67%)')

# Gráfico 6
ggplot(df_an6) +
  aes(
    x = `6. Tem formação na área de ciência da informação ou gestão da informação?`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 6", y = "Frequência") +
  theme_estat()
ggsave("graph6.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 7
# Ajeitando o banco
df_an7 = df_pablo %>%
  group_by(`7. Ocupa função comissionada ou cargo de direção?`) %>%
  summarize(n=n())
df_an7$label = '12 (100%)'

# Gráfico 7
ggplot(df_an7) +
  aes(
    x = `7. Ocupa função comissionada ou cargo de direção?`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 7", y = "Frequência") +
  theme_estat()
ggsave("graph7.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 8
# Ajeitando o banco
df_an8 = df_pablo %>%
  group_by(`8. Selecione a alternativa correspondente ao papel que você exerce na Universidade Federal de Rondônia.`) %>%
  summarize(n=n())
df_an8$label = c('5 (41,67%)', '7 (58,33%)')

# Gráfico 8
ggplot(df_an8) +
  aes(
    x = `8. Selecione a alternativa correspondente ao papel que você exerce na Universidade Federal de Rondônia.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 8", y = "Frequência") +
  theme_estat()
ggsave("graph8.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 9
# Ajeitando o banco
df_an9 = df_pablo %>%
  group_by(`1. O meu papel em relação à Área de planejamento, desenvolvimento e informação da universidade é importante.`) %>%
  summarize(n=n())
df_an9$label = c('1 (8,33%)', '11 (91,67%)')

# Gráfico 9
ggplot(df_an9) +
  aes(
    x = `1. O meu papel em relação à Área de planejamento, desenvolvimento e informação da universidade é importante.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 9", y = "Frequência") +
  theme_estat()
ggsave("graph9.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 10
# Ajeitando o banco
df_an10 = df_pablo %>%
  group_by(`2. O meu conhecimento sobre novas fontes de informação e tecnologias para gerenciamento de informação está atualizado.`) %>%
  summarize(n=n())
df_an10$label = c('4 (33,33%)', '1 (8,33%)', '7 (58,33%)')

# Gráfico 10
df_an10$`2. O meu conhecimento sobre novas fontes de informação e tecnologias para gerenciamento de informação está atualizado.` <- factor(df_an10$`2. O meu conhecimento sobre novas fontes de informação e tecnologias para gerenciamento de informação está atualizado.`,
                  levels = c("Discordo", 'Nem concordo, nem discordo (Neutro)', 'Concordo'))

ggplot(df_an10) +
  aes(
    x = `2. O meu conhecimento sobre novas fontes de informação e tecnologias para gerenciamento de informação está atualizado.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 10", y = "Frequência") +
  theme_estat()
ggsave("graph10.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")


# ANÁLISE 11
# Ajeitando o banco
df_an11 = df_pablo %>%
  group_by(`3. A Área de planejamento, desenvolvimento e informação possui processo de gestão da informação eficaz.`) %>%
  summarize(n=n())
df_an11$label = c('7 (58,33%)', '2 (16,67%)', '3 (25%)')

# Gráfico 11
df_an11$`3. A Área de planejamento, desenvolvimento e informação possui processo de gestão da informação eficaz.` <- factor(df_an11$`3. A Área de planejamento, desenvolvimento e informação possui processo de gestão da informação eficaz.`,
          levels = c("Discordo", 'Nem concordo, nem discordo (Neutro)', 'Concordo'))

ggplot(df_an11) +
  aes(
    x = `3. A Área de planejamento, desenvolvimento e informação possui processo de gestão da informação eficaz.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 11", y = "Frequência") +
  theme_estat()
ggsave("graph11.png", path = 'resultados/graficos_breno', width = 158, height = 93, units = "mm")
