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
getwd()
setwd()

#banco
library(readxl)
questionario_pablo <- read_excel("banco/questionario_pablo.xlsx")
View(questionario_pablo)

#banco so com minhas perguntas
data <- data.frame( questionario_pablo$`1. As estratégias e os objetivos da Área de planejamento, desenvolvimento e informação para a coleta e o uso de dados e informações são claros.`, 
                   questionario_pablo$`2. As estratégias e objetivos da Área de planejamento, desenvolvimento e informação em relação ao uso da informação são eficazes.`,
                   questionario_pablo$`3. A Área de planejamento, desenvolvimento e informação utiliza as informações geradas a partir de suas atividades de maneira estratégica.`,
                   questionario_pablo$`4. A Área de planejamento, desenvolvimento e informação possui um plano estratégico claro e bem definido para a gestão da informação.`,
                   questionario_pablo$`1. As políticas de gestão da informação na Área de planejamento, desenvolvimento e informação são claras.`,
                   questionario_pablo$`2. O cumprimento das políticas de gestão da informação na Área de planejamento, desenvolvimento e informação está adequado.`,
                   questionario_pablo$`3. Os papéis/responsabilidades das pessoas envolvidas no processo de Gestão da Informação da Área de planejamento, desenvolvimento e informação estão claros e bem definidos.`,
                   questionario_pablo$`1 - Avalio a cultura de compartilhamento de informações de forma positiva.`,
                   questionario_pablo$`2. Na Área de planejamento, desenvolvimento e informação, as pessoas são encorajadas a buscar informações de outras Áreas da universidade.`,
                   questionario_pablo$`3. A Área de planejamento, desenvolvimento e informação possui uma cultura de compartilhamento de informação entre os diferentes setores e Áreas da universidade.`,
                   questionario_pablo$`4. Na Área de planejamento, desenvolvimento e informação são identificados e estimulados comportamentos e cultura informacional positivos de Gestão da Informação.`,
                   questionario_pablo$`1 - A equipe da DPDI possui proficiência em gestão da informação.`)

##mudando o nome das colunas e apagando linhas desnecessarias 
colnames(data) <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6","Q7", "Q8", "Q9", "Q10", "Q11", "Q12")
data <- data[-c(1, 2),]
View(data)

data$Q1[data$Q1 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q1[data$Q1 == "Concordo totalmente"] <- "Concordo\n totalmente"
data$Q2[data$Q2 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q3[data$Q3 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q4[data$Q4 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q4[data$Q4 == "Concordo totalmente"] <- "Concordo\n totalmente"
data$Q5[data$Q5 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q5[data$Q5 == "Discordo totalmente"] <- "Discordo\n totalmente"
data$Q6[data$Q6 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q6[data$Q6 == "Discordo totalmente"] <- "Discordo\n totalmente"
data$Q7[data$Q7 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q7[data$Q7 == "Discordo totalmente"] <- "Discordo\n totalmente"
data$Q8[data$Q8 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q8[data$Q8 == "Concordo totalmente"] <- "Concordo\n totalmente"
data$Q9[data$Q9 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q9[data$Q9 == "Concordo totalmente"] <- "Concordo\n totalmente"
data$Q10[data$Q10 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q10[data$Q10 == "Concordo totalmente"] <- "Concordo\n totalmente"
data$Q10[data$Q10 == "Discordo totalmente"] <- "Discordo\n totalmente"
data$Q11[data$Q11 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q11[data$Q11 == "Concordo Totalmente"] <- "Concordo\n Totalmente"
data$Q12[data$Q12 == "Nem concordo, nem discordo (Neutro)"] <- "Nem concordo,\n nem discordo (Neutro)"
data$Q12[data$Q12 == "Concordo totalmente"] <- "Concordo\n totalmente"

###Caminho graficos
caminho_iza <- "C:/Users/izade/OneDrive/Área de Trabalho/ESTAT/PF23034-Pablo-Diego/resultados/Gráficos iza"


###Grafico Q1
q1 <- data %>%
  filter(!is.na(Q1)) %>%
  count(Q1) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q1) +
  aes(
    x = fct_reorder(Q1, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 24", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q1.pdf"), width = 158, height = 93, units = "mm")

###Grafico Q2
q2 <- data %>%
  filter(!is.na(Q2)) %>%
  count(Q2) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q2) +
  aes(
    x = fct_reorder(Q2, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 25", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q2.pdf"), width = 158, height = 93, units = "mm")



###Grafico Q3
q3 <- data %>%
  filter(!is.na(Q3)) %>%
  count(Q3) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q3) +
  aes(
    x = fct_reorder(Q3, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 26", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q3.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q4
q4 <- data %>%
  filter(!is.na(Q4)) %>%
  count(Q4) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q4) +
  aes(
    x = fct_reorder(Q4, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 27", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q4.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q5
q5 <- data %>%
  filter(!is.na(Q5)) %>%
  count(Q5) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q5) +
  aes(
    x = fct_reorder(Q5, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 28", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q5.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q6
q6 <- data %>%
  filter(!is.na(Q6)) %>%
  count(Q6) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q6) +
  aes(
    x = fct_reorder(Q6, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 29", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q6.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q7
q7 <- data %>%
  filter(!is.na(Q7)) %>%
  count(Q7) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q7) +
  aes(
    x = fct_reorder(Q7, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 30", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q7.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q8
q8 <- data %>%
  filter(!is.na(Q8)) %>%
  count(Q8) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q8) +
  aes(
    x = fct_reorder(Q8, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 31", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q8.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q9
q9 <- data %>%
  filter(!is.na(Q9)) %>%
  count(Q9) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q9) +
  aes(
    x = fct_reorder(Q9, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 32", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q9.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q10
q10 <- data %>%
  filter(!is.na(Q10)) %>%
  count(Q10) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q10) +
  aes(
    x = fct_reorder(Q10, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 33", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q10.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q11
q11 <- data %>%
  filter(!is.na(Q11)) %>%
  count(Q11) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q11) +
  aes(
    x = fct_reorder(Q11, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 34", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q10.pdf"), width = 158, height = 93, units = "mm")


###Grafico Q12
q12 <- data %>%
  filter(!is.na(Q12)) %>%
  count(Q12) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

ggplot(q12) +
  aes(
    x = fct_reorder(Q12, n, .desc = T),
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 35", y = "Frequência") +
  theme_estat()
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q12.pdf"), width = 158, height = 93, units = "mm")

