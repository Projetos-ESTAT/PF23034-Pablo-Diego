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

###Ordenando 
data$Q1 <- factor(data$Q1,
                  levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


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

q1$Q1 <- factor(q1$Q1,
                  levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q1) +
  aes(
    x = Q1,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
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

q2$Q2 <- factor(q2$Q2,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q2) +
  aes(
    x = Q2,
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

q3$Q3 <- factor(q3$Q3,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q3) +
  aes(
    x = Q3,
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

q4$Q4 <- factor(q4$Q4,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q4) +
  aes(
    x = Q4,
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

q5$Q5 <- factor(q5$Q5,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q5) +
  aes(
    x = Q5,
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

q6$Q6 <- factor(q6$Q6,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q6) +
  aes(
    x = Q6,
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

q7$Q7 <- factor(q7$Q7,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q7) +
  aes(
    x = Q7,
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

q8$Q8 <- factor(q8$Q8,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q8) +
  aes(
    x = Q8,
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

q9$Q9 <- factor(q9$Q9,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q9) +
  aes(
    x = Q9,
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

q10$Q10 <- factor(q10$Q10,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q10) +
  aes(
    x = Q10,
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

q11$Q11 <- factor(q11$Q11,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n Totalmente'))


ggplot(q11) +
  aes(
    x = Q11,
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
ggsave(filename = file.path(caminho_iza,"colunas-uni-freq-Q11.pdf"), width = 158, height = 93, units = "mm")


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

q12$Q12 <- factor(q12$Q12,
                levels = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente'))


ggplot(q12) +
  aes(
    x = Q12,
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

###Análise de variancia 
##mudando o banco 
data_v <- data
data_v

data_v[data_v == "Discordo\n totalmente"] <- "1"
data_v[data_v == "Discordo"] <- "2"
data_v[data_v == "Nem concordo,\n nem discordo (Neutro)"] <- "3"
data_v[data_v == "Concordo"] <- "4"
data_v[data_v == "Concordo\n totalmente"] <- "5"
data_v$Q11[data_v$Q11 == "Concordo\n Totalmente"] <- "5"

##Transaformando para numerico
sapply(data_v, class)

data_v$Q1 <- as.numeric(as.character(data_v$Q1))
data_v$Q2 <- as.numeric(as.character(data_v$Q2))
data_v$Q3 <- as.numeric(as.character(data_v$Q3))
data_v$Q4 <- as.numeric(as.character(data_v$Q4))
data_v$Q5 <- as.numeric(as.character(data_v$Q5))
data_v$Q6 <- as.numeric(as.character(data_v$Q6))
data_v$Q7 <- as.numeric(as.character(data_v$Q7))
data_v$Q8 <- as.numeric(as.character(data_v$Q8))
data_v$Q9 <- as.numeric(as.character(data_v$Q9))
data_v$Q10 <- as.numeric(as.character(data_v$Q10))
data_v$Q11 <- as.numeric(as.character(data_v$Q11))
data_v$Q12 <- as.numeric(as.character(data_v$Q12))

###Variancias 
#Q1
Q1 <- var(data_v$Q1)
mean(data_v$Q1)
#Q2
Q2 <- var(data_v$Q2)
#Q3
Q3 <- var(data_v$Q3)
#Q4
Q4 <- var(data_v$Q4)
#Q5
Q5 <- var(data_v$Q5)
#Q6
Q6 <- var(data_v$Q6)
#Q7
Q7 <- var(data_v$Q7)
#Q8
Q8 <- var(data_v$Q8)
#Q9
Q9 <- var(data_v$Q9)
#Q10
Q10 <- var(data_v$Q10)
#Q11
Q11 <- var(data_v$Q11)
#Q12
Q12 <- var(data_v$Q12)
