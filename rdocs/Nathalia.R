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

## carregando pacotes

pacman::p_load(readxl)

### importando banco
banco <- read_excel("banco/questionario_pablo.xlsx")

### criando ordem
ordem = c('Discordo\n totalmente', 'Discordo', 'Nem concordo,\n nem discordo (Neutro)', 'Concordo', 'Concordo\n totalmente')

### selecionando dados de interesse
banco <- banco[-(1:2),37:48]

### criando banco com as variáveis em valores numéricos
# 1 = 'Discordo totalmente'
# 2 = 'Discordo'
# 3 = 'Nem concordo, nem discordo (Neutro)'
# 4 = 'Concordo'
# 5 = 'Concordo totalmente'


banco_num <- banco %>%
  mutate(
    q36 = case_when(
      `2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.` == 'Discordo totalmente' ~ 1,
      `2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.` == 'Discordo' ~ 2,
      `2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.` == 'Concordo' ~ 4,
      `2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q37 = case_when(
      `3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.` == 'Discordo totalmente' ~ 1,
      `3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.` == 'Discordo' ~ 2,
      `3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.` == 'Concordo' ~ 4,
      `3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q38 = case_when(
      `4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Discordo totalmente' ~ 1,
      `4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Discordo' ~ 2,
      `4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Concordo' ~ 4,
      `4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q39 = case_when(
      `5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Discordo totalmente' ~ 1,
      `5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Discordo' ~ 2,
      `5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Concordo' ~ 4,
      `5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q40 = case_when(
      `1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.` == 'Discordo totalmente' ~ 1,
      `1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.` == 'Discordo' ~ 2,
      `1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.` == 'Concordo' ~ 4,
      `1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q41 = case_when(
      `2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Discordo totalmente' ~ 1,
      `2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Discordo' ~ 2,
      `2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Concordo' ~ 4,
      `2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q42 = case_when(
      `3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Discordo totalmente' ~ 1,
      `3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Discordo' ~ 2,
      `3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Concordo' ~ 4,
      `3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q43 = case_when(
      `4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.` == 'Discordo totalmente' ~ 1,
      `4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.` == 'Discordo' ~ 2,
      `4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.` == 'Concordo' ~ 4,
      `4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q44 = case_when(
      `1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.` == 'Discordo totalmente' ~ 1,
      `1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.` == 'Discordo' ~ 2,
      `1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.` == 'Concordo' ~ 4,
      `1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q45 = case_when(
      `2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.` == 'Discordo totalmente' ~ 1,
      `2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.` == 'Discordo' ~ 2,
      `2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.` == 'Concordo' ~ 4,
      `2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q46 = case_when(
      `3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.` == 'Discordo totalmente' ~ 1,
      `3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.` == 'Discordo' ~ 2,
      `3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.` == 'Concordo' ~ 4,
      `3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.` == 'Concordo totalmente' ~ 5,
      .default = NA),
    q47 = case_when(
      `4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.` == 'Discordo totalmente' ~ 1,
      `4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.` == 'Discordo' ~ 2,
      `4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.` == 'Nem concordo, nem discordo (Neutro)' ~ 3,
      `4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.` == 'Concordo' ~ 4,
      `4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.` == 'Concordo totalmente' ~ 5,
      .default = NA)
  ) %>% 
  select(q36,q37,q38,q39,q40,q41,q42,q43,q44,q45,q46,q47)


### acertando texto para o gráfico de barras

banco <- banco %>%
  mutate_at(vars(
    `2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.`,
    `3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.`,
    `4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.`,
    `5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.`,
    `1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.`,
    `2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.`,
    `3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.`,
    `4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.`,
    `1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.`,
    `2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.`,
    `3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.`,
    `4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.`
  ), ~ case_when(
    . == "Nem concordo, nem discordo (Neutro)" ~ "Nem concordo,\n nem discordo (Neutro)",
    . == "Concordo totalmente" ~ "Concordo\n totalmente",
    . == "Discordo totalmente" ~ "Discordo\n totalmente",
    . == "Discordo" ~ "Discordo",
    . == "Concordo" ~ "Concordo",
    TRUE ~ .
  ))

####################### análise 1 - q36 #######################

## algumas medidas
mean(banco_num$q36)
var(banco_num$q36)
sd(banco_num$q36)

## construindo o gráfico
q36 <- banco %>%
  count(`2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q36$`2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.` <- 
  factor(q36$`2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.`,
         levels = ordem)

## gráfico de barras
ggplot(q36) +
  aes(
    x = `2 - A equipe da DPDI possui qualificação profissional e acadêmica em gestão da informação.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,8) +
  labs(x = "Questão 36", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q36.pdf", width = 158, height = 93, units = "mm")


####################### análise 2 - q37 #######################

## algumas medidas
mean(banco_num$q37)
var(banco_num$q37)
sd(banco_num$q37)

## construindo o gráfico
q37 <- banco %>%
  count(`3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q37$`3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.` <- 
  factor(q37$`3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.`,
         levels = ordem)

## gráfico de barras
ggplot(q37) +
  aes(
    x = `3. A equipe da DPDI É incentivada a buscar qualificação e atualização profissional em relação à gestão da informação.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,5.5) +
  labs(x = "Questão 37", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q37.pdf", width = 158, height = 93, units = "mm")


####################### análise 3 - q38 #######################

## algumas medidas
mean(banco_num$q38)
var(banco_num$q38)
sd(banco_num$q38)

## construindo o gráfico
q38 <- banco %>%
  count(`4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q38$`4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` <- 
  factor(q38$`4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.`,
         levels = ordem)

## gráfico de barras
ggplot(q38) +
  aes(
    x = `4. A equipe da DPDI possui composição multidisciplinar adequada para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,4.5) +
  labs(x = "Questão 38", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q38.pdf", width = 158, height = 93, units = "mm")


####################### análise 4 - q39 #######################

## algumas medidas
mean(banco_num$q39)
var(banco_num$q39)
sd(banco_num$q39)

## construindo o gráfico
q39 <- banco %>%
  count(`5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q39$`5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.` <- 
  factor(q39$`5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.`,
         levels = ordem)

## gráfico de barras
ggplot(q39) +
  aes(
    x = `5. A equipe da DPDI possui número de servidores suficiente para desenvolver as atividades da gestão da informação na Área de planejamento, desenvolvimento e informação.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,8.5)+
  labs(x = "Questão 39", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q39.pdf", width = 158, height = 93, units = "mm")


####################### análise 5 - q40 #######################

## algumas medidas
mean(banco_num$q40)
var(banco_num$q40)
sd(banco_num$q40)

## construindo o gráfico
q40 <- banco %>%
  count(`1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q40$`1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.` <- 
  factor(q40$`1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.`,
         levels = ordem)

## gráfico de barras
ggplot(q40) +
  aes(
    x = `1 - O processo de gestão da informação na Área de planejamento, desenvolvimento e informação, desde a coleta até o compartilhamento e análise, é eficaz.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,4.5)+
  labs(x = "Questão 40", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q40.pdf", width = 158, height = 93, units = "mm")


####################### análise 6 - q41 #######################

## algumas medidas
mean(banco_num$q41)
var(banco_num$q41)
sd(banco_num$q41)

## construindo o gráfico
q41 <- banco %>%
  count(`2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q41$`2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.` <- 
  factor(q41$`2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.`,
         levels = ordem)

## gráfico de barras
ggplot(q41) +
  aes(
    x = `2. O processo de coleta, armazenamento e gestão de informações relevantes feito pela Área de planejamento, desenvolvimento e informação é satisfatório.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,5.5) +
  labs(x = "Questão 41", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q41.pdf", width = 158, height = 93, units = "mm")


####################### análise 7 - q42 #######################

## algumas medidas
mean(banco_num$q42)
var(banco_num$q42)
sd(banco_num$q42)

## construindo o gráfico
q42 <- banco %>%
  count(`3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q42$`3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.` <- 
  factor(q42$`3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.`,
         levels = ordem)

## gráfico de barras
ggplot(q42) +
  aes(
    x = `3. Os fluxos de compartilhamento de informações na Área de planejamento, desenvolvimento e informação é satisfatório.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,5.5) +
  labs(x = "Questão 42", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q42.pdf", width = 158, height = 93, units = "mm")


####################### análise 8 - q43 #######################

## algumas medidas
mean(banco_num$q43)
var(banco_num$q43)
sd(banco_num$q43)

## construindo o gráfico
q43 <- banco %>%
  count(`4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q43$`4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.` <- 
  factor(q43$`4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.`,
         levels = ordem)

## gráfico de barras
ggplot(q43) +
  aes(
    x = `4. A Área de planejamento, desenvolvimento e informação possui indicadores e métricas claras para medir, avaliar e melhorar sistematicamente a eficácia de seus processos de gestão da informação.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,8) +
  labs(x = "Questão 43", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q43.pdf", width = 158, height = 93, units = "mm")


####################### análise 9 - q44 #######################

## algumas medidas
mean(banco_num$q44)
var(banco_num$q44)
sd(banco_num$q44)

## construindo o gráfico
q44 <- banco %>%
  count(`1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q44$`1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.` <- 
  factor(q44$`1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.`,
         levels = ordem)

## gráfico de barras
ggplot(q44) +
  aes(
    x = `1. A Área de planejamento, desenvolvimento e informação possui sistemas e tecnologias adequados para gerenciar e compartilhar informações de maneira eficiente.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,5.5) +
  labs(x = "Questão 44", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q44.pdf", width = 158, height = 93, units = "mm")

####################### análise 10 - q45 #######################

## algumas medidas
mean(banco_num$q45)
var(banco_num$q45)
sd(banco_num$q45)

## construindo o gráfico
q45 <- banco %>%
  count(`2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q45$`2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.` <- 
  factor(q45$`2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.`,
         levels = ordem)

## gráfico de barras
ggplot(q45) +
  aes(
    x = `2. A Área de planejamento, desenvolvimento e informação utiliza tecnologias de informação e comunicação de maneira adequada para apoiar o processo de gestão da informação.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  labs(x = "Questão 45", y = "Frequência") +
  ylim (0,6.5) +
  theme_estat()
#ggsave("colunas-q45.pdf", width = 158, height = 93, units = "mm")


####################### análise 11 - q46 #######################

## algumas medidas
mean(banco_num$q46)
var(banco_num$q46)
sd(banco_num$q46)

## construindo o gráfico
q46 <- banco %>%
  count(`3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q46$`3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.` <- 
  factor(q46$`3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.`,
         levels = ordem)

## gráfico de barras
ggplot(q46) +
  aes(
    x = `3. O impacto do uso de tecnologias de informação na gestão da informação na Área de planejamento, desenvolvimento e informação é positivo.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,6.5) +
  labs(x = "Questão 46", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q46.pdf", width = 158, height = 93, units = "mm")


####################### análise 12 - q47 #######################

## algumas medidas
mean(banco_num$q47)
var(banco_num$q47)
sd(banco_num$q47)

## construindo o gráfico
q47 <- banco %>%
  count(`4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.`) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )


q47$`4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.` <- 
  factor(q47$`4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.`,
         levels = ordem)

## gráfico de barras
ggplot(q47) +
  aes(
    x = `4. A Área de planejamento, desenvolvimento e informação utiliza abordagem planejada e sistemática para selecionar dados e informações úteis.`,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  ylim(0,8.5) +
  labs(x = "Questão 47", y = "Frequência") +
  theme_estat()
#ggsave("colunas-q47.pdf", width = 158, height = 93, units = "mm")

