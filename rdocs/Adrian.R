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




# Banco -----------

# library(readxl)
dados1 <- read_excel("banco/questionario_pablo.xlsx")
View(dados)

# library(tidyverse)

dados1 <- dados1[3:14,]
dados <- dados1[13:24]

colnames(dados) <- c('Q4','Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14','Q15')

dados$Q4 <- case_when(
  dados$Q4 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q4
)

dados$Q5 <- case_when(
  dados$Q5 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q5
)
dados$Q6 <- case_when(
  dados$Q6 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q6
)
dados$Q7 <- case_when(
  dados$Q7 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q7
)
dados$Q8 <- case_when(
  dados$Q8 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q8
)
dados$Q9 <- case_when(
  dados$Q9 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q9
)
dados$Q10 <- case_when(
  dados$Q10 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q10
)
dados$Q11 <- case_when(
  dados$Q11 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q11
)
dados$Q12 <- case_when(
  dados$Q12 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q12
)
dados$Q13 <- case_when(
  dados$Q13 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q13
)
dados$Q14 <- case_when(
  dados$Q14 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q14
)


dados$Q15 <- case_when(
  dados$Q15 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q15
)



# Análises ----------------------

# 4

dados$Q4 <- factor(dados$Q4,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q4)) %>%
  count(Q4) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q4, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,6), breaks = c(0,2,4,6))+
  labs(x = " ", y = "Frequência") +
  theme_estat()
ggsave("Q12.pdf", width = 158, height = 93, units = "mm")


# 5

dados$Q5 <- factor(dados$Q5,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))
classes <- dados %>%
  filter(!is.na(Q5)) %>%
  count(Q5) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q5, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "   ", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q13.pdf", width = 158, height = 93, units = "mm")

# 6

dados$Q6 <- factor(dados$Q6,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q6)) %>%
  count(Q6) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q6, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = " ", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q14.pdf", width = 158, height = 93, units = "mm")
# 7

dados$Q7 <- factor(dados$Q7,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q7)) %>%
  count(Q7) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q7, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  labs(x = " ", y = "Frequência") +
  theme_estat()
ggsave("Q15.pdf", width = 158, height = 93, units = "mm")

# 8

dados$Q8 <- factor(dados$Q8,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q8)) %>%
  count(Q8) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q8, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = " ", y = "Frequência") +
  theme_estat()
ggsave("Q16.pdf", width = 158, height = 93, units = "mm")


# 9

dados$Q9 <- factor(dados$Q9,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q9)) %>%
  count(Q9) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q9, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = " ", y = "Frequência") +
  theme_estat()
ggsave("Q17.pdf", width = 158, height = 93, units = "mm")


# 10

dados$Q10 <- factor(dados$Q10,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q10)) %>%
  count(Q10) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q10, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = " ", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q18.pdf", width = 158, height = 93, units = "mm")

# 11

dados$Q11 <- factor(dados$Q11,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))
classes <- dados %>%
  filter(!is.na(Q11)) %>%
  count(Q11) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q11, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  labs(x = " ", y = "Frequência") +
  theme_estat()
ggsave("Q19.pdf", width = 158, height = 93, units = "mm")

# 12

dados$Q12 <- factor(dados$Q12,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q12)) %>%
  count(Q12) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q12, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = " ", y = "Frequência") +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))+
  theme_estat()
ggsave("Q20.pdf", width = 158, height = 93, units = "mm")


# 13

dados$Q13 <- factor(dados$Q13,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q13)) %>%
  count(Q13) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q13, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = " ", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q21.pdf", width = 158, height = 93, units = "mm")


# 14

dados$Q14 <- factor(dados$Q14,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q14)) %>%
  count(Q14) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q14, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = " ", y = "Frequência") +
  scale_y_continuous(limits = c(0,6), breaks = c(0,2,4,6))+
  theme_estat()
ggsave("Q22.pdf", width = 158, height = 93, units = "mm")




# 15
dados$Q15 <- factor(dados$Q15,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))


classes <- dados %>%
  filter(!is.na(Q15)) %>%
  count(Q15) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q15, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = " ", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q23.pdf", width = 158, height = 93, units = "mm")



# Variância -----------
dados1 <- read_excel("banco/questionario_pablo.xlsx")
# View(dados)

# library(tidyverse)


dados1 <- dados1[3:14,]
dados_numericos <- dados1[13:24]

colnames(dados_numericos) <- c('Q4','Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14','Q15')


# Substituir strings por valores numéricos
dados_numericos <- dados_numericos %>%
  mutate_all(~ recode(., "Discordo totalmente" = 1, "Discordo" = 2,
                      "Nem concordo, nem discordo (Neutro)" = 3, "Concordo" = 4,
                      "Concordo totalmente" = 5))

print(dados_numericos)


var(dados_numericos$Q4)
var(dados_numericos$Q5)
var(dados_numericos$Q6)
var(dados_numericos$Q7)
var(dados_numericos$Q8)
var(dados_numericos$Q9)
var(dados_numericos$Q10)
var(dados_numericos$Q11)
var(dados_numericos$Q12)
var(dados_numericos$Q13)
var(dados_numericos$Q14)
var(dados_numericos$Q15)

# Cores --------------



cores_estat <- c("#506175", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")


theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}





# PNG's---------

# Pra gerar os gráficos em .png que ele pediu <3

library(readxl)
dados1 <- read_excel("banco/questionario_pablo.xlsx")
# View(dados)

library(tidyverse)

# Banco

dados1 <- dados1[3:14,]
dados <- dados1[13:24]

colnames(dados) <- c('Q4','Q5','Q6','Q7','Q8','Q9','Q10','Q11','Q12','Q13','Q14','Q15')

dados$Q4 <- case_when(
  dados$Q4 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q4
)

dados$Q5 <- case_when(
  dados$Q5 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q5
)
dados$Q6 <- case_when(
  dados$Q6 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q6
)
dados$Q7 <- case_when(
  dados$Q7 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q7
)
dados$Q8 <- case_when(
  dados$Q8 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q8
)
dados$Q9 <- case_when(
  dados$Q9 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q9
)
dados$Q10 <- case_when(
  dados$Q10 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q10
)
dados$Q11 <- case_when(
  dados$Q11 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q11
)
dados$Q12 <- case_when(
  dados$Q12 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q12
)
dados$Q13 <- case_when(
  dados$Q13 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q13
)
dados$Q14 <- case_when(
  dados$Q14 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q14
)


dados$Q15 <- case_when(
  dados$Q15 == 'Nem concordo, nem discordo (Neutro)' ~ 'Nem concordo,\nnem discordo (Neutro)',
  T ~ dados$Q15
)



# Análises

# 4

dados$Q4 <- factor(dados$Q4,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q4)) %>%
  count(Q4) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q4, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,6), breaks = c(0,2,4,6))+
  labs(x = "Questão 12", y = "Frequência") +
  theme_estat()
ggsave("Q12.png", width = 158, height = 93, units = "mm")


# 5

dados$Q5 <- factor(dados$Q5,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))
classes <- dados %>%
  filter(!is.na(Q5)) %>%
  count(Q5) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q5, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 13", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q13.png", width = 158, height = 93, units = "mm")

# 6

dados$Q6 <- factor(dados$Q6,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q6)) %>%
  count(Q6) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q6, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 14", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q14.png", width = 158, height = 93, units = "mm")
# 7

dados$Q7 <- factor(dados$Q7,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q7)) %>%
  count(Q7) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q7, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  labs(x = "Questão 15", y = "Frequência") +
  theme_estat()
ggsave("Q15.png", width = 158, height = 93, units = "mm")

# 8

dados$Q8 <- factor(dados$Q8,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q8)) %>%
  count(Q8) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q8, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 16", y = "Frequência") +
  theme_estat()
ggsave("Q16.png", width = 158, height = 93, units = "mm")


# 9

dados$Q9 <- factor(dados$Q9,
                   levels = c('Discordo totalmente', 'Discordo',
                              'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                              'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q9)) %>%
  count(Q9) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q9, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 17", y = "Frequência") +
  theme_estat()
ggsave("Q17.png", width = 158, height = 93, units = "mm")


# 10

dados$Q10 <- factor(dados$Q10,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q10)) %>%
  count(Q10) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q10, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 18", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q18.png", width = 158, height = 93, units = "mm")

# 11

dados$Q11 <- factor(dados$Q11,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))
classes <- dados %>%
  filter(!is.na(Q11)) %>%
  count(Q11) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q11, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  labs(x = "Questão 19", y = "Frequência") +
  theme_estat()
ggsave("Q19.png", width = 158, height = 93, units = "mm")

# 12

dados$Q12 <- factor(dados$Q12,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q12)) %>%
  count(Q12) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q12, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 20", y = "Frequência") +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10))+
  theme_estat()
ggsave("Q20.png", width = 158, height = 93, units = "mm")


# 13

dados$Q13 <- factor(dados$Q13,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q13)) %>%
  count(Q13) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q13, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 21", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q21.png", width = 158, height = 93, units = "mm")


# 14

dados$Q14 <- factor(dados$Q14,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))

classes <- dados %>%
  filter(!is.na(Q14)) %>%
  count(Q14) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q14, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 22", y = "Frequência") +
  scale_y_continuous(limits = c(0,6), breaks = c(0,2,4,6))+
  theme_estat()
ggsave("Q22.png", width = 158, height = 93, units = "mm")




# 15
dados$Q15 <- factor(dados$Q15,
                    levels = c('Discordo totalmente', 'Discordo',
                               'Nem concordo,\nnem discordo (Neutro)', 'Concordo',
                               'Concordo totalmente'))


classes <- dados %>%
  filter(!is.na(Q15)) %>%
  count(Q15) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = paste0(round(100*n/sum(n), digits = 2),'%'),
    label = str_c(n, " (", gsub("\\.", ",", freq), ")") %>% str_squish()
  )

ggplot(classes) +
  aes(x = Q15, y = n, label = label) +
  geom_bar(stat = "identity", fill = "#506175", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, #hjust = .5,
    size = 3
  ) + 
  labs(x = "Questão 23", y = "Frequência") +
  scale_y_continuous(limits = c(0,8), breaks = c(0,2,4,6,8))+
  theme_estat()
ggsave("Q23.png", width = 158, height = 93, units = "mm")



# Cores --------------



cores_estat <- c("#506175", "#003366", "#CC9900", "#663333", "#FF6600", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666")


theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 12),
      axis.title.x = ggplot2::element_text(colour = "black", size = 12),
      axis.text = ggplot2::element_text(colour = "black", size = 9.5),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}





