getwd()
setwd('C:/Users/Melquias/Desktop/OMT/POF/') 
library(tidyr)
library(dplyr)
library(forcats)
library(readxl)
library(readr)
library(stringr)
library(kableExtra)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(scales)

rendimento_total_var_patrim_2017_2018 <- read_xlsx("21MA.xlsx",
                                                   skip = 11,
                                                   sheet = 5,
                                                   col_names = c("origem_rendimento",
                                                                 "total",
                                                                 "até 1908",
                                                                 "mais de 1908 a 2862",
                                                                 "mais de 2862 a 5724",
                                                                 "mais de 5724 a 9540",
                                                                 "mais de 9540 a 14310",
                                                                 "mais de 14310 a 23850",
                                                                 "mais de 23850"),
                                                   n_max = 22)

rendimento_total_var_patrim_2017_2018 <-  rendimento_total_var_patrim_2017_2018 %>%
  na.omit() #removendo "NA"

for(i in 1:nrow(rendimento_total_var_patrim_2017_2018)){
  rendimento_total_var_patrim_2017_2018[i,-1] <- gsub('[-]', 0, rendimento_total_var_patrim_2017_2018[i,-1])
} #trocando "-" por "0"
 
rendimento_total_var_patrim_2017_2018 <- rendimento_total_var_patrim_2017_2018 %>% 
  select(-total) # Removendo 'total'

#criando o dataframe especifico sobre as familias
familias_2017_2018 <- rendimento_total_var_patrim_2017_2018 %>% 
  filter(origem_rendimento == "Número de famílias" |
           origem_rendimento == "Tamanho médio da família")

#removendo informaçoes sobre as familias do dataframe principal
rendimento_total_var_patrim_2017_2018 <- rendimento_total_var_patrim_2017_2018 %>% 
  filter(origem_rendimento != "Número de famílias" &
           origem_rendimento != "Tamanho médio da família")

#Deixando na menor fragmentação de dados possivel
rendimento_total_var_patrim_2017_2018 <- rendimento_total_var_patrim_2017_2018 %>% 
  filter(origem_rendimento != "Rendimento total" &
           origem_rendimento != "Rendimento do trabalho" &
           origem_rendimento != "Transferência")

#desfragmentando os dados em niveis
nivel_1 <- c("Rendimento total", "Variação patrimonial")
nivel_1 <- c(rep(nivel_1[1], 12), rep(nivel_1[2], 1))

nivel_2 <- c("Rendimento do trabalho", "Transferência", "Rendimento de aluguel", 
             "Outras rendas", "Rendimento não monetário", "Variação patrimonial")
nivel_2 <-c(rep(nivel_2[1], 3),
            rep(nivel_2[2], 6),
            rep(nivel_2[3], 1),
            rep(nivel_2[4], 1),
            rep(nivel_2[5], 1),
            rep(nivel_2[6], 1))

rendimento_total_var_patrim_2017_2018 <- rendimento_total_var_patrim_2017_2018 %>% 
mutate(n1 = nivel_1, n2 = nivel_2)

#pivotando o data frame para se encaixar no padrão que o ggplot trabalha
rendimento_total_var_patrim_2017_2018  %>% pivot_longer(
  cols = `até 1908`:`mais de 23850`,
  names_to = c("Faixa_Renda"),
  values_to = "Rendimento") ->rendimento_total_var_patrim_2017_2018 

#convertendo a coluna "Rendimento" para doble
rendimento_total_var_patrim_2017_2018$Rendimento <- 
as.double(rendimento_total_var_patrim_2017_2018$Rendimento)

#convertendo a coluna "faixa_Renda" em factor 
rendimento_total_var_patrim_2017_2018$Faixa_Renda <- 
factor(rendimento_total_var_patrim_2017_2018$Faixa_Renda,
    levels = unique(rendimento_total_var_patrim_2017_2018$Faixa_Renda))

#convertendo a coluna "faixa_Renda" em factor 
rendimento_total_var_patrim_2017_2018$n2 <- 
factor(rendimento_total_var_patrim_2017_2018$n2,
    levels = unique(rendimento_total_var_patrim_2017_2018$n2))

ordem_classes <- c("até 1908",
                       "mais de 1908 a 2862",
                       "mais de 2862 a 5724",
                       "mais de 5724 a 9540",
                       "mais de 9540 a 14310",
                       "mais de 14310 a 23850",
                       "mais de 23850")


ordem_n2 <- c("Variação patrimonial",
              "Rendimento não monetário",
              "Outras rendas",
              "Rendimento de aluguel",
              "Transferência",
              "Rendimento do trabalho")

#pivotando o os dados sobre as familias

familias_2017_2018 <- familias_2017_2018 %>%  
pivot_longer(cols = `até 1908`:`mais de 23850`,
  names_to = c("Faixa_Renda"),
  values_to = "Rendimento")
#deixando apenas duas casas decimais
familias_2017_2018$valor <- as.double(familias_2017_2018$Rendimento)

#divindo em dois data frames
Numero_familias_2017_2018 <- familias_2017_2018 %>% 
  filter(origem_rendimento == "Número de famílias") %>% 
  mutate(percentual = round(valor/sum(valor) * 100, 2)) 
  
Tamanho_familias_2017_2018 <- familias_2017_2018 %>% 
  filter(origem_rendimento == "Tamanho médio da família")

#=========================== grafico do numero de familias =======================================

 Numero_familias_2017_2018 %>% 
  ggplot(aes(x = fct_relevel(Faixa_Renda, ordem_classes), 
             y = valor)) +
  geom_col(fill = "darksalmon", position = "stack", colour = "black", show.legend = FALSE) +
  labs(x = "Classes de rendimento",
       y = "Número de famílias",
       title = " Gráfico 1 -  Quantidade de famílias maranhenses por classes de rendimento (2017-2018)",
              caption = "Fonte: IBGE, Diretoria de Pesquisas, Coordenação de Trabalho e Rendimento, 
              Pesquisa de Orçamentos Familiares 2017-2018.
              \nElaboração: OMT-MA\nNota: a classe de rendimento até 1.908 reais inclui também 
              sem rendimento.")  + 
  theme_bw() +
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold")) + 
  guides(
    fill = guide_legend(reverse=TRUE),
    color = guide_legend(
      nrow = 1,
      override.aes = list(size = 4))) +
  scale_y_continuous(labels = number)
View(familias_2017_2018)