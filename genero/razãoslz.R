getwd()
setwd('C:/Users/Melquias/Desktop/OMT/genero') # definindo diretorio de trabalho

# carregando pacotes
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
rm(list = ls()) #limpando memoria

Razão <- read_xlsx("escoslz.xlsx",
                         skip = 0,
                         sheet = 2,
                            )
View(Razão)
Razão <-Razão %>% 
  select(`Analfabeto`, `Até 5ª Incompleto`, `5ª Completo Fundamental`,`Fundamental Completo`,`6ª a 9ª Fundamental`,
  `Médio Incompleto`,`Médio Completo`,`Superior Incompleto` ,`Superior Completo`, Ano) 

#pivotando o data frame para se encaixar no padrão que o ggplot trabalha
Razão  %>% pivot_longer(
  cols = `Analfabeto`:`Superior Completo` ,
  names_to = c("Variável"),
  values_to = "Valor") ->Razão 

Razão<- as.data.frame(Razão)
View(Razão2018)

#grafico de barras ensino medio

Razão %>%
ggplot(aes(x =`Ano`, y = Valor, fill =`Variável`)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="Ano", y="quantidade de pessoas", title="Razão de homens e mulheres empregados em todos o principais setores de São Luis",
       caption='Fonte: Rais-Elaboração: OMT-MA.')+
  geom_text(aes(label=round(Razão$Valor,3)), size=3, 
       hjust=1, vjust=-1, colour="#000000")+
  guides(fill=guide_legend(title= "Sexo"))

