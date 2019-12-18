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

Razão <- read_xlsx("escoma.xlsx",
                         skip = 0,
                         sheet = 2,
                            )
Razão <-Razão %>% 
  select(`Analfabeto`, `Até 5ª Incompleto`, `5ª Completo Fundamental`,`Fundamental Completo`,`6ª a 9ª Fundamental`,
  `Médio Incompleto`,`Médio Completo`,`Superior Incompleto` ,`Superior Completo`, Ano) 

#pivotando o data frame para se encaixar no padrão que o ggplot trabalha
Razão  %>% pivot_longer(
  cols = `Analfabeto`:`Superior Completo` ,
  names_to = c("Variável"),
  values_to = "Valor") ->Razão 

  
Razão<- as.data.frame(Razão)
View(Razão)


#grafico de barras ensino medio

Razão %>%
ggplot(aes(x =`Ano`, y = Valor, fill =`Variável`)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="Ano", y="quantidade de pessoas", title="Razão de homens e mulheres empregados em todos o principais setores do Maranhão",
       caption='Fonte: Rais-Elaboração: OMT-MA.')+
  geom_text(aes(label=round(Razão$Valor,1)), size=3, 
       hjust=0.5, vjust=-1, colour="#000000")+
  guides(fill=guide_legend(title= "Sexo"))+
  theme_grey()+ theme(axis.text.x=element_text(angle=0, hjust=1))+
  theme(legend.title = element_text(size=10, color = "black", face = "bold"))+
  theme(plot.title = element_text(color = "a", size = 14, face = "bold"))

