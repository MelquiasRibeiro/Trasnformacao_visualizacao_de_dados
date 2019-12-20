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

EscolaridadeSlz <- read_xlsx("escoma.xlsx",
                         skip = 0,
                         sheet = 1,
                            )

#pivotando o data frame para se encaixar no padrão que o ggplot trabalha
EscolaridadeSlz  %>% pivot_longer(
  cols = `Analfabeto`:`Superior Completo` ,
  names_to = c("Variável"),
  values_to = "Valor") ->EscolaridadeSlz 

Medio <-EscolaridadeSlz %>% 
  select(`Valor`, `Variável`, `Sexo`,Ano ) %>% 
  filter(`Variável`== "Médio Completo")
  
Medio<- as.data.frame(Medio)

Superior <-EscolaridadeSlz %>% 
  select(`Valor`, `Variável`, `Sexo`,Ano ) %>% 
  filter(`Variável`== "Superior Completo")

Superior<- as.data.frame(Superior)

#grafico de barras ensino medio

Medio %>%
ggplot(aes(x =`Ano`, y = Valor, fill =`Sexo`)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="Ano", y="quantidade de pessoas", title="Quantidade de pessoas no Maranhão empregadas em todos os grandes setores com ensino medio completo",
       caption='Fonte: Rais-Elaboração: OMT-MA.')+
  geom_text(aes(label=round(Medio$Valor,1)), size=3, 
       hjust=0.5, vjust=-1, colour="#000000")+
  guides(fill=guide_legend(title= "Sexo"))+
  theme_grey()+ theme(axis.text.x=element_text(angle=0, hjust=1))+
  theme(legend.title = element_text(size=10, color = "black", face = "bold"))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

#grafico de barras ensino superior
Superior %>%
ggplot(aes(x =`Ano`, y = Valor, fill =`Sexo`)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="Ano", y="quantidade de pessoas", title="Quantidade de pessoas no Maranhão empregadas em todos os grandes setores com ensino superior completo",
       caption='Fonte: Rais-Elaboração: OMT-MA.')+
  geom_text(aes(label=round(Superior$Valor,1)), size=3, 
       hjust=0.5, vjust=-1, colour="#000000")+
  guides(fill=guide_legend(title= "Sexo"))+
  theme_grey()+ theme(axis.text.x=element_text(angle=0, hjust=1))+
  theme(legend.title = element_text(size=10, color = "black", face = "bold"))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

