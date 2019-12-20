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

Media <- read_xlsx("escoma.xlsx",
                         skip = 0,
                         sheet = 3,
                            )
View(Media)
Media <-Media %>% 
  select(`Analfabeto`, `Até 5ª Incompleto`, `5ª Completo Fundamental`,`Fundamental Completo`,`6ª a 9ª Fundamental`,
  `Médio Incompleto`,`Médio Completo`,`Superior Incompleto` ,`Superior Completo`, Ano) 
  
#pivotando o data frame para se encaixar no padrão que o ggplot trabalha
Media  %>% pivot_longer(
  cols = `Analfabeto`:`Superior Completo` ,
  names_to = c("Variável"),
  values_to = "Valor") ->Media 

Media<- as.data.frame(Media)
View(Media)

Media$Variável <-factor(Media$Variável, levels = unique(Media$Variável[order(Media$Valor)]))

#grafico de barras ensino medio

Media %>%
ggplot(aes(x =`Variável`, y = Valor, fill =`Variável`)) + 
  geom_bar(stat="identity") + 
  labs(x="Ano", y="homens / mulheres", title="Media da Razão de Homens e Mulheres empregados em todos o principais setores do Maranhão",
       caption='Fonte: Rais-Elaboração: OMT-MA.')+
  geom_text(aes(label=round(Media$Valor,3)), size=3, 
       hjust=1, vjust=-1, colour="#000000")+
  guides(fill=guide_legend(title= "Sexo"))+
  theme_grey()+ theme(axis.text.x=element_text(angle=45, hjust=1))

