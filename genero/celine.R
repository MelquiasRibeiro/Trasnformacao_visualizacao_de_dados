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

getwd()
setwd('C:/Users/Melquias/Desktop/OMT/genero')
dados<- read_xlsx("data.xlsx",
                            skip = 0,
                            sheet = 1)

dados %>% pivot_longer(
  cols = `Força de trabalho`:`empregos formais` ,
  names_to = c("variavel"),
  values_to = "valor") ->dados

ggplot(dados, aes(x=Ano, y=valor, colour = variavel))+
  geom_line(size=1.5)+
  xlab('Ano')+ylab('milhões')+
  labs(title=' Brasil',
       caption='', 
       color="variavel")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados$valor,1),big.mark = ".", decimal.mark = ","), size=3, 
               hjust=0.5, vjust=-1, colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

glimpse(dados)
View(dados)