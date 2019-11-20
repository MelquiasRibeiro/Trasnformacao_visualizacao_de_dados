library(tidyverse)
library(sidrar)


dados<-get_sidra(api="/t/5917/n1/all/n3/21/n6/2111300/v/606,608/p/all/c2/all/d/v608%201")

dados<-dados %>%  
  dplyr::rename(D3C = `Trimestre (Código)`) %>%  
  dplyr::rename(D3N = `Trimestre`) %>% 
  dplyr::rename(Territorio = `Brasil, Unidade da Federação e Município`)


dados2<-dados %>% 
  select(`D3C`, `D3N`, `Valor`, `Territorio`, `Sexo`, Valor ) %>% 
  filter(`Sexo`== "Mulheres")

dados2$D3N <-factor(dados2$D3N, levels = unique(dados2$D3N[order(dados2$D3C)]))

ggplot(dados2, aes(x=D3N, y=Valor, colour = fct_reorder2(Territorio, D3N, Valor), group = Territorio))+
  geom_line(size=1.5)+
  xlab('')+ylab('Em %')+
  labs(title='Evolução da População Feminina',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  theme_grey()+ theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

  glimpse(dados)
  View(dados2)