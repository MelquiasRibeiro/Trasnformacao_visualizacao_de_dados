library(tidyverse)
library(sidrar)

#===============================Na força de trabalho===================================================
dados<- get_sidra(api= "/t/4093/n1/all/n2/2/n3/21/v/4088/p/first%2030/c2/allxt") #captando os dados da api
dados<-dados %>%  
  dplyr::rename(D3C = `Trimestre (Código)`) %>%  
  dplyr::rename(D3N = `Trimestre`) %>% 
  dplyr::rename(Territorio = `Brasil, Grande Região e Unidade da Federação`) #renomeando as colunas 


dados2<-dados %>% 
  select(`D3C`, `D3N`, `Valor`, `Territorio`, `Sexo`,Valor ) %>% 
  filter(`Sexo`== "Mulheres")  #filrando as colunas interessantes para nós e filtrando apenas as mulheres 

dados2$D3N <-factor(dados2$D3N, levels = unique(dados2$D3N[order(dados2$D3C)]))  #ordenando os dados por trismestre

ggplot(dados2, aes(x=D3N, y=Valor, colour = fct_reorder2(Territorio, D3N, Valor), group = Territorio))+
  geom_line(size=1.5)+
  xlab('')+ylab('Em mil')+
  labs(title='Mulheres de 14 anos ou mais de idade, na força de trabalho, na semana de referência (em mil)',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=0.5, shape=21, colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

glimpse(dados2)
View(dados2)

#===============================Ocupadas===================================================

dados<- get_sidra(api= "/t/4093/n1/all/n2/2/n3/21/v/4090/p/first%2030/c2/allxt") #captando os dados da api
dados<-dados %>%  
  dplyr::rename(D3C = `Trimestre (Código)`) %>%  
  dplyr::rename(D3N = `Trimestre`) %>% 
  dplyr::rename(Territorio = `Brasil, Grande Região e Unidade da Federação`) #renomeando as colunas 


dados2<-dados %>% 
  select(`D3C`, `D3N`, `Valor`, `Territorio`, `Sexo`,Valor ) %>% 
  filter(`Sexo`== "Mulheres")  #filrando as colunas interessantes para nós e filtrando apenas as mulheres 

dados2$D3N <-factor(dados2$D3N, levels = unique(dados2$D3N[order(dados2$D3C)]))  #ordenando os dados por trismestre

ggplot(dados2, aes(x=D3N, y=Valor, colour = fct_reorder2(Territorio, D3N, Valor), group = Territorio))+
  geom_line(size=1.5)+
  xlab('')+ylab('Em mil')+
  labs(title='Mulheres de 14 anos ou mais de idade, ocupadas na semana de referência (em mil)',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=0.5, shape=21, colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))


glimpse(dados2)
View(dados2)

#===============================Desocupados===================================================

dados<- get_sidra(api= "/t/4093/n1/all/n2/2/n3/21/v/4092/p/first%2030/c2/allxt") #captando os dados da api
dados<-dados %>%  
  dplyr::rename(D3C = `Trimestre (Código)`) %>%  
  dplyr::rename(D3N = `Trimestre`) %>% 
  dplyr::rename(Territorio = `Brasil, Grande Região e Unidade da Federação`) #renomeando as colunas 


dados2<-dados %>% 
  select(`D3C`, `D3N`, `Valor`, `Territorio`, `Sexo`,Valor ) %>% 
  filter(`Sexo`== "Mulheres")  #filrando as colunas interessantes para nós e filtrando apenas as mulheres 

dados2$D3N <-factor(dados2$D3N, levels = unique(dados2$D3N[order(dados2$D3C)]))  #ordenando os dados por trismestre

ggplot(dados2, aes(x=D3N, y=Valor, colour = fct_reorder2(Territorio, D3N, Valor), group = Territorio))+
  geom_line(size=1.5)+
  xlab('')+ylab('Em mil')+
  labs(title='Mulheres de 14 anos ou mais de idade, desocupadas na semana de referência (em mil)',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=0.5, shape=21, colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

#===============================Fora da força de trabalho===================================================

dados<- get_sidra(api= "/t/4093/n1/all/n2/2/n3/21/v/4094/p/first%2030/c2/allxt") #captando os dados da api
dados<-dados %>%  
  dplyr::rename(D3C = `Trimestre (Código)`) %>%  
  dplyr::rename(D3N = `Trimestre`) %>% 
  dplyr::rename(Territorio = `Brasil, Grande Região e Unidade da Federação`) #renomeando as colunas 


dados2<-dados %>% 
  select(`D3C`, `D3N`, `Valor`, `Territorio`, `Sexo`,Valor ) %>% 
  filter(`Sexo`== "Mulheres")  #filrando as colunas interessantes para nós e filtrando apenas as mulheres 

dados2$D3N <-factor(dados2$D3N, levels = unique(dados2$D3N[order(dados2$D3C)]))  #ordenando os dados por trismestre

ggplot(dados2, aes(x=D3N, y=Valor, colour = fct_reorder2(Territorio, D3N, Valor), group = Territorio))+
  geom_line(size=1.5)+
  xlab('')+ylab('Em mil')+
  labs(title='Mulheres de 14 anos ou mais de idade, fora da força de trabalho na semana de referência (em mil)',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=0.5, shape=21, colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))
