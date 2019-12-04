library(tidyverse)
library(sidrar)

#===============================Na força de trabalho===================================================
dados<- get_sidra(api= "/t/4093/n1/all/n2/2/n3/21/v/4106/p/first%2030/c2/allxt/d/v4106%201") #captando os dados da api
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
  xlab('')+ylab('%')+
  labs(title=' Distribuição percentual das mulheres de 14 anos ou mais de idade, na força de trabalho, na semana de referência ',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=-1,colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold")) 
  
summary(dados2[1:30,3])
summary(dados2[31:60,3])
summary(dados2[61:90,3])

glimpse(dados2)
View(dados2)
#===============================Fora da força de trabalho===================================================

dados<- get_sidra(api="/t/4093/n1/all/n2/2/n3/21/v/4112/p/first%2030/c2/allxt/d/v4112%201") #captando os dados da api
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
  xlab('')+ylab('%')+
  labs(title='Distribuição percentual das mulheres de 14 anos ou mais de idade, fora da força de trabalho na semana de referência (em mil)',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=-1,colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

statBra<-summary(dados2[1:30,3])
statNor<-summary(dados2[31:60,3])
statMa<-summary(dados2[61:90,3])

#===============================Ocupadas===================================================

dados<- get_sidra(api= "/t/4093/n1/all/n2/2/n3/21/v/4108/p/first%2030/c2/allxt/d/v4108%201") #captando os dados da api
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
  xlab('')+ylab('%')+
  labs(title='Distribuição percentual das mulheres de 14 anos ou mais de idade, ocupadas na semana de referência (em mil)',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=-1, colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

statBra<-summary(dados2[1:30,3])
statNor<-summary(dados2[31:60,3])
statMa<-summary(dados2[61:90,3])

glimpse(dados2)
View(statBra)

#===============================Desocupados===================================================

dados<- get_sidra(api= "/t/4093/n1/all/n2/2/n3/21/v/4110/p/first%2030/c2/allxt/d/v4110%201") #captando os dados da api
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
  xlab('')+ylab('%')+
  labs(title='Distribuição percentual das mulheres de 14 anos ou mais de idade, desocupadas na semana de referência (em mil)',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=-1,colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

statBra<-summary(dados2[1:30,3])
statNor<-summary(dados2[31:60,3])
statMa<-summary(dados2[61:90,3])



