library(tidyverse)
library(sidrar)


dados<-get_sidra(api="/t/5919/n1/all/n2/2/n3/21/v/608/p/all/c1568/11630/d/v608%201")

dados<-dados %>%  
  dplyr::rename(D3C = `Trimestre (Código)`) %>%  
  dplyr::rename(D3N = `Trimestre`) %>% 
  dplyr::rename(Territorio = `Brasil, Grande Região e Unidade da Federação`)


dados2<-dados %>% select(`D3C`, `D3N`, `Valor`, `Territorio`,Valor )

dados2$D3N <-factor(dados2$D3N, levels = unique(dados2$D3N[order(dados2$D3C)]))

ggplot(dados2, aes(x=D3N, y=Valor, colour = fct_reorder2(Territorio, D3N, Valor), group = Territorio))+
  geom_line(size=1.5)+
  xlab('')+ylab('%')+
  labs(title='Percentual da população com ensino médio completo ou equivalente',
       caption='Fonte: PNAD Contínua/IBGE. Elaboração: OMT-MA.', 
       color="Nível Territorial")+
  geom_point(size=5)+
  geom_text(aes(label=round(dados2$Valor,1)), size=3, 
               hjust=0.5, vjust=-1, colour="#000000")+
  theme_grey()+ theme(axis.text.x=element_text(angle=45, hjust=1))+
  theme(legend.title = element_text(size=10))+
  theme(plot.title = element_text(color = "black", size = 14, face = "bold"))

  glimpse(dados)
  View(dados2)

 #################################### senso 2010 ############################################################ 


dados<-get_sidra(api="/t/2981/n1/all/n2/2/n3/21/n6/2111300/v/1001643/p/all/c12400/108885/c2/allxt/c1/0/c58/0/d/v1001643%202")

dados<-dados %>% dplyr::rename(Territorio = `Brasil, Grande Região, Unidade da Federação e Município`)

dados2<-dados %>% select( `Valor`, `Territorio`, `Sexo`) 
dados2%>%
ggplot(aes(x =`Territorio`, y = Valor, fill =`Sexo`)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="", y="%", title="Percentual do total geral de pessoas de 25 anos ou mais de idade com ensino medio completo") + 
  guides(fill=guide_legend(title= NULL))
  
  

View(dados2)
glimpse(dados2)

#dados<-get_sidra(api="/t/2981/n1/all/n2/2/n3/21/n6/2111300/v/allxp/p/all/c12400/108885/c2/allxt/c1/0/c58/0/d/v1643%200")
#dados<-dados %>% dplyr::rename(Territorio = `Brasil, Grande Região, Unidade da Federação e Município`)
#dados2<-dados %>% select( `Valor`, `Territorio`, `Sexo`) 
#dados2%>%
#ggplot(aes(x =`Territorio`, y = Valor, fill =`Sexo`)) + 
#geom_bar(stat="identity", position = "dodge") + 
#labs(x="", y="%", title="Número absoluto de pessoas com 25 anos ou mais com ensino médio concluído") + 
#guides(fill=guide_legend(title= NULL))
  