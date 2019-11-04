getwd()
setwd('C:/Users/Melquias/Desktop/OMT/POF/') #
library(tidyverse)
library(readxl)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(lme4)
library(plotly)
library(tidyr)

rendimento_total_var_patrim_2017_2018 <- read_xlsx("21MA.xlsx",
                            skip = 9,
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
rendimento_total_var_patrim_2017_2018<-na.omit(rendimento_total_var_patrim_2017_2018)

for(i in 1:nrow(rendimento_total_var_patrim_2017_2018)){
  rendimento_total_var_patrim_2017_2018[i,-1] <- gsub('[-]', 0, rendimento_total_var_patrim_2017_2018[i,-1])
}
 
rendimento_total_var_patrim_2017_2018 <- rendimento_total_var_patrim_2017_2018 %>% 
  select(-total) 

#Deixando na menor fragmentação de dados possivel
rendimento_total_var_patrim_2017_2018 <- rendimento_total_var_patrim_2017_2018 %>% 
  filter(origem_rendimento != "Rendimento total" &
           origem_rendimento != "Rendimento do trabalho" &
           origem_rendimento != "Transferência")

rendimento_total_var_patrim_2017_2018 %>% pivot_longer(
  cols = `até 1908`:`mais de 23850`,
  names_to = c("Classes de Renda"),
  values_to = "Rendimento") ->rendimento_total_var_patrim_2017_2018

  #convertendo a coluna "Rendimento" para doble
rendimento_total_var_patrim_2017_2018$Rendimento <- 
as.double(rendimento_total_var_patrim_2017_2018$Rendimento)

#=============================Rendimento do trabalho========================================================

Rendimento_do_trabalho<-rendimento_total_var_patrim_2017_2018[8:28,]

ggplot(Rendimento_do_trabalho, aes(x =`Classes de Renda`, y = Rendimento, fill = origem_rendimento)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="", y="Rendimento em R$", title="Rendimento total do trabalho em suas categorias") + 
  guides(fill=guide_legend(title= NULL))+
   theme_economist()
#=================================Transferência========================================================


Transferência<-rendimento_total_var_patrim_2017_2018[29:70,]
ggplot(Transferência, aes(x = `Classes de Renda`, y = Rendimento, fill = origem_rendimento)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="", y="Rendimento em R$", title="Rendimento total de transferências em suas categorias") + 
  guides(fill=guide_legend(title= NULL))+
   theme_economist()


#=================================Aluguel========================================================

Rendimento_de_aluguel <-rendimento_total_var_patrim_2017_2018[71:77,]
ggplot(Rendimento_de_aluguel, aes(x = `Classes de Renda`, y = Rendimento, fill = origem_rendimento)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="", y="Rendimento em R$", title="Rendimento total de aluguel em suas categorias") + 
  guides(fill=guide_legend(title= NULL))+
   theme_economist()

#=================================outros========================================================

Outras_rendas<-rendimento_total_var_patrim_2017_2018[78:84,]
ggplot(Outras_rendas, aes(x = `Classes de Renda`, y = Rendimento, fill = origem_rendimento)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="", y="Rendimento em R$", title="Rendimento total de Outras fontes de renda em suas categorias") + 
  guides(fill=guide_legend(title= NULL))+
   theme_economist()

#=================================Não monetários========================================================
Rendimento_não_monetario<-rendimento_total_var_patrim_2017_2018[85:91,]

ggplot(Rendimento_não_monetario, aes(x = `Classes de Renda`, y = Rendimento, fill = origem_rendimento)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="", y="Rendimento em R$", title=" Rendimentos não monetarios em suas categorias") + 
  guides(fill=guide_legend(title= NULL))+
   theme_economist()
#=================================Numero de familias========================================================
Numero_de_familias<-rendimento_total_var_patrim_2017_2018[99:105,] 

ggplot(Numero_de_familias, aes(x = `Classes de Renda`, y = Rendimento, fill = origem_rendimento)) + 
  geom_bar(stat="identity") + 
  labs(x="", y="qunatidade de familias", title=" Numero de familias em suas categorias") + 
  guides(fill=guide_legend(title= NULL))+
   theme_economist()


#=================================Tamnho das familias========================================================
tamanho_de_familias<-rendimento_total_var_patrim_2017_2018[106:112,] 

ggplot(tamanho_de_familias, aes(x = `Classes de Renda`, y = Rendimento, fill = origem_rendimento)) + 
  geom_bar(stat="identity") + 
  labs(x="", y="Numero de pessoas", title=" Tamanho das familias em suas categorias") + 
  guides(fill=guide_legend(title= NULL))+
   theme_economist()

View(Numero_de_familias)