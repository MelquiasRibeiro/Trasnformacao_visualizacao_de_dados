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

rendimento_total_var_patrim_2017_2018 %>% pivot_longer(
  cols = `total`:`mais de 23850`,
  names_to = c("faixa_de_renda"),
  values_to = "valor") ->rendimento_total_var_patrim_2017_2018

#=============================Rendimento do trabalho========================================================

Rendimento_do_trabalho<-rendimento_total_var_patrim_2017_2018[15:42,]

ggplot(Rendimento_do_trabalho, aes(x = faixa_de_renda, y = valor, fill = origem_rendimento)) + 
  geom_bar(stat="identity", position = "dodge") + 
  labs(x="", y="Rendimento em R$", title="Rendimento total do trabalho em suas categorias") + 
  theme_economist()

#=================================Transferência========================================================


Transferência<-rendimento_total_var_patrim_2017_2018[8:13,]
Transferência<-Transferência %>% arrange(desc(total))


#=================================Aluguel========================================================


Rendimento_de_aluguel <-rendimento_total_var_patrim_2017_2018[14,]
Rendimento_de_aluguel<-Rendimento_de_aluguel%>% arrange(desc(total))

#=================================outros========================================================

Outras_rendas<-rendimento_total_var_patrim_2017_2018[15,]
Outras_rendas<-Outras_rendas %>% arrange(desc(total))
#=================================Não monetários========================================================

Rendimento_não_monetario<-rendimento_total_var_patrim_2017_2018[16,]
Rendimento_não_monetario<-r
Rendimento_não_monetario %>% arrange(desc(total))

View(rendimento_total_var_patrim_2017_2018)
