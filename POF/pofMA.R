library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(lme4)
library(plotly)

rendimento_total_var_patrim_2017_2018 <- read_xlsx("21MA.xlsx",
                            skip = 9,
                            sheet = 5,
                            col_names = c("origem_rendimento",
                            "total",
                            "ate_1908","Mais_de_1908_a_2862",
                            "Mais_de_2862_a_5_724",
                            "Mais_de_5724_a_9540",
                            "Mais_de_9540_a_14310",
                            "Mais_de_14_310_a_23_850",
                            "Mais_de_23850"),
                            n_max = 22)
rendimento_total_var_patrim_2017_2018<-na.omit(rendimento_total_var_patrim_2017_2018)

Rendimento_do_trabalho<-rendimento_total_var_patrim_2017_2018[4:6,]
Rendimento_do_trabalho<-Rendimento_do_trabalho %>% arrange(desc(total))

Rendimento_do_trabalhoG<-ggplot(Rendimento_do_trabalho, aes(x=origem_rendimento, y=total)) + 
  geom_bar(stat="identity",fill= "#0C41F7") + 
  labs(x="", y="Rendimento em R$", title="Rendimento total do trabalho em suas categorias") + 
  theme_economist()
plotly("Rendimento_do_trabalhoG")


Transferência<-rendimento_total_var_patrim_2017_2018[8:13,]
Transferência<-Transferência %>% arrange(desc(total))


Rendimento_de_aluguel <-rendimento_total_var_patrim_2017_2018[14,]
Rendimento_de_aluguel<-Rendimento_de_aluguel%>% arrange(desc(total))

Outras_rendas<-rendimento_total_var_patrim_2017_2018[15,]
Outras_rendas<-Outras_rendas %>% arrange(desc(total))

Rendimento_não_monetario<-rendimento_total_var_patrim_2017_2018[16,]
Rendimento_não_monetario<-Rendimento_não_monetario %>% arrange(desc(total))

View(rendimento_total_var_patrim_2017_2018)
