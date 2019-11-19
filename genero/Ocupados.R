getwd()
setwd('genero/')
library(readr)
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
library(sidrar)

OcupadosMasculino <- read_xlsx("Tabela 4093.xlsx",
                                sheet = 3,
                                skip = 5,
                                n_max = 8,
                                col_names = c("Região",
                                               "1º trimestre 2012",
                                               "2º trimestre 2012",
                                               "3º trimestre 2012",
                                               "4º trimestre 2012",
                                               "1º trimestre 2013",
                                               "2º trimestre 2013",
                                               "3º trimestre 2013",
                                               "4º trimestre 2013",
                                               "1º trimestre 2014",
                                               "2º trimestre 2014",
                                               "3º trimestre 2014",
                                               "4º trimestre 2014",
                                               "1º trimestre 2015",
                                               "2º trimestre 2015",
                                               "3º trimestre 2015",
                                               "4º trimestre 2015",
                                               "1º trimestre 2016",
                                               "2º trimestre 2016",
                                               "3º trimestre 2016",
                                               "4º trimestre 2016",
                                               "1º trimestre 2017",
                                               "2º trimestre 2017",
                                               "3º trimestre 2017",
                                               "4º trimestre 2017",
                                               "1º trimestre 2018",
                                               "2º trimestre 2018",
                                               "3º trimestre 2018",
                                               "4º trimestre 2018",
                                               "1º trimestre 2019",
                                               "2º trimestre 2019"),)


OcupadosMasculino <-  OcupadosMasculino %>%
  na.omit() #removendo "NA"

for(i in 1:nrow(OcupadosMasculino)){
  OcupadosMasculino[i,-1] <- gsub('[-]', 0, OcupadosMasculino[i,-1])
}

OcupadosMasculino  %>% pivot_longer(
    cols = `1º trimestre 2012`:`2º trimestre 2019`,
    names_to = c("Período"),
    values_to = "Quantidade") ->OcupadosMasculino 

OcupadosMasculino$Quantidade <- 
as.integer(OcupadosMasculino$Quantidade)
OcupadosMasculino$Período <- 
as.factor(OcupadosMasculino$Período)
OcupadosMasculino$Região <- 
as.factor(OcupadosMasculino$Região )

OcupadosMasculino %>% 
   ggplot(aes(x = Período, 
             y = Quantidade,
             color = Região,
             group = Região,)) +
   geom_line(size=1) +
   geom_point(size=11, fill="white")+
   geom_text(aes(label=round(OcupadosMasculino$Quantidade,1)), size=3, 
               hjust=0.5, vjust=0.5, shape=21, colour="#000000")+
   theme(axis.text.x=element_text(angle=45, hjust=1))+
   labs(x = "Período",
        y = "Quantidade (em mil)",
        title = "Homens de 14 anos ou mais de idade, Ocupados na semana de referência (em mil)",
        caption = "Fonte:  IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua trimestral.
                \nElaboração: OMT-MA")+
   scale_y_continuous(labels =round) 

                            
OcupadosFeminino <- read_xlsx("Tabela 4093.xlsx",
                                sheet = 4,
                                skip = 5,
                                n_max = 8,
                                col_names = c("Região",
                                               "1º trimestre 2012",
                                               "2º trimestre 2012",
                                               "3º trimestre 2012",
                                               "4º trimestre 2012",
                                               "1º trimestre 2013",
                                               "2º trimestre 2013",
                                               "3º trimestre 2013",
                                               "4º trimestre 2013",
                                               "1º trimestre 2014",
                                               "2º trimestre 2014",
                                               "3º trimestre 2014",
                                               "4º trimestre 2014",
                                               "1º trimestre 2015",
                                               "2º trimestre 2015",
                                               "3º trimestre 2015",
                                               "4º trimestre 2015",
                                               "1º trimestre 2016",
                                               "2º trimestre 2016",
                                               "3º trimestre 2016",
                                               "4º trimestre 2016",
                                               "1º trimestre 2017",
                                               "2º trimestre 2017",
                                               "3º trimestre 2017",
                                               "4º trimestre 2017",
                                               "1º trimestre 2018",
                                               "2º trimestre 2018",
                                               "3º trimestre 2018",
                                               "4º trimestre 2018",
                                               "1º trimestre 2019",
                                               "2º trimestre 2019"),)




OcupadosFeminino <-  OcupadosFeminino %>%
  na.omit() #removendo "NA"

for(i in 1:nrow(OcupadosFeminino)){
  OcupadosFeminino[i,-1] <- gsub('[-]', 0, OcupadosFeminino[i,-1])
}

OcupadosFeminino  %>% pivot_longer(
    cols = `1º trimestre 2012`:`2º trimestre 2019`,
    names_to = c("Período"),
    values_to = "Quantidade") ->OcupadosFeminino 

OcupadosFeminino$Quantidade <- 
as.integer(OcupadosFeminino$Quantidade)
OcupadosFeminino$Período <- 
as.factor(OcupadosFeminino$Período)
OcupadosFeminino$Região <- 
as.factor(OcupadosFeminino$Região )

OcupadosFeminino %>% 
   ggplot(aes(x = Período, 
             y = Quantidade,
             color = Região,
             group = Região,)) +
   geom_line(size=1) +
   geom_point(size=11, fill="white")+
   geom_text(aes(label=round(OcupadosFeminino$Quantidade,1)), size=3, 
               hjust=0.5, vjust=0.5, shape=21, colour="#000000")+
   theme(axis.text.x=element_text(angle=45, hjust=1))+
   labs(x = "Período",
        y = "Quantidade (em mil)",
        title = "Mulheres de 14 anos ou mais de idade, Ocupados na semana de referência (em mil)",
        caption = "Fonte:  IBGE - Pesquisa Nacional por Amostra de Domicílios Contínua trimestral.
                \nElaboração: OMT-MA")+
   scale_y_continuous(labels =round)

   
View(OcupadosFeminino)
glimpse(OcupadosFeminino)