# projet-stat

library(dplyr)
library(DataExplorer)
library(FactoMineR)
library(factoextra)
library(corrplot)

bdd <- read.csv("bdd_2022 (1).csv", sep =";")

dico_var <- read.csv("dico.csv")

toutes_les_rep <- dico_var[c(3:5, 18:21, 37:58, 154:230, 378:508, 698:721 ), c(1:4)]
toutes_les_q <- bdd[c(4, 6:7, 13:22 , 61:85, 142:181 , 231:234)]

#DDER POUR FUSIONNER P7RE M7RE , QAO7 et Q35 

tab_peremereenf <- toutes_les_q[c(35, 68:69)]

tab_peremereenf <- tab_peremereenf %>% 
  rename(Nb_verres_30 = Q35) %>% 
  rename(Conso_mere = QA07A) %>% 
  rename(Conso_pere = QA07B)

toutes_les_q <- toutes_les_q %>%
  mutate(Nb_verres_30 = ifelse(Nb_verres_30[1:2243] == 1, "1 à 2 fois"))
