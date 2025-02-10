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
