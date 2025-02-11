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


tab_peremereenf <- tab_peremereenf %>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == 1 ~ "0 fois",
    Nb_verres_30 == 2 ~ "1-2 fois",
    Nb_verres_30 == 3 ~ "3-5 fois",
    Nb_verres_30 == 4 ~ "6-9 fois",
    Nb_verres_30 == 5 ~ "10-19 fois",
    Nb_verres_30 == 6 ~ "20-29 fois",
    Nb_verres_30 == 7 ~ "30 fois ou plus",
    TRUE ~ as.character(Nb_verres_30) # Conserve les autres valeurs
  ))


CAROOOOOOOOO 
  Projet Stat - 12/02

Nombre de NA par question : 

{r}
colSums(is.na(q))

Création des différentes tables 

{r}
library(dplyr)
bdd <-read.csv2("bdd_2022.csv")
dico <-read.csv2("dico.csv", sep = ",")
rep <- dico[c(3:5, 18:21, 37:58, 154:230, 378:508, 698:721), c(1:4)] q <- bdd[c(4, 6:7, 13:22 , 61:85, 142:181 , 231:234)]

Création de tables pour le chideux : on modifie le nom des questions mais aussi des réponses pour qu'elles renvoient à une modalité signifiante

{r}
pere_mere_enf <-q[c(35, 68:69)]

pere_mere_enf <- pere_mere_enf %>% 
  rename(Nb_verres_30 = Q35) %>% 
  rename(Conso_mere = QA07A) %>% 
  rename(Conso_pere = QA07B)

pere_mere_enf <- pere_mere_enf %>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == 1 ~ "0 fois",
    TRUE ~ as.character(Nb_verres_30) # Conserve les autres valeurs
  ))

pere_mere_enf <- pere_mere_enf %>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == 2 ~ "1-2 fois",
    TRUE ~ as.character(Nb_verres_30) # Conserve les autres valeurs
  ))
pere_mere_enf <- pere_mere_enf %>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == 3 ~ "3-5 fois",
    TRUE ~ as.character(Nb_verres_30) # Conserve les autres valeurs
  ))

pere_mere_enf <- pere_mere_enf %>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == 4 ~ "6-9 fois",
    TRUE ~ as.character(Nb_verres_30) # Conserve les autres valeurs
  ))
pere_mere_enf <- pere_mere_enf %>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == 5 ~ "10-19 fois",
    TRUE ~ as.character(Nb_verres_30) # Conserve les autres valeurs
  ))
pere_mere_enf <- pere_mere_enf %>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == 6 ~ "20- 29fois",
    TRUE ~ as.character(Nb_verres_30) # Conserve les autres valeurs
  ))
pere_mere_enf <- pere_mere_enf %>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == 7 ~ "30 fois ou plus",
    TRUE ~ as.character(Nb_verres_30) # Conserve les autres valeurs
  ))


Une fois que ce travail est fait, on scinde les tables pour effectuer 2 chideux différents : entre la conso mère/ nbr verres 30 de l'enfant et conso père/nbr verres. 

On a retiré les lignes où la modalité est 6 car en fait ça renvoie à rien... on sait pas quoi en faire et peur que ça fausse les résultats.

{r}
mere_enf <-pere_mere_enf[,1:2] %>%
  filter(Conso_mere!=6)

table_contingence1 <-table(mere_enf)
chisq.test(table_contingence1)

# ici on regroupe des modalités car elles étaient sous RPZ
mere_enf <-mere_enf%>%
  mutate(Nb_verres_30 = case_when(
    Nb_verres_30 == "20- 29fois" ~ "20 fois ou plus",
    Nb_verres_30 == "30 fois ou plus" ~ "20 fois ou plus",
    TRUE ~ Nb_verres_30                       
  ))
table_contingence1 <-table(mere_enf)
chisq.test(table_contingence1)


Question : doit-on regrouper plus et englober 10-19 fois avec ou pas ? 

Même chose avec le père : 

{r}
pere_enf <-pere_mere_enf[c(1,3)]%>%
  filter(Conso_pere !=6)

table_contingence2 <-table(pere_enf)
chisq.test(table_contingence2)

Dans les deux cas, on a une p value très faible donc on en déduit l'existence d'une dépendance. Essai de calcul du V de cramer qui donnait à peu près 0,15 pour la mère.

donc association pas dingue non plus...

ACM 

{r}
library(FactoMineR)
library(factoextra)
library(ggplot2)

# Réalisation de l'ACM : NUL 20% 
acm <- MCA(pere_mere_enf)

# Affichage des résultats
summary(acm) 

flopesque : 20% de l'info résumée par l'axe 1 et le 2... on voit rien je crois...

