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

IDÉE POUR FUSIONNER PERE MERE , QAO7 et Q35 

    tab_peremereenf <- toutes_les_q[c(35, 68:69)]

    tab_peremereenf <- tab_peremereenf %>% 
      rename(Nb_verres_30 = Q35) %>% 
      rename(Conso_mere = QA07A) %>% 
      rename(Conso_pere = QA07B)


CAROOOOOOOOO 
  Projet Stat - 12/02

Nombre de NA par question : 


    colSums(is.na(q))

Création des différentes tables 


    library(dplyr)
    bdd <-read.csv2("bdd_2022.csv")
    dico <-read.csv2("dico.csv", sep = ",")
    rep <- dico[c(3:5, 18:21, 37:58, 154:230, 378:508, 698:721), c(1:4)] q <- bdd[c(4, 6:7, 13:22 , 61:85, 142:181 , 231:234)]

Création de tables pour le chideux : on modifie le nom des questions mais aussi des réponses pour qu'elles renvoient à une modalité signifiante


    pere_mere_enf <-q[c(35, 68:69)]

    pere_mere_enf <- pere_mere_enf %>% 
      rename(Nb_verres_30 = Q35) %>% 
      rename(Conso_mere = QA07A) %>% 
      rename(Conso_pere = QA07B)

    pere_mere_enf <- pere_mere_enf %>%
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

Une fois que ce travail est fait, on scinde les tables pour effectuer 2 chideux différents : entre la conso mère/ nbr verres 30 de l'enfant et conso père/nbr verres. 

On a retiré les lignes où la modalité est 6 car en fait ça renvoie à rien... on sait pas quoi en faire et peur que ça fausse les résultats.


    mere_enf <-pere_mere_enf[,1:2] %>%
      filter(Conso_mere!=6)

    table_contingence1 <-table(mere_enf)
    chisq.test(table_contingence1)

 ici on regroupe des modalités car elles étaient sous RPZ
 
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


    pere_enf <-pere_mere_enf[c(1,3)]%>%
      filter(Conso_pere !=6)

    table_contingence2 <-table(pere_enf)
    chisq.test(table_contingence2)

Dans les deux cas, on a une p value très faible donc on en déduit l'existence d'une dépendance. 

# Essai de calcul du V de cramer qui donnait à peu près 0,15 pour la mère.

donc association pas dingue non plus...

TEST KHIDEUX : conso tabac parents / enfant
#1re clope / conso pere-mere
#Q27B age col 25 / parent QA08A QA08B

    pere_mere_age <-q[c(25, 70:71)]

    pere_mere_age <- pere_mere_age %>% 
      mutate(Age_1re_cig = Q27B) %>% 
      mutate(cig_mere = QA08A) %>% 
      mutate(cig_pere = QA08B) 

    pere_mere_age <- pere_mere_age %>%
      select(-all_of(c("Q27B","QA08A","QA08B")))

    age_cig_mere <-pere_mere_age[,1:2] %>%
      filter(cig_mere!=4)

    table_contingence6 <-table(age_cig_mere)
    chisq.test(table_contingence6)


# ici on regroupe des modalités car elles étaient sous RPZ

    age_cig_mere <- age_cig_mere %>%
      mutate(Age_1re_cig = case_when(
        Age_1re_cig < 12 ~ "avant 12 ans",
        Age_1re_cig >= 12 & Age_1re_cig <= 14 ~ "entre 12 et 14 ans",
        Age_1re_cig > 14 ~ "après 14 ans"
      ))
    table_contingence6 <-table(age_cig_mere)
    chisq.test(table_contingence6)

--> khideux égal à 0.22 environ : pas de lien. 

TEST KHIDEUX : conso cig père / age 1re cig enfant
#conso cig pere / enfant
pere_mere_age <-q[c(25, 70:71)]

    pere_mere_age <- pere_mere_age %>% 
      mutate(Age_1re_cig = Q27B) %>% 
      mutate(cig_mere = QA08A) %>% 
      mutate(cig_pere = QA08B) 

    pere_mere_age <- pere_mere_age %>%
      select(-all_of(c("Q27B","QA08A","QA08B")))

    age_cig_pere <-pere_mere_age[c(1,3)] %>%
      filter(cig_pere!=4)

    table_contingence7 <-table(age_cig_pere)
    chisq.test(table_contingence7)

#modalités pas encore regroupées mais pvalue = 0.01 : il existerait un lien

# ACM 


    library(FactoMineR)
    library(factoextra)
    library(ggplot2)

## Réalisation de l'ACM 
NUL 20% 

    acm <- MCA(pere_mere_enf)

## Affichage des résultats

    summary(acm) 

échec: 20% de l'info résumée par l'axe 1 et le 2... on voit rien je crois...


POUR LA CONSO DE CIGARETTES 

    cig_parent_enfant<- toutes_les_q[c(24 , 70:71)]

    cig_parent_enfant <- cig_parent_enfant%>% 
      rename(Nb_cig_30 = Q27A) %>% 
      rename(Conso_mere = QA08B) %>% 
      rename(Conso_pere = QA08A)

    cig_parent_enfant <- cig_parent_enfant %>%
      mutate(Nb_cig_30 = case_when(
        Nb_cig_30 == 1 ~ "Aucune",
        Nb_cig_30 == 2 ~ "Moins d'une par semaine",
        Nb_cig_30 == 3 ~ "Moins d'une par semaine",
        Nb_cig_30 == 4 ~ "Entre 1 à 5 par jour",
        Nb_cig_30 == 5 ~ "Entre 6 et 20 par jour",
        Nb_cig_30 == 6 ~ "Entre 6 et 20 par jour",
        Nb_cig_30 == 7 ~ "Plus de 20 par jour",
        TRUE ~ as.character(Nb_cig_30) # Conserve les autres valeurs
      ))

    table_cig_mere <- cig_parent_enfant[c(1,3)]
    table_contingence_cig <-table(table_cig_mere)
    table_cig_mere
    chisq.test(table_contingence_cig)

    table_cig_pere<-cig_parent_enfant[c(1,2)]
    table_contingence_cig2<-table(table_cig_pere)
    table_cig_pere
    chisq.test(table_contingence_cig2)

sans les NA 

    q_sans_les_qcm <- toutes_les_q[c(4:14, 25, 27, 30:33, 35:37, 39)]

## Convertir le résultat en data.frame pour ggplot

    na_count <- sapply(q_sans_les_qcm, function(x) sum(is.na(x)))

    na_count_df <- data.frame(
      variable = names(na_count),
      na_count = na_count
    )

## Créer le graphique

      ggplot(na_count_df, aes(x = variable, y = na_count, fill = variable)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Nombre de NAs par variable", x = "Variable", y = "Nombre de NAs") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

## table pour le profil des parents 

j'isole les variables qui m'intéresse

    profil_parents <- bdd[c("Q14A", "Q14B","q15a","q15b", "QA07A","QA07B", "QA08A", "QA08B", "QA09", "QA11A", "QA11B", "QA11C", "QA11D")]

j'enlève les NA

    profil_parents_sans_na <- profil_parents %>% filter(!is.na(Q14A) & !is.na(Q14B))

  ### Calcul des pourcentages pour Q14A et Q14B

    q14a_counts <- profil_parents_sans_na %>%
      count(Q14A) %>%
      mutate(percentage = n / sum(n) * 100)

    q14b_counts <- profil_parents_sans_na %>%
      count(Q14B) %>%
      mutate(percentage = n / sum(n) * 100)

  ### Dictionnaire des significations

    significations <- c(
      "1" = "travaille",
      "2" = "chômage",
      "3" = "au foyer",
      "4" = "invalide",
      "5" = "retraité",
      "6" = "je ne sais pas",
      "7" = "décédé"
    )

  ### Créer l'histogramme pour Q14A

    ggplot(q14a_counts, aes(x = factor(Q14A), y = percentage)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_text(aes(label = paste0(round(percentage, 1), "%"), 
                vjust = -1.5)) +
      scale_x_discrete(name = "Valeurs") +
      ylab("Pourcentage (%)") +
      ggtitle("situation du père") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(data = q14a_counts, 
            aes(x = factor(Q14A), y = percentage + 2, 
                label = significations[as.character(Q14A)]), 
            size = 3, angle = 0, hjust = 0.5)

  ### Créer l'histogramme pour Q14B
  
      ggplot(q14b_counts, aes(x = factor(Q14B), y = percentage)) +
      geom_bar(stat = "identity", fill = "lightgreen") +
      geom_text(aes(label = paste0(round(percentage, 1), "%"), 
                vjust = -1.5)) +
      scale_x_discrete(name = "Valeurs") +
      ylab("Pourcentage (%)") +
      ggtitle("situation de la mère") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(data = q14b_counts, 
            aes(x = factor(Q14B), y = percentage + 2, 
                label = significations[as.character(Q14B)]), 
            size = 3, angle = 0, hjust = 0.5)

  ### Calcul des pourcentages pour Q15A et Q15B

    q15a_counts <- profil_parents_sans_na %>%
      count(q15a) %>%
      mutate(percentage = n / sum(n) * 100)

    q15b_counts <- profil_parents_sans_na %>%
      count(q15b) %>%
      mutate(percentage = n / sum(n) * 100)

  ### Dictionnaire des significations

    significations <- c(
      "1" = "Agriculteur.e",
      "2" = "artisan.e, commerçant.e, chef.fe petite entreprise",
      "3" = "chef.fe d'entreprise",
      "4" = "cadre, professeur.e, profession libérale",
      "5" = "profession intermédiaire",
      "6" = "employé.e",
      "7" = "ouvrier.e",
      "8" = "sans profession",
      "9" = "je ne sais pas"
    )

  ### Créer l'histogramme pour Q15A

    ggplot(q15a_counts, aes(x = factor(q15a), y = percentage)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      geom_text(aes(label = paste0(round(percentage, 1), "%"), 
                vjust = -1.5)) +
      scale_x_discrete(name = "Valeurs") +
      ylab("Pourcentage (%)") +
      ggtitle("profession du père") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(data = q15a_counts, 
            aes(x = factor(q15a), y = percentage + 2, 
                label = significations[as.character(q15a)]), 
            size = 3, angle = 0, hjust = 0.5)

  ### Créer l'histogramme pour Q15B

    ggplot(q15b_counts, aes(x = factor(q15b), y = percentage)) +
      geom_bar(stat = "identity", fill = "lightpink") +
      geom_text(aes(label = paste0(round(percentage, 1), "%"), 
                vjust = -3)) +
      scale_x_discrete(name = "Valeurs") +
      ylab("Pourcentage (%)") +
      ggtitle("profession de la mère") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_text(data = q15b_counts, 
            aes(x = factor(q15b), y = percentage + 2, 
                label = significations[as.character(q15b)]), 
            size = 3, angle = 0, hjust = 0.5)


#stat univariée : age moyen 

    prof_enfant <- dico_var[c(3:5, 18:25, 31:58, 154:181, 203:233, 378:297 ), c(1:4)] 
    prof_enfantbdd <- bdd[c(4, 6:22, 61:72, 79:87 , 142:145)]


    prof_enfant_dico <- bdd[c(4)]
    prof_enfant_bdd <- prof_enfant[ c(1:3), c(1:3)]

    compte_occurrences <- prof_enfant_dico %>%
      count(Q01) %>% 
      rename( Modalite = Q01) %>% 
      rename(nb_occ = n)

    resultat_age <- merge(compte_occurrences, prof_enfant_bdd, by = "Modalite")

    resultat_age_df <- as.data.frame(resultat_age)

    ggplot(resultat_age_df, aes(x = Modalite, y = nb_occ)) + 
      geom_bar(stat = "identity", fill = "blue") + 
      labs(title = "Nombre d'occurrences pour chaque modalité", 
       x = "Modalité", 
       y = "Nombre d'occurrence") +
      theme_minimal()


# Calcul des pourcentages

    resultat_age_df <- resultat_age_df %>%
      mutate(pourcentage = nb_occ / sum(nb_occ) * 100)

creation histogramme pourcentagez

    ggplot(resultat_age_df, aes(x = Modalite, y = pourcentage)) + 
      geom_bar(stat = "identity", fill = "blue") + 
      labs(title = "Distribution en pourcentage des modalités", 
       x = "Modalité", 
       y = "Pourcentage (%)") +
      theme_minimal()


Création du graphique en camembert
    
    ggplot(resultat_age_df, aes(x = "", y = nb_occ, fill = Modalite)) + 
    geom_bar(stat = "identity", width = 1) + 
    coord_polar(theta = "y") + 
    geom_text(aes(label = paste0(round(pourcentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) + 
    labs(title = "Répartition des années", 
       x = "Choix", 
       y = "Nombre d'occurrences") +
    theme_void()

questions sur l'alcool 

    alcool_bdd <- bdd[c(79:85)]
    alcool_dico <- dico_var[c(203:230), c(1:4)]

Question sur si la personne a déja été ivre 

    ivre_ <- alcool_dico[c(19:20), c(1:4)]
    occu_bdd <- alcool_bdd %>% 
      select(5)%>%
      filter(!is.na(Q36))


    ivre_occu <- occu_bdd %>% 
      count(Q36) %>% 
      rename(oui_non = Q36) %>% 
      rename(Modalite = n)

    ivre_occu <- ivre_occu %>%
      mutate(oui_non = ifelse(oui_non == "1", "Oui", oui_non)) %>% 
      mutate(oui_non = ifelse(oui_non == "2", "Non", oui_non))


    ggplot(ivre_occu, aes(x = oui_non, y = Modalite, fill = oui_non)) +
      geom_bar(stat = "identity") +
      labs(title = "Est ce que le jeune a déjà été ivre dans sa vie",
       x = "Réponse",
       y = "Nombre d'occurrences") +
      theme_minimal() +
      scale_fill_manual(values = c("Oui" = "lightblue", "Non" = "lightgreen"))

    library(ggplot2)
    library(dplyr)

Calculer les pourcentages

    ivre_occu <- ivre_occu %>%
      mutate(pourcentage = Modalite / sum(Modalite) * 100)

Création de l'histogramme des pourcentages
    
      ggplot(ivre_occu, aes(x = oui_non, y = pourcentage, fill = oui_non)) +
      geom_bar(stat = "identity") +
      labs(title = "Pourcentage des jeunes ayant déjà été ivres",
       x = "Réponse",
       y = "Pourcentage (%)") +
    theme_minimal() +
    scale_fill_manual(values = c("Oui" = "lightblue", "Non" = "lightgreen")) +
    geom_text(aes(label = paste0(round(pourcentage, 1), "%")), vjust = -0.5)  
    
  Ajout des pourcentages sur les barres

Créer le graphique en camembert avec les pourcentages
  
    ggplot(ivre_occu, aes(x = "", y = Modalite, fill = oui_non)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Est-ce que le jeune a déjà été ivre dans sa vie",
       x = NULL,  # Enlever le label de l'axe x
       y = NULL) +  # Enlever le label de l'axe y
      theme_void() +  # Pour enlever les axes et le fond
      scale_fill_manual(values = c("Oui" = "lightblue", "Non" = "lightgreen")) +
      geom_text(aes(label = paste0(round(pourcentage, 1), "%")),  # Afficher les pourcentages
            position = position_stack(vjust = 0.5),  # Positionner le texte au centre de chaque segment
            color = "white",  # Couleur du texte
            size = 6)


age de la première fois où vous avez été ivre
table pour le profil des parents 

j'isole la variable qui m'interesse
  
    age_ivresse <- bdd[c("Q36A")]

j'enlève les NA

    age_ivresse_sans_NA <- age_ivresse %>% filter(!is.na(Q36A))

je compte les modalités
    
    compte_modalites <- age_ivresse_sans_NA %>%
      count(Q36A) %>%
      rename(Modalite = Q36A, Nb_Occurrences = n)

on corrige les valeurs impossibles 

    compte_modalites <- compte_modalites %>%
      filter(Modalite >5 & Modalite <= 19)

histogramme

    ggplot(compte_modalites, aes(x = Modalite, y = Nb_Occurrences, fill = factor(Modalite))) +
      geom_bar(stat = "identity") +  
      labs(title = "Histogramme des modalités filtrées et de leur nombre d'occurrences",
           x = "age", 
           y = "Nombre d'occurrences") +
      theme_minimal()

pourcentage age ivresse
  
    compte_modalites <- compte_modalites %>%
      mutate(pourcentage = (Nb_Occurrences / sum(Nb_Occurrences)) * 100)

Création de l'histogramme avec les pourcentages

    ggplot(compte_modalites, aes(x = Modalite, y = pourcentage, fill = factor(Modalite))) +
      geom_bar(stat = "identity") +  
      labs(title = "Histogramme des modalités filtrées et de leur pourcentage",
           x = "age", 
           y = "Pourcentage (%)") +
      theme_minimal() +
      geom_text(aes(label = paste0(round(pourcentage, 1), "%")), vjust = -0.5)  # Affiche les pourcentages sur les barres

on-t-il déjà fumé 

    question_tabac<-bdd[c("Q26A1","Q26B1","Q26C1","Q26D1","Q26E1")]

on veut regrouper les questions

    question_tabac_transformed <- question_tabac %>%
      mutate(across(everything(), ~ case_when(
        . == "1" ~ 0,       # Transforme "1" en 0
        . %in% c("2", "3") ~ 1,  # Transforme "2" et "3" en 1
        TRUE ~ as.numeric(.)  )))%>%
      mutate(somme_tabac = rowSums(., na.rm = TRUE))

    question_tabac_transformed <- question_tabac_transformed %>%
      mutate(consommation_tabac = case_when(
        somme_tabac == 0 ~ "pas de consommation de tabac",  # Si somme_tabac est égal à 0
        somme_tabac >= 1 ~ "a déjà consommé du tabac"       # Si somme_tabac est supérieur ou égal à 1
      ))
      
histogramme

    ggplot(question_tabac_transformed, aes(x = consommation_tabac, fill = consommation_tabac)) +
      geom_bar(stat = "count", show.legend = FALSE) + # stat = "count" pour compter les occurrences
      labs(title = "Répartition de la consommation de tabac", 
           x = "Consommation de tabac", 
           y = "Nombre d'individus") +
      scale_fill_manual(values = c("pas de consommation de tabac" = "lightgreen", 
                               "a déjà consommé du tabac" = "lightblue")) + 
      theme_minimal()

pourcentage

    question_tabac_transformed <- question_tabac_transformed %>%
      mutate(consommation_tabac = factor(consommation_tabac, levels = c("pas de consommation de tabac", "a déjà consommé du tabac")))

    question_tabac_pourcentage <- question_tabac_transformed %>%
      mutate(consommation_tabac = case_when(
        somme_tabac == 0 ~ "pas de consommation de tabac",  # Si somme_tabac est égal à 0
    somme_tabac >= 1 ~ "a déjà consommé du tabac"       # Si somme_tabac est supérieur ou égal à 1
      )) %>%
      count(consommation_tabac) %>%  # Compter le nombre d'occurrences de chaque modalité
      mutate(pourcentage = n / sum(n) * 100) 

Créer l'histogramme avec pourcentages

    ggplot(question_tabac_pourcentage, aes(x = consommation_tabac, y = pourcentage, fill = consommation_tabac)) +
      geom_bar(stat = "identity", show.legend = FALSE) +  # Utilisez stat = "identity" pour afficher les pourcentages
      labs(title = "Répartition de la consommation de tabac (%)", 
       x = "Consommation de tabac", 
       y = "Pourcentage d'individus") +
      scale_fill_manual(values = c("pas de consommation de tabac" = "lightgreen", 
                               "a déjà consommé du tabac" = "lightblue")) + 
      theme_minimal() +
      geom_text(aes(label = paste0(round(pourcentage, 1), "%")), 
            vjust = -0.5, size = 5)  
            
  Afficher les pourcentages sur les barres

