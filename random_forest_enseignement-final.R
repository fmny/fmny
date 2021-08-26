
#lien
#https://thinkr.fr/premiers-pas-en-machine-learning-avec-r-volume-4-random-forest/


#Principe de RandomForest:
#utiliser un grand nombre d arbres de decision construits chacun avec un sous-echantillon different 
#de l ensemble d apprentissage, et pour chaque construction d'arbre, la decision à un noeud est fait 
#en fonction d un sous-ensemble de variables tirées au hasard.
#puis, on utilise l ensemble des arbres de décision produits pour faire la prediction, 
#avec un vote a la majorite (pour de la classification, variable prédite de type facteur), 
#ou une moyenne (pour de la regression, variable predite de type numerique).


#lien data gouv (education nationale)
#https://data.education.gouv.fr/explore/dataset/fr-en-effectifs-des-personnels-des-ecoles-et-etablissements-du-2nd-degre/

#import du fichier CSv car l'autre pose des problèmes d'accents
#Lien qui est naturellement à actualiser

setwd("D:\\R_new\\random forest\\data-enseignement")
#verification
#getwd()
educ <- read.csv(file=".\\fr-en-effectifs-des-personnels-des-ecoles-et-etablissements-du-2nd-degre-ss-accent.csv",sep=';')

install.packages("dplyr")
install.packages("tidyverse")
install.packages("tidy")


library(dplyr)      # pour mutate_at et %>%
library(tidyverse)
library(tidyr)      # pour unnest et separate

#dplyr contient les opérateurs %>% qui permettent le data wrangling (opération sur la BDD)

educ_small <- educ %>%
  filter(Academie == "RENNES") %>%
  select(Type.etablissement,Secteur.enseignement,
         Groupe.de.personnels, Titulaire, Sexe, Borne.inferieure.de.la.tranche.age, Nombre.agents, 
         Code.region)


sum(educ_small$Nombre.agents)
#[1] 43510    #sur le site data ancienne
#[1] 87123    #résultat FM

nrow(educ_small)
#[1] 16248    #sur le site data ancienne
#[1] 32107    #résultat FM



#ici test_dup2 est un de type tibble (15*4)
#donnée tidy
#Les principes d'un jeu de données tidy sont les suivants :
#1)  chaque variable est une colonne
#2) chaque observation est une ligne
#3) chaque type d'observation est dans une table différente


#code d'origine
educ_small <- educ_small %>% 
  mutate(Idx = 1:n()) %>% 
  group_by(Idx) %>%
  mutate( Agent = list(rep( Sexe, Nombre.d.agents) ) ) %>%
  unnest() %>%
  ungroup()
select(-Idx)
#Fin code d'origine




#on refait sur les data d'origine
educ_small2<-educ_small
a<-list(rep(educ_small$Sexe, educ_small$Nombre.agents )) #fonctionne bien
b<-list(rep(educ_small$Secteur.enseignement, educ_small$Nombre.agents ))
c<-list(rep(educ_small$Titulaire, educ_small$Nombre.agents ))
d<-list(rep(educ_small$Borne.inferieure.de.la.tranche.age, educ_small$Nombre.agents ))
e<-list(rep(educ_small$Type.etablissement, educ_small$Nombre.agents ))
f<-list(rep(educ_small$Groupe.de.personnels, educ_small$Nombre.agents ))

  


test5<-as.data.frame(c(a,b,c,d,e,f))
colnames(test5)<-c("Sexe","Secteur_enseignement","Titulaire","Borne.inferieure.de.la.tranche.age","Type.etablissementé","Groupe.de.personnels")

#J'y arrive mais c'est un peu laborieux



#??unnest
#If you have a list-column, this makes each element of the list its own row. unnest() 
#can handle list-columns that can atomic vectors, lists, 
#or data frames (but not a mixture of the different types).


#Train and test
#Same old same old : on crée son modèle sur une partie de ses données, 
#et on effectue les prédictions sur une partie test. 
#Ici, nous allons essayer de prédire la variable binaire Titulaire.

#-code d origine
#train <- educ_small2 %>% sample_frac(0.8)
#test <- anti_join(educ_small, train)

#library(randomForest)
#(model <- randomForest(Titulaire ~ ., data = train, ntree = 100, na.action = na.omit))
# Peut prendre un peu de temps ????

#Result
#Call:
#  randomForest(formula = Titulaire ~ ., data = train, ntree = 500,      na.action = na.omit) 
#Type of random forest: classification
#Number of trees: 500
#No. of variables tried at each split: 2

#OOB estimate of  error rate: 6.54%
#Confusion matrix:
#  Contractuel Titulaire class.error
#Contractuel       10430       803  0.07148580
#Titulaire           857     13275  0.06064251



train5 <- test5 %>% sample_frac(0.8)

#je ne comprends pas trop le anti-join
#je pense que anti-join prend les 20% restant de test5
test6 <- anti_join(test5, train5)


library(randomForest)
model <- randomForest(Titulaire ~ ., data = train5, ntree = 100, na.action = na.omit)


#Fonctionne lorsqu'on passe de 500 à 100 Tree


hist(model$oob.times)

model$votes[1:10,]

model$importance

varImpPlot(model)

#Résultat sur 5 variables 
#"Sexe","Secteur_enseignement",
#"Borne.inferieure.de.la.tranche.age"
#"Type.etablissementé"
#"Groupe.de.personnels"
#variable à prédire: "Titulaire"

#library(randomForest)
# (model <- randomForest(Titulaire ~ ., data = train_test4, ntree = 50, na.action = na.omit))

#Call:
#  randomForest(formula = Titulaire ~ ., data = train_test4, ntree = 50,      na.action = na.omit) 
#Type of random forest: classification
#Number of trees: 50
#No. of variables tried at each split: 2


#Out Of Bag estimate of error rate
#Comme vous le savez, chaque arbre est entraîné sur une fraction des data, 
#qui est considérée comme « in-bag ». Ce qui permet à chaque arbre, une fois construit, 
#d'estimer son taux d'erreur sur les données qu'il a laissé « out-of-bag » : 
#plus ce taux est faible, plus le modèle est juste. 
#Ce chiffre à lui seul pourrait servir d'indicateur de performance du modèle.

#Vous pouvez accéder au nombre de fois qu'un individu a été laissé « out of bag » 
#en avec model$oob.times. Si nous dressons un histogramme :



#OOB estimate of  error rate: 5.69%
#Confusion matrix:
#  Contractuel Titulaire class.error
#Contractuel       28490      3164  0.09995577
#Titulaire           801     37243  0.02105457


#model$importance
#MeanDecreaseGini
#Sexe                                       42.37304
#Secteur_enseignement                    23317.60988
#Borne.inferieure.de.la.tranche.age       2164.31646
#Type.etablissementé                       781.34073
#Groupe.de.personnels                     1117.95426


############################################################
#Utilisation de notre randomForest pour faire une prédiction
############################################################
test5$predicted <- predict(model, test5)


table(test5$predicted, test5$Titulaire)


test5$predicted

write.csv(test5$predicted,file=".\\result_ens.csv")
write.csv(test5,file=".\\data_test5.csv")

print(model)


############################################################################################
#on recommence la prediction sur l'academie de toulouse et on retire la colonne titulaire


educ_toulouse<-read.csv(file=".\\test-personnel-enseignement-Toulouse2.csv",sep=';')


#on refait sur les data d'origine
educ_toul<-educ_toulouse
a1<-list(rep(educ_toul$Sexe, educ_toul$Nombre.agents )) #fonctionne bien
b1<-list(rep(educ_toul$Secteur.enseignement, educ_toul$Nombre.agents ))
c1<-list(rep(educ_toul$Titulaire, educ_toul$Nombre.agents ))
d1<-list(rep(educ_toul$Borne.inferieure.de.la.tranche.age, educ_toul$Nombre.agents ))
e1<-list(rep(educ_toul$Type.etablissement, educ_toul$Nombre.agents ))
f1<-list(rep(educ_toul$Groupe.de.personnels, educ_toul$Nombre.agents ))




test_toul<-as.data.frame(c(a1,b1,c1,d1,e1,f1))
colnames(test_toul)<-c("Sexe","Secteur_enseignement","Titulaire","Borne.inferieure.de.la.tranche.age","Type.etablissementé","Groupe.de.personnels")


#je ne comprends pas bien comment utiliser ma randomForest pour les nouvelles données



train_toul <- test_toul %>% sample_frac(0.8)

test_toul2 <- anti_join(test_toul, train_toul)

library(randomForest)
model <- randomForest(Titulaire ~ ., data = train_toul, ntree = 100, na.action = na.omit)


#Utilisation de notre randomForest pour faire une prédiction
test_toul$predicted <- predict(model, test_toul)

write.csv(test_toul,file=".\\test_toul.csv")




