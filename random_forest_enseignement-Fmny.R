################################################################
#Utilisation de RandomForest en vue de faire de la prevision:
###############################################################

#lien
#https://thinkr.fr/premiers-pas-en-machine-learning-avec-r-volume-4-random-forest/


#import du fichier CSv sans accent 

#Chemin ? modifier
setwd("C:\\Users\\Francis\\R_new\\random forest\\data-enseignement")
#verification
#getwd()
educ <- read.csv(file=".\\fr-en-effectifs-des-personnels-des-ecoles-et-etablissements-du-2nd-degre-ss-accent.csv",sep=';')

#install.packages("Rtools")
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("tidy")
#install.packages("caret")


library(dplyr)      # pour mutate_at et %>%
library(tidyverse)
library(tidyr)      # pour unnest et separate
library(caret)
#dplyr contient les op?rateurs %>% qui permettent le data wrangling (operation sur la BDD)




educ_small <- educ %>%
  filter(Academie == "RENNES") %>%##filtre par academie
  select(Type.etablissement, Secteur.enseignement,Groupe.de.personnels, Titulaire, Sexe, Borne.inferieure.de.la.tranche.age, Nombre.agents,Code.region)##selectionne les colonnes qu'on veut





#faire tourner ces operations 1 par 1 pour comprendre le data wrangling

educ_small_test <- educ_small %>% 
  mutate(Idx = 1:n()) %>%
  group_by(Idx) %>% #ne sert ? rien
mutate( Agent = list(rep( Sexe, Nombre.agents) ) ) %>%
unnest() %>% # explose la base de maniere ? avoir 87123 ligne (cad le nb d agent)
ungroup() %>% #ne sert a rien
select(-Idx) #retire la colonne Idx


sum(educ_small$Nombre.agents)
#[1] 43510    #sur le site data ancienne
#[1] 87123    #resultat FM

nrow(educ_small)
#[1] 16248    #sur le site data ancienne
#[1] 32107    #resultat FM



sum(educ_small_test$Nombre.agents)
#611857 (ici, il faut modifier cette colonne )

#on remplace Nombre.agents par 1 dans la colonne
educ_small_test$Nombre.agents<-1


#Mise en facteur des variables pour random forest


educ_small2 <- educ_small
##avec ou sans IDX a la fin
educ_small2 <- educ_small %>% mutate(Idx = 1:n()) %>%group_by(Idx) %>%mutate( Agent = list(rep( Sexe, Nombre.agents) ) )%>%unnest() %>%ungroup()
##educ_small2 <- educ_small2 %>% mutate(Idx = 1:n()) %>%group_by(Idx) %>%mutate( Agent = list(rep( Sexe, Nombre.agents) ) )%>%unnest() %>%ungroup()%>%select(-Idx)


#Var quanti et var quali
educ_small2$Type.etablissement   =  factor(educ_small2$Type.etablissement)
educ_small2$Secteur.enseignement =  factor(educ_small2$Secteur.enseignement)
educ_small2$Groupe.de.personnels =  factor(educ_small2$Groupe.de.personnels)
educ_small2$Titulaire =  factor(educ_small2$Titulaire)
educ_small2$Sexe      =  factor(educ_small2$Sexe)
educ_small2$Borne.inferieure.de.la.tranche.age = factor(educ_small2$Borne.inferieure.de.la.tranche.age)
educ_small2$Code.region=factor(educ_small2$Code.region)
educ_small2$Agent=factor(educ_small2$Agent)

#attention ici, le fait d avoir d avoir demultiplie la base pour avoir un enregistrement
#par enseignant a cree des doublons dans la colonne Idx
#de sortes que anti_join ne fonctionnait pas

educ_small2$Idx<-1:nrow(educ_small2)


set.seed(2811)

train <- educ_small2 %>% sample_frac(0.8) #prend 80% de la base pour le training
nrow(train)
#69698
test <- anti_join(educ_small2, train) #prend les 20% restant
nrow(test)
#17425/87123=0.2000046


library(randomForest)
set.seed(2811)
model <- randomForest(Titulaire ~ ., data = train, ntree = 100, na.action = na.omit)

#500 est un peu long, je prends 100 arbres

hist(model$oob.times)
model$votes[1:10,]
model$importance
varImpPlot(model)

############################################################
#Utilisation de notre randomForest pour faire une prediction
############################################################

set.seed(2811)
test$predicted <- predict(model, test)

table(test$predicted, test$Titulaire)

summary(test$predicted)
summary(test$Titulaire)
nrow(test)

#summary(test$predicted)
#Contractuel   Titulaire        NA's 
#       7776        9485         164 
# summary(test$Titulaire)
#Contractuel   Titulaire 
#       7847        9578 


summary(test$predicted)
summary(test$Titulaire)
nrow(test)

#Fichier des résulats avec la colonne de resultat des previsions titulaire et contractuel
#write.csv(test$predicted,file=".\\temp.csv")
write.csv(test,file=".\\test.csv")

library(caret)
conf <- confusionMatrix(data = test$predicted, reference = test$Titulaire)

conf$byClass["Sensitivity"] #taux de vrais positifs



#resultats refaits 164 NA dans la colonne region
#nrow(test)-164 = 17261


#conf$byClass["Sensitivity"] #taux de vrais positifs
#Sensitivity 
#0.9518874      #7439/7815
#conf$byClass["Specificity"] #taux de vrais negatifs
#Specificity 
#0.9643235      #9109/9446

#conf
#              Reference
#Prediction    Contractuel Titulaire
#Contractuel        7439       337
#Titulaire           376      9109

#lecture du tableau
# on a donc 7439 Contractuel bien predit et 337 mal predit (etaient des titulaires)
#les bonnes predictions sont sur la diagonale