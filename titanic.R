
#lien
#http://apiacoa.org/blog/2014/02/initiation-a-rpart.fr.html

#FM
#d apres ce que je comprends les arbres de decision calculent des proba conditionnelles
#ici de deces ou de survie par rapport au sexe par exemple
#ratio de base: 
#titanic 62% de morts
#homme:81% de morts
#Femme:27% de morts
#en fait l arbre calcul sur les données les probas conditionnelles calculees sur l echantillon
#avec comme variables explicatives l ensemble des données de la base mis en parametre
#ex: proba de décès selon le sexe, l age,... ce qui donne l arbre que l'on peut visualiser 
#grace à la fonction prp

#Installation des packages
install.packages("Rtools")
install.packages("rpart")
install.packages("rpart.plot")


library(rpart)
library(rpart.plot)
data(ptitanic)


setwd("C:\\Users\\Francis\\R_new\\titanic")
write.csv(ptitanic,file=".\\Data\\titanic.csv")


read.csv(file ="C:\\Users\\Francis\\R_new\\titanic\\Data\\titanic.csv" )
 
#Les donnees decrivent 1309 passagers selon 6 variables :
#pclass donne la classe tarifaire sur le bateau ;
#survided indique si le passager a survecu ;
#sex donne le genre du passager ;
#age donne l age du passager exprimé en années ;
#sibsp donne le nombre de frères, soeurs, mari ou epouse à bord ;
#parch donne le nombre d'enfants ou de parents à bord.
#Les trois dernières variables sont numériques alors que les trois premieres sont nominales. 
#Il faut bien s'assurer que la représentation en R des variables respecte leur nature. 
#Pour ce faire, on utilise :

lapply(ptitanic,class)
#La fonction lapply() permet d'appliquer une fonction à chaque élément d'une liste.

#Les trois variables nominales sont donc bien des factor. 
#Pour les valeurs numériques, on obtient la classe labelled qui est specifique au package Hmisc. 
#On peut verifier en supprimant cette classe que les variables correspondantes sont bien numériques, 
#comme dans le code suivant :


attr(ptitanic$age,"class") <- NULL
class(ptitanic$age)

#Construction de l arbre complet
#La fonction de construction d un arbre s appelle rpart, comme le package. 
#On l utilise en general avec l interface des formules de R, comme ci-dessous :

ptitanicTree <- rpart(survived~.,data=ptitanic)

#La formule utilisee survived~. indique qu on souhaite predire la variable survived en fonction de toutes les autres. 
#Le principe general est que la (ou les) variable(s) a predire sont à gauche du symbole ~ alors que les variables predictives 
#sont a droite du symbole. Ici, le point . permet d indiquer qu on souhaite utiliser toutes les variables des donnees comme predicteurs 
#sauf les variables a predire (ce qui evite d avoir a ecrire la liste des predicteurs).

#On a utilise ici les parametres par defaut de la fonction rpart, 
#ce qui ne conduit pas toujours a la solution desiree.
#En effet, rpart ne construit en general pas l arbre le plus complet possible, pour des raisons d efficacite. 
#Il est rare en pratique qu un arbre tres profond qui ne réalise aucune erreur de classement sur les donnees d apprentissage soit le plus adapte.
#Il sur-apprend massivement, en general. Il n est donc pas tres utile de construire un tel arbre, puisqu on devra en pratique l elaguer.

#Cependant, il arrive sur des donnees de petite taille que les parametres par defaut de rpart soient trop conservateurs. 
#Par exemple, rpart ne decoupe pas une feuille contenant 20 observations. 
#De meme rpart demande une amelioration relative 
#d au moins 1 % de la qualite d une partition pour effectuer un decoupage. Pour changer ces valeurs, 
#il suffit d utiliser la commande rpart.
#control en precisant les elements a modifier. Le code suivant
#construit un arbre en continuant les decoupages dans les feuilles qui contiennent au moins 5 observations (parametre minsplit) 
#et sans contrainte sur la qualité du découpage (parametre cp mis a 0). 
#L arbre construit de cette facon est assez volumineux et contient 
#65 feuilles.

ptitanicTree <- rpart(survived~.,data=ptitanic,control=rpart.control(minsplit=5,cp=0))
prp(ptitanicTree,extra=1) #graphe complet mais illisible
summary(ptitanicTree)

#Rem FM: minsplit =5, semble signifier que l'on ne redécoupe une branch+e 
#en plusieurs feuilles que si celle-ci possède au moins 5 obs
#par défaut minsplit=20 si l'on ne le précise pas

plotcp(ptitanicTree)
#on voit le nombre de feuilles (65 feuilles) sur le plotcp


#Simplification
#Comme attendu, les performances s améliorent dans un premier temps quand on augmente le nombre de feuilles 
#puis se degradent en raison du sur-apprentissage. On choisit en general la complexité qui minimise 
#l erreur estimee, soit ici 11 feuilles

#0.0047= racine(0.008*0.0028) que l'on peut lire dans la table ptitanicTree$cptable
#Pour obtenir l arbre simplifie, on utilise la fonction prune, comme dans le code suivant :
ptitanicSimple <- prune(ptitanicTree,cp=0.0047)
plotcp(ptitanicSimple) #le graphe optimal a 11 feuilles
prp(ptitanicSimple,extra=1)


ptitanicOptimal <- prune(ptitanicTree,cp=ptitanicTree$cptable[which.min(ptitanicTree$cptable[,4]),1])

plotcp(ptitanicOptimal) #le graphe optimal a 11 feuilles

prp(ptitanicOptimal,extra=1)
#le parametre extra=1 permet d'afficher le comptage des morts/vivants


#fin Titanic avant test sur d'autres jeux de données
###########################################################






#Test des parametres de rpart----
#extra-code
#https://stackoverflow.com/questions/35490949/format-split-labels-in-rpart-plot

data <- ptitanic
data$sibsp <- as.integer(data$sibsp) # just to show that these are integers
data$age <- as.integer(data$age) # just to show that these are integers
tree <- rpart(survived~., data=data, cp=.02)
prp(tree,  fallen.leaves = FALSE, type=4, extra=1, varlen=0, faclen=0, yesno.yshift=-1)

data <- transform(data, sibsp = ordered(sibsp), parch = ordered(parch))
tree <- rpart(survived~., data=data, cp=.02)
prp(tree,  fallen.leaves = FALSE, type=4, extra=1, varlen=0, faclen=0, yesno.yshift=-1)

#Sur ce graphe je trouve le nombre exact (calcul sur excel) par branche (ex: 682 morts de sexe masculin)
#fin google----



#test sur les MII
#peu d interet car les variables ne sont pas tres correlees 
mii<-read.csv(file="Data\\MII2T10E-reduit3.csv",header=T,sep=";",dec=".")

attr(mii$ener1,"class") <- NULL
class(mii$ener1)

miiTree <- rpart(prop~.,data=mii)
plotcp(miiTree)
prp(miiTree,extra=1)

#graphe simplifie
miiSimple <- prune(miiTree,cp=0.058)

prp(miiSimple,extra=1)

#prp(ptitanicTree,extra=1) #graphe illisible
summary(miiTree)

plotcp(miiTree)

#ptitanicOptimal <- prune(ptitanicTree,cp=ptitanicTree$cptable[which.min(ptitanicTree$cptable[,4]),1])
#prp(ptitanicOptimal,extra=1)
#Fin test MII


#Par défaut, la fonction estime les probabilités d'appartenance aux classes pour chaque observation 
#(simplement par le ratio dans la feuille correspondante). Par exemple le code suivant
predict(ptitanicOptimal, ptitanic[1:10,], type="class")

table(ptitanic$survived, predict(ptitanicOptimal, ptitanic, type="class"))

#result
#died survived
#died      752       57
#survived  173      327

tree<-ptitanicOptimal
data<- ptitanic
#Pour obtenir la classe prédite, il suffit d ajouter 
#le parametre type avec la bonne valeur, soit :
predict(tree, data[1:10,], type="class")



#predict(ptitanicOptimal, type="class")

pred<-predict(tree, data, type="class")

write.csv(pred,file=".\\Data\\pred.csv")

#je cherche les probas

tree$predicted <- predict(tree, data)
write.csv(tree$predicted,file=".\\data\\pred2.csv")

#visualisation graphique du résultat
nblines<-nrow(data) #nombre de ligne de la base titanic
x<-1:nblines


for (i in 1 : nblines)
{
  if (data[i,2] == "died" )  x[i]<- -1 
  else x[i]<-1 
  
}

#prediction
pred2<-as.data.frame(pred)
pred_x<-1:nblines

for (i in 1 : nblines)
{
  if (pred[i] == "died" )  pred_x[i]<- -1 
  else pred_x[i]<-1 
  
}

#graphique ininteressant
plot(1:nblines,pred_x-x)
lines(pred_x,col='green')

matplot(1:nblines,x,pred_x,col=rainbow(4),type="o",lty=1)

#exemple de graphiques superposés
x <- 1:100
y <- matrix(rnorm(400),100,4)
matplot(x,y,col=rainbow(4),type="l",lty=1)

#test matplot
x <- 0:50/50
matplot(x, outer(x, 1:8, function(x, k) sin(k*pi * x)),
        ylim = c(-2,2), type = "plobcsSh",
        main= "matplot(,type = \"plobcsSh\" )")






#############################################################
#je recommence avec titanic2 qui est la meme base que titanic 
#sauf que j'ai supprimé la colonne survie/mort

titanic2<-read.csv(file=".\\data\\titanic2.csv",sep=';')
data2<-titanic2
data2 <- transform(data2, sibsp = ordered(sibsp), parch = ordered(parch))

#on obtient bien la prediction sans la colonne survived/died
pred2<-predict(tree, data2, type="class")

write.csv(pred2,file=".\\titanic\\data\\pred_titanic2.csv")

titanic2[1:20,]
data2[1:20,]


#comparaison des method tree de la class rpart avec le bayesien naif, la fonction
#naiveBayes de la classe library(e1071)
#naiveBayes est simle a implementer lorsque toutes les variables expliquees sont qualitatives ou toutes sont quantitatives, voir:
#http://eric.univ-lyon2.fr/~ricco/tanagra/fichiers/fr_Tanagra_Naive_Bayes_R_Programming.pdf


library(e1071)
g <- naiveBayes(survived ~ ., data = ptitanic)
g$apriori
g$tables
#predict(g, HouseVotes84[1:10,])
#predict(g, HouseVotes84[1:10,], type = "raw")

pred_bayes<-predict(g, ptitanic)

write.csv(pred_bayes,file=".\\data\\pred_titanic2_bayes.csv")

#on constate que l arbre de decision est bien meilleur pour prévoir les morts que la méthode de bayes
#93% de bonne prevision (rpart) contre 85% pour bayes
#et legerement moins bonne pour prevoir les vivants (65% rpart et 67% de bonnes prev pour bayes)


#rappel de syntaxe data.frame
#data.frame
v1 <- c(15,8.2,14)
v2 <- c(TRUE,FALSE,TRUE)
v3 <- factor(c("M","F","M"))
#liste
liste <- list(v1,v2,v3)


donnees <- data.frame(liste)
#nommer les colonnes et les lignes
colnames(donnees) <- c("x1","x2","x3")
rownames(donnees) <- c("pierre","paul","jacques")
#affichage
print(class(donnees))
print(summary(donnees))
print(donnees)

#simulation
n<-100
x<-runif(n,0,1)

#Donnees simulees naives
#test syntaxe
#y<-x[x>0]
y<-ifelse ( x[x>0] < 1/2 ,1 ,-1)
liste <- list(x,y)
data1 <- data.frame(liste)
colnames(data1) <- c("x","y")
rownames(data1) <- 1:n
data1

library(rpart)
library(rpart.plot)

dataTree <- rpart(y~x,data=data1,control=rpart.control(minsplit=5,cp=0))
prp(dataTree,extra=1) #graphe complet 
summary(dataTree)

plotcp(dataTree)
#test naif realise avec succes


#test avec naiveBayes
library(e1071)
g1 <- naiveBayes(y ~ x, data = data1)
g1$apriori
g1$tables
g1
#test naifBayes realise avec succes
#on retrouve les memes resultats mais pas la condition x>0.5


#Donnees simulees simples avec plus de colonnes
#test des conditions if, else if, else

n<-100
x<-runif(n,0,1)
y2<-ifelse ( x < 1/4  ,0.25, ifelse(x<0.5,0.5,0.8))
y3<-1:n

for (i in 1 : n)
  {
    if (x[i] < 1/4 )  y3[i]<-0.25  
    else if (0.25<x[i] & x[i]<0.5 )  y3[i]<-0.5
    else y3[i]<-1
}


liste2 <- list(x,y2,y3,z)
data2 <- data.frame(liste2)
colnames(data2) <- c("x","y2","y3","z")
rownames(data2) <- 1:n
data2

library(rpart)
library(rpart.plot)

dataTree2 <- rpart(x~y2+y3+z,data=data2,control=rpart.control(minsplit=5,cp=0))
prp(dataTree2,extra=1) #graphe complet 
summary(dataTree2)

plotcp(dataTree2)

#test sur un modele lineaire basique
z<-3*x
liste3<- list(x,z)
data3 <- data.frame(liste3)
colnames(data3) <- c("x","z")
rownames(data3) <- 1:n
data3

dataTree3 <- rpart(x~z,data=data3,control=rpart.control(minsplit=5,cp=0))
prp(dataTree3,extra=1) #graphe complet

#on obtient bien la prediction 
pred3<-predict(dataTree3, data3, type="vector")

#attention ci-dessus on met type="vector" (regression) et non type ="class" (logical->classification)
#on vérifie le type des objets et leurs longueurs
#class(pred3)
#length(pred3)

#E(X-x)^2-E^2(X)
erreur<-sqrt(mean((x-pred3)^2)+var(pred3))
print(erreur)


list4<-list(x,z,pred3)

modlin_pred<-data.frame(list4)
colnames(modlin_pred) <- c("x","z","pred_x")

write.csv(modlin_pred,file=".\\data\\pred_modlin.csv")


#par rapport a un modele linéaire basique, on voit que le modele d'arbre decisionnel est plus que satisfaisant (c'en est presque surprenant)

#test du modele lineaire
modlin<-lm(x~z)
summary(modlin)

print(modlin)
modlin$fitted.values

#test sur un modele lineaire + general
n<-100
x1<-rnorm(n,5,1)
x2<-rnorm(n,10,1)
y1<-3*x1+5*x2

list5<- list(y1,x1,x2)
data4 <- data.frame(list5)
colnames(data4) <- c("y1","x1","x2")
rownames(data4) <- 1:n
data4

library(rpart)
library(rpart.plot)

dataTree4 <- rpart(y1~x1+x2,data=data4,control=rpart.control(minsplit=5,cp=0))
prp(dataTree4,extra=1) #arbre

#prediction 
pred4<-predict(dataTree4, data4, type="vector")

erreur<-sqrt(mean((y1-pred4)^2)+var(pred4))
print(erreur)

plot(y1)
lines(pred4,col="green")


list6<- list(y1,x1,x2,pred4)
data5 <- data.frame(list6)
colnames(data5) <- c("y1","x1","x2","y1_pred")
rownames(data5) <- 1:n
data5

#Prediction par un modele lineaire (le modele étant linéaire la prévision est parfaite)
modlin2<-lm(y1~x1+x2)
plot(y1)
lines(modlin2$coefficients[2]*x1+modlin2$coefficients[3]*x2+modlin2$coefficients[1], col='red')



write.csv(data5,file=".\\data\\pred_modlin2.csv")
#sur ce model moins trivial que le précédent on observe que l arbre decisionnel donne encore des resultats tres satisfaisants
#NB: je ne sais pas trop bien dans quel cas on utilise un arbre plutot qu une regression (lineaire ou non)




