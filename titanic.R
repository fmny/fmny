
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




