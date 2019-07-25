#' ---
#' title: "GGplot2"
#' output: 
#'   learnr::tutorial:
#'     progressive: false
#'     theme: 'rstudio'
#' runtime: shiny_prerendered
#' fontsize: 22pt
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
library(learnr)
knitr::opts_chunk$set(echo = FALSE)
library(magrittr)
library(ggplot2)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(mapview) # for interactive maps
library(ggplot2) # tidyverse vis package
library(shiny)   # for web applications
library(plotly)
library(reshape)
library(dplyr)
library(scales)
library(RColorBrewer)
library(ggthemes)

#' 
#' 
#' Avant-propos
#' -------
#' 
#' Dans la suite de documents html qui suivent, voici quelques conventions que j'ai prise:
#' 
#' - le texte normal
#' - `les commandes R`
#' - *les objets R*
#' - $les\ quelques\ formules\ mathématiques$
#' - ➤ indique que c'est à vous d'écrire/réfléchir pour obtenir ce qui vous est demandé
#' 
#' La console de R est affichée comme suit, avec les sorties marquées ##. 
## ----echo=TRUE-----------------------------------------------------------
print("La console de R")

#' 
#' Le champ suivant vous permet d'écrire du code et de l'exécuter en cliquant sur Run code. Vous avez parfois des aides ou des astuces qui sont disponibles en cliquant sur le bouton Hint.
#' 
## ----field, exercise=TRUE, exercise.eval=TRUE----------------------------


#' 
## ----field-hint-1--------------------------------------------------------
# Solution ou aide ici

#' 
#' A tout moment, vous pouvez faire `help(fonction)` ou `?fonction` pour obtenir de l'aide sur une fonction.
#' 
#' GGplot2
#' -------------
#' 
#' Ce mini-cours va tenter de vous expliquer la logique et la grammaire de la représentation graphique avec le language GGplot.
#' 
#' Une grande source d'informations se situe ici : https://ggplot2.tidyverse.org/reference/. Toutes les fonctions de GGplot sont décrites avec des exemples.
#' 
#' 
#' ### Avec des mots
#' 
#' La grammaire GGplot en texte, ça ressemble à :
#' 
#' mongraphe <- données et axes du graphe +  
#' représente une géométrie (couleur et forme en fonction d'une variable si besoin) +  
#' rajoute une courbe de tendance +  
#' définition des noms d'axes +  
#' ajout d'un titre +  
#' définition des couleurs et formes utilisés +  
#' réglage du thème du graphique
#' 
#' 
#' Les graphes faits sous GGplot sont donc couches successives d'informations diverses (les données, le type de graphes, les paramètres...).
#' 
#' #### Un exemple simple
#' 
#' Reprenons l'exemple des données du *Zoo*. Faisons un graphe très basique de la nourriture consommée en fonction du nombre d'individus.
#' 
## ------------------------------------------------------------------------

link_to_tab2 <-"C:/Users/WOODIV_2/Documents/Kevin Cilleros/WOODIV/Formation/Formation_R/GGplot2/www/dataexple.csv"
  
  # "https://raw.githubusercontent.com/kcilleros/tutoriel/master/dataexple.csv?token=AMF6JOO2B5MQ3SVWYFGHCGS5IAIOO"
zoo <- read.csv(link_to_tab2, sep = ";", h = T, dec = ",")
assign("zoo", zoo, envir=globalenv())

#' 
#' 
#' 
## ----data, exercise = T, exercise.eval = T, exercise.lines = 10----------

path_to_zoo <-"C:/Users/WOODIV_2/Documents/Kevin Cilleros/WOODIV/Formation/Formation_R/GGplot2/www/dataexple.csv"

zoo <-read.csv(path_to_zoo, sep = ";", h = T, dec = ",")


#' 
#' 
#' Le graphe obtenu avec `plot()`
## ----echo=TRUE-----------------------------------------------------------
with(zoo, plot(Food.kg ~ Number.ind, 
               main = "Nourriture consommée f(nombre d'individus)",
               xlab = "Nombre d'individus", 
               ylab = "Nourriture consommée (en kg)" 
)
)

#' 
#' Maintenant avec `ggplot()` :
#' 
## ----echo=TRUE-----------------------------------------------------------
ggplot(data = zoo, aes(x = Number.ind, y = Food.kg)) +
  geom_point() +
  ggtitle("Nourriture consommée f(nombre d'individus)") +
  xlab("Nombre d'individus") +
  ylab("Nourriture consommée (en kg)")

#' 
#' Si on compare les deux scripts, la majeure différence est que les arguments de `plot` deviennent des fonctions dans `ggplot`. Ensuite GGplot ne devine pas le meilleur graphe possible avec les variables renseignées, ce qui permet une plus grande flexibilité dnas les sorties.
#' 
#' Seul bémol pour GGplot : les données doivent nécessairement être sous format de 'data.frame' et en format long.
#' 
#' 
#' Format long et large
#' -------------
#' 
## ------------------------------------------------------------------------

link_to_tab2 <-"C:/Users/WOODIV_2/Documents/Kevin Cilleros/WOODIV/Formation/Formation_R/GGplot2/www/dataexple.csv" 
# "https://raw.githubusercontent.com/kcilleros/tutoriel/master/dataexple.csv?token=AMF6JOO2B5MQ3SVWYFGHCGS5IAIOO"
zoo <- read.csv(link_to_tab2, sep = ";", h = T, dec = ",")

#' 
#' Les données peuvent être généralement sous deux formats dans un tableau : le format long ('long') ou le format large ('wide').
#' 
#' Pour comprendre la différence entre les deux formats, il faut être clair sur la terminologie des variables qui concerne les observations :
#' 
#' - les variables identifiant : permet de classifier les observations en groupe
#' 
#' - les variables mesure : ce qui a été mesuré à chaque observation
#' 
#' - les valeurs : la valeur de chaque mesure
#' 
#' Dans un tableau large :
#' 
#' ![](images/wide.PNG)
#' 
#' 
#' 
#' Dans un tableau long :
#' 
#' ![](images/long.PNG)
#' 
#' Chacun de ces formats a ses avantages (remplissage, vision de données, calculs).
#' 
#' Prenons un exemple avec le tableau suivant :
#' 
## ------------------------------------------------------------------------
olddata_wide <- read.table(header=TRUE, text='
 subject sex control cond1 cond2
       1   M     7.9  12.3  10.7
       2   F     6.3  10.6  11.1
       3   F     9.5  13.1  13.8
       4   M    11.5  13.4  12.9
')

olddata_wide$subject <- factor(olddata_wide$subject)
print("olddata_wide data")
olddata_wide

#' 
#' 
#' ➤ Dans ce tableau, quelles sont les différents types de variables ?
## ----description, exercise = T, exercise.eval=FALSE, exercise.lines = 6----
#Identifiants ?

#Mesures ?

#Valeurs ?



#' 
#' Pour passer d'un format à l'autre, on peut utiliser les fonctions `melt()` et `cast()` du package 'reshape'. `melt()` permet de convertir de large en long et `cast()` fait l'inverse.
#' 
#' Exemple :
## ----echo=TRUE-----------------------------------------------------------
olddata_wide %>% melt

#' 
#' La fonction `melt()` nous avertie qu'elle utilise les variables 'subject' et 'sex' comme variable identifiant, ce qui est en accord avec les réflexions précédentes. Elle crée donc ensuite une colonne qui va contenir les variables mesure ('variable') et les valeurs associées ('value').
#' 
#' Pour repasser en format large et retrouver le tableau original:
## ----echo=T, message=FALSE, warning=FALSE--------------------------------
olddata_wide %>% melt %>% cast

#' 
#' 
#' Une comparaison entre format long et large pour faire des résumés des variables :
#' 
## ----echo=TRUE, message=FALSE--------------------------------------------
olddata_wide %>% summary
olddata_wide %>% nrow
olddata_wide %>% '[' (3:5) %>% apply(2,sd)

#' 
## ----echo=TRUE, message=FALSE--------------------------------------------
olddata_wide %>% 
  melt %>% 
  group_by(variable) %>% #grâce à la library dplyr
  summarise(N=n(), 
            moy=mean(value, na.rm=TRUE),
            med=median(value,na.rm=TRUE),
            ecart_type=sd(value,na.rm=TRUE))

#' 
#' 
#' ![](images/warning.png){width=50px} Selon le type de graphe que vous souhaitez faire, tout convertir en format long n'est peut être pas nécessaire.
#' 
#' 
#' Les types de graphes
#' -------------
#' 
## ------------------------------------------------------------------------

link_to_tab2 <-"C:/Users/WOODIV_2/Documents/Kevin Cilleros/WOODIV/Formation/Formation_R/GGplot2/www/dataexple.csv"

# "https://raw.githubusercontent.com/kcilleros/tutoriel/master/dataexple.csv?token=AMF6JOO2B5MQ3SVWYFGHCGS5IAIOO"
zoo <- read.csv(link_to_tab2, sep = ";", h = T, dec = ",")

#' 
#' Les différents types de graphes communs dans R sont obtenus avec GGplot en utilisant la fonction `geom_xxx` qui définit la géométrie voulue pour les données, avec xxx qui dépend du type de graphe voulu.
#' 
#' 
#' On va voir les principaux graphes maintenant. Pour chacun, je vous montre la version simple puis une version customisée.
#' 
#' **1. Les histogrammes `geom_histogram`**
#' 
## ----hist1, echo=TRUE, message=FALSE, warning=FALSE----------------------
zoo %>% ggplot(aes(x = Number.ind)) +
  geom_histogram()

#' 
## ----hist2, exercise = T, exercise.eval = T------------------------------
zoo %>% ggplot(aes(x = Number.ind)) +
  geom_histogram(binwidth = 20, #gamme des barres
                 fill = "#cbd5e8", #couleur des rectangles
                 col = "#4279e3") + #couleur des bords
  xlab("Nombre d'individus") +
  ylab("Fréquence") +
  ggtitle("Histogramme")

#' 
#' ---
#' 
#' ---
#' 
#' **2. Les boxplot `geom_boxplot`**
#' 
## ----box1, echo=TRUE, message=FALSE, warning=FALSE-----------------------

zoo %>% ggplot(aes(x = Animal, y = Number.ind)) +
  geom_boxplot()


#' 
## ----box2, exercise = T, exercise.eval = T-------------------------------

zoo %>% ggplot(aes(x = Animal, y = Number.ind)) +
  geom_boxplot(aes(fill = Animal, col = Animal), alpha = 0.6) + #ajout de transparence avec alpha
  xlab("Animal") +
  ylab("Nombre d'individus") +
  ggtitle("Boxplot") +
  scale_color_brewer(type = "qual", palette = 2) + #sélection d'une palette de couleur pour la couleur (voir le site http://colorbrewer2.org)
  scale_fill_brewer(type = "qual", palette = 2) #sélection d'une palette de couleur pour le remplissage (voir le site http://colorbrewer2.org)


#' 
#' ---
#' 
#' ---
#' 
#' **3. Les violin `geom_violin`**
#' 
## ----vio1, echo=TRUE, message=FALSE, warning=FALSE-----------------------

zoo %>% ggplot(aes(x = Animal, y = Number.ind)) +
  geom_violin() 


#' 
## ----vio2, message=FALSE, warning=FALSE, exercise=T, exercise.eval=T-----

zoo %>% ggplot(aes(x = Animal, y = Number.ind)) +
  geom_violin(aes(fill = Animal, col = Animal), alpha = 0.6) + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=.5, aes(fill = Animal)) + #ajout des observations sur les violin
  xlab("Animal") +
  ylab("Nombre d'individus") +
  ggtitle("Violin") +
  scale_color_brewer(type = "qual", palette = 2) + 
  scale_fill_brewer(type = "qual", palette = 2) 


#' 
#' ---
#' 
#' ---
#' 
#' **4. Les barplot `geom_bar`**
#' 
## ----bar1, echo=TRUE, message=FALSE, warning=FALSE-----------------------

zoo %>% 
  ggplot(aes(x = Animal, y = Size.cm)) +
  geom_bar(stat = "summary", fun.y = "mean") # pour chaque animal, représenter une valeur résumée qui est la moyenne


#' 
## ----bar2, exercise = T, exercise.eval = T-------------------------------

zoo %>% 
  ggplot(aes(x = Animal, y = Size.cm)) +
  geom_bar(stat = "summary", fun.y = "mean", aes(fill = Animal, col = Animal), alpha = 0.6) +
  xlab("Animal") +
  ylab("Taille (cm)") +
  ggtitle("Barplot") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2)


#' 
#' ---
#' 
#' ---
#' 
#' **5. Les scatterplot `geom_point`**
#' 
## ----xyplot1, echo=TRUE, message=FALSE, warning=FALSE--------------------

zoo %>% ggplot(aes(x = Size.cm, y = Food.kg)) +
  geom_point() 


#' 
## ----xyplot2, message=FALSE, warning=FALSE, exercise=T, exercise.eval=T----

zoo %>% ggplot(aes(x = Size.cm, y = Food.kg)) +
  geom_point(aes(col = Animal, shape = Zoo, size = Number.ind)) + #taille du point est fonction du nombre d'individus
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", col = "black") + #ajout de la droite 1:1
  geom_hline(yintercept = 0) + # ajout de y=0
  geom_vline(xintercept = 0) + # ajout de x=0
  xlab("Taille (cm) (log10 scale)") +
  ylab("Nourriture consommée (kg) (log10 scale)") +
  ggtitle("XYplot") + 
  scale_color_brewer(type = "qual", palette = 2) +
  scale_shape_manual(values = c(4,8,15:18)) + #définition d'une palette pour les formes de points
  scale_x_log10(limits = c(1, 10e3), 
                breaks = trans_breaks("log10", function(x) 10^x, n = 4),
                labels = trans_format("log10", math_format(10^.x))) + # transforme l'échelle de l'axe X en échelle log10, avec un changement des limites de l'axe, où se fait la coupure de l'axe et un formatge des noms des marques de l'axe
  scale_y_log10(limits = c(1, NA), 
                breaks = trans_breaks("log10", function(x) 10^x, n = 7),
                labels = trans_format("log10", math_format(10^.x))) + #idem
  annotation_logticks()  #montrer les traits


#' 
#' ---
#' 
#' ---
#' 
#' **6. Ajout de courbe de tendance `geom_smooth`**
#' 
## ----lmplot1, message=FALSE, warning=FALSE, exercise=T, exercise.eval=T----
zoo %>% ggplot(aes(x = Number.ind, y = Food.kg)) +
  geom_point() +
  scale_color_brewer(type = "qual", palette = 2) +
  xlab("Nombre d'individus") +
  ylab("Nourriture consommée (kg) (log10 scale)") +
  ggtitle("Modèle Food ~ Number") +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 7),
                labels = trans_format("log10", math_format(10^.x)))  +
  geom_smooth(method = lm, fullrange=TRUE) #le modèle fitté se sert des variables stockées dans x et y des aesthetics définis au départ. !!!! Les échelles sont prises en comptes, le modèle est donc lm(log(Food) ~ Number)!!!



#' 
## ----lmplot2, message=TRUE, warning=FALSE, exercise=T, exercise.eval=T----
zoo %>% ggplot(aes(x = Number.ind, y = Food.kg, col = Animal)) +
  geom_point() +
  xlab("Nombre d'individus") +
  ylab("Nourriture consommée (kg) (log10 scale)") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 7),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_smooth(method = lm, fullrange=TRUE)


#' 
#' ---
#' 
#' ---
#' 
#' **7. Facetting des graphes `facet_xxx`**
#' 
#' 1 graphe par catégorie
## ----lmplot3, exercise = T, exercise.eval = T----------------------------
zoo %>% ggplot(aes(x = Number.ind, y = Food.kg, col = Animal)) +
  geom_point() +
  xlab("Nombre d'individus") +
  ylab("Nourriture consommée (kg) (log10 scale)") +
  ggtitle("Modèle Food ~ Number * Animal") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 7),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_smooth(method = lm, fullrange=TRUE) +
  facet_wrap( . ~ Animal, nrow = 2) +
  guides(col = F)


#' 
#' 1 graphe par couple de variables
#' 
## ----lmplot4, exercise = T, exercise.eval = T----------------------------
zoo %>% ggplot(aes(x = Number.ind, y = Food.kg, col = Animal)) +
  geom_point() +
  xlab("Nombre d'individus") +
  ylab("Nourriture consommée (kg) (log10 scale)") +
  ggtitle("Modèle Food ~ Number * Animal") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n = 7),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_smooth(method = lm, fullrange=TRUE) +
  facet_grid( Area ~ Zoo) + #ligne ~ colonne
  guides(col = F)


#' 
#' 
#' Thème
#' -------------
#' 
#' Il existe deux façons de modifier le thème général d'un graphe GGplot : en paramétrant manuellement avec `theme()` ou alors en appliquant des thèmes pré-définis.
#' 
#' Voici toutes les choses paramétrables dans le thème :
#' 
## ------------------------------------------------------------------------
formalArgs(theme)[-91:-93]

#' 
#' Chacun de ces éléments peut être configurés en ajoutant à la suite des commandes d'un GGplot avec `theme(parametre1 = element_xxx(), parametre2 = element_xxx())`. Le `element_xxx` dépend du paramètre pour lequel il est appelé : pour du texte, c'est `element_text()`, pour les lignes `element_line()`, pour des zones `element_rect()`. Chacun des `element_xxx()` comporte ensuite une liste d'arguments à modifier, par exemple la couleur, la taille, la police ...
#' 
#' Pour une description exacte de chaque argument, il faut regarder l'aide de `theme()` ou ici https://ggplot2.tidyverse.org/reference/theme.html.
#' 
#' 
#' Le plus simple reste quand même d'utiliser les thèmes pré-définis et si besoin de modifier certains éléments de ceux-ci. Voici une liste de thèmes pré-défini dans ggplot2 :
#' 
## ------------------------------------------------------------------------
ls("package:ggplot2") %>% grep(pattern = "theme_", x = ., value = T)

#' 
#' et dans le package ggthemes :
#' 
## ------------------------------------------------------------------------
ls("package:ggthemes") %>% grep(pattern = "theme_", x = ., value = T)


#' 
#' Pour les appliquer, il suffit d'ajouter à la fin des lignes de commandes pour créer le graphe une ligne avec `theme_xxx()`.
#' 
## ----theme, exercise = T, exercise.eval = T------------------------------
zoo %>% ggplot(aes(x = Animal, y = Number.ind)) +
  geom_boxplot() +
  theme_classic()

#' 
#' 
#' Pour modifier un thème pré-défini : 
#' 
## ----thememodif, exercise = T, exercise.eval = T-------------------------
zoo %>% ggplot(aes(x = Animal, y = Number.ind)) +
  geom_boxplot() +
  theme_classic() +
  theme(axis.title = element_text(colour = "red"))

#' 
#' 
#' 
#' Graphe interactif
#' -------------
#' 
#' On peut facilement rendre un graphe interactif avec la library 'plotly'. Les plot classiques et les graphes GGplot sont compatibles. 
#' 
#' L'avantage avec les GGplot, il suffit de stocker le GGplot dans un objet et de faire `ggplotly(objet)` ou alors de 'piper' le GGplot dans `ggplotly()`
#' 
#' 
## ----interactif1, exercise = T, exercise.eval = T------------------------
ggplot_bar <- zoo %>% 
  ggplot(aes(x = Animal, y = Size.cm)) +
  geom_bar(stat = "summary", fun.y = "mean", aes(fill = Animal, col = Animal), alpha = 0.6) +
  xlab("Animal") +
  ylab("Taille (cm)") +
  ggtitle("Barplot interactif") +
  scale_color_brewer(type = "qual", palette = 2) +
  scale_fill_brewer(type = "qual", palette = 2)

ggplotly(ggplot_bar, 
         dynamicTicks = T, ) #permet d'automatiquement modifier l'échelle lors des zooms

#' 
#' 
## ----interactif2, exercise = T, exercise.eval = T------------------------

(zoo %>% ggplot(aes(x = Animal, y = Number.ind)) +
   geom_boxplot(aes(fill = Animal, col = Animal), alpha = 0.6) + 
   xlab("Animal") +
   ylab("Nombre d'individus") +
   ggtitle("Boxplot interactif") +
   scale_color_brewer(type = "qual", palette = 2) + 
   scale_fill_brewer(type = "qual", palette = 2)) %>%
  ggplotly(dynamicTicks = T)

#' 
#' 
#' Les cartes avec tmap
#' -------------
#' 
#' Avant 2018, il était possible d'utiliser le package ggmap pour construire des cartes dans R avec des fonds de carte Googlemap ou OpenStreet. Cependant, il est maintenant nécessaire d'avoir un API et donc de ratacher sa carte bancaire à un compte Google pour utiliser la fonction.
#' 
#' Du coup, une alternative est de passer par tmap. Le mode interactif permet d'afficher des cartes OpenStreet ou ESRI.
#' 
#' Les cartes tmap peuvent être visualisées en format statique ou interactif. Pour contrôler ça, il faut switcher dans la fonction `tmap_mode()` entre "plot" et "view".
#' 
#' La grammaire est ensuite similaire à GGplot. On donne d'abord les données que l'on souhaite représenter, puis on décrit le type de géométrie voulu.
#' 
#' Une spécificité est qu'on peut à tout moment switcher de données pour représenter plusieurs tableaux sur une même carte.
#' 
#' On va utiliser l'exemple contenu dans le package pour introduire le fonctionnnement.
#' 
## ------------------------------------------------------------------------

data(World, metro) #des jeux de données internes
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100




#' 
#' 
## ----echo=TRUE-----------------------------------------------------------
head(World)

head(metro)

#' 
## ----map1, exercise = T, exercise.eval = T, exercise.lines = 25----------

data(World, metro) #des jeux de données internes
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

#####faire entrer la ligne après le point virgule
tmap_mode("plot");


tm_shape(World) + #sur quelle données on commence
  tm_fill("grey70") + #on remplit les polygones des pays
  tm_shape(metro) + #on switche de données
  tm_bubbles("pop2010", #on fait des bulles à partir des données de populations
             col = "growth", #la couleur de chaque bulle dépend de la variable croissance
             border.col = "black", #couleur du bord des bulles
             border.alpha = .5, #transparence
             style="fixed", #quand une échelle de couleur est demandée, comment la construire. Ici on la fabrique manuellement. Plus de possibilités dans l'aide.
             breaks=c(-Inf, seq(0, 6, by=2), Inf), #l'échelle
             palette="-RdYlBu", #la palette de couleur
             title.size="Metro population", #nom pour la légende concernant la taille des bulles
             title.col="Growth rate (%)") #nom pour la légende concernant la couleur des bulles

#' 
#' Pour exporter les cartes créées, la commande dépend du type de carte (interative ou statique).
#' 
#' Pour les interactives, l'export se fait sous format HTML : `tmap_save(tm = carte, filename = "nomfichier.html")`.
#' 
#' Pour les statiques, l'export se fait sous un format comme les graphes classiques (png, pdf, svg, eps...) : `tmap_save(tm = carte, filename = "nomfichier.png")`.
