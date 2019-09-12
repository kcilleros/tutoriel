#' ---
#' title: "Tests statistiques et régressions"
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
knitr::opts_chunk$set(echo = TRUE)
library(magrittr)
library(MASS)
library(car)
library(plyr)
library(reshape)
library(FSA)
library(HH)
library(lmtest)
library(ade4)
library(mgcv)

ht <- function(d, m=5, n=m){
  # print the head and tail together
  list(HEAD = head(d,m), TAIL = tail(d,n))
}

#' 
#' 
#' 
#' Avant-propos
#' -------
#' 
#' Dans la suite de documents html qui suit, voici quelques conventions que j'ai prise:
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
#' 
## ----include=FALSE-------------------------------------------------------
# link_to_tab2 <-"https://raw.githubusercontent.com/kcilleros/tutoriel/master/dataexple.csv?token=AMF6JOO2B5MQ3SVWYFGHCGS5IAIOO"
# zoo <- read.csv(link_to_tab2, sep = ";", h = T, dec = ",", colClasses = c("factor", "factor", "numeric", "numeric", "factor", "numeric"))
# assign("zoo", zoo, envir=globalenv())

#' 
#' 
#' Corrélations
#' -------
#' 
#' La corrélation permet de voir si il existe une relation entre deux ou plusieurs variables.
#' 
#' ### Coefficient de corrélation de Pearson $r$
#' 
#' Il mesure une relation linéaire entre deux variables. Il ne peut donc être utilisé que lorsque les deux variables suivent une distribution normale.
#' 
#' Avec deux vecteurs *x* et *y* de taille n: 
#' 
#' $$
#' r = \frac{\sum_{i=1}^{n}(x_{i} - \overline{x})(y_{i} - \overline{y})}{\sqrt{\sum_{i=1}^{n}(x_{i} - \overline{x})^2\sum_{i=1}^{n}(y_{i} - \overline{y})^2}}
#' $$
#' 
#' Sa significativité est basée sur le calcul de $t$, avec un nombre de degré de liberté $\nu$ égal à $n-2$ :
#' 
#' $$
#' t = \frac{r\sqrt{\nu}}{\sqrt{1-r^2}}
#' $$
#' 
#' ### Coefficient de corrélation de Spearman $r$ ou $\rho$
#' 
#' Il mesure la relation entre les rangs de deux variables. C'est une mesure non paramétrique de corrélation et donc ne suppose pas de normalité et est approprié si les variables sont mesurées sur une échelle ordinale.
#' 
#' Avec deux vecteurs *x* et *y* de taille n. Pour chaque élément i, on calcule la diférence $d_{i}$ entre son classement dans *x* et son classement dans *y*.
#' 
#' $$
#' \rho = 1 - \frac{6\sum_{i=1}^{n}d_{i}^2}{n^3 - n}
#' $$
#' 
#' Sa significativité est basée sur le calcul de $t$, avec un nombre de degré de liberté $\nu$ égal à $n-2$ (comme pour le coefficient de Pearson).
#' 
#' ### Coefficient de corrélation de Kendall $\tau$
#' 
#' Il mesure la relation entre les rangs de deux variables. C'est une mesure non paramétrique de corrélation et donc ne suppose pas de normalité.
#' 
#' Avec deux vecteurs *x* et *y* de taille n. On classe chaque élément selon un vecteur et on regarde combien de paires de rang des éléments sont dans l'ordre croissant dans le second vecteur. Ceci donne un score $S$ utilisé pour calculer $\tau$
#' 
#' $$
#' \tau = \frac{2S}{n(n-1)}
#' $$
#' 
#' Sa significativité est basée sur le calcul de $z$ :
#' 
#' $$
#' z = \biggl[|\tau|\sqrt{\frac{9n(n-1)}{2(2n+5)}}\biggr] - \sqrt{\frac{18}{n(n-1)(2n+5)}}
#' $$
#' 
#' ### Implémentation dans R
#' 
#' <div style= "float:right;position: relative;">
#' ![](images/car.jpg){width=200px}
#' </div>
#' Tous les coefficients de corrélation s'obtiennent à partir de la commande `cor(x, y, "nom.du.coefficient")` et leur significativité avec `cor.test(x,y,"nom.du.coefficient")`.
#' 
#' 
#' Le jeu de données *mtcars* contient la consommation de carburant et 10 caractéristiques de 32 automobiles.
#' 
#' 
#' ➤ Existe-t-il une relation entre la masse d'une voiture (*wt*) et sa consommation de carburant (*mpg*) ?
## ----include=FALSE-------------------------------------------------------
data(mtcars)


#' 
#' 
## ----cor-exe, fig.height=7, exercise=T, exercise.eval=T------------------
head(mtcars)


#' 
#' 
#' Tests d'hypothèses (1) : $\chi^2$ d'indépendance
#' -------
#' <div style= "float:right;position: relative;">
#' ![](images/Caithness.jpg){width=128px}
#' </div>
#' 
#' Le jeu de données *caith* de la library MASS est un tableau de contingence, présentant les effectifs d'individus de la région de Caithness en Écosse, répartis selon la couleur de leur yeux et de leur cheveux.
#' 
## ----echo=FALSE----------------------------------------------------------
data("caith")
caith

#' 
#' 
## ----echo=FALSE, fig.height=8, fig.width=8-------------------------------

caith_melt <- melt(as.matrix(caith))
caith_melt$X1 %<>% factor(levels = rev(c("blue", "light", "medium","dark")))
caith_melt$X2 %<>% factor(levels = c("fair", "red", "medium","dark", "black"))
caith_melt$hair <- as.numeric(caith_melt$X2)
caith_melt$eye <- as.numeric(caith_melt$X1)

symbols(y = caith_melt$eye, x = caith_melt$hair, 
        circles = sqrt( caith_melt$value/ pi ), inches=0.35,
        yaxt="n", xaxt="n",xlab = "", ylab = "Eye",
        bg = c("#462100", "#8b6c42","ivory2", "lightblue1")[caith_melt$eye], 
        fg = c("#faf0be", "#ad4f09","#8b6c42", "#462100", "black")[caith_melt$hair],
        ylim = c(0,5), xlim = c(0,6),
        lwd = 2)
axis(3, at = 1:ncol(caith), labels = levels(caith_melt$X2))
mtext("Hair", side=3, line=3)
axis(2, at = 1:nrow(caith), labels = levels(caith_melt$X1))

legPop <- c( 1000,500,100,10)
legRad <- sqrt( legPop / pi )
hin <- par('pin')[2]
burgPerInch <- ( 5 - 0 ) / hin
radPerInch <- max(sqrt( caith_melt$value/ pi ))/0.35
heightAdj <- legRad/radPerInch*burgPerInch
symbols( rep(0.25,4), rep(0.5,4) + heightAdj, circles = legRad, inches = 0.35, add = TRUE)
tAdj <- strheight('1000', cex = 0.5)
text( rep(0.25,4), rep(0.5,4) + heightAdj*2 + tAdj, c('1000', '500', '100', '10'), cex = 0.5)


#' 
#' Pour tester l'indépendance entre deux variables qualitatives avec un test du $\chi^2$, on passe par la fonction `chisq.test`.
#' 
## ----chi2, exercise = T, exercise.eval = F-------------------------------
test.1 <- chisq.test(caith)
test.1

#' 
#' 
#' Le test du $\chi^2$ ne donne qu'une réponse globale concernant la relation entre les deux variables. Pour interpréter facilement la relation, on peut passer par une représentation graphique grâce à une analyse factorielle des correspondances (AFC, "correspondence analysis"). (Voir "Analyses multivariées").
#' 
#' Tests d'hypothèses (2) : Comparaison de deux échantillons
#' -------
#' 
#' La démarche générale pour comparer 2 échantillons peut être résumée comme suit :
#' 
#' ![](images/demarche.png){width=750px}
#' 
#' ### Crabes de couleur
#' <div style= "float:right;position: relative;">
#' ![](images/crabs.jpg){width=128px}
#' </div>
#' Le jeu de données *crabs* contient des mesures morphologiques (5 traits) prises sur l'espèce *Leptograpsus variegatus*, chez les femelles et les mâles et les deux morphes.
#' 
## ----include=FALSE-------------------------------------------------------
data("crabs")

#' 
#' ➤ Existe-t-il une différence de longueur de carapace entre les deux morphes de crabes ? Si oui, dans quel sens est-elle ?
#' 
## ----crab_p, exercise = T, exercise.eval = T, exercise.lines = 9---------
head(crabs)

#' 
#' 
#' 
#' ### Qualité de l'air à New York
#' <div style= "float:right;position: relative;">
#' ![](images/NY.jpg){width=128px}
#' </div>
#' Le jeu de données *airquality* contient les mesures quotidiennes de qualité de l'air de New York. Les données *airquality2* sont les données pour Mai et Août.
#' 
## ----include=FALSE-------------------------------------------------------
airquality %>% subset(Month %in% c(5, 8)) %>% droplevels() -> airquality2
airquality2$Month %<>% as.factor %>% revalue(c("5" = "May", "8" = "August"))


#' 
#' ➤ Existe-t-il une différence de concentration en ozone entre mai et août ? Si oui, dans quel sens est-elle ?
#' 
## ----air_np, exercise = T, exercise.eval = T, exercise.lines = 9---------
head(airquality2)

#' 
#' 
#' ### Thérapie contre l'anxiété
#' <div style= "float:right;position: relative;">
#' ![](images/Anxiety.jpg){width=128px}
#' </div>
## ----include=FALSE-------------------------------------------------------
anxiety <- data.frame(patient = LETTERS[1:9],
                      first = c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30),
                      second = c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29))

#' 
#' Le jeu de données *anxiety* contient les mesures de l'anxiété chez des patients lors de deux visites de contrôles après le début d'une thérapie.
#' 
#' ➤ Est-ce que la thérapie a eu un effet chez les patients ? Si oui, dans quel sens est-il ?
#' 
## ----anxie_pair, exercise = T, exercise.eval = T, exercise.lines = 9-----
head(anxiety)

#' 
#' Tests d'hypothèses (3) : Comparaison de + de deux échantillons
#' -------
#' 
#' Comme pour les tests de comparaisons de deux échantillons, il existe des tests paramétriques et non-paramétriques.
#' 
#' Tout d'abord le test non-paramétrique : le test de Kruskall-Wallis qui est une comparaison de rang (similaire au test de Mann-Whitney). Il se code en langage R comme suit : `kruskal.test(vecteur.des.valeurs, vecteur.des.groupes)` ou `kruskal.test(valeur ~ groupe, data = tableau)`.
#' 
#' Le test paramétrique correspond à l'analyse de variance ou ANOVA, qui teste une différence de moyenne entre les groupes. 
#' 
#' Les conditions d'applications de l'ANOVA se vérifient non pas sur les données brutes, mais sur les résidus du modèle (car oui, l'ANOVA est un modèle linéaire en fait). Il y a trois conditions à vérifier :
#' 
#' - indépendances des résidus (`dwtest()` dans 'lmtest')
#' 
#' - distribution normale des résidus (`shapiro.test()` et `plot`)
#' 
#' - homoscédasticité des résidus (`bartlett.test()`)
#' 
#' Son implémentation dans R s'effectue avec la fonction `aov(valeur ~ groupe)`. Pour interpréter le modèle on utilise la fonction `summary` sur le modèle. Les résidus sont stockés dans *nom.du.modèle$residuals* ou peuvent être obtenus avec la fonction `residuals`.
#' 
#' Ces deux tests donnent seulement un signal général (il existe ou non une différence) mais n'explicite pas où se trouve la/les différence(s). On peut donc réaliser des tests post-hoc pour trouver les différences : `TukeyHSD(modèle)` en paramétrique et `dunnTest(valeur, groupe)` (dans la librairie 'FSA') pour le cas non paramétrique.
#' 
#' 
#' ### Qualité de l'air 2
#' 
#' ➤ La concentration en ozone varie-t-elle au cours de l'année ? Si oui, quel(s) mois diffèrent ?
#' 
## ----include=FALSE-------------------------------------------------------
airquality$Month %<>% factor

#' 
#' 
## ----air_groups_np, exercise = T, exercise.eval = F, exercise.lines = 9----


#' 
#' 
#' ### Croissance des plantes
#' <div style= "float:right;position: relative;">
#' ![](images/plant.jpg){width=128px}
#' </div>
#' Le jeu de données *PlantGrowth* contient les résultats d'une expérience de comparaison du rendement (mesuré en matière sèche) obtenu selon deux traitements dfférents (avec un témoin).
#' 
## ----include=FALSE-------------------------------------------------------
data("PlantGrowth")

#' 
#' ➤ Le rendement diffère-t-il selon le traitement ? Si oui, comment ?
#' 
## ----yields_groups_p, exercise = T, exercise.eval = T, exercise.lines = 9----
head(PlantGrowth)

#' 
#' ### Effets multiples et interactions
#' 
#' L'ANOVA permet aussi de tester l'effet de plusieurs facteurs indépendants sur une variable réponses. Il s'agit juste d'une extension de la décomposition de la variance sur plusieurs facteurs. 
#' 
#' Cela fait aussi intervenir la notion d'intéractions entre facteurs : l'effet d'un facteur diffère selon l'état de l'autre. L'ANOVA dans ce cas là va tester les hypothèses : pas d'effet des facteurs et pas d'effet des interactions. Une intéraction se code en utilisant `*` à la place du `+` dans le modèle.
#' 
#' Pour tester la significativité de l'interaction, il faut comparer des modèles emboîtés qui permet de tester l'effet d'un terme additionel dans l'un des deux modèles. Cette comparaison se fait avec la fonction `anova(modèle1, modèle2)`.
#' 
## ----include=FALSE-------------------------------------------------------
data("ToothGrowth")

#' <div style= "float:right;position: relative;">
#' ![](images/Vitamine_C.png){width=128px}
#' </div>
#' Le jeu de données *ToothGrowth* contient les résultats d'une étude sur l'effet de la vitamine C sur la croissance des dents chez le cochon d'Inde, sur 60 individus. Chacun a reçu 1 dose de vitamine C (0.5, 1 ou 2 mg/jour) selon deux sources (jus d'orange ou acide ascrobique).
#' 
#' ➤ La longueur des dents dépend-elle de la dose et la forme de vitamine C ?
#' 
## ----tooth_inter, exercise = T, exercise.eval = T, exercise.lines = 9----
head(ToothGrowth)

#' 
#' 
#' Régressions (1) : Régression linéaire
#' -------
#' 
#' L'objectif d'une régression linéaire est de déterminer l'équation linéaire (forme $y=\alpha+\beta x$) exprimant la relation entre les variables explicatives et la variable à expliquer. La régression permet de prédire des valeurs et implique des relations causales.
#' 
#' Le modèle général peut être écrit sous la forme :
#' 
#' $$
#' 
#' y_{i} = \alpha + \sum_{j = 1}^{k}\beta_{j}x_{ij} + \epsilon_{i}
#' 
#' $$
#' 
#' Comme pour l'ANOVA, les conditions d'applications d'un modèle linéaire sont basées sur les résidus :
#' 
#' - les résidus sont indépendants (`dwtest()` dans 'lmtest')
#' 
#' - les résidus sont distribués normalement
#' 
#' - les résidus sont homoscédastiques (`bartlett.test()`)
#' 
#' 
#' Il faut aussi faire attention à la multicolinéarité entre les varaibles explicatives. La multicolinéarité entre deux variables explicatives les rend non indépendante. Pour vérifier son absence, deux solutions complémentaires sont possibles : regarder la corrélation entre les variables et calculer le Variance Inflation Factor (VIF) pour chaque prédicteur (fonction `vif` du package 'HH').
#' 
#' #### Qu'est-ce que le VIF ?
#' 
#' Soit un modèle de forme $y_{i} = \alpha + \beta_{1}x_{i1} + \beta_{2}x_{i2} + \beta_{3}x_{i3} + \epsilon_{i}$ (généralisable pour $k$ variables).
#' 
#' Le VIF pour la variable $1$ est : $VIF_{1} = \frac{1}{1-R_{1}^2}$, avec $R_{1}^2$ le $R^2$ de la régression linéaire $x_{i1} = a + b_{2}x_{i2} + b_{3}x_{i3} + \epsilon_{i}$. 
#' 
#' Si le $R_{i}^2$ est proche de 1, la variable $i$ est très bien expliquée par une combinaison linéaire des autres variables et  est redondante dans le modèle$y_{i} = \alpha + \beta_{1}x_{i1} + \beta_{2}x_{i2} + \beta_{3}x_{i3} + \epsilon_{i}$. 
#' 
#' Plus le $R_{i}^2$ tend vers 1, plus le VIF tend vers l'infini. Du coup, un VIF plus grand que 5 pour la variable $i$ ($R_{i}^2 > 0.8$) est considéré comme signe de colinéarité (et donc que l'information de cette variable est déjà contenue en partie par les autres).
#' 
#' <div style= "float:right;position: relative;">
#' ![](images/crabs.jpg){width=128px}
#' </div>
#' Dans R, un modèle linéaire s'écrit `lm(y ~ x1 + x2 + x3, data = données)`. Les différents résultats du modèle s'obtiennent à partir des fonctions `summary`, `anova`.
#' 
#' 
#' ➤ Si on reprend notre jeu de données *crabs*, on peut se demander si il existe une relation entre la longueur de la carapace et la taille du lobe frontal chez *Leptograpsus variegatus*.
#' 
## ----crab_lm, exercise = T, exercise.eval = T, exercise.lines = 9--------
head(crabs)

#' 
#' 
#' ➤ Cette relation dépend-elle du morphe des crabes ?
#' 
## ----crab_ancova, exercise = T, exercise.eval = T, exercise.lines = 9----
head(crabs)

#' 
#' Régressions (2) : Modèle linéaire généralisé (GLM)
#' -------
#' 
#' Les GLM sont une généralisation du modèle linéaire à des données qui ne suivent pas une distribution normale (par exemple, des données sous format "0-1", des comptages, des mesures de précipitations...) et dont les résidus ne sont donc pas normalement distribués.
#' 
#' Le but est de linéariser la relation entre deux variables grâce à une fonction de lien (transformation des données). Quelques fonctions de lien courantes :
#' 
#' 
#' | Distribution de y |           Fonction de lien          |           Inverse du lien (modèle)          |
#' |:-----------------:|:-----------------------------------:|:-------------------------------------------:|
#' |      Normale      |      Identité : $\beta x = E(y)$      |              $E(y) = \beta x$             |
#' |      Poisson      |      Log : $\beta x = ln[E(y)]$      |            $E(y) = e^{\beta x}$            |
#' |     Binomiale     |    Logit : $\beta x = logit[E(y)]$   | $E(y) = \frac{e^{\beta x}}{1+e^{\beta x}}$ |
#' |       Gamma       | Inverse : $\beta x = \frac{1}{E(y)}$ |         $E(y) = \frac{1}{\beta x}$         |
#' 
#' 
#' Le codage dans R dun GLM s'écrit `glm(y ~ x1 + x2 + x3, family = nom.fonction.lien)`.
#' 
#' Contrairement au modèle linéaire, l'ajustement du modèle ne se fait pas par la méthode des moindres carrés, mais par maximum de vraisemblance ($P(observer~les~données|paramètres~inconnus)$) et minimisation de la déviance. La sélection de modèle s'effectue ensuite par AIC ou AICc (quand peu d'observations par rapport au nombre de variables). La fonction `stepAIC` permet une automatisation de la sélection des variables par AIC.
#' 
## ----include=FALSE-------------------------------------------------------
data("doubs")

#' <div style= "float:right;position: relative;">
#' ![](images/truite.jpg){width=128px}
#' </div>
#' 
#' Le jeu de données *doubs* contient l'abondance en classe de 27 espèces de poissons (*doubs\$fish*) et les mesures de 11 variables environnemenales (*doubs\$env*).
#' 
#' On va modéliser la présence de la truite selon le gradient d'oxygène dissous et la distance à la source tout d'abord, puis à partir des 11 variables environnementales.
#' 
#' 
#' Quelle variable explique le mieux l'occurence de la truite ?
## ----truite_glm, exercise = T, exercise.eval = T, exercise.lines = 9-----
truite <- ifelse(doubs$fish$Satr > 0, 1 ,0)
oxygene <- doubs$env$oxy; dsource <- doubs$env$dfs

#' 
#' Quel est le modèle le plus parcimonieux qui explique l'occurence de la truite ?
## ----truite_step, exercise = T, exercise.eval = T, exercise.lines = 9----
truite <- ifelse(doubs$fish$Satr > 0, 1 ,0)


#' 
#' Régressions (3) : Modèle additifs généralisé GAM
#' -------
#' 
#' Les GAM ajoutent un niveau supplémentaire par rapport aux GLM. Il y a toujours une fonction de lien associer aux valeurs de $y$ mais les prédicteurs $x_{i}$ ne sont pas directement inclus dans le modèle. En effet la relation entre $y$ et $x_{i}$ s'écrit : $g(E(y)) = \alpha + \sum_{i=1}^{m}f_{m}(x_{m})$, avec $f_{m}$ des fonctions spécifiées (e.g. polynômes) ou des fonctions de lissage.
#' 
#' Dans R, il faut utiliser la fonction `gam` disponible dans la package 'mgcv' : `gam(y ~ s(x1) + s(x2), family = fonction.de.lien)`. La fonction `s()` permet d'inclure un terme lissé dans le modèle.
#' 
#' <div style= "float:right;position: relative;">
#' ![](images/NY.jpg){width=128px}
#' </div>
#' La qualité du modèle s'évalue par la fonction `gam.check()`.
#' 
#' On va modéliser la concentration en ozone (Ozone) des données *airquality* en fonction de la vitesse du vent (Wind) et de la température (Temp) avec un GAM.
## ----ozone-gam, exercise = T, exercise.eval = T, exercise.lines = 9------


#' 
#' 
#' Quelques graphiques pour l'interprétation :
#' 
## ----gam-plot, exercise = T, exercise.eval = F, exercise.lines = 9-------
plot(modgam, pages = 1, scheme = 1)
vis.gam(modgam, view = c("Wind", "Temp"), main = "Ozone", plot.type = "contour", color = "cm")
vis.gam(modgam, view = c("Wind", "Temp"), main = "Ozone", plot.type = "persp", color = "cm")

