library(tidymodels)
library(tidyverse)
library(FactoMineR)
library(factoextra)

#load db
data("USArrests")

#analis global de los datos
head(USArrests)

library(skimr)
skim(USArrests)
#test de normalidad para verificar la normalidad
#H0 data sigue distri normal
shapiro.test(USArrests$Murder) #si
shapiro.test(USArrests$Assault)
shapiro.test(USArrests$UrbanPop ) #si
shapiro.test(USArrests$Rape)

library(PerformanceAnalytics)
chart.Correlation(USArrests,histogram=T,pch=19)
#para q correlaciones ?
#como cor murder y assault correlacion = 0.8 es muy alta
#existe dependendcia entre ellos alta, entonces no las podria poner
#en nu modelo entre ellas existe dependencia entonces
#podria usar PCA para ver su independencia ...


arrestpca <- PCA(USArrests, graph=T)
#var proyecciones de variables originales
#ind individudos
#eig vemos los pesos de vars para determinar q var es mas importante q otra
#escoger uno o dos q expliquen me mayor manera la varianza
arrestpca$eig
#       eigenvalue percentage of variance cumulative percentage of variance
#comp 1  2.4802416              62.006039                          62.00604
#comp 2  0.9897652              24.744129                          86.75017

#tomo solo comp1 y 2 porq juntos explican 86% varianza osea la mayoria

arrestpca$var
#$cor
#Dim.1      Dim.2      Dim.3       Dim.4
#Murder   0.8439764 -0.4160354  0.2037600  0.27037052
#Assault  0.9184432 -0.1870211  0.1601192 -0.30959159
#UrbanPop 0.4381168  0.8683282  0.2257242  0.05575330
#Rape     0.8558394  0.1664602 -0.4883190  0.03707412


arrestpca$ind
#coord explica los valores por dimension
arrestpca.pca_var <- arrestpca$var
arrestpca.pca_ind <- arrestpca$ind

#grafico de la varianza ecplicada
fviz_eig(arrestpca)
#grafica de indiv
fviz_pca_ind(arrestpca,repel=T)
fviz_pca(arrestpca)
fviz_pca_biplot(arrestpca,repel=T)


##########3 CLASI NO SUPERSIVZADA
#CA analisis de correspondencias

data("housetasks")
head(housetasks)

#para ver correlation , si hay H0= son iguales
chisq <- chisq.test(housetasks)

  #se acepta H1 son indpendientes   
chart.Correlation(housetasks,histogram=T,pch=19)     
     
tasksca <- CA(housetasks, graph = F)


#grafica
  
  fviz_ca_biplot(tasksca, repel=T)
  
########################## clusters 
  #no jeararquico
USArrests_sc <- scale(USArrests)
set.seed(123)
km_USArrests <- kmeans(USArrests_sc,3,nstart = 25) #25 iteraciones k=3 

fviz_cluster(km_USArrests,data=USArrests_sc)
fviz_cluster(km_USArrests,data=USArrests_sc,palette="jco",ggtheme = theme_minimal())
  
#cluster jerarquixo
#dist normalizamos datos
hc_USArrests <-  hclust(dist(USArrests_sc),method = "ward.D2")
fviz_dend(hc_USArrests,cex=0.5,palette = "jco",k=10) 


###################################3 REMUESTREO COMPARANDO MODELOS
### CROSS VLAIDATION
#validando la robustes de mi modelo, me da los parametros promedio mejores al final
#en cambio bootstrap y bagging me da el modelo final

#LOOCV leave one out cross : no tengo muchos datos
#un dato qda afura y con esa hago validation

#K-FOLD CROSS VALIDATION
#armo 10 grupos, 1 grupo para validar y 9 para training, y lueg otra division con otro gupo de valiy training



######################3 SOBREAJUSTE
#funciona bien con datos de training pero no con datos reales entonces sobreajuste
#entonces PARAMETROS DE AJUSTE HYER PARAMETROS

#BUSQUEDA DE PARAMETRS PARA DISCRIMINAR VARIABELES
#prospeccion eficente de modelos
#forward selection,
#bakward selection








