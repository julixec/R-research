#Machine Learnig 8 (regresión lineal)

#Cargar la data

library(tidyverse)
library(lattice)
library(caret)

theme_set(theme_bw())

install.packages("devtools") 
library(devtools)
devtools::install_github("kassambara/datarium")

#Cargar datos

data("marketing", package = "datarium")
head(marketing)

#Características de los datos

str(marketing)

library(skimr)

skim(marketing)


#Análisis de correlaciones

library(PerformanceAnalytics)

chart.Correlation(marketing,histogram = T,pch=19)

library(recipes)

#aplicación de preprocesamiento

mk_rec <- recipe(sales~.,data = marketing)%>%
  step_zv(all_numeric(),-all_outcomes())%>%
  step_mutate(distcic=runif(nrow(marketing),0,1))%>%          
  step_mutate(cic=as.factor(if_else(distcic>0.8,"Norm","Rec")))%>%
  step_select(-distcic)%>%
  prep()

mk_rec

marketing1 <- juice(mk_rec)

#Partir base

library(caret)

set.seed(123)
training_sample <- marketing1$sales%>%
  createDataPartition(p=0.8,list = F)

training_base <- marketing1[training_sample,]

test_base <- marketing1[-training_sample,]


#modelo 2

modventas2 <- lm(sales~youtube+facebook+newspaper+youtube:facebook+youtube:newspaper,data = training_base)

summary(modventas2)

#predicciones

predicventas2 <- modventas2%>%
  predict(test_base)

predicventas2_df <- data.frame(predicventas2)

#Métricas de desempeño

rendimiento <-data.frame(modelo="modventas2",RMSE=RMSE(predicventas2,test_base$sales),R2=R2(predicventas2,test_base$sales),MAE=MAE(predicventas2,test_base$sales))


#modelo 6

#especificación de modelo a utilzar con train
names(getModelInfo())
library(caret)

#k-fold remuestro
set.seed(123)
modventas6 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                    +youtube:newspaper,data = training_base
                    ,method="lm",trControl=trainControl(method = "cv",number=10))

modventas6$bestTune

#predicciones

predicventas6 <- modventas6%>%
  predict(test_base)

predicventas6_df <- data.frame(pred_sales=predicventas6,sales=test_base$sales)

#Métricas de desempeño

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas6",RMSE=RMSE(predicventas6_df$pred_sales,predicventas6_df$sales),R2=R2(predicventas6_df$pred_sales,predicventas6_df$sales),MAE=MAE(predicventas6_df$pred_sales,predicventas6_df$sales)))


#modelo 8
###Baggin- random forest

#selección automática de parámetros

set.seed(123)
modventas8 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                    +youtube:newspaper,data = training_base
                    ,method="rf",trControl=trainControl(method = "cv",number=10))

modventas8

#predicciones

predicventas8 <- modventas8%>%
  predict(test_base)

predicventas8_df <- data.frame(pred_sales=predicventas8,sales=test_base$sales)

#Métricas de desempeño

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas8",RMSE=RMSE(predicventas8_df$pred_sales,predicventas8_df$sales),R2=R2(predicventas8_df$pred_sales,predicventas8_df$sales),MAE=MAE(predicventas8_df$pred_sales,predicventas8_df$sales)))


#modelo9

#selección a prior de parámetros

rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

###Baggin- random forest (2)
set.seed(123)
modventas9 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                    +youtube:newspaper,data = training_base
                    ,method="rf"
                    ,trControl=trainControl(method = "cv",number=10)
                    ,tuneGrid=rfGrid)

modventas9$bestTune

#predicciones

predicventas9 <- modventas9%>%
  predict(test_base)

predicventas9_df <- data.frame(pred_sales=predicventas9,sales=test_base$sales)

#Métricas de desempeño

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas9",RMSE=RMSE(predicventas9_df$pred_sales,predicventas9_df$sales),R2=R2(predicventas9_df$pred_sales,predicventas9_df$sales),MAE=MAE(predicventas9_df$pred_sales,predicventas9_df$sales)))

#Gráfico
trellis.par.set(caretTheme())
plot(modventas9)  


#modelo 10
#Especificación directa del modelo

###Baggin- random forest (3)
set.seed(123)
modventas10 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                     +youtube:newspaper,data = training_base
                     ,method="rf"
                     ,trControl=trainControl(method = "none")
                     ,tuneGrid=data.frame(mtry=4))

modventas10

#predicciones

predicventas10 <- modventas10%>%
  predict(test_base)

predicventas10_df <- data.frame(pred_sales=predicventas10,sales=test_base$sales)

#Métricas de desempeño

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas10",RMSE=RMSE(predicventas10_df$pred_sales,predicventas10_df$sales),R2=R2(predicventas10_df$pred_sales,predicventas10_df$sales),MAE=MAE(predicventas10_df$pred_sales,predicventas10_df$sales)))



#modelo 11
#Especificación directa del modelo

###Regresión lineal

modventas11 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                     +youtube:newspaper,data = training_base
                     ,method="lm"
                     ,trControl=trainControl(method = "none")
                     ,tuneGrid=data.frame(intercept=T))

modventas11

#predicciones

predicventas11 <- modventas11%>%
  predict(test_base)

predicventas11_df <- data.frame(pred_sales=predicventas11,sales=test_base$sales)

#Métricas de desempeño

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas11",RMSE=RMSE(predicventas11_df$pred_sales,predicventas11_df$sales),R2=R2(predicventas11_df$pred_sales,predicventas11_df$sales),MAE=MAE(predicventas11_df$pred_sales,predicventas11_df$sales)))


#modelo 12
#Selección de modelo stepward

###Regresión lineal

library(leaps)
set.seed(123)
modventas12 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                     +youtube:newspaper,data = training_base
                     ,method="leapSeq"
                     ,trControl=trainControl(method = "cv",number=10)
                     ,tuneGrid=data.frame(nvmax=1:5))#parametros 
#rgresion 1 nivel entonces te dice q variables incluir
#1  ( 1 ) " "     " "      " "       "*"              " " 

#entonces youtube debe ser seleccionada si se toma en cuenta regresion lineal con 1 nivel
modventas12$results
modventas12$bestTune

summary(modventas12)


#####Modelos de clasificación


data("iris")
head(iris)

distinct(iris,Species)

set.seed(123)
training_sample <- iris$Species%>%
  createDataPartition(p=0.8,list = F)

training_base <- iris[training_sample,]

test_base <- iris[-training_sample,]


#Análisis discriminante

library(MASS)

irismodel1 <- train(Species~.,data = training_base
                    ,method="lda"
                    ,trControl=trainControl(method = "cv",number=10)
                    ,preProcess=c("center", "scale"))
irismodel1$bestTune

#predicciones

prediciris1 <- irismodel1%>%
  predict(test_base)

prediciris1_df <- data.frame(pred_class=prediciris1,class=test_base$Species)

#Métricas de desempeño

install.packages("rfUtilities")
library(rfUtilities)

accu_iris <- accuracy(prediciris1_df$pred_class,prediciris1_df$class)

accu_iris$
  
  rendimiento <- data.frame(modelo="irismodel1"
                            ,Accur=accu_iris$PCC)



#Boosting
library(xgboost)

irismodel2 <- train(Species~.,data = training_base
                    ,method="xgbTree"
                    ,trControl=trainControl(method = "cv",number=10))

irismodel2$bestTune

irismodel2

#predicciones

prediciris2 <- irismodel2%>%
  predict(test_base)

prediciris2_df <- data.frame(pred_class=prediciris2,class=test_base$Species)

#Métricas de desempeño

library(rfUtilities)

accu_iris2 <- accuracy(prediciris2_df$pred_class,prediciris2_df$class)

rendimiento <- rendimiento%>%
  bind_rows(data.frame(modelo="irismodel2"
                       ,Accur=accu_iris2$PCC))


#importancia de las variables
varImp(irismodel2)
