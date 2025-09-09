#Machine Learnig 7 (regresión lineal)

#Cargar la data

library(tidyverse)
library(lattice)
library(caret)

theme_set(theme_bw())

#install.packages("devtools") 
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

RMSE(predicventas2,test_base$sales)

R2(predicventas2,test_base$sales)

MAE(predicventas2,test_base$sales)

rendimiento <-data.frame(modelo="modventas2",RMSE=RMSE(predicventas2,test_base$sales),R2=R2(predicventas2,test_base$sales),MAE=MAE(predicventas2,test_base$sales))


#especificación de modelo a utilzar con train
names(getModelInfo())
library(caret)


##LOOCV
#remuestreo
#pocos datos dejo uno a fuera y entreno con el resto para pocos datos !!!
#remuestreo a la regresion lineal
modventas5 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                    +youtube:newspaper,data = training_base
                    ,method="lm",trControl=trainControl(method = "LOOCV"))

#rendimiento de todos los remuestreos usando LM
modventas5$results
modventas5$bestTune
#  intercept
#1      TRUE me dice q delei use un punto  de corte

#predicciones

#predicciones

predicventas5 <- modventas5%>%
  predict(test_base)

predicventas5_df <- data.frame(pred_sales=predicventas5,sales=test_base$sales)

#Métricas de desempeño

RMSE(predicventas5_df$pred_sales,predicventas5_df$sales)

R2(predicventas5_df$pred_sales,predicventas5_df$sales)

MAE(predicventas5_df$pred_sales,predicventas5_df$sales)

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas5",RMSE=RMSE(predicventas5_df$pred_sales,predicventas5_df$sales),R2=R2(predicventas5_df$pred_sales,predicventas5_df$sales),MAE=MAE(predicventas5_df$pred_sales,predicventas5_df$sales)))


#k-fold remuestro

modventas6 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                    +youtube:newspaper,data = training_base
                    ,method="lm",trControl=trainControl(method = "cv",number=10))
#el promedio de cada iteracion
modventas6$results

#predicciones

#predicciones

predicventas6 <- modventas6%>%
  predict(test_base)

predicventas6_df <- data.frame(pred_sales=predicventas6,sales=test_base$sales)

#Métricas de desempeño

RMSE(predicventas6_df$pred_sales,predicventas6_df$sales)

R2(predicventas6_df$pred_sales,predicventas6_df$sales)

MAE(predicventas6_df$pred_sales,predicventas6_df$sales)

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas6",RMSE=RMSE(predicventas6_df$pred_sales,predicventas6_df$sales),R2=R2(predicventas6_df$pred_sales,predicventas6_df$sales),MAE=MAE(predicventas6_df$pred_sales,predicventas6_df$sales)))


#Repated-fold remuestro

modventas7 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                    +youtube:newspaper,data = training_base
                    ,method="lm",trControl=trainControl(method = "repeatedcv",number=10,repeats = 3))
#el promedio de cada iteracion
modventas7$results


#predicciones

#predicciones

predicventas7 <- modventas6%>%
  predict(test_base)

predicventas7_df <- data.frame(pred_sales=predicventas7,sales=test_base$sales)

#Métricas de desempeño

RMSE(predicventas7_df$pred_sales,predicventas7_df$sales)

R2(predicventas7_df$pred_sales,predicventas7_df$sales)

MAE(predicventas7_df$pred_sales,predicventas7_df$sales)

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas7",RMSE=RMSE(predicventas7_df$pred_sales,predicventas7_df$sales),R2=R2(predicventas7_df$pred_sales,predicventas7_df$sales),MAE=MAE(predicventas7_df$pred_sales,predicventas7_df$sales)))




###Baggin- random forest
set.seed(123)
modventas8 <- train(sales~youtube+facebook+newspaper+youtube:facebook
                    +youtube:newspaper,data = training_base
                    ,method="rf",trControl=trainControl(method = "cv",number=10))

modventas8$bestTune


#predicciones

predicventas8 <- modventas8%>%
  predict(test_base)

predicventas8_df <- data.frame(pred_sales=predicventas8,sales=test_base$sales)

#Métricas de desempeño

RMSE(predicventas8_df$pred_sales,predicventas8_df$sales)

R2(predicventas8_df$pred_sales,predicventas8_df$sales)

MAE(predicventas8_df$pred_sales,predicventas8_df$sales)

rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas8",RMSE=RMSE(predicventas8_df$pred_sales,predicventas8_df$sales),R2=R2(predicventas8_df$pred_sales,predicventas8_df$sales),MAE=MAE(predicventas8_df$pred_sales,predicventas8_df$sales)))







