#Machine Learning 4 (clasificación supervisada)
#regresion lodistica
#



#Preprocesamiento de datos

#Problema: Predecir qué tipo de reservas en un hotel podría tener niños

#Cargar base de datos

#ecosistema de paquetes tidymodels

#install.packages("tidymodels")
library(tidymodels)
#install.packages("klaR")

library(tidyverse)

hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

str(hotels)
head(hotels)
View(hotels)
#depuración de la base con casos de interés

hotels_stays <- hotels%>%
  filter(is_canceled==0)%>%  
  mutate(children=case_when(children+babies>0~"children",T~"none"),
         required_car_parking_spaces=case_when(required_car_parking_spaces>0~"parquing",T~"none"))%>%
  select(-is_canceled,-reservation_status,-babies)

summary(hotels_stays$adr)


install.packages("skimr")

library(skimr)

skim(hotels_stays)

#convertir variaibles de caracter a factor 


hotels_df <- hotels_stays%>%
  select(children,hotel,arrival_date_month,meal,adr,adults,required_car_parking_spaces,
         total_of_special_requests,stays_in_week_nights,stays_in_weekend_nights)%>%
  mutate_if(is.character,factor)

skim(hotels_df)
View(hotels_df)
contrasts(hotels_df$children)

hotels_df <-hotels_df%>% 
  mutate(children = relevel(children, ref ="none"))
#encontrar la variable base si child no child
contrasts(hotels_df$children)
View(hotels_df)
#generar aleatoriamente una base de entrenamiento y una base de prueba

library(recipes)
library(rsample)

hotel_split <- initial_split(hotels_df)

hotel_train <- training(hotel_split)
hotel_test <- testing(hotel_split)

#Procesamiento de datos

hotel_rec <- recipe(children~.,data = hotel_train)%>%
  themis::step_downsample(children)%>%
  recipes::step_dummy(all_nominal(),-all_outcomes())%>%
  recipes::step_zv(all_numeric())%>%
  recipes::step_normalize(all_numeric())%>%
  recipes::prep()
hotel_rec

View(hotel_rec)
View(hotel_test)

test_proc <- bake(hotel_rec,new_data = hotel_test)

#Comando que permite extraer la data de la receta

hotel_train_rec <- juice(hotel_rec) #valores binarios
str(hotel_train_rec)

#modelo de regresion logistica
modeloreg1 <-glm(children~.,
                 data=hotel_train_rec,
                 family = binomial)
View(modeloreg1)                
summary(modeloreg1)
autoplot(modeloreg1)

coefmodelreg1 <- data.frame(b=coef(modeloreg1))%>%
mutate(exp_b=exp(b))

plot(coefmodelreg1$b)
#interpretacion 
#con var quantitativas adr 1.214955392  3.3701437 si chldren then adr precio reservacion se incrementa 3.37 veces mas
#con nominales mber meal_FB
#-0.018737195
#0.9814373 98% de probabilidad si children=Y de seleccionar esta comida
contrasts(hotels_df$meal)
#########3
#predicciones del modelo
predictmodel1 <- modeloreg1%>%
  predict(test_proc,type="response")

View(predictmodel1)
#vamos a estimar los valores de las clases
predictmodel1_class <- if_else(predictmodel1>0.5,"children","none")
View(predictmodel1_class)
resultadosmodel1 <- data.frame(Predic_class=as.factor(predictmodel1_class),
class=test_proc$children)

pm1 <- data.frame(predictmodel1)
pm1 <- data.frame(child=test_proc$children,pm1=predictmodel1)
#desplegar las probabilidades


View(pm1)
#pm1[pm1["child"] == "children"] <- 1
#pm1[pm1 == "none"] <- 2

View(pm1)

#cambiamos el children objetivo a 1
contrasts(resultadosmodel1$Predic_class)
resultadosmodel1 <- resultadosmodel1%>%
mutate(Predic_class= relevel(Predic_class, ref ="none")) #

contrasts(resultadosmodel1$Predic_class)
contrasts(resultadosmodel1$class)

View(resultadosmodel1)
#accuracy
mean(predictmodel1_class == test_proc$children)
mean(predictmodel1_class != test_proc$children)
View(test_proc)

#tabla de confusion
table(resultadosmodel1$Predic_class,resultadosmodel1$class)

library(caret)
install.packages("e107")
install.packages("e1071")
library(e107)
library(e1071)
confusionMatrix(resultadosmodel1$Predic_class,resultadosmodel1$class)

#matiz roc alternativa
library(pROC)
roc_model1 <- roc(resultadosmodel1$class,predictmodel1)
plot.roc(roc_model1,print.auc = T)


#curva roc goal mayor nivel de y superior derecha lo mas cercano a 1
#sepecificy realmente es 1 - specificity

#metrica de rendimiento para clasfiicacion y regresion
library(MASS)
modeloopt <- modeloreg1%>%
  stepAIC(trace=F)
  
summary(modeloopt)
#clasification:analisis discriminante
# similar a regresion los betas no se interpretan
#si se cumple ???? check conditions



#NAIVE BAYESIANO
#vars deben ser independientes
library(Rcpp)
library(klaR)

modelonaive2 <- klaR::NaiveBayes(children~.,data=hotel_train_rec)
summary(modelonaive2)


#rcmodelonaive2 <-naiveBayes(children~.,data=hotel_train_rec)
#summary(modelonaive2)
#almacenar las predicciones
predicmodel2 <- modelonaive2 %>%
predict(test_proc)
  
  
#le objeto de la prediccion es diferente (prob. posterior y clase)
predicmodel2$posterior
#
resultadosmodel2 <- data.frame(pred_prob=predicmodel2$posterior[,2],
                               pred_class=predicmodel2$class,
                               class=test_proc$children)
  
contrasts(resultadosmodel2$pred_class)
#para sacar la proporcion verdadero vs predicho
mean(resultadosmodel2$pred_class==test_proc$children)
mean(resultadosmodel2$pred_class!=test_proc$children)
#tabla de confusion

table(resultadosmodel2$pred_class,resultadosmodel2$class)
confusionMatrix(resultadosmodel2$pred_class,resultadosmodel2$class)

#matiz roc alternativa
library(pROC)
roc_model2 <- roc(resultadosmodel2$class,resultadosmodel2$pred_prob)
plot.roc(roc_model2,print.auc = T)


## Máquina vectorial de soporte (SVM)

library(kernlab)

set.seed(123)

#10 hiperplanos
View(hotel_train_rec)
#preProcess=c("center","scale") preprocesar los datos, centrar scalar para normalizar
#esta clasificando
modelsvm3 <- train(children~.,data = hotel_train_rec,method="svmLinear",
                   trControl=trainControl("cv",number = 10),preProcess=c("center","scale"))

summary(modelsvm3)

#predicciones

predicmodel3 <- modelsvm3%>%
  predict(test_proc)

#predice directamente la clase
#meno que 0 : none caso contrario children respecto al hiperplano
View(predicmodel3)    
#identicas a verdades entonces clacule la propocion
mean(predicmodel3==test_proc$children) #accuracy de svm
#> mean(predicmodel3==test_proc$children)
#[1] 0.7644742 = accuracy


mean(predicmodel3!=test_proc$children)

table(predicmodel3,test_proc$children)

library(caret)

#install.packages("e1071")
library(e1071)

confusionMatrix(predicmodel3,test_proc$children)

##en este método se puede optimizar un parámetro c (costo) (posibles errores de clasificación)

#matiz roc alternativa no se puede calcular porq no existe predicciones solo svm me arroja las categorias y exactitud con el mean

####################### MODELO DISCRIMINANTE LINEAL ###########
# hay q normalizar

library(MASS)

modelodisc4 <- lda(children~.,data=hotel_train_rec)


summary(modelodisc4)
predicmodel4 <- modelodisc4%>%
  predict(test_proc)
  
predicmodel4

resultadosmodel4 <- data.frame(Pred_class=predicmodel4$class, #clase prediccion
                               class=test_proc$children,#clase verdadera
                               pred_prob=predicmodel4$posterior[,2]) #probabilidad
#> predicmodel4[["posterior"]]
#none   children
#1     0.9091178647 0.09088214
#2     0.7047590205 0.29524098
#logistica #3     0.6571051526 0.34289485
contrasts(resultadosmodel4$Pred_class)
View(predicmodel4)


#para sacar la proporcion verdadero vs predicho
#para ver el accuracy
mean(resultadosmodel4$Pred_class==test_proc$children)
mean(resultadosmodel4$Pred_class!=test_proc$children)
#tabla de confusion

table(resultadosmodel4$Pred_class,resultadosmodel2$class)
confusionMatrix(resultadosmodel4$Pred_class,resultadosmodel2$class)

#matiz roc alternativa
library(pROC)
roc_model4 <- roc(resultadosmodel4$class,resultadosmodel4$pred_prob)

plot.roc(roc_model4,print.auc = T)


#hemos visto regresion logistica - CLASSIFICATION SUPERVIZED

#regresion logistica
#$cuantitiv a o nominal

#analisis discriminante
#izquierdo los regresores sin interpretacion, lado derecho combinacion lineal
#vars cuantitavias deben seguir distribucion normal con balanceo en var dependiente


#naive vayas
#calula las proba a priori y clacula la posteriori
#limitacion: deben ser las vars independneites, nos ayuda a clasificar pero el 
#resultado de probabilidad on es muy potente, si como clasificador funciona


#svm
#definicion de un hiperplano la cmnicaicon de 2 variables = 0, varios hiperplanos para enocntrar
#el mejor hiperplano para una semjor seleccion
#no considera probailidades
#valor >0 si al plano caso contrario fuera del plano
#mas perfomrmance en PC uso

#decision treeeeeeeeeeeeeeeeeeee
#para hacer analisis exploratorio
#ejecuto el dtree y me quedo solo con las variables arrojadas por el dtree
#entonces hay q hacer parametricas (comparacion de medias correlaciones) y no parametricas (dtree) para eliminar una variable
#con esto puedo eliminar una variable teórica caso contrario si delei se necesita por teoria
#tengo q dejarla

library(rpart)
library(rpart.plot)
#install.packages("rpart.plot")

#modelo basado en iteraciones

set.seed(2) #identificador
modelotree5 <- rpart(children~.,data=hotel_train_rec,method="class")
View(modelotree5)

print(modelotree5)
#grafica
rpart.plot(modelotree5,extra=4,cex=0.75,cex.main=0.75)


#se puede optimizar el tree al eliminar ramas podar el arbol
#ajustar el modelo
#hay veces q el cp baj baj baja y luego sube enotnces donde suvae ahay q pruning podar esa rama
printcp(modelotree5)
plotcp(modelotree5)
#hay q identificar en q cp
modelotree5_f <- prune(modelotree5,cp=0.011206)
print(modelotree5_f)
rpart.plot(modelotree5_f,extra=4,cex=0.75,cex.main=0.75)
printcp(modelotree5_f)

#predicciones
predicmodel5 <- modelotree5%>%
  predict(test_proc)

resultadosmodel5 <- data.frame(pred_class=as.factor(if_else(predicmodel5[,2]>0.5,"children","none")), #clase prediccion
                               class=test_proc$children,#clase verdadera
                               pred_prob=predicmodel5[,2]) #probabilidad



contrasts(resultadosmodel5$pred_class)
resultadosmodel5 <- resultadosmodel5%>%
  mutate(pred_class=relevel(pred_class, ref="none"))

contrasts(resultadosmodel5$pred_class)


#para sacar la proporcion verdadero vs predicho
#para ver el accuracy
mean(resultadosmodel5$pred_class==test_proc$children)
mean(resultadosmodel5$pred_class!=test_proc$children)
#tabla de confusion

table(resultadosmodel5$pred_class,resultadosmodel5$class)
confusionMatrix(resultadosmodel5$pred_class,resultadosmodel5$class)

#matiz roc alternativa
library(pROC)
roc_model5 <- roc(resultadosmodel5$class,resultadosmodel5$pred_prob)
plot.roc(roc_model5,print.auc = T)

############################## KNN
##kknn

library(kknn)

set.seed(12345)
modelkknn7 <- train(children ~., data = hotel_train_rec,#selecicon datos 
                    method = "knn",#selecionamos knn
                    trControl = trainControl("cv", number = 10), #resampleo10 10 veces toma vecinos diferentes
                    preProcess=c("center","scale"), #normalizando datos ya lo hicimos en la reseta delei hay q hacerla
                    tuneLength = 9)#definir el num de vecinos k=9
#10 veces toma 9 vecinos, por cada vez toma 9 vecinos

plot(modelkknn7)

modelkknn7$
  
  #generar predicciones
  
  predicmodel7 <- modelkknn7%>%
  predict(test_proc)

#guardar el chilndre, none
resultadosmodel7 <- data.frame(pred_class=predicmodel7)

#accuracy
mean(resultadosmodel7$pred_class==test_proc$children)
mean(resultadosmodel7$pred_class!=test_proc$children)

#Matriz de confusion
library(caret)
library(e1071)
confusionMatrix(resultadosmodel7$pred_class,test_proc$children)
#matiz roc alternativa no se puede no se tiene predicciones
#library(pROC)
#roc_model5 <- roc(resultadosmodel7$class,resultadosmodel7$)
#plot.roc(roc_model5,print.auc = T)

#baggin reducir el error de aprendizaje
#muestreo aleatorio con reemplazo o sin reemplazo


#bossting secuencial es mas pesado q baggin
##########################
#Boosting
library(xgboost)

set.seed(123)

#aqui tambien se pueden tunear los campos luego del besttune
model6 <- train(children ~., data = hotel_train_rec, 
                method = "xgbTree", #porq usamos el train xboostgbtree de arboles de decision
                trControl = trainControl("cv", number = 10)) #genera 10 arboles de decision



model6
View(model6)
#importancia de las variables
varImp(model6)

#predicciones
predicmodel6 <- model6%>%
  predict(test_proc)

resultadosmodel6 <- data.frame(pred_class=predicmodel6)

#occuracy
mean(resultadosmodel6$pred_class==test_proc$children)
mean(resultadosmodel6$pred_class!=test_proc$children)

#Matriz de confusion
library(caret)
library(e1071)
confusionMatrix(resultadosmodel6$pred_class,test_proc$children)


#matiz roc alternativa no se puede

#mejores parametros encontrados
model6$bestTune
#help(bestTune)



##Baggin
install.packages("Rtools")
install.packages("randomForest")
install.packages("randomForest", repos="http://R-Forge.R-project.org")
library(randomForest)

set.seed(123)
model8 <- train(children ~., data = hotel_train_rec, 
                method = "rf",
                trControl = trainControl("cv", number = 10),importance=T)


##Comandos boosting
#train
#metod="rf"
#importance=T




