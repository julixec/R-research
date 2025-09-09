#Machine Learning 4 (clasificación supervisada)

#Preprocesamiento de datos

#Problema: Predecir qué tipo de reservas en un hotel podría tener niños

#Cargar base de datos

#ecosistema de paquetes tidymodels

install.packages("tidymodels")
library(tidymodels)
install.packages("klaR")

library(tidyverse)

hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

str(hotels)
head(hotels)

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

contrasts(hotels_df$children)

hotels_df <-hotels_df%>% 
  mutate(children = relevel(children, ref ="none"))

contrasts(hotels_df$children)

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

test_proc <- bake(hotel_rec,new_data = hotel_test)

#Comando que permite extraer la data de la receta

hotel_train_rec <- juice(hotel_rec)

#Modelo de regresión logística

modeloreg1 <- glm(children~.,
                  data = hotel_train_rec,
                  family = binomial)

summary(modeloreg1)

#coeficientes y ratios odds

coefmodelreg1 <- data.frame(b=coef(modeloreg1))%>%
  mutate(exp_b=exp(b))

contrasts(hotels_df$meal)

#predicciones

predicmodel1 <- modeloreg1%>%
  predict(test_proc,type="response")

predicmodel1_class <- if_else(predicmodel1>0.5,"children","none")

pm1 <- data.frame(child=test_proc$children,pm1=predicmodel1)
resultadosmodel1 <- data.frame(Predict_class=as.factor(predicmodel1_class),
                               class=test_proc$children)

contrasts(resultadosmodel1$Predict_class)
resultadosmodel1 <-resultadosmodel1%>% 
  mutate(Predict_class = relevel(Predict_class, ref ="none"))
contrasts(resultadosmodel1$Predict_class)

#occuracy
mean(predicmodel1_class==test_proc$children)

mean(predicmodel1_class!=test_proc$children)

table(resultadosmodel1$Predict_class,resultadosmodel1$class)

library(caret)

install.packages("e1071")
library(e1071)

confusionMatrix(resultadosmodel1$Predict_class,resultadosmodel1$class)


#matriz ROC alternativa

library(pROC)

roc_model1 <- roc(resultadosmodel1$class,predicmodel1)
plot.roc(roc_model1,print.auc=T)

library(MASS)

modeloopt <- modeloreg1%>%
  stepAIC(trace=F)

summary(modeloopt)


##Naive Bayesiano

library(klaR)
modelonaive2 <- NaiveBayes(children~.,data = hotel_train_rec)
summary(modelonaive2)

#predicciones

predicmodel2 <- modelonaive2%>%
  predict(test_proc)

predicmodel2$posterior


#El objeto de la predicción es diferente (prob.posterior y clase)  

resultadosmodel2 <- data.frame(pred_prob=predicmodel2$posterior[,2],
                               pred_class=predicmodel2$class,
                               class=test_proc$children)

contrasts(resultadosmodel2$pred_class)

mean(resultadosmodel2$pred_class==test_proc$children)

mean(resultadosmodel2$pred_class!=test_proc$children)

table(resultadosmodel2$pred_class,resultadosmodel2$class)

library(caret)

install.packages("e1071")
library(e1071)

confusionMatrix(resultadosmodel2$pred_class,resultadosmodel2$class)

#matriz ROC alternativa

library(pROC)

roc_model2 <- roc(resultadosmodel2$class,resultadosmodel2$pred_prob)
plot.roc(roc_model2,print.auc=T)


# Máquina vectorial de soporte (SVM)

library(kernlab)

set.seed(123)
modelsvm3 <- train(children~.,data = hotel_train_rec,method="svmLinear",
                   trControl=trainControl("cv",number = 10),
                   preProcess=c("center","scale"))

summary(modelsvm3)

#predicciones
predicmodel3 <- modelsvm3%>%
  predict(test_proc)

#predice directamente la clase
predicmodel3    

mean(predicmodel3==test_proc$children)

mean(predicmodel3!=test_proc$children)

table(predicmodel3,test_proc$children)

library(caret)

install.packages("e1071")
library(e1071)

confusionMatrix(predicmodel3,test_proc$children)

##en este método se puede optimizar un parámetro c (costo) (posibles errores de clasificación)


#Modelo discriminante lineal

library(MASS)

modelodisc4 <- lda(children~.,data = hotel_train_rec)

summary(modelodisc4)

#predicciones

predicmodel4 <- modelodisc4%>%
  predict(test_proc)

#analizar
predicmodel4$posterior

resultadosmodel4 <- data.frame(Pred_class=predicmodel4$class, 
                               class=test_proc$children,
                               pred_prob=predicmodel4$posterior[,2])


contrasts(resultadosmodel4$Pred_class)

mean(resultadosmodel4$Pred_class==test_proc$children)

mean(resultadosmodel4$Pred_class!=test_proc$children)


library(caret)
install.packages("e1071")
library(e1071)

confusionMatrix(resultadosmodel4$Pred_class,resultadosmodel4$class)

#matriz ROC alternativa

library(pROC)

roc_model4 <- roc(resultadosmodel4$class,resultadosmodel4$pred_prob)
plot.roc(roc_model4,print.auc=T)

##arbol de decisión

library(rpart)
library(rpart.plot)

#modelo

set.seed(2)
modelotree5 <- rpart(children~.,data = hotel_train_rec,method = "class")

print(modelotree5)

#Graficar arbol de decisión
par(mfrow=c(1,1))
rpart.plot(modelotree5,extra = 4,cex = 0.75,cex.main=0.75)

#ajustar modelo
printcp(modelotree5)
plotcp(modelotree5)

#podar árbol
modelotree5_f <- prune(modelotree5,cp=0.015357)
print(modelotree5_f)

#predicciones

predicmodel5 <- modelotree5%>%
  predict(test_proc)

resultadosmodel5 <- data.frame(pred_class=as.factor(if_else(predicmodel5[,2]>0.5,
                                                            "children","none"))
                               ,class=test_proc$children
                               ,pred_prob=predicmodel5[,2])

contrasts(resultadosmodel5$pred_class)
resultadosmodel5 <-resultadosmodel5%>% 
  mutate(pred_class = relevel(pred_class, ref ="none"))
contrasts(resultadosmodel5$pred_class)

#occuracy
mean(resultadosmodel5$pred_class==test_proc$children)
mean(resultadosmodel5$pred_class!=test_proc$children)
table(resultadosmodel5$pred_class,resultadosmodel5$class)

#Matriz de confusion
library(caret)
library(e1071)

confusionMatrix(resultadosmodel5$pred_class,resultadosmodel5$class)

#matriz ROC alternativa
library(pROC)
roc_model5 <- roc(resultadosmodel5$class,resultadosmodel5$pred_prob)
plot.roc(roc_model5,print.auc=T)


##kknn

library(kknn)

set.seed(123)
modelkknn7 <- train(children ~., data = hotel_train_rec, 
                    method = "knn",
                    trControl = trainControl("cv", number = 10),
                    preProcess=c("center","scale"),
                    tuneLength = 9)

plot(modelkknn7)

modelkknn7$bestTune
  
  #predicciones
  
  predicmodel7 <- modelkknn7%>%
  predict(test_proc)

resultadosmodel7 <- data.frame(pred_class=predicmodel7)

#occuracy
mean(resultadosmodel7$pred_class==test_proc$children)
mean(resultadosmodel7$pred_class!=test_proc$children)

#Matriz de confusion
library(caret)
library(e1071)
confusionMatrix(resultadosmodel7$pred_class,test_proc$children)

#Boosting
library(xgboost)

set.seed(123)
model6 <- train(children ~., data = hotel_train_rec, 
                method = "xgbTree",
                trControl = trainControl("cv", number = 10))

model6

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

##Baggin
install.packages("randomForest")
library(randomForest)

set.seed(123)
model8 <- train(children ~., data = hotel_train_rec, 
                method = "rf",
                trControl = trainControl("cv", number = 10),importance=T)


##Comandos boosting
#train
#metod="rf"
#importance=T






