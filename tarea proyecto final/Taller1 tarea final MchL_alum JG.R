##Taller1: Machine Learning

#Problema: Se requiere clasificar a los individuos entre quienes pueden ganar
#m?s de 50 mil d?lares al a?o (variable "Mayor50k") a partir de las restantes variables 
# contenidas en la base "adultos".

#La evaluación final se realizará a partir de la aplicación del siguiente trabajo:

# A partir de la base de datos utilizado en el taller 1 (script “Taller1MchL_alum”) o en el taller 2 (script “Taller2MchL_alum”) solventar mediante la aplicación de técnicas de machine learning el problema:“clasificar a los individuos de la base entre quienes pueden ganar más de 50 mil dólares al año (variable "Mayor50k") y quienes no, mediante la utilización de las restantes variables contenidas en la base de datos” en el primer caso, y "Pronosticar el valor de un índice de fertilidad a partir de 5 indicadores socio-económicos medidos en 47 provincias suizas de habla francesa" en el segundo caso.

#Los puntos mínimos requeridos en esta actividad son los siguientes:

# 1) Realizar un pre-procesamiento de los datos y mediante un comentario justificar las acciones realizadas en esta etapa.

#2)    Segmentar la base de datos en una base de entrenamiento y en una base de testeo

#3) Construir al menos 3 modelos que permitan abordar el problema y mediante un comentario justifique su selección.

#4) Opcional: Generar procesos de re-muestreo.

#5) Evaluar los modelos mediante las métricas de rendimiento pertinentes.

#6) Seleccionar el modelo final y justificar la elección mediante un comentario.

#Los comandos y los comentarios requeridos para el trabajo final deberán ser incorporados en un script que deberá ser guardado con el siguiente formato “tfinal_(letra inicial del nombre)_(apellido)”. Ejemplo: “tfinal_L_Guevara”.

#Finalmente, enviar el script al mail luis_felipe_guevara@yahoo.com y subirlo a la plataforma hasta las 23h55 del 9 de mayo 2022.


##Comandos para la carga de la informaci?n (ejecutar los comandos correspondientes)
##########################################################
#1) Cargar la informaci?n de la base de datos (ejecutar los comandos provistos)


library(tidyverse)
#base para un tema de clasificacion
adultos0 <- readr::read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",col_names =F)
View(adultos0)

#rename columns
names(adultos0) <- c("age","workclass","fnlwgt","education",
                     "educationNum","maritalStatus","occupation",
                     "relationship","race","sex","capitalGain","capitalLoss",
                     "hoursPerWeek","nativeCountry","Mayor50K")


adultos <- select(adultos0,-fnlwgt)
help(select)
#View(adultos0)


#DATA EXPLORATORY
library(skimr)
distinct(adultos0$age)
skim(adultos)
#de1. find missing values
adultos_dfna <- is.na(adultos)
View(adultos_dfna)
distinct(adultos$nativeCountry)

library(sqldf)

#originalmente existen 32561 rows
count(adultos)
#eliminar los valores missing values con ?
adultosdf1 <- sqldf("select age,workclass,education,educationNum,maritalStatus,occupation,
                    relationship,race,sex,capitalGain,capitalLoss,
                    hoursPerWeek,nativeCountry,Mayor50K from adultos0 where
                    age != '?' and
                    workclass != '?' and
                    education != '?' and
                    educationNum != '?' and
                    maritalStatus != '?' and
                    occupation != '?' and
                    relationship != '?' and
                    race != '?' and
                    capitalGain != '?' and
                    capitalLoss != '?' and
                    hoursPerWeek != '?' and
                    nativeCountry != '?' and
                    Mayor50K != '?'    ")

count(adultosdf1)
#uso de tecnica para encontrar las mejores variables con decision trees

#convertir variaibles de caracter a factor 

adultosdf1_factor <- adultosdf1%>%
  # select(children,hotel,arrival_date_month,meal,adr,adults,required_car_parking_spaces,
  #       total_of_special_requests,stays_in_week_nights,stays_in_weekend_nights)%>%
  mutate_if(is.character,factor)
#nos aseguramoes del factor
contrasts(adultosdf1_factor$Mayor50K)
#>50K
#<=50K    0
#>50K     1
#we are ok
##############3 tree
#modelo basado en iteraciones
library(rpart)
library(rpart.plot)


set.seed(2) #identificador
#adultosdf1_factor$

modelotree5 <- rpart(Mayor50K~.,data=adultosdf1_factor,method="class")
View(modelotree5)

print(modelotree5)
#grafica
rpart.plot(modelotree5,extra=4,cex=0.75,cex.main=0.75)

# conclusion1: we found these vars: relathinship, education, capitalGain


#############

#analisis de medias con 2 medias pero como tns mas vars entonces anova

#anova eplicar adr precio basado en el meal
#age
a_anova <- aov(age~Mayor50K, data=adultosdf1_factor)
summary(a_anova)
#entonces H0: means son iguales y H1: son diferentes
#Mayor50K        1  304725  304725    1876 <2e-16 ***
#entonces si age

# educationNum 
a_anova <- aov(educationNum~Mayor50K, data=adultosdf1_factor)
summary(a_anova)
#entonces si educationNum

# capitalGain   
a_anova <- aov(capitalGain~Mayor50K, data=adultosdf1_factor)
summary(a_anova)
# si capitalGain

# capitalLoss     
a_anova <- aov(capitalLoss~Mayor50K, data=adultosdf1_factor)
summary(a_anova)
#si capitalLoss

# hoursPerWeek     
a_anova <- aov(hoursPerWeek ~Mayor50K, data=adultosdf1_factor)
summary(a_anova)

# conclusion1: we found these vars: relathinship, education, capitalGain (decision tree)
# conclusion2: con esto las medias son diferentes es decir existe variabilidad 
#entonces se usaran para el modelo estas variables 
#age, educationNum, capitalGain, capitalLoss, hoursPerWeek, relathinship, education, 

############################################## LOGISTIC REGRESION PARA VALIDAR VARIABLES #################33

#generar aleatoriamente una base de entrenamiento y una base de prueba

library(recipes)
library(rsample)

adultos_split <- initial_split(adultosdf1_factor)
adultos_train <- training(adultos_split)
adultos_test <- testing(adultos_split)


#Procesamiento de datos
#entrenamiento
adultos_rec <- recipe(Mayor50K~.,data = adultos_train)%>%
  themis::step_downsample(Mayor50K)%>%
  recipes::step_dummy(all_nominal(),-all_outcomes())%>%
  recipes::step_zv(all_numeric())%>%
  recipes::step_normalize(all_numeric())%>%
  recipes::prep()
adultos_rec


test_proc <- bake(hotel_rec,new_data = hotel_testing)



#Comando que permite extraer la data de la receta

adultos_train_rec <- juice(adultos_rec) #valores binarios
str(adultos_train_rec)

#modelo de regresion logistica
modeloreg1 <-glm(Mayor50K~.,
                 data=adultos_train_rec,
                 family = binomial)
View(modeloreg1)                
summary(modeloreg1)
#autoplot(modeloreg1)

#coefmodelreg1 <- data.frame(b=coef(modeloreg1))%>%
#  mutate(exp_b=exp(b))

#plot(coefmodelreg1$b)
#interpretacion 

# conclusion1: we found these vars: relathinship, education, capitalGain (decision tree)
# conclusion2: con esto las medias son diferentes es decir existe variabilidad 
#entonces se usaran para el modelo estas variables 
#age, educationNum, capitalGain, capitalLoss, hoursPerWeek, relathinship, education, 
#conclusion3: dado los valores de reglogistica se incluyen las variables maritalStatus,occupation, 

#entonces solo dejamos esas variables
str(adultosdf1_factor)

datos <- select(adultosdf1_factor,age,educationNum,education,maritalStatus,occupation,relationship,capitalGain,
                capitalLoss,hoursPerWeek,Mayor50K)   
############3 MODELAMIENTO #########
######### LOGISTIC REGRESSION
datos_split <- initial_split(datos)
datos_train <- training(datos_split)
datos_test <- testing(datos_split)


#Procesamiento de datos
#entrenamiento
datos_rec <- recipe(Mayor50K~.,data = datos_train)%>%
  themis::step_downsample(Mayor50K)%>%
  recipes::step_dummy(all_nominal(),-all_outcomes())%>%
  recipes::step_zv(all_numeric())%>%
  recipes::step_normalize(all_numeric())%>%
  recipes::prep()
datos_rec


test_proc <- bake(datos_rec,new_data = datos_test)



#Comando que permite extraer la data de la receta

datos_train_rec <- juice(datos_rec) #valores binarios
str(datos_train_rec)

#modelo de regresion logistica
modeloreg1 <-glm(Mayor50K~.,
                 data=datos_train_rec,
                 family = binomial)
#View(modeloreg1)                
summary(modeloreg1)



#predicciones del modelo
predictmodel1 <- modeloreg1%>%
  predict(test_proc,type="response")

View(predictmodel1)
#vamos a estimar los valores de las clases
predictmodel1_class <- if_else(predictmodel1>0.5,">50K","<=50K")
View(predictmodel1_class)
resultadosmodel1 <- data.frame(Predic_class=as.factor(predictmodel1_class),
                               class=test_proc$Mayor50K)

#pm1 <- data.frame(predictmodel1)
#pm1 <- data.frame(child=test_proc$children,pm1=predictmodel1)
#desplegar las probabilidades


#View(pm1)
#pm1[pm1["child"] == "children"] <- 1
#pm1[pm1 == "none"] <- 2

#View(pm1)

#cambiamos el children objetivo a 1
contrasts(resultadosmodel1$Predic_class)
resultadosmodel1 <- resultadosmodel1%>%
  mutate(Predic_class= relevel(Predic_class, ref ="<=50K")) #

contrasts(resultadosmodel1$Predic_class)
contrasts(resultadosmodel1$class)

View(resultadosmodel1)
#accuracy
str(predictmodel1_class)
str(test_proc$Mayor50K)
mean(predictmodel1_class == as.character(test_proc$Mayor50K))
mean(predictmodel1_class != test_proc$Mayor50K)
#View(test_proc)


#tabla de confusion
table(resultadosmodel1$Predic_class,resultadosmodel1$class)

library(caret)
#install.packages("e107")
#install.packages("e1071")
library(e107)
library(e1071)

conf<- confusionMatrix(resultadosmodel1$Predic_class,resultadosmodel1$class)
sensitivity <- conf$byClass['Sensitivity']
precision <- conf$byClass['Pos Pred Value']    
specificity <- conf$byClass['Specificity']
accuracy <- conf$overall['Accuracy']


#matiz roc alternativa
library(pROC)
roc_model1$auc <- roc(resultadosmodel1$class,predictmodel1)
plot.roc(roc_model1,print.auc = T)

#resultados <-data.frame(c(0.8069))

tablacomparativa <- data.frame (modelo  = c("regresion_logistica"),
                                accuracy = c(accuracy),
                                specificity = c(specificity),
                                precision = c(precision),
                                sensitivity = c(sensitivity),
                                roc=c(roc_model1$auc)
)

############################# KNN
set.seed(12345)
modelkknn7 <- train(Mayor50K ~., data = datos_train_rec,#selecicon datos 
                    method = "knn",#selecionamos knn
                    trControl = trainControl("cv", number = 10), #resampleo10 10 veces toma vecinos diferentes
                    preProcess=c("center","scale"), #normalizando datos ya lo hicimos en la reseta delei hay q hacerla
                    tuneLength = 9)#definir el num de vecinos k=9
#10 veces toma 9 vecinos, por cada vez toma 9 vecinos

plot(modelkknn7)

#modelkknn7$

#generar predicciones

predicmodel7 <- modelkknn7%>%
  predict(test_proc)

#guardar
resultadosmodel7 <- data.frame(pred_class=predicmodel7)

#accuracy
mean(resultadosmodel7$pred_class==test_proc$Mayor50K)
mean(resultadosmodel7$pred_class!=test_proc$Mayor50K)



#Matriz de confusion
library(caret)
library(e1071)
conf<-confusionMatrix(resultadosmodel7$pred_class,test_proc$Mayor50K)

sensitivity <- conf$byClass['Sensitivity']
precision <- conf$byClass['Pos Pred Value']    
specificity <- conf$byClass['Specificity']
accuracy <- conf$overall['Accuracy']


#matiz roc alternativa
#library(pROC)
roc_model <- ""
#plot.roc(roc_model1,print.auc = T)


tablacomparativa1 <- data.frame (modelo  = c("knn"),
                                 accuracy = c(accuracy),
                                 specificity = c(specificity),
                                 precision = c(precision),
                                 sensitivity = c(sensitivity),
                                 roc="")
tablacomparativa <-rbind(tablacomparativa, tablacomparativa1)



################################ SVM
#MODELO 3
# Maquina vectorial de soporte (SVM)

#install.packages("kernlab")
library(kernlab)

set.seed(123)
modelsvm3 <- train(Mayor50K~.,data = datos_train_rec,method="svmLinear",
                   trControl=trainControl("cv",number = 10),preProcess=c("center","scale"))

summary(modelsvm3)

#10 interacciones  # es importante que la data este estandarizada o normalizado (centrado y escalado)
#predicciones

predicmodel3 <- modelsvm3%>%
  predict(test_proc)

# esta prediciendo cuál es la clase
#el hiperplano tiene que ser igual a cero, esta generando en los otro si es 1 children mientras menor 0  va generar none

#predice directamente la clase
predicmodel3    

mean(predicmodel3==test_proc$Mayor50K)
mean(predicmodel3!=test_proc$Mayor50K)
table(predicmodel3,test_proc$Mayor50K)

library(caret)
#install.packages("e1071")
library(e1071)

conf<-confusionMatrix(predicmodel3,test_proc$Mayor50K)


sensitivity <- conf$byClass['Sensitivity']
precision <- conf$byClass['Pos Pred Value']    
specificity <- conf$byClass['Specificity']
accuracy <- conf$overall['Accuracy']


#matiz roc alternativa
#library(pROC)
#roc_model1$auc <- roc(resultadosmodel1$class,predictmodel1)
#plot.roc(roc_model1,print.auc = T)


tablacomparativa2 <- data.frame (modelo  = c("svm"),
                                 accuracy = c(accuracy),
                                 specificity = c(specificity),
                                 precision = c(precision),
                                 sensitivity = c(sensitivity),
                                 roc="")
tablacomparativa <-rbind(tablacomparativa, tablacomparativa2)
metricasrendimiento = tablacomparativa[-c(2),]
metricasrendimiento = metricasrendimiento[-c(2),]

# FINAL OUTPUT
#> metricasrendimiento
#modelo  accuracy specificity precision sensitivity               roc
#Accuracy  regresion_logistica 0.8069222   0.8490165 0.9404862   0.7929329 0.903261130929905
#Accuracy3                 knn 0.7823896   0.8234981 0.9291053   0.7687279                  
#Accuracy4                 svm 0.7822570   0.8591175 0.9417326   0.7567138


################################## SVM
#CONCLUSIONES FINALES
#Deacuerdo a las metricas evaluadas se tiene:
#accuracy: el mejor modelo q presenta una exactitud mas alta es el logicr, 
#mientras que el de mejor  Especificidad (Ratio de verdaderos negativos) se eonctronto en SVM.
#POr otro lado, la Sensibilidad (Ratio de verdaderos positivos) se encontro tambien en logicR. Mientras
#que la mejor Precisión (Ratio) fue encontrada en SVM.

#SI queremos enfocarnos en encontrar  verdaderos positivos seleccionamos el logicstic mientras que en 
#verdaderons negativos podriamos usar SVM













