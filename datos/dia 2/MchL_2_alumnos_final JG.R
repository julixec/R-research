#Machine Learning 2

#Preprocesamiento de datos

#Problema: Predecir qu? tipo de reservas en un hotel podr?a tener ni?os

#Cargar base de datos

#ecosistema de paquetes tidymodels

install.packages("tidymodels")
library(tidymodels)

install.packages("recipes")

library(tidyverse)

install.packages("themis")

hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")

str(hotels)
head(hotels)

#depuraci?n de la base con casos de inter?s

hotels_stays <- hotels%>%
  filter(is_canceled==0)%>%  
  mutate(children=case_when(children+babies>0~"children",T~"none"),
         required_car_parking_spaces=case_when(required_car_parking_spaces>0~"parquing",T~"none"))%>%
  select(-is_canceled,-reservation_status,-babies)

names(hotels)

# T permite asignar una opci?n al resto de casos

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

#analisis variable continua

install.packages("moments")
library(moments)

skim(hotels_df$adr)
skewness(hotels_df$adr)
kurtosis(hotels_df$adr)

help("skewness")
hist(hotels_df$adr)
hist(hotels_df$adr,main = "Variable Adr",col = "purple",xlab = "USD",ylab = "Frecuencia")
boxplot(hotels_df$adr)


#variable continua vs factor
mealtalble <- table(hotels_df$meal)
prop.table(mealtalble)

hotels_df%>%     
  group_by(meal)%>%
  summarise(media=mean(adr))

#modelo anova

a_anova <- aov(adr~meal,data = hotels_df)

summary(a_anova)

require(car)
fligner.test(adr ~ meal, data = hotels_df)

#diferencia de medias

t.test(hotels_df$adr~hotels_df$children,var.equal= F,conf.int=T)

#generar aleatoriamente una base de entrenamiento y una base de prueba

library(recipes)
library(rsample)
library(themis)

set.seed(1234)
hotel_split <- initial_split(hotels_df)

help("initial_split")

hotel_train <- training(hotel_split)
hotel_test <- testing(hotel_split)

#Pre-procesamiento de datos
hotel_rec <- recipe(children~.,data = hotel_train)%>%
  themis::step_downsample(children)%>%
  recipes::step_dummy(all_nominal(),-all_outcomes())%>%
  recipes::step_zv(all_numeric())%>%
  recipes::step_normalize(all_numeric())%>%
  recipes::prep()

hotel_rec

test_proc <- bake(hotel_rec,new_data = hotel_test)

#Comando que permite extraer la data de la receta

juice(hotel_rec)%>%count(children)

#Comando que permite extraer la data de la receta

juice(hotel_rec)%>%count(children)


#Aplicaci?n de la librer?a parsnip para seleccionar el modelo

library(workflows)
library(parsnip)
library(tune)
library(yardstick)
library(dials)
library(rpart)

install.packages("kknn")
install.packages("data.table")
library(kknn)
library(data.table)

#Arbol de decisi?n

tree_spec <-decision_tree()%>%
  set_engine("rpart")%>%
  set_mode("classification")

tree_fit <- tree_spec%>%
  fit(children~.,data=juice(hotel_rec))

tree_fit


#Vecino m?s cercano

kknn_spec <-nearest_neighbor()%>%
  set_engine("kknn")%>%
  set_mode("classification")

kknn_fit <- kknn_spec%>%
  fit(children~.,data=juice(hotel_rec))
help(fit)

kknn_fit



#Funcionamiento del modelo

set.seed(1234)
validation_splits <- mc_cv(juice(hotel_rec), prop = 0.9, strata = children)
validation_splits

#arbol de decisi?n

tree_res <- tune::fit_resamples(tree_spec,children ~ ., validation_splits, 
                                control = control_resamples(save_pred = TRUE))
tree_res %>%
  collect_metrics()


#Vecino m?s cercano

kknn_res <- tune::fit_resamples(kknn_spec,children ~ ., validation_splits, 
                                control = control_resamples(save_pred = TRUE))
kknn_res %>%
  collect_metrics()
#con esto knn tiene mejor accuracy 1 accuracy binary     0.742    25 0.00340 Preprocessor1_Model1
#vs decision tree

#Probar el modelo en datos nuevos que mejor accuracy tiene

kknn_fit%>%
  predict(new_data =test_proc,type="prob")%>%
  mutate(verdadero=hotel_test$children)%>%
  roc_auc(verdadero,.pred_children)
kknn_fit


#################################################
#cruce matriz de confusion

#Matriz de confusi?n a partir de los nuevos datos (definiendo el corte en 0,5)
kknn_fit%>% #
  predict(new_data =test_proc,type="prob")%>%
  mutate(verdadero=hotel_test$children)%>%
  #0.5 probabilidad por defecto
  mutate(pred_class=as.factor(if_else(.pred_children>0.5,"children","none")))%>%
  conf_mat(verdadero,pred_class)

#Prediction children  none
#  children     1164  4539
#  none          393 12696


