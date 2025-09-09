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
  step_downsample(children)%>%
  step_dummy(all_nominal(),-all_outcomes())%>%
  step_zv(all_numeric())%>%
  step_normalize(all_numeric())%>%
  prep()
hotel_rec

test_proc <- bake(hotel_rec,new_data = hotel_test)

#Comando que permite extraer la data de la receta

hotel_train_rec <- juice(hotel_rec)