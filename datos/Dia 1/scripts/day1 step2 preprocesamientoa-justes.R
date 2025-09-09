#day2
# preprocesamiento de datos
#problema :predecir tipo de reservas por hotel, existe o no ni?os en la reserva

#"ecosistema de pquetes tifymodels"
#install.packages("tidymodels")
library(tidymodels)
library(recipes)
#install.packages("themis")
library(themis)
#install.packages("rsample")
library(rsample)
#install.packages("workflow")
library(workflows)
library(parsnip)
library(yardstick)
library(dials)
library(rpart)
#install.packages("kknn")
library(kknn)
library(data.table)
library(tidyverse)
library(ggplot2) 

#load data of hotels
hotels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-11/hotels.csv")
View(hotels)


#distinct(hotels,hotel)
str(hotels)
head(hotels)

hotels_stays <-hotels%>%
  filter(is_canceled==0)%>%
    mutate(
     children=case_when(children+babies>0~"children",T~"none"), 
     required_car_parking_spaces=case_when(required_car_parking_spaces>0~"parking",T~"none"))%>%
     select(-is_canceled,-reservation_status,-babies)
View(hotels_stays)  
names(hotels_stays)  
  

#finds stats
summary(hotels_stays$adr)
plot(hotels_stays$adr)
hist(hotels_stays$adr)

install.packages("skimr")
library(skimr)
skim(hotels_stays)

  
hotels_df <-hotels_stays%>%
  select(children,hotel,arrival_date_month, meal, adr, adults, required_car_parking_spaces,
         total_of_special_requests,stays_in_week_nights,stays_in_weekend_nights)%>%
 mutate_if(is.character,factor)
  
skim(hotels_df)  
summary(hotels_stays)
summary(hotels_df)

skim(hotels_df$adr)   
#coeficiente de asimetria

#package.install(moments)
library(moments)
#install.packages("moments")
#las vars en estadistica tienen momentos

#valor asimetria
skewness(hotels_df$adr)
#La curtosis es una medida estadística que determina el grado de concentración que presentan los valores de una variable alrededor de la zona central de la distribución de frecuencias. También es conocida como medida de apuntamiento.
kurtosis(hotels_df$adr)
hist(hotels_df$adr)
hist(hotels_df$adr,main="variable adr",col="yellow",xlab = "USD",ylab = "Frecuencia")
boxplot(hotels_df$adr,main="variable adr",col="yellow",xlab = "USD",ylab = "Frecuencia")


#variable continua vs factor
mealtable <-table(hotels_df$meal) 
#% de variables 76% en BB, importancia de categorias, mantener al menos categorias con el 10%
prop.table(mealtable)


hotels_df%>%
  group_by(meal)%>%
  summarise(media=mean(adr))

#analisis de medias con 2 medias pero como tns mas vars entonces anova

#anova eplicar adr precio basado en el meal
a_anova <- aov(adr~meal, data=hotels_df)
summary(a_anova)

#diferencia de medias existe diferenca en el pago cuando se tiene o no niños
#predecir adr entonces puedo usar children porq si es relevane si me aporta una diferencia
t.test(hotels_df$adr~hotels_df$children,var.equal=F,conf.int=T)

#para ML una base de training 3/4 y 1/4 para testear.

#generar aletaroiamente
# opciones en ml
#1.tratar datos y luego training and testing
#pero se pude 2, fraccionar y luego tratar datos

library(recipes)
library(rsample)
library(themis)

set.seed(1234)
#75 data real y 25% aleatorio
str(hotels_df)
View(hotels_df)

hotel_split= initial_split(hotels_df)  # por defecto 75 training, 25 testing
hotel_training <- training(hotel_split) #25%
hotel_testing <- testing(hotel_split) #75%

head(hotel_training)

#preprocesamiento de datos automatizado
# establecer pasos #quiero predecir children . todas las
#variables aportan a predecir children

hotel_rec <- recipe(children~.,data=hotel_training)%>%   
  themis::step_downsample(children)%>%  #equilibra los 2 casos Y children and N children si tiene o no children, va a tomar la misma cantidad de casos Y y N children.
  recipes::step_dummy(all_nominal(),-all_outcomes())%>% # todas pasan a binarias menos children por ser el outcome la var a predecir
  recipes::step_zv(all_numeric())%>% #elimina variables con no varianza, no aportan al modelo
  recipes::step_normalize(all_numeric())%>% #normaliza las variabls numericas
  recipes::prep()

test_proc <- bake(hotel_rec,new_data = hotel_testing)

<- <- <- <- 