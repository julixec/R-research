#JG - Machine Learnig (regresión lineal)

#Cargar la data

library(tidyverse)
library(lattice)
#install.packages("caret")
library(caret)
install.packages("devtools") 
library(devtools)
library(skimr)

theme_set(theme_bw())
help("theme_set")
devtools::install_github("kassambara/datarium")
#install.packages("pkgload") 
#Cargar datos

data("marketing", package = "datarium")
head(marketing)

str(marketing)
skim(marketing)
hist(marketing$youtube)
hist(marketing$facebook)
hist(marketing$newspaper)
hist(marketing$sales)
plot(marketing$sales,marketing$youtube)
boxplot(marketing$sales,marketing$youtube)

#1. Primer analisis - correlacion
#si existe relacion entre depen y indepe entonces si va en el modelo
# no se aplica el caso si existe cor en tre var independientes 
#mayor corelacion con yb luego fb, neWSPAPER
cor(marketing)

cor.test(marketing$sales,marketing$youtube)
cor.test(marketing$sales,marketing$facebook)
cor.test(marketing$sales,marketing$newspaper)

library(PerformanceAnalytics)
chart.Correlation(marketing, histogram = F, pch=19)
chart.Correlation(marketing, histogram = T, pch=19)

library(recipes)

#ciclo de recesion clc
mk_rec <- recipe(sales~.,data = marketing)%>%
  step_zv(all_numeric(),-all_outcomes())%>%
  step_mutate(distcic=runif(nrow(marketing),0,1))%>%
  step_mutate(cic=as.factor(if_else(distcic>0.8,"Norma","Rec")))%>%  
  step_select(-distcic)%>% 
  prep()

mk_rec
marketing1 <- juice(mk_rec)

#partir base
library(caret)

set.seed(123)
#aleatorias respecto al 80% demi muestra
training_sample <- marketing1$sales %>%
  createDataPartition(p=0.8,list=F)
#todas las filas del training sample
training_base <- marketing1[training_sample,]
#TODAS MENOS LAS aleatorias del training sample
test_base <- marketing1[-training_sample,]

#modelo1 lm para regresion lineal
modventas1 <- lm(sales~.,data = training_base)
summary(modventas1)
#por cada venta se gana 4 centavos en yb 
#yb y fb son representativos
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.678903   0.543941   6.763 2.51e-10 ***
#  youtube      0.044657   0.001559  28.644  < 2e-16 ***
#  facebook     0.188924
#F-statistic: 336.4 on 4 and 157 DF,  p-value: < 2.2e-16 menor a 0.05

#predicciones con el modelo lm generado

predicventas1 <- modventas1%>%
  predict(test_base)  
predicventas1_df <- data.frame(predicventas1)  
#mayor confirmacion de q newspaper no es significativo ni cic (agregado solo como referencia pero no es obligatorio)
anova(modventas1)

#############3metricas de desempeño
#calcular el error prediccion vs real
RMSE(predicventas1, test_base$sales)
R2(predicventas1,test_base$sales)
#comparar metricas de rendimiento
#metricas primer modelo
rendimiento <- data.frame(modelo="modventas1",RMSE=RMSE(predicventas1, test_base$sales),R2=R2(predicventas1,test_base$sales))

##############modelo2

modventas2 <- lm(sales~youtube+facebook+newspaper
                 +youtube:facebook+youtube:newspaper,data = training_base)

summary(modventas2)
#por cada venta se gana 4 centavos en yb 
#yb y fb son representativos
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.678903   0.543941   6.763 2.51e-10 ***
#  youtube      0.044657   0.001559  28.644  < 2e-16 ***
#  facebook     0.188924
#F-statistic: 336.4 on 4 and 157 DF,  p-value: < 2.2e-16 menor a 0.05

#predicciones con el modelo lm generado

predicventas2 <- modventas2%>%
  predict(test_base)  
predicventas2_df <- data.frame(predicventas2)  
#mayor confirmacion de q newspaper no es significativo ni cic (agregado solo como referencia pero no es obligatorio)
anova(modventas2)

#############3metricas de desempeño
#calcular el error prediccion vs real
RMSE(predicventas2, test_base$sales)
R2(predicventas2,test_base$sales)
#comparar metricas de rendimiento
#metricas primer modelo
rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas2",RMSE=RMSE(predicventas2, test_base$sales),R2=R2(predicventas2,test_base$sales)))

#aparentemente modelo2 mejor q 1 
#modelo     RMSE        R2
#1 modventas1 1.967182 0.9047633
#2 modventas2 1.068444 0.9709427


################ modelo3 ###############
#agregamos un polinomio en youtube porq parece ser datos q suben y bajan ose un polinomio grado2
modventas3 <- lm(sales~poly(youtube,2) + facebook+newspaper
                 +youtube:facebook+youtube:newspaper,data = training_base)

summary(modventas3)

#predicciones con el modelo lm generado

predicventas3 <- modventas3%>%
  predict(test_base)  
predicventas3_df <- data.frame(predicventas3)  
#mayor confirmacion de q newspaper no es significativo ni cic (agregado solo como referencia pero no es obligatorio)
anova(modventas3)

#############3metricas de desempeño
#calcular el error prediccion vs real
RMSE(predicventas3, test_base$sales)
R2(predicventas3,test_base$sales)
#comparar metricas de rendimiento
#metricas primer modelo
rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas3",RMSE=RMSE(predicventas3, test_base$sales),R2=R2(predicventas3,test_base$sales)))


ggplot(marketing1, aes(x=youtube, y=sales)) + geom_point()+stat_smooth()
ggplot(marketing, aes(x=facebook, y=sales)) + geom_point()+stat_smooth()


#############3 modelo4
#definir puntos de corte para encontrar modelos lineales por puntos definidos
library(splines)
#P=c son quartiles normalnente aqui podemos enocntrar los puntos para deifnir modelos lineales
#deifnimos rectas por puntos en los cuartiles
knots <- quantile(training_base$youtube,p=c(0.25,0.50,0.75))

modventas4 <- lm(sales~bs(youtube, knots=knots)+ facebook+newspaper
                 +youtube:facebook+youtube:newspaper,data = training_base)



summary(modventas4)

#predicciones con el modelo lm generado

predicventas4 <- modventas4%>%
  predict(test_base)  
predicventas4_df <- data.frame(predicventas4)  
#mayor confirmacion de q newspaper no es significativo ni cic (agregado solo como referencia pero no es obligatorio)
anova(modventas4)

#############3metricas de desempeño
#calcular el error prediccion vs real
RMSE(predicventas4, test_base$sales)
R2(predicventas4,test_base$sales)
#comparar metricas de rendimiento
#metricas primer modelo
rendimiento <-rendimiento%>%
  bind_rows(data.frame(modelo="modventas4",RMSE=RMSE(predicventas4, test_base$sales),R2=R2(predicventas4,test_base$sales)))



###########3 evaluacion modelos
par(mfrow=c(2,2))
plot(modventas4)

install.packages("ggfortify")
library(ggfortify)
autoplot(modventas4)
#calcular mulcolineadlidad metrica q mide varianza entre variables

car::vif(modventas4)
autoplot(car::vif(modventas4))

#otras tecnicas de rendimient
AIC(modventas4)
BIC(modventas4)

library(modelr)
rsquare(modventas4,test_base)
rmse(modventas4,test_base)
mae(modventas4,test_base)

<- <- 