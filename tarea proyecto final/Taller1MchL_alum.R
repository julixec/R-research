##Taller1: Machine Learning

#Problema: Se requiere clasificar a los individuos entre quienes pueden ganar
#m?s de 50 mil d?lares al a?o (variable "Mayor50k") a partir de las restantes variables 
# contenidas en la base "adultos".


##Implementar los siguientes numerales:

#1) Cargar la informaci?n de la base de datos (ejecutar los comandos provistos)

#2) Gerenerar la sub-base "adultos1", considerando exclusivamente las personas 
#cuyo pa?s de nacimiento es Estados Unidos de Norteam?rica, y generar las variables:
#"capitalgain_si" como una variable con dos categor?as "Si" y "No" derivada de la variable 
#continua "capital-gain"
#"capitalloss_si" como una variable con dos categor?as "Si" y "No" derivada de la variable 
#continua "capital-loss"

#3) Realizar un an?lisis general de las variables contenidas en la base "adultos"(Aplicar la librer?a correspondiente y realizar un breve comentario de lo hallado)

#4) Transformar las variables de tipo caracter en tipo factor

#5) Genere a partir de la base "adultos" una subbase de entrenamiento y de testeo

#6) Aplicar una receta para el preprocesamiento de datos 
#con la base de entrenamiento. Los criterios son los siguientes: seleccionar una
#base que contenga casos equilibrados en la variable resultado "Mayor50K",Recodificar
#las opciones de respuesta marginales de la variable "occupation" con la categor?a "otros"",
#generar variables dummy para las variables tipo factor, eliminar variables continuas con 
#varianza m?nima, normalizar las variables continuas

#7)Aplicar la receta en la base de testeo

#8)Grabar en la variable "datarec" los datos que ingresaron en la receta.



##Comandos para la carga de la informaci?n (ejecutar los comandos correspondientes)

library(tidyverse)

adultos0 <- readr::read_csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",col_names =F)
View(adultos0)


names(adultos0) <- c("age","workclass","fnlwgt","education",
                    "education-num","marital-status","occupation",
                    "relationship","race","sex","capital-gain","capital-loss",
                    "hours-per-week","native-country","Mayor50K")


adultos <- select(adultos0,- fnlwgt)

##Desarrollo
