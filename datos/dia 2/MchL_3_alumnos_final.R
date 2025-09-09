#Machine Learnig (regresión lineal)

#Cargar la data

library(tidyverse)
library(lattice)
library(caret)

theme_set(theme_bw())

help("theme_set")

install.packages("devtools") 
library(devtools)
devtools::install_github("kassambara/datarium")

#Cargar datos

data("marketing", package = "datarium")
head(marketing)

