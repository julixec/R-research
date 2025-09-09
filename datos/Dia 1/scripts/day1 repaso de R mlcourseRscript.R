#machine learning
# <- <- alt guion

a <- 3

#install packages and use xls format
#install.packages("openxls");
library(openxlsx)
#to introduce vectors
x <- c(1,3,7,9,10)
y <- c(2,7,3,8,7)
z <- c(1,3,7,9,10)

#ctlr + l borrar consoloa
datos <- data.frame(x,y,z)
#datos[rows,columns]
b <- datos[2,1]
b <- datos[,1]
b <- datos[1,]

#borrar objetos
rm(b)

#buscar directorio
file.choose()

#detectDates detectar fechas en archivos
data1 <-  read.xlsx("C:\\Users\\julian.galindo\\OneDrive - Escuela Politécnica Nacional\\Documentos\\EPN\\capacitación\\machine learning con R\\datos\\Dia 1\\Base1.xlsx",detectDates=T)
View(data1)
#like info of vars
str(data1)

data2 <- data1[1:100,]
View(data2)
data3 <- data.frame(data1$FECHA,data1$ENTIDAD,data1$PROVINCIA,data1$SALDO.TOTAL)
View(data3)

#nombres variables
names(data1)
str(data1)
head(data1)
head(data1,n=10L)
#help of all commands
help(head)


#filter in rows

data4 <- data1[data1$POR.VENCER>0 & data1$PROVINCIA=="DEL TUNGURAHUA",]
#transformaciones
library(dplyr)

#seleccionar o quitar variables
data5 <- select(data1,ENTIDAD, everything())
head(data5)
data6 <- select(data1,-ENTIDAD)

data7 <- filter(data1,PROVINCIA=="DEL CARCHI")
head(data7)

distin <- distinct(data1,PROVINCIA)
#seleccionar casos similar to data2 <- data1[1:100,]
data8 <- slice(data1,1:100)

#mutate
data9 <- mutate(data1,cart_morosa=NO.DEVENGA.INTERESES + VENCIDA,PROVINCIA2=PROVINCIA )
head(data9)

data10 <- mutate(data9,provincia2=if_else(PROVINCIA=="DEL CARCHI","CARCHI",PROVINCIA) )
data10$provincia2

distinct(data10,PROVINCIA2)
head(data10)

data11 <- arrange(data10,PROVINCIA2)
head(data11)


#convert upper lower case
sapply(data11$provincia2,tolower)
sapply(data11$provincia2,toupper)

#agrupaciones
data12 <- group_by(data11,PROVINCIA)
data12a <-data.frame(summarise(data12,n=n(),
                               c_mora=sum(cart_morosa, na.rm = T),
                               c_total=sum(SALDO.TOTAL,na.rm = T),
                               med_c_mora=mean(cart_morosa,na.rm = T))) 
data12b <- mutate(data12a,Ratiomorosidad=c_mora/c_total*100)
head(data12b)
View(data12b)

plot(data12b$Ratiomorosidad,data12b$med_c_mora)
#data12b$c_mora/data12b$c_total*100
# % pipe continue a line without execution


rm(data12b)
data13 <- data12%>%
group_by(PROVINCIA)%>%
summarise(n=n(),
            c_mora=sum(cart_morosa, na.rm = T),
            c_total=sum(SALDO.TOTAL,na.rm = T),
            med_c_mora=mean(cart_morosa,na.rm = T))%>% 
mutate(Ratiomorosidad=c_mora/c_total*100)

View(data13)
