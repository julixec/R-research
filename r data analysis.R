# compute PCA2 aalysis
# SAM illustrative variable is not taken into account
# Method implementation by following https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/
# PCA: http://yatani.jp/teaching/doku.php?id=hcistats:pca
# 07/07/17
# Julix !!!
# data input is the result of experimentalscript_FINAL_correlation.R


library(ggplot2) 
library(sqldf)
library(devtools)
library(ggbiplot)
library(ade4)
library(outliers)
library(pastecs)
library(ggedit)
library(dplyr)


# PCA algorithm
w <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/wallFinal.csv", header=TRUE, sep=",")
w1 <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/w1Final.csv", header=TRUE, sep=",")
w2 <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/w2Final.csv", header=TRUE, sep=",")
w3 <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/w3Final.csv", header=TRUE, sep=",")
w4 <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/w4Final.csv", header=TRUE, sep=",")

#MyData <- read.csv(file="C:/datos/emociones/03may2017.csv", header=TRUE, sep=";")
MyData <- read.csv(file="C:/datos/emociones/uxexpe/consolidado.csv", header=TRUE, sep=";")
dim(MyData)
head(MyData)

View(MyData)


############## outliers detection
grubbs.flag <- function(x) { 
  outliers <- NULL 
  test <- x 
  grubbs.result <- grubbs.test(test) 
  pv <- grubbs.result$p.value 
  print('pv')
  print(pv)
  while(pv < 0.05) 
  { 
    
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3])) 
  test <- x[!x %in% outliers]
  print(grubbs.test(test))
  grubbs.result <- grubbs.test(test) 
  
  pv <- grubbs.result$p.value 
  print('pv')
  print(pv)
  } 
  return(data.frame(X=x,Outlier=(x %in% outliers))) 
}
##############


############### stats 
w1<-MyData

w2<-subset(w1,webSite=='2')
w3<-subset(w1,webSite=='3')
w11<-subset(w1,webSite=='1')
w4<-subset(w1,webSite=='4')

w1$userId

s1<-sqldf("select distinct userId from w11")
s2<-sqldf("select distinct userId from w2")
s3<-sqldf("select distinct userId from w3")
s4<-sqldf("select distinct userId from w4")

count(s1) 
count(s2)
count(s3)
count(s4)

########## number of images per website########
#walls<-sqldf("select webSite,count(dom_element_id),sum(minute),avg(minute) from w1 where dom_element_id='capture_btn' group by webSite")

#users interactions time and photos
ws1<-sqldf("select webSite,userId, count(dom_element_id) as numPhotos,count(dom_element_id)*0.17 as interactionTime_minutes from w1 where dom_element_id='capture_btn' group by webSite,userId")
#one photo every 10 seconds =/- some Error. 0.16 1photo x 10 sec -> numphotos * (10/60 = 0.16 minutes)=total time interaction in minutes
#website and interaction time
walls<-sqldf("select webSite,count(dom_element_id) as numPhotos,count(dom_element_id)*0.1666 as total_interactionTime_minutes from w1 where dom_element_id='capture_btn' group by webSite")

View(w1)

sa1<-sqldf("select webSite,userId, min(minute) as startMinute,max(minute) as lastMinute,max(minute)-min(minute) as differenceMinute  from w1 group by webSite,userId")
#all users sa11<-sqldf("select webSite,userId,avg(differenceMinute) as avg_experiment_time_minute from sa1 group by webSite,userId")

sa11<-sqldf("select webSite,avg(differenceMinute) as avg_experiment_time_minute from sa1 group by webSite")

w1$userId
sw1$
View(sw1)

########## number of images ########

########## number of images per website for women and men########
#users interactions time and photos

ws1w<-sqldf("select webSite,userId,genre, count(dom_element_id) as numPhotos,count(dom_element_id)*0.17 as interactionTime_minutes from w1 where dom_element_id='capture_btn' and genre='F'  group by webSite,userId")
stat.desc(ws1w)

ws1m<-sqldf("select webSite,userId,genre, count(dom_element_id) as numPhotos,count(dom_element_id)*0.17 as interactionTime_minutes from w1 where dom_element_id='capture_btn' and genre='H'  group by webSite,userId")
stat.desc(ws1m)

########## number of images per website for women and men########


########## number of images per website for men ########


########## number of images per website for men ########





############### stats



###########################################################################################

##############
print('######### UXPCA AVG for ALL ######################################################')

datos1<-sqldf("select website, neutral, happiness, sadness, anger, fear, disgust, surprise from MyData")

datosEmotions<-sqldf("select neutral, happiness, sadness, anger, fear, disgust, surprise from datos1")
datosEmotions$neutral<-as.numeric(datosEmotions$neutral) 
datosEmotions$happiness<-as.numeric(datosEmotions$happiness) 
datosEmotions$sadness<-as.numeric(datosEmotions$sadness) 
datosEmotions$anger<-as.numeric(datosEmotions$anger) 
datosEmotions$fear<-as.numeric(datosEmotions$fear) 
datosEmotions$disgust<-as.numeric(datosEmotions$disgust) 
datosEmotions$surprise<-as.numeric(datosEmotions$surprise) 


#replace; missing values by zero

datosEmotions <- datosEmotions %>% replace(is.na(.), 0.000000000000000000001)

#rotacion 
View(datosEmotions)
log.em <- log(datosEmotions)
em.periods <- datos1$website

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 

em.pca <- prcomp(na.omit(log.em),center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)

# loading library
library(ggfortify)
iris.pca.plot <- autoplot(em.pca,
                          data = datos1,
                          colour = 'website',loadings = TRUE, loadings.colour = 'blue',
                          loadings.label = TRUE, loadings.label.size = 3)

print(iris.pca.plot)


library(ggbiplot)

g <- ggbiplot(em.pca, choices = 1:2,obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE,var.axes=TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

###########################################################################################




######### PCA AVG for ALL ######################################################
# log transform 
print('######### PCA AVG for ALL ######################################################')

w$dominance<-w$dominance+1
w$usas<-paste(w$usability,w$aesthetics,sep='-')


w$period[w$period==1]<-'period1'
w$period[w$period==2]<-'period2'
w$period[w$period==3]<-'period3'

w$sam<- as.numeric(paste(w$valence,w$arousal,w$dominance,sep=''))

# to find new categories such as website
waux<-sqldf("select w.user,w.period,w.usability,w.aesthetics,w.havg,w.cavg,w.aavg,w.davg,w.favg,w.savg,w.navg,data.webSite,w.valence,w.arousal,w.dominance from w,MyData as data where w.user=data.userId group by w.user,w.period,w.havg,w.cavg,w.aavg,w.davg,w.favg,w.savg,w.navg,data.webSite")
w=waux
w$webSite[w$webSite==1]<-'website1'
w$webSite[w$webSite==2]<-'website2'
w$webSite[w$webSite==3]<-'website3'
w$webSite[w$webSite==4]<-'website4'

# to find new categories such as website


#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w<-subset(w,havg!=0)


avgds<-data.frame(w$havg,w$cavg,w$aavg,w$davg,w$favg,w$savg)
log.em <- log(avgds)
em.periods <- w[, 4]
 
# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 

em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)




iris.pca.plot









############ contribution 
print('######### variable contribution per PCA ######################################################')
zebu.acp <-log.em #data


acp=dudi.pca(log.em)
acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)

#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100




############ contrubution











#compute correlations
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation

scores <- em.pca$x
correlations<-cor(scores,avgds)
plot(scores[,1])#PCA1
barplot(scores[,1])


em.periods <- w$aesthetics
g <- ggbiplot(em.pca, choices = 1:2,obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)




plot3d(scores[,1:3],size=2.5)
text3d(loadings[,1:3], texts=rownames(loadings), col="red")
coords <- NULL
for (i in 1:nrow(loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),loadings[i,1:3]))}
lines3d(coords, col="red", lwd=4)


######### PCA AVG for ALL ######################################################

######### PCA AVG for ALL pos ne neu######################################################
# log transform 
print('######### PCA pos neg neutral ######################################################')
w$dominance<-w$dominance+1

w$period[w$period==1]<-'period1'
w$period[w$period==2]<-'period2'
w$period[w$period==3]<-'period3'
w$sam<- as.numeric(paste(w$valence,w$arousal,w$dominance,sep=''))

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w<-subset(w,havg!=0)


positive<-w$havg
negative<-(w$cavg+w$aavg+w$davg+w$favg+w$savg)/5
neutral<-w$navg

avgds<-data.frame(positive,negative,neutral,w$sam)
log.em <- log(avgds)
em.periods <- w[, 3]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)
g <- ggbiplot(em.pca, choices = 1:2,obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

######### PCA AVG for ALL ######################################################




######### PCA AVG for w1 ######################################################
# log transform 
print('######### PCA AVG for w1 ######################################################')
w1<-subset(w,webSite=='website1')

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,havg!=0)

avgds<-data.frame(w1$havg,w1$cavg,w1$aavg,w1$davg,w1$favg,w1$savg,w1$navg)
log.em <- log(avgds)
em.periods <- w1[, 2]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

######### PCA AVG for w1 ######################################################
######### PCA AVG for w2 ######################################################
# log transform 
print('######### PCA AVG for w2######################################################')
w2<-subset(w,webSite=='website2')


#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w2<-subset(w2,havg!=0)


avgds<-data.frame(w2$havg,w2$cavg,w2$aavg,w2$davg,w2$favg,w2$savg,w2$navg)
log.em <- log(avgds)
em.periods <- w2[, 2]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

######### PCA AVG for w2 ######################################################
######### PCA AVG for w2 ######################################################

######### PCA AVG for w3 ######################################################
# log transform 
print('######### PCA AVG for w3 ######################################################')
w3<-subset(w,webSite=='website3')

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w3<-subset(w3,havg!=0)


avgds<-data.frame(w3$havg,w3$cavg,w3$aavg,w3$davg,w3$favg,w3$savg,w3$navg)
log.em <- log(avgds)
em.periods <- w3[, 2]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

######### PCA AVG for w3 ######################################################
######### PCA AVG for w3 ######################################################

######### PCA AVG for w4 ######################################################
# log transform 
print('######### PCA AVG for w4 ######################################################')
w4<-subset(w,webSite=='website4')

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w4<-subset(w4,havg!=0)


avgds<-data.frame(w4$havg,w4$cavg,w4$aavg,w4$davg,w4$favg,w4$savg,w4$navg)
log.em <- log(avgds)
em.periods <- w4[, 2]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

######### PCA AVG for w4 ######################################################
######### PCA AVG for w4 ######################################################

######### PCA AVG vad for ALL ######################################################
# log transform 
print('######### PCA AVG for ALL ######################################################')

w$dominance<-w$dominance+1

w$period[w$period==1]<-'period1'
w$period[w$period==2]<-'period2'
w$period[w$period==3]<-'period3'
w$sam<- as.numeric(paste(w$valence,w$arousal,w$dominance,sep=''))

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w<-subset(w,havg!=0)

avgds<-data.frame(w$havg,w$cavg,w$aavg,w$davg,w$favg,w$savg,w$navg,w$dominance)
log.em <- log(avgds)
em.periods <- w[, 3]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)
g <- ggbiplot(em.pca, choices = 1:2,obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

######### PCA AVG vad for alll ######################################################

######### PCA MAX for all websites######################################################
# log transform 
print('######### PCA MAX for ALL ######################################################')
w$period[w$period==1]<-'period1'
w$period[w$period==2]<-'period2'
w$period[w$period==3]<-'period3'
w$sam<- as.numeric(paste(w$valence,w$arousal,w$dominance,sep=''))

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w<-subset(w,havg!=0)


avgds<-data.frame(w$hmax,w$cmax,w$amax,w$dmax,w$fmax,w$smax,w$nmax,w$sam)
log.em <- log(avgds)
em.periods <- w[, 3]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

#compute correlations
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation

scores <- em.pca$x
correlations<-cor(scores,avgds)



######### PCA MAX for all websites ######################################################


######### PCA MAX for all websites w1######################################################
# log transform 
print('######### PCA MAX for w1 ######################################################')
w1$period[w1$period==1]<-'period1'
w1$period[w1$period==2]<-'period2'
w1$period[w1$period==3]<-'period3'
w1$sam<- as.numeric(paste(w1$valence,w1$arousal,w1$dominance,sep=''))

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,havg!=0)


avgds<-data.frame(w1$hmax,w1$cmax,w1$amax,w1$dmax,w1$fmax,w1$smax,w1$nmax,w1$sam)
log.em <- log(avgds)
em.periods <- w1[, 3]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



######### PCA MAX for all websites  w1######################################################

######### PCA MAX for all websites w2######################################################
# log transform 
print('######### PCA MAX for w2 ######################################################')
w2$period[w2$period==1]<-'period1'
w2$period[w2$period==2]<-'period2'
w2$period[w2$period==3]<-'period3'
w2$sam<- as.numeric(paste(w2$valence,w2$arousal,w2$dominance,sep=''))

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w2<-subset(w2,havg!=0)


avgds<-data.frame(w2$hmax,w2$cmax,w2$amax,w2$dmax,w2$fmax,w2$smax,w2$nmax,w2$sam)
log.em <- log(avgds)
em.periods <- w2[, 3]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



######### PCA MAX for all websites  w2######################################################

######### PCA MAX for all websites w3######################################################
# log transform 
print('######### PCA MAX for w3 ######################################################')
w3$period[w3$period==1]<-'period1'
w3$period[w3$period==2]<-'period2'
w3$period[w3$period==3]<-'period3'
w3$sam<- as.numeric(paste(w3$valence,w3$arousal,w3$dominance,sep=''))

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w3<-subset(w3,havg!=0)


avgds<-data.frame(w3$hmax,w3$cmax,w3$amax,w3$dmax,w3$fmax,w3$smax,w3$nmax,w3$sam)
log.em <- log(avgds)
em.periods <- w3[, 3]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



######### PCA MAX for all websites  w3######################################################


######### PCA MAX for all websites w4######################################################
# log transform 
print('######### PCA MAX for w4 ######################################################')
w4$period[w4$period==1]<-'period1'
w4$period[w4$period==2]<-'period2'
w4$period[w4$period==3]<-'period3'
w4$sam<- as.numeric(paste(w4$valence,w4$arousal,w4$dominance,sep=''))

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w4<-subset(w4,havg!=0)


avgds<-data.frame(w4$hmax,w4$cmax,w4$amax,w4$dmax,w4$fmax,w4$smax,w4$nmax,w4$sam)
log.em <- log(avgds)
em.periods <- w4[, 3]



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")

# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



######### PCA MAX for all websites  w4######################################################



######### PCA per clic per user  ######################################################
# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#w1<-subset(w1,genre=='H')
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")
w2<-subset(w1,webSite=='2')
w3<-subset(w1,webSite=='3')
w11<-subset(w1,webSite=='1')
w4<-subset(w1,webSite=='4')

#w1<-rbind(w11,w4)


w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'


#w1<-w1[-2492,]
w1<-w1[-1736,]
w1<-w1[-228,]
w1<-w1[-1840,]

stat.desc(w1$age)
agea<-data.frame(w1$userId,w1$age,w1$genre,w1$webSite)
agea1<-sqldf("select distinct * from agea") 
agea2<-sqldf("select distinct * from agea where w1.webSite='website1' ") 

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
log.em <- log(avgds)
em.periods <- w1$webSite



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method

#CHECKING OUTLIERS
x<-data.frame(em.pca$x)

summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = FALSE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)





######## PCA per clic per user  ######################################################


######## PCA per clic per user  - GENRE ######################################################
#                     age     genre      webSite              R1_NB5      R1_Mieux   
#2017-04-04 13:10:01: 348   Min.   :19   F:1393   Length:3376        Min.   :1.00   Atlas: 450  
#2017-03-14 13:46:48: 162   1st Qu.:23   H:1983   Class :character   1st Qu.:1.00   Kenzi:2926  
#2017-03-14 11:29:00: 155   Median :27            Mode  :character   Median :1.00               
#2017-03-14 10:44:59: 154   Mean   :31                               Mean   :1.52               
#2017-04-05 17:37:28: 129   3rd Qu.:32
# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#remove outliers (heuristic far the data concentration)

summary(w1)
w1<-w1[-2492,]
w1<-w1[-1352,]
w1<-w1[-3320,]

w1<-w1[-3319,]
w1<-w1[-2595,]
#w1<-w1[-1541,]


#w1<-subset(w1,genre=='H')
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")

w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'


#w1<-w1[-2492,]

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
log.em <- log(avgds)
em.periods <- w1$genre
w1[1,]


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method

#CHECKING OUTLIERS
x<-data.frame(em.pca$x)

#validation of PCA function dim1 dim 2 to see that loadings values represent PCi=f(emotions)
#scores > x
#PC1     PC2     PC3     PC4     PC5     PC6
#1   -0.721  0.5879  0.8149  1.1397  0.3292  0.2440
#2   -0.721  0.5879  0.8149  1.1397  0.3292  0.2440

v1aux=-11.0935*0.37+      -5.95*0.36+    -1.95*0.4+      -6.8*0.51+   -9.23*0.39+   -3.815*0.28+   -0.18522*-0.29
v1aux=0.00001521124*0.37+
0.002607*0.36+
0.1429087*0.4+
0.001099457*0.51+
0.000097663*0.39+
0.02203*0.28+
0.83*-0.29

summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = FALSE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

############ contribution variable for all genre (individual)  ############
#inertia: https://pbil.univ-lyon1.fr/ade4/ade4-html/inertia.dudi.html

zebu.acp <-log.em
acp=dudi.pca(log.em)
acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)
#inertieC<-inertia.dudi(acp,row.inertia = TRUE)

#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100

inertieC$col.cum/100

#inertieC$row.abs/100

############ contribution variable for all genre (individual)  ############
 

######## PCA per clic per user  - genre ######################################################

######## PCA per clic per user  - GENRE women ######################################################
#                     age     genre      webSite              R1_NB5      R1_Mieux   
#2017-04-04 13:10:01: 348   Min.   :19   F:1393   Length:3376        Min.   :1.00   Atlas: 450  
#2017-03-14 13:46:48: 162   1st Qu.:23   H:1983   Class :character   1st Qu.:1.00   Kenzi:2926  
#2017-03-14 11:29:00: 155   Median :27            Mode  :character   Median :1.00               
#2017-03-14 10:44:59: 154   Mean   :31                               Mean   :1.52               
#2017-04-05 17:37:28: 129   3rd Qu.:32 
# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#remove outliers (heuristic far the data concentration)

summary(w1)
w1<-w1[-2492,]
w1<-w1[-1352,]
w1<-w1[-3320,]

w1<-w1[-3319,]
w1<-w1[-2595,]
#w1<-w1[-1541,]

#numer of women and men
#w1<-subset(w1,genre=='H')


w1<-subset(w1,genre=='F')

count(w1)
w2<-subset(w1,webSite=='2')
w3<-subset(w1,webSite=='3')
w11<-subset(w1,webSite=='1')
w4<-subset(w1,webSite=='4')

#w1<-rbind(w2,w3)
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")

w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'

w1$agecategory[w1$age>=19 & w1$age<=27]<-'Youngers:19-27'
w1$agecategory[w1$age>=27 & w1$age<=32]<-'Young adults:27-32'
w1$agecategory[w1$age>32]<-'Olders:32-67'
#w1<-w1[-2492,]

stat.desc(w1$age)
agew<-data.frame(w1$userId,w1$age,w1$webSite)
agew1<-sqldf("select distinct * from agew") #21

View(agew1)
#> stat.desc(w1$age)
#nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean      SE.mean CI.mean.0.95          var 
#1394.00         0.00         0.00        23.00        63.00        40.00     45403.00        27.00        32.57         0.33         0.64       150.07 
#std.dev     coef.var 
#12.25         0.38 

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
nrow(avgds)
ncol(avgds)

log.em <- log(avgds)
count(log.em)
em.periods <- w1$webSite

#https://www.researchgate.net/post/What_is_the_best_way_to_scale_parameters_before_running_a_Principal_Component_Analysis_PCA


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#scale #a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with S, but in general scaling is advisable. Alternatively, a vector of length equal the number of columns of x can be supplied. The value is passed to scale.
#scale for every variable : normalization (mean(x1) - x1) / standar deviation(x1)
#to see the used scale em.pca$center , #em.pca$scale
# it is called zVar <- (myVar - mean(myVar)) / sd(myVar)


sh=sd(log.em$w1.happiness)
sc=var(log.em$w1.contempt)
sa=var(log.em$w1.anger)
sd=var(log.em$w1.disgust)
sf=var(log.em$w1.fear)
ss=var(log.em$w1.sadness)
sn=var(log.em$w1.neutral)

msh=mean(log.em$w1.happiness)
msc=mean(log.em$w1.contempt)
msa=mean(log.em$w1.anger)
msd=mean(log.em$w1.disgust)
msf=mean(log.em$w1.fear)
mss=mean(log.em$w1.sadness)
msn=mean(log.em$w1.neutral)


#sd

em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

#em.pca$center
#em.pca$scale

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
em.pca$sdev

#CHECKING OUTLIERS - x=scores
x<-data.frame(em.pca$x)

#w1.happiness w1.contempt w1.anger w1.disgust w1.fear w1.sadness w1.neutral
#aux1=-7.6198*0.429 -5.74*0.380  -9.2*0.477 -7.7*0.488 -6.23*0.410 -1.11*0.194 -0.41616*0.027
#aux2<-data.frame(w1$happiness*0.33+
#w1$contempt*0.41+
#w1$anger*0.50+
#w1$disgust*0.57+
#w1$fear*0.23+
#w1$sadness*0.25+
#w1$neutral*-0.18,1)


summary(em.pca)

g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = TRUE, 
              circle = FALSE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


############ contribution variable for women  ############
#inertia: https://pbil.univ-lyon1.fr/ade4/ade4-html/inertia.dudi.html

zebu.acp <-log.em
acp=dudi.pca(log.em)
acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)
#inertieC<-inertia.dudi(acp,row.inertia = TRUE)

#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100

inertieC$col.cum/100

#inertieC$row.abs/100

############ contribution variable for women  ############


############ hierarchichal classification for women  ############

library(questionr)
#1, compute distance of acp (#Distance du ??? Il s'agit de la distance utilis?e dans les analyses de correspondance multiples (ACM))
md <- dist.dudi(acp)

#2 Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. 
#Calcul du dendrogramme
#hclust: Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.

arbre <- hclust(md, method = "ward.D2")
plot(arbre, labels = FALSE, main = "Dendrogramme")

#summary(arbre)

#3 how many relevant classes(partitions)
inertie <- sort(arbre$height, decreasing = TRUE)
summary(inertie)

plot(inertie[1:20], type = "s", xlab = "Number de classes",ylab = "Inertie of dendogramme")


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 2(green), 3(red) ou 5(blue) classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 2, border = "green3")
rect.hclust(arbre, 3, border = "red3")
rect.hclust(arbre, 5, border = "blue3")



#let's get the best cut of the tree
library(devtools)
#install_github("larmarange/JLutils")

library(JLutils)
#Par d?faut, best.cutree regarde quelle serait la meilleure partition entre 3 et 20 classes.
#partition ayant la plus grande perte relative d'inertie.
#(to use the partition with the greatest relative loss of inertia.)

numberClasses=best.cutree(arbre)  # by default minimum=3
#numberClasses=3
best.cutree(arbre,min=2) #min number of partitions = 2

best.cutree(arbre, min = 2, graph = TRUE, xlab = "Nombre de classes", 
            ylab = "Inertie relative")

#we cut in 3 classes because the minimum best partition is 2 and the second best one is 3
typo <- cutree(arbre, 3)
freq(typo)
#summary(typo)
#plot(typo)

#graphical representation of scores(acp$li) in the new HCA 

par(mfrow = c(1, 2))
library(RColorBrewer)
s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")
#s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = as.factor(typo), ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 3 classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 3, border = "red3")


#s.class(acp$li, as.factor(typo), 3, 4, col = brewer.pal(3,"Set1"), sub = "Axes 3 et 4")


############ chi square hierarchichal classification for women  ############
#change typo into data frame
dftypo<-data.frame(typo)
dftypo$obs <- seq.int(nrow(dftypo))
scoresw<-data.frame(em.pca$x)
dftypo$pc1<-scoresw$PC1
dftypo$pc2<-scoresw$PC2

#get emotions from pc1
dftypo$disgust<-w1$disgust
dftypo$anger<-w1$anger
dftypo$fear<-w1$fear
dftypo$sadness<-w1$sadness
dftypo$happiness<-w1$happiness
dftypo$neutral<-w1$neutral
dftypo$contempt<-w1$contempt
dftypo$age<-w1$age
dftypo$webSite<-w1$webSite

dfhca<-data.frame(dftypo$obs,dftypo$disgust,dftypo$anger,dftypo$fear,dftypo$sadness,dftypo$happiness,dftypo$neutral,dftypo$contempt,dftypo$pc1,dftypo$pc2,dftypo$typo,dftypo$age,dftypo$webSite)

names(dfhca)<-c('obs','disgust','anger','fear','sadness','h','n','c','pc1','pc2','class','age','webSite')

View(dfhca)
View(dftypo)
View(log.em)
library(sqldf)

c1<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=1")
c2<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=2")
c3<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=3")

call<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca")

c1all<-sqldf("select * from dfhca where class=1")
c2all<-sqldf("select * from dfhca where class=2")
c3all<-sqldf("select * from dfhca where class=3")

summary(c1all$age)
summary(c2all$age)
summary(c3all$age)

hist(c1all$age)
hist(c2all$age)
hist(c3all$age)

#prepare cross table w1
w1$obs<-seq.int(nrow(w1))
#w1$webSite
#422
tbw1c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website4'")

######## classes per website aesthetics usability
uh1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")

ul1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")

ah1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")

al1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")


datachiu <- matrix(c(123,225,110,299,367,270), ncol=3, byrow=T)
datachia <- matrix(c(212,286,215,210,306,165), ncol=3, byrow=T)


testchiu=chisq.test(datachiu)

#plot residuals
corrplot(testchiu$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchiu$residuals^2/testchiu$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachiu)


testchia=chisq.test(datachia)
#plot residuals
corrplot(testchia$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchia$residuals^2/testchia$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachia)

#########


datachi <- matrix(c(115,173,75,8,52,35,95,133,90,204,234,180), ncol=3, byrow=T)

testchi=chisq.test(datachi)
#X-squared = 35.468, df = 6, p-value = 3.497e-06 -> we reject Ho(independet) so that there is dependency between classes and websites(websites influence classes).


#visualize the residuals 
library(corrplot)
rownames(datachi) <- c('website1', 'website2','website3', 'website4')
#plot residuals
corrplot(testchi$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchi$residuals^2/testchi$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

library(vcd) 
assocstats(datachi)



datachi <- matrix(c(115,173,75,8,52,35,95,133,90,204,234,180), ncol=3, byrow=T)
chisq.test(data)
############ chi square hierarchichal classification for women  ############

########### t-test


###########


############ hierarchichal classification for women  ############


############### compute confidence intervals per emotion per class - women #############
library(rcompanion)
call<-sqldf("select class,w1.webSite,(w1.disgust),'disgust',(w1.anger),'anger',(w1.fear),'fear',(w1.sadness),'fear',(w1.happiness),'happiness',(w1.neutral),'neutral',(w1.contempt),'contempt' from dfhca as hca,w1 where  hca.obs=w1.obs  ")
#stat.desc(c1w1)
a<-groupwiseMean(disgust ~ class + webSite,
              data = call,
              conf = 0.95,
              digits = 3,
              normal      = TRUE)
a$emotion<-"disgust"




b<-groupwiseMean(anger ~ class + webSite,
              data = call,
              conf = 0.95,
              digits = 3,
              normal      = TRUE)

b$emotion<-"anger"

c<-groupwiseMean(fear ~ class + webSite,
              data = call,
              conf = 0.95,
              digits = 3,
              normal      = TRUE)

c$emotion<-"fear"


d<-groupwiseMean(sadness ~ class + webSite,
              data = call,
              conf = 0.95,
              digits = 3,
              normal      = TRUE)

d$emotion<-"sadness"

e<-groupwiseMean(happiness ~ class + webSite,
              data = call,
              conf = 0.95,
              digits = 3,
              normal      = TRUE)

e$emotion<-"happiness"

f<-groupwiseMean(neutral ~ class + webSite,
              data = call,
              conf = 0.95,
              digits = 3,
              normal      = TRUE)

f$emotion<-"neutral"

g<-groupwiseMean(contempt ~ class + webSite,
              data = call,
              conf = 0.95,
              digits = 3,
              normal      = TRUE)

g$emotion<-"contempt"

pc1gm<-groupwiseMean(pc1 ~ class + webSite,
                 data = dfhca,
                 conf = 0.95,
                 digits = 3,
                 normal      = TRUE)
pc1gm$emotion<-"pc1"

pc2gm<-groupwiseMean(pc2 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     normal      = TRUE)

pc2gm$emotion<-"pc2"

pc1all<-groupwiseMean(pc1 ~  webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     normal      = TRUE)
pc1all$emotion<-"pc1"

pc2all<-groupwiseMean(pc2 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      normal      = TRUE)
pc2all$emotion<-"pc2"

stat.desc(scores)

ciall<-rbind(a,b,c,d,e,f,g)
ciall1<-sqldf("select * from ciall where class=1 and webSite='website1'")
ciall2<-sqldf("select * from ciall where class=1 and webSite='website2'")
ciall3<-sqldf("select * from ciall where class=1 and webSite='website3'")
ciall4<-sqldf("select * from ciall where class=1 and webSite='website4'")

c2all1<-sqldf("select * from ciall where class=2 and webSite='website1'")
c2all2<-sqldf("select * from ciall where class=2 and webSite='website2'")
c2all3<-sqldf("select * from ciall where class=2 and webSite='website3'")
c2all4<-sqldf("select * from ciall where class=2 and webSite='website4'")

c3all1<-sqldf("select * from ciall where class=3 and webSite='website1'")
c3all2<-sqldf("select * from ciall where class=3 and webSite='website2'")
c3all3<-sqldf("select * from ciall where class=3 and webSite='website3'")
c3all4<-sqldf("select * from ciall where class=3 and webSite='website4'")

#fear anger and disgst
#ciall1fad1<-sqldf("select * from ciall where class=1 and webSite='website1' and (emotion='fear' or emotion='anger' or emotion='disgust') ")
ciall1fad1<-sqldf("select * from ciall where class=1 and (emotion='fear') ")
ciall1fad2<-sqldf("select * from ciall where class=1 and (emotion='anger') ")
ciall1fad3<-sqldf("select * from ciall where class=1 and (emotion='disgust') ")
ciall1fad4<-sqldf("select * from ciall where class=1 and (emotion='happiness') ")
ciall1fad5<-sqldf("select * from ciall where class=1 and (emotion='sadness') ")
ciall1fad6<-sqldf("select * from ciall where class=1 and (emotion='neutral') ")
ciall1fad7<-sqldf("select * from ciall where class=1 and (emotion='contempt') ")

ciall2fad1<-sqldf("select * from ciall where class=2 and (emotion='fear') ")
ciall2fad2<-sqldf("select * from ciall where class=2 and (emotion='anger') ")
ciall2fad3<-sqldf("select * from ciall where class=2 and (emotion='disgust') ")
ciall2fad4<-sqldf("select * from ciall where class=2 and (emotion='happiness') ")
ciall2fad5<-sqldf("select * from ciall where class=2 and (emotion='sadness') ")
ciall2fad6<-sqldf("select * from ciall where class=2 and (emotion='neutral') ")
ciall2fad7<-sqldf("select * from ciall where class=2 and (emotion='contempt') ")

ciall3fad1<-sqldf("select * from ciall where class=3 and (emotion='fear') ")
ciall3fad2<-sqldf("select * from ciall where class=3 and (emotion='anger') ")
ciall3fad3<-sqldf("select * from ciall where class=3 and (emotion='disgust') ")
ciall3fad4<-sqldf("select * from ciall where class=3 and (emotion='happiness') ")
ciall3fad5<-sqldf("select * from ciall where class=3 and (emotion='sadness') ")
ciall3fad6<-sqldf("select * from ciall where class=3 and (emotion='neutral') ")
ciall3fad7<-sqldf("select * from ciall where class=3 and (emotion='contempt') ")


pc1c<-sqldf("select * from pc1gm where class=1  ")
pc2c<-sqldf("select * from pc1gm where class=2  ")
pc3c<-sqldf("select * from pc1gm where class=3  ")

pc2c1<-sqldf("select * from pc2gm where class=1  ")
pc2c2<-sqldf("select * from pc2gm where class=2  ")
pc2c3<-sqldf("select * from pc2gm where class=3  ")

pc1all<-sqldf("select * from pc1all ")
pc2all<-sqldf("select * from pc2all ")



require(ggplot2)
library(cowplot)


p1<-ggplot(ciall1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w1")
p2<-ggplot(ciall2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w2") 
p3<-ggplot(ciall3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w3")
p4<-ggplot(ciall4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w4")

p5<-ggplot(c2all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w1")
p6<-ggplot(c2all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w2") 
p7<-ggplot(c2all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w3")
p8<-ggplot(c2all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w4")

p9<-ggplot(c3all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w1")
p10<-ggplot(c3all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w2") 
p11<-ggplot(c3all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w3")
p12<-ggplot(c3all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w4")

plot_grid(p1,p2,p3,p4, ncol = 2, nrow = 2)
plot_grid(p5,p6,p7,p8, ncol = 2, nrow = 2)
plot_grid(p9,p10,p11,p12, ncol = 2, nrow = 2)




p1f<-ggplot(ciall1fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("fear for all websites in c1")
p1a<-ggplot(ciall1fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("anger for all websites in c1")
p1d<-ggplot(ciall1fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("disgust for all websites in c1")
p1h<-ggplot(ciall1fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("happiness for all websites in c1")
p1s<-ggplot(ciall1fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("sadness for all websites in c1")
p1n<-ggplot(ciall1fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("neutral for all websites in c1")
p1c<-ggplot(ciall1fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("contempt for all websites in c1")

p2f<-ggplot(ciall2fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("fear for all websites in c2")
p2a<-ggplot(ciall2fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("anger for all websites in c2")
p2d<-ggplot(ciall2fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("disgust for all websites in c2")
p2h<-ggplot(ciall2fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("happiness for all websites in c2")
p2s<-ggplot(ciall2fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("sadness for all websites in c2")
p2n<-ggplot(ciall2fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("neutral for all websites in c2")
p2c<-ggplot(ciall2fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("contempt for all websites in c2")

p3f<-ggplot(ciall3fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("fear for all websites in c3")
p3a<-ggplot(ciall3fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("anger for all websites in c3")
p3d<-ggplot(ciall3fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("disgust for all websites in c3")
p3h<-ggplot(ciall3fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("happiness for all websites in c3")
p3s<-ggplot(ciall3fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("sadness for all websites in c3")
p3n<-ggplot(ciall3fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("neutral for all websites in c3")
p3c<-ggplot(ciall3fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("contempt for all websites in c3")


plot_grid(p1f,p1a,p1d,p1h,p1s,p1n,p1c, ncol = 4, nrow = 2)
plot_grid(p2f,p2a,p2d,p2h,p2s,p2n,p2c, ncol = 4, nrow = 2)
plot_grid(p3f,p3a,p3d,p3h,p3s,p3n,p3c, ncol = 4, nrow = 2)


ppc1c1<-ggplot(pc1c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c1")
ppc1c2<-ggplot(pc2c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c2")
ppc1c3<-ggplot(pc3c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c3")



ppc2c1<-ggplot(pc2c1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c1")
ppc2c2<-ggplot(pc2c2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c2")
ppc2c3<-ggplot(pc2c3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c3")

plot_grid(ppc1c1,ppc1c2,ppc1c3, ncol = 3, nrow = 1)
plot_grid(ppc2c1,ppc2c2,ppc2c3, ncol = 3, nrow = 1)


ppc1all<-ggplot(pc1all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites ")
ppc2all<-ggplot(pc2all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites ")


library(party)
dfhcaaux<-dfhca
dfhcaaux[dfhcaaux$class == 1]<-"class1"


fit <- ctree( age~class,data=dfhca,controls = ctree_control(maxdepth = 10))
fit1 <- ctree( class~age,data=dfhca,controls = ctree_control(maxdepth = 10))
plot(fit, main="Age-class tree ")
plot(fit1, main="Age-class tree ")




############### compute confidence intervals per emotion per class - women #############

############### descrip stats for pc1 and pc2 #############

pcw1<-sqldf("select webSite,hca.* from dfhca as hca,w1 
           where class=1 and hca.obs=w1.obs and (w1.webSite ='website1')  ")

pcw2<-sqldf("select webSite,hca.* from dfhca as hca,w1 
           where class=1 and hca.obs=w1.obs and (w1.webSite ='website2')  ")

pcw3<-sqldf("select webSite,hca.* from dfhca as hca,w1 
           where class=1 and hca.obs=w1.obs and (w1.webSite ='website3')  ")

pcw4<-sqldf("select webSite,hca.* from dfhca as hca,w1 
           where class=1 and hca.obs=w1.obs and (w1.webSite ='website4')  ")


sdpcw1<-stat.desc(pcw1)
sdpcw2<-stat.desc(pcw2)
sdpcw3<-stat.desc(pcw3)
sdpcw4<-stat.desc(pcw4)
View(sdpcw4)


sdpc12<-stat.desc(dfhca)
View(sdpc12)

dspca1<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
           stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
           variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
           where class=1 and hca.obs=w1.obs and (w1.webSite ='website1')  ")

dspca2<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
           stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
             variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
             where class=1 and hca.obs=w1.obs and (w1.webSite ='website2')  ")

dspca3<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
           stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
              variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
              where class=1 and hca.obs=w1.obs and (w1.webSite ='website3')  ")

dspca4<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
           stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
              variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
              where class=1 and hca.obs=w1.obs and (w1.webSite ='website4')  ")

dspca12<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
           stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
              variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
              where class=1 and hca.obs=w1.obs  ")

pc12all<-rbind(dspca1,dspca2,dspca3,dspca4)


############### descrip stats for pc1 and pc2 #############



######## PCA per clic per user  - genre - women ######################################################


######## PCA per clic per user  - GENRE men ######################################################
#                     age     genre      webSite              R1_NB5      R1_Mieux   
#2017-04-04 13:10:01: 348   Min.   :19   F:1393   Length:3376        Min.   :1.00   Atlas: 450  
#2017-03-14 13:46:48: 162   1st Qu.:23   H:1983   Class :character   1st Qu.:1.00   Kenzi:2926  
#2017-03-14 11:29:00: 155   Median :27            Mode  :character   Median :1.00               
#2017-03-14 10:44:59: 154   Mean   :31                               Mean   :1.52               
#2017-04-05 17:37:28: 129   3rd Qu.:32
# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#remove outliers (heuristic far the data concentration)

summary(w1)
w1<-w1[-2492,]
w1<-w1[-1352,]
w1<-w1[-3320,]

w1<-w1[-3319,]
w1<-w1[-2595,]
#w1<-w1[-1541,]

View(w1)
# export data
write.csv(w1, "C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/w1finalwomenandmen08jan2018.csv")
#number of men
w1<-subset(w1,genre=='H')
count(w1)

w2<-subset(w1,webSite=='2')
w3<-subset(w1,webSite=='3')
w11<-subset(w1,webSite=='1')
w4<-subset(w1,webSite=='4')

#w1<-rbind(w2,w3)
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")

w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'

w1$agecategory[w1$age>=19 & w1$age<=27]<-'Youngers:19-27'
w1$agecategory[w1$age>=27 & w1$age<=32]<-'Young adults:27-32'
w1$agecategory[w1$age>32]<-'Olders:32-67'
#w1<-w1[-2492,]

stat.desc(w1$age)
agew<-data.frame(w1$userId,w1$age,w1$webSite)
agew1<-sqldf("select distinct * from agew") #24

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
log.em <- log(avgds)
em.periods <- w1$webSite

count(w1)

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method

#CHECKING OUTLIERS
x<-data.frame(em.pca$x)

summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


############ contribution variable for men  ############
#inertia: https://pbil.univ-lyon1.fr/ade4/ade4-html/inertia.dudi.html

zebu.acp <-log.em
count(log.em)
acp=dudi.pca(log.em)

acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)
#inertieC<-inertia.dudi(acp,row.inertia = TRUE)

#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100

inertieC$col.cum/100

#inertieC$row.abs/100

############ contribution variable for men  ############


############ hierarchichal classification for men  ############

library(questionr)
#1, compute distance of acp (#Distance du ??? Il s'agit de la distance utilis?e dans les analyses de correspondance multiples (ACM))
md <- dist.dudi(acp)

#2 Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. 
#Calcul du dendrogramme
#hclust: Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.

arbre <- hclust(md, method = "ward.D2")
plot(arbre, labels = FALSE, main = "Dendrogramme")

summary(arbre)

#3 how many relevant classes(partitions)
inertie <- sort(arbre$height, decreasing = TRUE)
summary(inertie)

plot(inertie[1:20], type = "s", xlab = "Number de classes",ylab = "Inertie of dendogramme")


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 2(green), 3(red) ou 5(blue) classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 2, border = "green3")
rect.hclust(arbre, 3, border = "red3")
rect.hclust(arbre, 5, border = "blue3")



#let's get the best cut of the tree
library(devtools)
#install_github("larmarange/JLutils")

library(JLutils)
#Par d?faut, best.cutree regarde quelle serait la meilleure partition entre 3 et 20 classes.
#partition ayant la plus grande perte relative d'inertie.
#(to use the partition with the greatest relative loss of inertia.)

numberClasses=best.cutree(arbre)  # by default minimum=3
#numberClasses=3
best.cutree(arbre,min=2) #min number of partitions = 2

best.cutree(arbre, min = 2, graph = TRUE, xlab = "Nombre de classes", 
            ylab = "Inertie relative")

#we cut in 3 classes because the minimum best partition is 2 and the second best one is 3
typo <- cutree(arbre, 3)
freq(typo)
#summary(typo)
#plot(typo)

#graphical representation of scores(acp$li) in the new HCA 

par(mfrow = c(1, 2))
library(RColorBrewer)
s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")
#s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = as.factor(typo), ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 3 classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 3, border = "red3")


#s.class(acp$li, as.factor(typo), 3, 4, col = brewer.pal(3,"Set1"), sub = "Axes 3 et 4")


############ chi square hierarchichal classification for men  ############

############ chi square hierarchichal classification for men  ############
#change typo into data frame
dftypo<-data.frame(typo)
dftypo$obs <- seq.int(nrow(dftypo))
scoresw<-data.frame(em.pca$x)
dftypo$pc1<-scoresw$PC1
dftypo$pc2<-scoresw$PC2
count(dftypo)
#get emotions from pc1
dftypo$disgust<-w1$disgust
dftypo$anger<-w1$anger
dftypo$fear<-w1$fear
dftypo$sadness<-w1$sadness
dftypo$happiness<-w1$happiness
dftypo$neutral<-w1$neutral
dftypo$contempt<-w1$contempt
dftypo$age<-w1$age
dftypo$webSite<-w1$webSite


dfhca<-data.frame(dftypo$obs,dftypo$disgust,dftypo$anger,dftypo$fear,dftypo$sadness,dftypo$happiness,dftypo$neutral,dftypo$contempt,dftypo$pc1,dftypo$pc2,dftypo$typo,dftypo$age,dftypo$webSite)
names(dfhca)<-c('obs','disgust','anger','fear','sadness','h','n','c','pc1','pc2','class','age','webSite')
count(dftypo)

View(dfhca)
View(dftypo)
View(log.em)
library(sqldf)

c1<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=1")
c2<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=2")
c3<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=3")

call<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca")

calldf<-rbind(c1,c2,c3,call)


c1all<-sqldf("select * from dfhca where class=1")
c2all<-sqldf("select * from dfhca where class=2")
c3all<-sqldf("select * from dfhca where class=3")

summary(c1all$age)
summary(c2all$age)
summary(c3all$age)

hist(c1all$age)
hist(c2all$age)
hist(c3all$age)

#prepare cross table w1
w1$obs<-seq.int(nrow(w1))
#w1$webSite
#422
tbw1c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website4'")
datachi <- matrix(c(200,25,112,176,112,165,676,364,27,37,89,0), ncol=3, byrow=T)


######## classes per website aesthetics usability
uh1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")

ul1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")

ah1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")

al1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")


datachiu <- matrix(c(376,137,277,713,453,27), ncol=3, byrow=T)
datachia <- matrix(c(213,201,165,876,389,139), ncol=3, byrow=T)


testchiu=chisq.test(datachiu)

#plot residuals
corrplot(testchiu$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchiu$residuals^2/testchiu$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachiu)


testchia=chisq.test(datachia)
#plot residuals
corrplot(testchia$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchia$residuals^2/testchia$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachia)

#########




testchi=chisq.test(datachi)
#X-squared = 35.468, df = 6, p-value = 3.497e-06 -> we reject Ho(independet) so that there is dependency between classes and websites(websites influence classes).


#visualize the residuals 
library(corrplot)
rownames(datachi) <- c('website1', 'website2','website3', 'website4')
#plot residuals
corrplot(testchi$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchi$residuals^2/testchi$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

library(vcd) 
assocstats(datachi)



datachi <- matrix(c(115,173,75,8,52,35,95,133,90,204,234,180), ncol=3, byrow=T)
chisq.test(data)
############ chi square hierarchichal classification for men  ############

############### compute confidence intervals per emotion per class - men #############
library(rcompanion)
call<-sqldf("select class,w1.webSite,(w1.disgust),'disgust',(w1.anger),'anger',(w1.fear),'fear',(w1.sadness),'fear',(w1.happiness),'happiness',(w1.neutral),'neutral',
            (w1.contempt),'contempt' from dfhca as hca,w1 where  hca.obs=w1.obs  ")
#stat.desc(c1w1)
a<-groupwiseMean(disgust ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 normal      = TRUE)
a$emotion<-"disgust"




b<-groupwiseMean(anger ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 normal      = TRUE)

b$emotion<-"anger"

c<-groupwiseMean(fear ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 normal      = TRUE)

c$emotion<-"fear"


d<-groupwiseMean(sadness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 normal      = TRUE)

d$emotion<-"sadness"

e<-groupwiseMean(happiness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 normal      = TRUE)

e$emotion<-"happiness"

f<-groupwiseMean(neutral ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 normal      = TRUE)

f$emotion<-"neutral"

g<-groupwiseMean(contempt ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 normal      = TRUE)

g$emotion<-"contempt"

pc1gm<-groupwiseMean(pc1 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     normal      = TRUE)
pc1gm$emotion<-"pc1"

pc2gm<-groupwiseMean(pc2 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     normal      = TRUE)

pc2gm$emotion<-"pc2"

pc1all<-groupwiseMean(pc1 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      normal      = TRUE)
pc1all$emotion<-"pc1"

pc2all<-groupwiseMean(pc2 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      normal      = TRUE)
pc2all$emotion<-"pc2"

stat.desc(scores)

ciall<-rbind(a,b,c,d,e,f,g)
ciall1<-sqldf("select * from ciall where class=1 and webSite='website1'")
ciall2<-sqldf("select * from ciall where class=1 and webSite='website2'")
ciall3<-sqldf("select * from ciall where class=1 and webSite='website3'")
ciall4<-sqldf("select * from ciall where class=1 and webSite='website4'")

c2all1<-sqldf("select * from ciall where class=2 and webSite='website1'")
c2all2<-sqldf("select * from ciall where class=2 and webSite='website2'")
c2all3<-sqldf("select * from ciall where class=2 and webSite='website3'")
c2all4<-sqldf("select * from ciall where class=2 and webSite='website4'")

c3all1<-sqldf("select * from ciall where class=3 and webSite='website1'")
c3all2<-sqldf("select * from ciall where class=3 and webSite='website2'")
c3all3<-sqldf("select * from ciall where class=3 and webSite='website3'")
c3all4<-sqldf("select * from ciall where class=3 and webSite='website4'")

#fear anger and disgst
#ciall1fad1<-sqldf("select * from ciall where class=1 and webSite='website1' and (emotion='fear' or emotion='anger' or emotion='disgust') ")
ciall1fad1<-sqldf("select * from ciall where class=1 and (emotion='fear') ")
ciall1fad2<-sqldf("select * from ciall where class=1 and (emotion='anger') ")
ciall1fad3<-sqldf("select * from ciall where class=1 and (emotion='disgust') ")
ciall1fad4<-sqldf("select * from ciall where class=1 and (emotion='happiness') ")
ciall1fad5<-sqldf("select * from ciall where class=1 and (emotion='sadness') ")
ciall1fad6<-sqldf("select * from ciall where class=1 and (emotion='neutral') ")
ciall1fad7<-sqldf("select * from ciall where class=1 and (emotion='contempt') ")

ciall2fad1<-sqldf("select * from ciall where class=2 and (emotion='fear') ")
ciall2fad2<-sqldf("select * from ciall where class=2 and (emotion='anger') ")
ciall2fad3<-sqldf("select * from ciall where class=2 and (emotion='disgust') ")
ciall2fad4<-sqldf("select * from ciall where class=2 and (emotion='happiness') ")
ciall2fad5<-sqldf("select * from ciall where class=2 and (emotion='sadness') ")
ciall2fad6<-sqldf("select * from ciall where class=2 and (emotion='neutral') ")
ciall2fad7<-sqldf("select * from ciall where class=2 and (emotion='contempt') ")

ciall3fad1<-sqldf("select * from ciall where class=3 and (emotion='fear') ")
ciall3fad2<-sqldf("select * from ciall where class=3 and (emotion='anger') ")
ciall3fad3<-sqldf("select * from ciall where class=3 and (emotion='disgust') ")
ciall3fad4<-sqldf("select * from ciall where class=3 and (emotion='happiness') ")
ciall3fad5<-sqldf("select * from ciall where class=3 and (emotion='sadness') ")
ciall3fad6<-sqldf("select * from ciall where class=3 and (emotion='neutral') ")
ciall3fad7<-sqldf("select * from ciall where class=3 and (emotion='contempt') ")


pc1c<-sqldf("select * from pc1gm where class=1  ")
pc2c<-sqldf("select * from pc1gm where class=2  ")
pc3c<-sqldf("select * from pc1gm where class=3  ")

pc2c1<-sqldf("select * from pc2gm where class=1  ")
pc2c2<-sqldf("select * from pc2gm where class=2  ")
pc2c3<-sqldf("select * from pc2gm where class=3  ")

pc1all<-sqldf("select * from pc1all ")
pc2all<-sqldf("select * from pc2all ")



require(ggplot2)
library(cowplot)


p1<-ggplot(ciall1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w1")
p2<-ggplot(ciall2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w2") 
p3<-ggplot(ciall3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w3")
p4<-ggplot(ciall4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w4")

p5<-ggplot(c2all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w1")
p6<-ggplot(c2all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w2") 
p7<-ggplot(c2all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w3")
p8<-ggplot(c2all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w4")

p9<-ggplot(c3all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w1")
p10<-ggplot(c3all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w2") 
p11<-ggplot(c3all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w3")
p12<-ggplot(c3all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w4")

plot_grid(p1,p2,p3,p4, ncol = 2, nrow = 2)
plot_grid(p5,p6,p7,p8, ncol = 2, nrow = 2)
plot_grid(p9,p10,p11,p12, ncol = 2, nrow = 2)




p1f<-ggplot(ciall1fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("fear for all websites in c1")
p1a<-ggplot(ciall1fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("anger for all websites in c1")
p1d<-ggplot(ciall1fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("disgust for all websites in c1")
p1h<-ggplot(ciall1fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("happiness for all websites in c1")
p1s<-ggplot(ciall1fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("sadness for all websites in c1")
p1n<-ggplot(ciall1fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("neutral for all websites in c1")
p1c<-ggplot(ciall1fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("contempt for all websites in c1")

p2f<-ggplot(ciall2fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("fear for all websites in c2")
p2a<-ggplot(ciall2fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("anger for all websites in c2")
p2d<-ggplot(ciall2fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("disgust for all websites in c2")
p2h<-ggplot(ciall2fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("happiness for all websites in c2")
p2s<-ggplot(ciall2fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("sadness for all websites in c2")
p2n<-ggplot(ciall2fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("neutral for all websites in c2")
p2c<-ggplot(ciall2fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("contempt for all websites in c2")

p3f<-ggplot(ciall3fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("fear for all websites in c3")
p3a<-ggplot(ciall3fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("anger for all websites in c3")
p3d<-ggplot(ciall3fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("disgust for all websites in c3")
p3h<-ggplot(ciall3fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("happiness for all websites in c3")
p3s<-ggplot(ciall3fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("sadness for all websites in c3")
p3n<-ggplot(ciall3fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("neutral for all websites in c3")
p3c<-ggplot(ciall3fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("contempt for all websites in c3")


plot_grid(p1f,p1a,p1d,p1h,p1s,p1n,p1c, ncol = 4, nrow = 2)
plot_grid(p2f,p2a,p2d,p2h,p2s,p2n,p2c, ncol = 4, nrow = 2)
plot_grid(p3f,p3a,p3d,p3h,p3s,p3n,p3c, ncol = 4, nrow = 2)


ppc1c1<-ggplot(pc1c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c1")
ppc1c2<-ggplot(pc2c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c2")
ppc1c3<-ggplot(pc3c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c3")



ppc2c1<-ggplot(pc2c1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c1")
ppc2c2<-ggplot(pc2c2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c2")
ppc2c3<-ggplot(pc2c3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c3")

plot_grid(ppc1c1,ppc1c2,ppc1c3, ncol = 3, nrow = 1)
plot_grid(ppc2c1,ppc2c2,ppc2c3, ncol = 3, nrow = 1)


ppc1all<-ggplot(pc1all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites ")
ppc2all<-ggplot(pc2all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites ")


library(party)
dfhcaaux<-dfhca
dfhcaaux[dfhcaaux$class == 1]<-"class1"


fit <- ctree( age~class,data=dfhca,controls = ctree_control(maxdepth = 10))
fit1 <- ctree( class~age,data=dfhca,controls = ctree_control(maxdepth = 10))
plot(fit, main="Age-class tree ")
plot(fit1, main="Age-class tree ")




############### compute confidence intervals per emotion per class - men #############
############### descrip stats for pc1 and pc2 #############

pcw1<-sqldf("select webSite,hca.* from dfhca as hca,w1 
            where class=1 and hca.obs=w1.obs and (w1.webSite ='website1')  ")

pcw2<-sqldf("select webSite,hca.* from dfhca as hca,w1 
            where class=1 and hca.obs=w1.obs and (w1.webSite ='website2')  ")

pcw3<-sqldf("select webSite,hca.* from dfhca as hca,w1 
            where class=1 and hca.obs=w1.obs and (w1.webSite ='website3')  ")

pcw4<-sqldf("select webSite,hca.* from dfhca as hca,w1 
            where class=1 and hca.obs=w1.obs and (w1.webSite ='website4')  ")


sdpcw1<-stat.desc(pcw1)
sdpcw2<-stat.desc(pcw2)
sdpcw3<-stat.desc(pcw3)
sdpcw4<-stat.desc(pcw4)
View(sdpcw4)


sdpc12<-stat.desc(dfhca)
View(sdpc12)

dspca1<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
              stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
              variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
              where class=1 and hca.obs=w1.obs and (w1.webSite ='website1')  ")

dspca2<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
              stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
              variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
              where class=1 and hca.obs=w1.obs and (w1.webSite ='website2')  ")

dspca3<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
              stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
              variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
              where class=1 and hca.obs=w1.obs and (w1.webSite ='website3')  ")

dspca4<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
              stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
              variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
              where class=1 and hca.obs=w1.obs and (w1.webSite ='website4')  ")

dspca12<-sqldf("select avg(hca.disgust),avg(hca.anger),avg(hca.fear),avg(hca.sadness),avg(hca.h),avg(hca.n),avg(hca.c),stdev(hca.disgust),
               stdev(hca.anger),stdev(hca.fear),stdev(hca.sadness),stdev(hca.h),stdev(hca.n),stdev(hca.c),variance(hca.disgust),variance(hca.anger),
               variance(hca.fear),variance(hca.sadness),variance(hca.h),variance(hca.n),variance(hca.c) from dfhca as hca,w1 
               where class=1 and hca.obs=w1.obs  ")

pc12all<-rbind(dspca1,dspca2,dspca3,dspca4)


############### descrip stats for pc1 and pc2 #############


######## PCA per clic per user  - genre - men ######################################################

######## PCA per clic per user  - AGE ######################################################
#                     age     genre      webSite              R1_NB5      R1_Mieux   
#2017-04-04 13:10:01: 348   Min.   :19   F:1393   Length:3376        Min.   :1.00   Atlas: 450  
#2017-03-14 13:46:48: 162   1st Qu.:23   H:1983   Class :character   1st Qu.:1.00   Kenzi:2926  
#2017-03-14 11:29:00: 155   Median :27            Mode  :character   Median :1.00               
#2017-03-14 10:44:59: 154   Mean   :31                               Mean   :1.52               
#2017-04-05 17:37:28: 129   3rd Qu.:32
# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#remove outliers (heuristic far the data concentration)
w1<-w1[-2492,]
w1<-w1[-1352,]
w1<-w1[-3320,]

w1<-w1[-3319,]
w1<-w1[-2595,]

summary(w1)


#w1<-subset(w1,genre=='H')
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")

w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'

w1$agecategory[w1$age>=19 & w1$age<=27]<-'Youngers:19-27'
w1$agecategory[w1$age>=27 & w1$age<=32]<-'Yound adults:27-32'
w1$agecategory[w1$age>32]<-'Olders:32-67'


#w1<-w1[-2492,]

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
log.em <- log(avgds)
em.periods <- w1$agecategory



# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method

#CHECKING OUTLIERS
x<-data.frame(em.pca$x)

summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

######## PCA per clic per user  - AGE ######################################################






print('######### PCA individual ######################################################')
w <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/wallFinal.csv", header=TRUE, sep=",")
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#w1<-subset(w1,webSite==1)

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
log.em <- log(avgds)
em.periods <- w1$webSite

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, 
              ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



############ indv w1  

############ indv w2  
print('######### PCA individual ######################################################')
w <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/wallFinal.csv", header=TRUE, sep=",")
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
w1<-subset(w1,webSite==2)

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
log.em <- log(avgds)
em.periods <- w1$dom_element_tag

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, 
              ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



############ indv w2  


############ indv w3  
print('######### PCA individual ######################################################')
w <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/wallFinal.csv", header=TRUE, sep=",")
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
w1<-subset(w1,webSite==3)
w1<-w1[-1187,]
nrow(w1)

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
log.em <- log(avgds)
em.periods <- w1$dom_element_tag

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)
scores<-em.pca$x

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, 
              ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



############ indv w3  


############ indv w4 
print('######### PCA individual ######################################################')
w <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/wallFinal.csv", header=TRUE, sep=",")
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
w1$webSite[w$webSite==1]<-'website1'
w1$webSite[w$webSite==2]<-'website2'
w1$webSite[w$webSite==3]<-'website3'
w1$webSite[w$webSite==4]<-'website4'

nrow(w1)

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness)
log.em <- log(avgds)
em.periods <- w1$webSite

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)
scores<-em.pca$x

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, 
              ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)



############ indv w4  

############ contribution 

zebu.acp <-log.em


acp=dudi.pca(log.em)
acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)
#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100

############ contrubution


######### PCA AVG for w1 ######################################################








############ websites emotions ag
print('######### PCA individual ######################################################')
w <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/final population/outputr/wallFinal.csv", header=TRUE, sep=",")
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
w1$webSite


w1aux<-sqldf('select webSite,avg(happiness) as happiness,avg(contempt) as contempt,avg(anger)
as anger,avg(disgust) as disgust,avg(fear) as fear,avg(sadness) as sadness,avg(neutral) as neutral
           from w1 group by website')
w1<-w1aux

w1$webSite[w1$webSite=="1"]<-'website1 high-low'
w1$webSite[w1$webSite=='2']<-'website2 high-high'
w1$webSite[w1$webSite=='3']<-'website3 low-low'
w1$webSite[w1$webSite=='4']<-'website4 low-high'

nrow(w1)

avgds<-data.frame(w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness)
colnames(avgds)<-c('happiness','contempt','anger','disgust','fear','sadness')
log.em <- log(avgds)
em.periods <- w1$webSite

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)
scores<-em.pca$x

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
summary(em.pca)


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, 
              ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


############ websites emotions avg


############ boxplot for raw data
require(ggplot2)
MyData <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/03may2017.csv", header=TRUE, sep=";")
d<-MyData

d$gender[d$genre == 'F']<- 'W'
d$gender[d$genre=='H']<- "M"



d$webSite[d$webSite=="1"]<-'website1 high-low'
d$webSite[d$webSite=='2']<-'website2 high-high'
d$webSite[d$webSite=='3']<-'website3 low-low'
d$webSite[d$webSite=='4']<-'website4 low-high'
########################################### boxplot women #########
d<-subset(d,gender=='W')
#[1] 1399
nrow(d)

# plot per category women men
#bps<-ggplot(data = d, aes(x=webSite, y=sadness)) + 
 # geom_boxplot(aes(fill=genre),position=position_dodge(1.2),width=0.3) +
#  stat_summary(aes(fill=genre),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
#  stat_summary(aes(fill=genre),fun.y = mean, geom = "point")


bpn<-ggplot(data = d, aes(x=webSite, y=neutral)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of neutral intensity per website for women")+
  xlab("website") + ylab("neutral") +labs(fill = "gender")
  
 
bps<-ggplot(data = d, aes(x=webSite, y=sadness)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of sadness intensity per website for women")+
  xlab("website") + ylab("sadness") +labs(fill = "gender")


bph<-ggplot(data = d, aes(x=webSite, y=happiness)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of happiness intensity per website for women")+
  xlab("website") + ylab("happiness") +labs(fill = "gender")


bpd<-ggplot(data = d, aes(x=webSite, y=disgust)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of disgust intensity per website for women")+
  xlab("website") + ylab("disgust") +labs(fill = "gender")



bpa<-ggplot(data = d, aes(x=webSite, y=anger)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of anger intensity per website for women")+
  xlab("website") + ylab("anger") +labs(fill = "gender")



bpf<-ggplot(data = d, aes(x=webSite, y=fear)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of fear intensity per website for women")+
  xlab("website") + ylab("fear") +labs(fill = "gender")


bpc<-ggplot(data = d, aes(x=webSite, y=contempt)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of contempt intensity per website for women")+
  xlab("website") + ylab("contempt") +labs(fill = "gender")


bpn
bps
bph
bpd
bpa
bpf 
bpc
########################################### boxplot women #########

########################################### boxplot men #########
require(ggplot2)
MyData <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/03may2017.csv", header=TRUE, sep=";")
d<-MyData
 
d$gender[d$genre == 'F']<- 'W'
d$gender[d$genre=='H']<- "M"



d$webSite[d$webSite=="1"]<-'website1 high-low'
d$webSite[d$webSite=='2']<-'website2 high-high'
d$webSite[d$webSite=='3']<-'website3 low-low'
d$webSite[d$webSite=='4']<-'website4 low-high'
d<-subset(d,gender=='M')
#[1] 1399
nrow(d)

# plot per category women men
#bps<-ggplot(data = d, aes(x=webSite, y=sadness)) + 
# geom_boxplot(aes(fill=genre),position=position_dodge(1.2),width=0.3) +
#  stat_summary(aes(fill=genre),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
#  stat_summary(aes(fill=genre),fun.y = mean, geom = "point")
#update_geom_defaults("boxplot",   list(fill = "blue"))

bpn<-ggplot(data = d, aes(x=webSite, y=neutral)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of neutral intensity per website for men")+
  xlab("website") + ylab("neutral") +labs(fill = "gender") 


bps<-ggplot(data = d, aes(x=webSite, y=sadness)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of sadness intensity per website for men")+
  xlab("website") + ylab("sadness") +labs(fill = "gender")


bph<-ggplot(data = d, aes(x=webSite, y=happiness)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of happiness intensity per website for men")+
  xlab("website") + ylab("happiness") +labs(fill = "gender")


bpd<-ggplot(data = d, aes(x=webSite, y=disgust)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of disgust intensity per website for men")+
  xlab("website") + ylab("disgust") +labs(fill = "gender")



bpa<-ggplot(data = d, aes(x=webSite, y=anger)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of anger intensity per website for men")+
  xlab("website") + ylab("anger") +labs(fill = "gender")



bpf<-ggplot(data = d, aes(x=webSite, y=fear)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of fear intensity per website for men")+
  xlab("website") + ylab("fear") +labs(fill = "gender")


bpc<-ggplot(data = d, aes(x=webSite, y=contempt)) + 
  geom_boxplot(aes(fill=gender),position=position_dodge(1.2),width=0.3) +
  stat_summary(aes(fill=gender),fun.data = mean_cl_boot,width=0.5, geom = "errorbar") + 
  stat_summary(aes(fill=gender),fun.y = mean, geom = "point")+
  ggtitle("Comparison of contempt intensity per website for men")+
  xlab("website") + ylab("contempt") +labs(fill = "gender")


bpn
bps
bph
bpd
bpa
bpf 
bpc
 

########################################### boxplot men #########

########### boxplot for raw data

############ stats
#w1 h
MyData <- read.csv(file="C:/Users/julix/Google Drive/000-Doctorado/000-Fr/1-year/000-JF/2-year/task 6 experiment online/03may2017.csv", header=TRUE, sep=";")
d<-MyData
library(pastecs)


w1h<-subset(d,webSite==1 & genre=='H')
options(scipen=100)
options(digits=2)
scores<-cbind(w1h$neutral,w1h$sadness,w1h$contempt,w1h$disgust,w1h$anger,w1h$fear,w1h$happiness)
colnames(scores)<-c('neutral','sadness','contempt','disgust','anger','fear','happiness')
stat.desc(scores)


w1h<-subset(d,webSite==1 & genre=='F')
options(scipen=100)
options(digits=2)
scores<-cbind(w1h$neutral,w1h$sadness,w1h$contempt,w1h$disgust,w1h$anger,w1h$fear,w1h$happiness)
colnames(scores)<-c('neutral','sadness','contempt','disgust','anger','fear','happiness')
stat.desc(scores)
#w2
w1h<-subset(d,webSite==2 & genre=='H')
options(scipen=100)
options(digits=2)
scores<-cbind(w1h$neutral,w1h$sadness,w1h$contempt,w1h$disgust,w1h$anger,w1h$fear,w1h$happiness)
colnames(scores)<-c('neutral','sadness','contempt','disgust','anger','fear','happiness')
stat.desc(scores)


w1h<-subset(d,webSite==2 & genre=='F')
options(scipen=100)
options(digits=2)
scores<-cbind(w1h$neutral,w1h$sadness,w1h$contempt,w1h$disgust,w1h$anger,w1h$fear,w1h$happiness)
colnames(scores)<-c('neutral','sadness','contempt','disgust','anger','fear','happiness')
stat.desc(scores)

#w3
w1h<-subset(d,webSite==3 & genre=='H')
options(scipen=100)
options(digits=2)
scores<-cbind(w1h$neutral,w1h$sadness,w1h$contempt,w1h$disgust,w1h$anger,w1h$fear,w1h$happiness)
colnames(scores)<-c('neutral','sadness','contempt','disgust','anger','fear','happiness')
stat.desc(scores)


w1h<-subset(d,webSite==3 & genre=='F')
options(scipen=100)
options(digits=2)
scores<-cbind(w1h$neutral,w1h$sadness,w1h$contempt,w1h$disgust,w1h$anger,w1h$fear,w1h$happiness)
colnames(scores)<-c('neutral','sadness','contempt','disgust','anger','fear','happiness')
stat.desc(scores)


#w4
w1h<-subset(d,webSite==4 & genre=='H')
options(scipen=100)
options(digits=2)
scores<-cbind(w1h$neutral,w1h$sadness,w1h$contempt,w1h$disgust,w1h$anger,w1h$fear,w1h$happiness)
colnames(scores)<-c('neutral','sadness','contempt','disgust','anger','fear','happiness')
stat.desc(scores)


w1h<-subset(d,webSite==4 & genre=='F')
options(scipen=100)
options(digits=2)
scores<-cbind(w1h$neutral,w1h$sadness,w1h$contempt,w1h$disgust,w1h$anger,w1h$fear,w1h$happiness)
colnames(scores)<-c('neutral','sadness','contempt','disgust','anger','fear','happiness')
stat.desc(scores)



########### stats


########### stats testing PCA
library(devtools)
library(ggfortify)
library(ggfortify)
library(ggplot2)

data(iris)
#iris.pca <- iris[c(1, 2, 3, 4)] 
#bpc<-ggplot(data = d, aes(x=webSite, y=contempt))
#autoplot(bpc)

# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
########### stats


############### RCIS computation ##############


############### RCIS computation <= 27 women ##############
#                     age     genre      webSite              R1_NB5      R1_Mieux   
#2017-04-04 13:10:01: 348   Min.   :19   F:1393   Length:3376        Min.   :1.00   Atlas: 450  
#2017-03-14 13:46:48: 162   1st Qu.:23   H:1983   Class :character   1st Qu.:1.00   Kenzi:2926  
#2017-03-14 11:29:00: 155   Median :27            Mode  :character   Median :1.00               
#2017-03-14 10:44:59: 154   Mean   :31                               Mean   :1.52               
#2017-04-05 17:37:28: 129   3rd Qu.:32 
# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#remove outliers (heuristic far the data concentration)

summary(w1)
w1<-w1[-2492,]
w1<-w1[-1352,]
w1<-w1[-3320,]

w1<-w1[-3319,]
w1<-w1[-2595,]
#w1<-w1[-1541,]




w1<-subset(w1,genre=='F')
stat.desc(w1$age)
summary(w1$age)

count(w1)
w2<-subset(w1,webSite=='2')
w3<-subset(w1,webSite=='3')
w11<-subset(w1,webSite=='1')
w4<-subset(w1,webSite=='4')

#w1<-rbind(w2,w3)
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")

w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'

w1$agecategory[w1$age>=19 & w1$age<=27]<-'Youngers:19-27'
w1$agecategory[w1$age>=27 & w1$age<=32]<-'Young adults:27-32'
w1$agecategory[w1$age>32]<-'Olders:32-67'

## filter < 27
w1a <-sqldf("select * from w1 where age <= 27")
w1b <-sqldf("select * from w1 where age > 27")
count(w1a)
count(w1b)
w1<-w1a
count(w1)

#w1<-w1[-2492,]

stat.desc(w1$age)
agew<-data.frame(w1$userId,w1$age,w1$webSite)
agew1<-sqldf("select distinct * from agew") #21
 
#View(agew1)
#> stat.desc(w1$age)
#nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean      SE.mean CI.mean.0.95          var 
#1394.00         0.00         0.00        23.00        63.00        40.00     45403.00        27.00        32.57         0.33         0.64       150.07 
#std.dev     coef.var 
#12.25         0.38 

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
nrow(avgds)
ncol(avgds)

log.em <- log(avgds)
count(log.em)
em.periods <- w1$a

#https://www.researchgate.net/post/What_is_the_best_way_to_scale_parameters_before_running_a_Principal_Component_Analysis_PCA


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#scale #a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with S, but in general scaling is advisable. Alternatively, a vector of length equal the number of columns of x can be supplied. The value is passed to scale.
#scale for every variable : normalization (mean(x1) - x1) / standar deviation(x1)
#to see the used scale em.pca$center , #em.pca$scale
# it is called zVar <- (myVar - mean(myVar)) / sd(myVar)


sh=sd(log.em$w1.happiness)
sc=var(log.em$w1.contempt)
sa=var(log.em$w1.anger)
sd=var(log.em$w1.disgust)
sf=var(log.em$w1.fear)
ss=var(log.em$w1.sadness)
sn=var(log.em$w1.neutral)

msh=mean(log.em$w1.happiness)
msc=mean(log.em$w1.contempt)
msa=mean(log.em$w1.anger)
msd=mean(log.em$w1.disgust)
msf=mean(log.em$w1.fear)
mss=mean(log.em$w1.sadness)
msn=mean(log.em$w1.neutral)


#sd

em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

#em.pca$center
#em.pca$scale

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
em.pca$sdev

#CHECKING OUTLIERS - x=scores
x<-data.frame(em.pca$x)

#w1.happiness w1.contempt w1.anger w1.disgust w1.fear w1.sadness w1.neutral
#aux1=-7.6198*0.429 -5.74*0.380  -9.2*0.477 -7.7*0.488 -6.23*0.410 -1.11*0.194 -0.41616*0.027
#aux2<-data.frame(w1$happiness*0.33+
#w1$contempt*0.41+
#w1$anger*0.50+
#w1$disgust*0.57+
#w1$fear*0.23+
#w1$sadness*0.25+
#w1$neutral*-0.18,1)


summary(em.pca)

g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
############ contribution variable for women  ############

#inertia: https://pbil.univ-lyon1.fr/ade4/ade4-html/inertia.dudi.html

zebu.acp <-log.em
acp=dudi.pca(log.em)
acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)
#inertieC<-inertia.dudi(acp,row.inertia = TRUE)

#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100

inertieC$col.cum/100

#inertieC$row.abs/100

############ contribution variable for women  ############


############ hierarchichal classification for women <27 ############

library(questionr)
#1, compute distance of acp (#Distance du ??? Il s'agit de la distance utilis?e dans les analyses de correspondance multiples (ACM))
md <- dist.dudi(acp)

#2 Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. 
#Calcul du dendrogramme
#hclust: Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.

arbre <- hclust(md, method = "ward.D2")
plot(arbre, labels = FALSE, main = "Dendrogramme")

#summary(arbre)

#3 how many relevant classes(partitions)
inertie <- sort(arbre$height, decreasing = TRUE)
summary(inertie)

plot(inertie[1:20], type = "s", xlab = "Number de classes",ylab = "Inertie of dendogramme")


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 2(green), 3(red) ou 5(blue) classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 2, border = "green3")
rect.hclust(arbre, 3, border = "red3")
rect.hclust(arbre, 5, border = "blue3")



#let's get the best cut of the tree
library(devtools)
#install_github("larmarange/JLutils")

library(JLutils)
#Par d?faut, best.cutree regarde quelle serait la meilleure partition entre 3 et 20 classes.
#partition ayant la plus grande perte relative d'inertie.
#(to use the partition with the greatest relative loss of inertia.)

numberClasses=best.cutree(arbre)  # by default minimum=3
#numberClasses=3
best.cutree(arbre,min=2) #min number of partitions = 2

best.cutree(arbre, min = 2, graph = TRUE, xlab = "Nombre de classes", 
            ylab = "Inertie relative")

#we cut in 3 classes because the minimum best partition is 2 and the second best one is 3
typo <- cutree(arbre, 3)
freq(typo)
#summary(typo)
#plot(typo)

#graphical representation of scores(acp$li) in the new HCA 

par(mfrow = c(1, 2))
library(RColorBrewer)
s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")
#s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = as.factor(typo), ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 3 classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 3, border = "red3")


#s.class(acp$li, as.factor(typo), 3, 4, col = brewer.pal(3,"Set1"), sub = "Axes 3 et 4")


############ chi square hierarchichal classification for women  ############
#change typo into data frame
dftypo<-data.frame(typo)
dftypo$obs <- seq.int(nrow(dftypo))
scoresw<-data.frame(em.pca$x)
dftypo$pc1<-scoresw$PC1
dftypo$pc2<-scoresw$PC2

#get emotions from pc1
dftypo$disgust<-w1$disgust
dftypo$anger<-w1$anger
dftypo$fear<-w1$fear
dftypo$sadness<-w1$sadness
dftypo$happiness<-w1$happiness
dftypo$neutral<-w1$neutral
dftypo$contempt<-w1$contempt
dftypo$age<-w1$age
dftypo$webSite<-w1$webSite

dfhca<-data.frame(dftypo$obs,dftypo$disgust,dftypo$anger,dftypo$fear,dftypo$sadness,dftypo$happiness,dftypo$neutral,dftypo$contempt,dftypo$pc1,dftypo$pc2,dftypo$typo,dftypo$age,dftypo$webSite)

names(dfhca)<-c('obs','disgust','anger','fear','sadness','h','n','c','pc1','pc2','class','age','webSite')

View(dfhca)
View(dftypo)
View(log.em)
library(sqldf)

c1<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=1")
c2<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=2")
c3<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=3")

call<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca")

c1all<-sqldf("select * from dfhca where class=1")
c2all<-sqldf("select * from dfhca where class=2")
c3all<-sqldf("select * from dfhca where class=3")

summary(c1all$age)
summary(c2all$age)
summary(c3all$age)

hist(c1all$age)
hist(c2all$age)
hist(c3all$age)

#prepare cross table w1
w1$obs<-seq.int(nrow(w1))
#w1$webSite
#422

tbw1c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website4'")

#create matrix
dfw1<-data.frame(tbw1c1,tbw1c2,tbw1c3)
dfw2<-data.frame(tbw2c1,tbw2c2,tbw2c3)
dfw3<-data.frame(tbw3c1,tbw3c2,tbw3c3)
dfw4<-data.frame(tbw4c1,tbw4c2,tbw4c3)

aux1<-rbind(dfw1,dfw2,dfw3,dfw4)
datachi=data.matrix(aux1)
rownames(datachi) <- c('website1', 'website2','website3', 'website4')
colnames(datachi) <- c('class1', 'class2','class3')
#### websites vs classes

#datachi <- matrix(c(27,7,66), ncol=3, byrow=T)
testchi=chisq.test(datachi)
#X-squared = 35.468, df = 6, p-value = 3.497e-06 -> we reject Ho(independet) so that there is dependency between classes and websites(websites influence classes).


#visualize the residuals 
library(corrplot)

#plot residuals
corrplot(testchi$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchi$residuals^2/testchi$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

library(vcd) 
assocstats(datachi)

#### websites vs classes

######## classes per website aesthetics usability
uh1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")

ul1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")

ah1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")

al1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")

uh1 
uh2
uh3

ul1
ul2
ul3

ah1
ah2
ah3

al1
al2
al3

datachiu <- matrix(c(52,11,68,164,248,236), ncol=3, byrow=T)
datachia <- matrix(c(160,236,206,56,23,98), ncol=3, byrow=T)

datachiu <- matrix(c(uh1,uh2,uh3,ul1,ul2,ul3), ncol=3, byrow=T)

testchiu=chisq.test(datachiu)

#plot residuals
corrplot(testchiu$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchiu$residuals^2/testchiu$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachiu)


testchia=chisq.test(datachia)
#plot residuals
corrplot(testchia$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchia$residuals^2/testchia$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachia)

#########




############ chi square hierarchichal classification for women  ############

########### t-test


###########


############ hierarchichal classification for women <27 ############

############### compute confidence intervals per emotion per class - women <27 #############
library(rcompanion)
call<-sqldf("select class,w1.webSite,(w1.disgust),'disgust',(w1.anger),'anger',(w1.fear),'fear',(w1.sadness),'fear',(w1.happiness),'happiness',(w1.neutral),'neutral',(w1.contempt),'contempt' from dfhca as hca,w1 where  hca.obs=w1.obs  ")
#stat.desc(c1w1)
a<-groupwiseMean(disgust ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)
a$emotion<-"disgust"




b<-groupwiseMean(anger ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

b$emotion<-"anger"

c<-groupwiseMean(fear ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

c$emotion<-"fear"


d<-groupwiseMean(sadness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

d$emotion<-"sadness"

e<-groupwiseMean(happiness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

e$emotion<-"happiness"

f<-groupwiseMean(neutral ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

f$emotion<-"neutral"

g<-groupwiseMean(contempt ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

g$emotion<-"contempt"

pc1gm<-groupwiseMean(pc1 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     traditional      = TRUE)
pc1gm$emotion<-"pc1"

pc2gm<-groupwiseMean(pc2 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     traditional      = TRUE)

pc2gm$emotion<-"pc2"

pc1all<-groupwiseMean(pc1 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      traditional      = TRUE)
pc1all$emotion<-"pc1"

pc2all<-groupwiseMean(pc2 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      traditional      = TRUE)
pc2all$emotion<-"pc2"

stat.desc(scores)

ciall<-rbind(a,b,c,d,e,f,g)
View(ciall)
ciall1<-sqldf("select * from ciall where class=1 and webSite='website1'")
ciall2<-sqldf("select * from ciall where class=1 and webSite='website2'")
ciall3<-sqldf("select * from ciall where class=1 and webSite='website3'")
ciall4<-sqldf("select * from ciall where class=1 and webSite='website4'")

c2all1<-sqldf("select * from ciall where class=2 and webSite='website1'")
c2all2<-sqldf("select * from ciall where class=2 and webSite='website2'")
c2all3<-sqldf("select * from ciall where class=2 and webSite='website3'")
c2all4<-sqldf("select * from ciall where class=2 and webSite='website4'")

c3all1<-sqldf("select * from ciall where class=3 and webSite='website1'")
c3all2<-sqldf("select * from ciall where class=3 and webSite='website2'")
c3all3<-sqldf("select * from ciall where class=3 and webSite='website3'")
c3all4<-sqldf("select * from ciall where class=3 and webSite='website4'")

#fear anger and disgst
#ciall1fad1<-sqldf("select * from ciall where class=1 and webSite='website1' and (emotion='fear' or emotion='anger' or emotion='disgust') ")
ciall1fad1<-sqldf("select * from ciall where class=1 and (emotion='fear') ")
ciall1fad2<-sqldf("select * from ciall where class=1 and (emotion='anger') ")
ciall1fad3<-sqldf("select * from ciall where class=1 and (emotion='disgust') ")
ciall1fad4<-sqldf("select * from ciall where class=1 and (emotion='happiness') ")
ciall1fad5<-sqldf("select * from ciall where class=1 and (emotion='sadness') ")
ciall1fad6<-sqldf("select * from ciall where class=1 and (emotion='neutral') ")
ciall1fad7<-sqldf("select * from ciall where class=1 and (emotion='contempt') ")

ciall2fad1<-sqldf("select * from ciall where class=2 and (emotion='fear') ")
ciall2fad2<-sqldf("select * from ciall where class=2 and (emotion='anger') ")
ciall2fad3<-sqldf("select * from ciall where class=2 and (emotion='disgust') ")
ciall2fad4<-sqldf("select * from ciall where class=2 and (emotion='happiness') ")
ciall2fad5<-sqldf("select * from ciall where class=2 and (emotion='sadness') ")
ciall2fad6<-sqldf("select * from ciall where class=2 and (emotion='neutral') ")
ciall2fad7<-sqldf("select * from ciall where class=2 and (emotion='contempt') ")

ciall3fad1<-sqldf("select * from ciall where class=3 and (emotion='fear') ")
ciall3fad2<-sqldf("select * from ciall where class=3 and (emotion='anger') ")
ciall3fad3<-sqldf("select * from ciall where class=3 and (emotion='disgust') ")
ciall3fad4<-sqldf("select * from ciall where class=3 and (emotion='happiness') ")
ciall3fad5<-sqldf("select * from ciall where class=3 and (emotion='sadness') ")
ciall3fad6<-sqldf("select * from ciall where class=3 and (emotion='neutral') ")
ciall3fad7<-sqldf("select * from ciall where class=3 and (emotion='contempt') ")


pc1c<-sqldf("select * from pc1gm where class=1  ")
pc2c<-sqldf("select * from pc1gm where class=2  ")
pc3c<-sqldf("select * from pc1gm where class=3  ")

pc2c1<-sqldf("select * from pc2gm where class=1  ")
pc2c2<-sqldf("select * from pc2gm where class=2  ")
pc2c3<-sqldf("select * from pc2gm where class=3  ")

pc1all<-sqldf("select * from pc1all ")
pc2all<-sqldf("select * from pc2all ")



require(ggplot2)
library(cowplot)


p1<-ggplot(ciall1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w1")
p2<-ggplot(ciall2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w2") 
p3<-ggplot(ciall3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w3")
p4<-ggplot(ciall4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w4")

p5<-ggplot(c2all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w1")
p6<-ggplot(c2all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w2") 
p7<-ggplot(c2all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w3")
p8<-ggplot(c2all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w4")

p9<-ggplot(c3all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w1")
p10<-ggplot(c3all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w2") 
p11<-ggplot(c3all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w3")
p12<-ggplot(c3all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w4")

plot_grid(p1,p2,p3,p4, ncol = 2, nrow = 2)
plot_grid(p5,p6,p7,p8, ncol = 2, nrow = 2)
plot_grid(p9,p10,p11,p12, ncol = 2, nrow = 2)


################ emotion per class per websites ##########

p1f<-ggplot(ciall1fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c1")
p1a<-ggplot(ciall1fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c1")
p1d<-ggplot(ciall1fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c1")
p1h<-ggplot(ciall1fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c1")
p1s<-ggplot(ciall1fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c1")
p1n<-ggplot(ciall1fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c1")
p1c<-ggplot(ciall1fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c1")

p2f<-ggplot(ciall2fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c2")
p2a<-ggplot(ciall2fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c2")
p2d<-ggplot(ciall2fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c2")
p2h<-ggplot(ciall2fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c2")
p2s<-ggplot(ciall2fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c2")
p2n<-ggplot(ciall2fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c2")
p2c<-ggplot(ciall2fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c2")

p3f<-ggplot(ciall3fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c3")
p3a<-ggplot(ciall3fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c3")
p3d<-ggplot(ciall3fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c3")
p3h<-ggplot(ciall3fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c3")
p3s<-ggplot(ciall3fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c3")
p3n<-ggplot(ciall3fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c3")
p3c<-ggplot(ciall3fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c3")


plot_grid(p1f,p1a,p1d,p1h,p1s,p1n,p1c, ncol = 4, nrow = 2)
plot_grid(p2f,p2a,p2d,p2h,p2s,p2n,p2c, ncol = 4, nrow = 2)
plot_grid(p3f,p3a,p3d,p3h,p3s,p3n,p3c, ncol = 4, nrow = 2)

################ emotion per class per websites ##########

ppc1c1<-ggplot(pc1c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c1")
ppc1c2<-ggplot(pc2c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c2")
ppc1c3<-ggplot(pc3c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c3")



ppc2c1<-ggplot(pc2c1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c1")
ppc2c2<-ggplot(pc2c2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c2")
ppc2c3<-ggplot(pc2c3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c3")

plot_grid(ppc1c1,ppc1c2,ppc1c3, ncol = 3, nrow = 1)
plot_grid(ppc2c1,ppc2c2,ppc2c3, ncol = 3, nrow = 1)


ppc1all<-ggplot(pc1all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites ")
ppc2all<-ggplot(pc2all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites ")


library(party)
dfhcaaux<-dfhca
dfhcaaux[dfhcaaux$class == 1]<-"class1"


fit <- ctree( age~class,data=dfhca,controls = ctree_control(maxdepth = 10))
fit1 <- ctree( class~age,data=dfhca,controls = ctree_control(maxdepth = 10))
plot(fit, main="Age-class tree ")
plot(fit1, main="Age-class tree ")




############### compute confidence intervals per emotion per class - women < 27 #############





############### RCIS computation <= 27 ##############

############### RCIS computation > 27 ##############
#                     age     genre      webSite              R1_NB5      R1_Mieux   
#2017-04-04 13:10:01: 348   Min.   :19   F:1393   Length:3376        Min.   :1.00   Atlas: 450  
#2017-03-14 13:46:48: 162   1st Qu.:23   H:1983   Class :character   1st Qu.:1.00   Kenzi:2926  
#2017-03-14 11:29:00: 155   Median :27            Mode  :character   Median :1.00               
#2017-03-14 10:44:59: 154   Mean   :31                               Mean   :1.52               
#2017-04-05 17:37:28: 129   3rd Qu.:32 
# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#remove outliers (heuristic far the data concentration)

summary(w1)
w1<-w1[-2492,]
w1<-w1[-1352,]
w1<-w1[-3320,]

w1<-w1[-3319,]
w1<-w1[-2595,]
#w1<-w1[-1541,]



w1<-subset(w1,genre=='F')

count(w1)
w2<-subset(w1,webSite=='2')
w3<-subset(w1,webSite=='3')
w11<-subset(w1,webSite=='1')
w4<-subset(w1,webSite=='4')

#w1<-rbind(w2,w3)
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")

w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'

w1$agecategory[w1$age>=19 & w1$age<=27]<-'Youngers:19-27'
w1$agecategory[w1$age>=27 & w1$age<=32]<-'Young adults:27-32'
w1$agecategory[w1$age>32]<-'Olders:32-67'

## filter < 27
w1a <-sqldf("select * from w1 where age <= 27")
w1b <-sqldf("select * from w1 where age > 27")
count(w1a)
count(w1b)
w1<-w1b
count(w1)

#w1<-w1[-2492,]

stat.desc(w1$age)
agew<-data.frame(w1$userId,w1$age,w1$webSite)
agew1<-sqldf("select distinct * from agew") #21

View(agew1)
#> stat.desc(w1$age)
#nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean      SE.mean CI.mean.0.95          var 
#1394.00         0.00         0.00        23.00        63.00        40.00     45403.00        27.00        32.57         0.33         0.64       150.07 
#std.dev     coef.var 
#12.25         0.38 

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
nrow(avgds)
ncol(avgds)

log.em <- log(avgds)
count(log.em)
em.periods <- w1$a

#https://www.researchgate.net/post/What_is_the_best_way_to_scale_parameters_before_running_a_Principal_Component_Analysis_PCA


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#scale #a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with S, but in general scaling is advisable. Alternatively, a vector of length equal the number of columns of x can be supplied. The value is passed to scale.
#scale for every variable : normalization (mean(x1) - x1) / standar deviation(x1)
#to see the used scale em.pca$center , #em.pca$scale
# it is called zVar <- (myVar - mean(myVar)) / sd(myVar)


sh=sd(log.em$w1.happiness)
sc=var(log.em$w1.contempt)
sa=var(log.em$w1.anger)
sd=var(log.em$w1.disgust)
sf=var(log.em$w1.fear)
ss=var(log.em$w1.sadness)
sn=var(log.em$w1.neutral)

msh=mean(log.em$w1.happiness)
msc=mean(log.em$w1.contempt)
msa=mean(log.em$w1.anger)
msd=mean(log.em$w1.disgust)
msf=mean(log.em$w1.fear)
mss=mean(log.em$w1.sadness)
msn=mean(log.em$w1.neutral)


#sd

em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

#em.pca$center
#em.pca$scale

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
em.pca$sdev

#CHECKING OUTLIERS - x=scores
x<-data.frame(em.pca$x)

#w1.happiness w1.contempt w1.anger w1.disgust w1.fear w1.sadness w1.neutral
#aux1=-7.6198*0.429 -5.74*0.380  -9.2*0.477 -7.7*0.488 -6.23*0.410 -1.11*0.194 -0.41616*0.027
#aux2<-data.frame(w1$happiness*0.33+
#w1$contempt*0.41+
#w1$anger*0.50+
#w1$disgust*0.57+
#w1$fear*0.23+
#w1$sadness*0.25+
#w1$neutral*-0.18,1)


summary(em.pca)

g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
############ contribution variable for women  ############

#inertia: https://pbil.univ-lyon1.fr/ade4/ade4-html/inertia.dudi.html

zebu.acp <-log.em
acp=dudi.pca(log.em)
acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)
#inertieC<-inertia.dudi(acp,row.inertia = TRUE)

#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100

inertieC$col.cum/100

#inertieC$row.abs/100

############ contribution variable for women  ############


############ hierarchichal classification for women >=27 ############

library(questionr)
#1, compute distance of acp (#Distance du ??? Il s'agit de la distance utilis?e dans les analyses de correspondance multiples (ACM))
md <- dist.dudi(acp)

#2 Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. 
#Calcul du dendrogramme
#hclust: Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.

arbre <- hclust(md, method = "ward.D2")
plot(arbre, labels = FALSE, main = "Dendrogramme")

#summary(arbre)

#3 how many relevant classes(partitions)
inertie <- sort(arbre$height, decreasing = TRUE)
summary(inertie)

plot(inertie[1:20], type = "s", xlab = "Number de classes",ylab = "Inertie of dendogramme")


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 2(green), 3(red) ou 5(blue) classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 2, border = "green3")
rect.hclust(arbre, 3, border = "red3")
rect.hclust(arbre, 5, border = "blue3")



#let's get the best cut of the tree
library(devtools)
#install_github("larmarange/JLutils")

library(JLutils)
#Par d?faut, best.cutree regarde quelle serait la meilleure partition entre 3 et 20 classes.
#partition ayant la plus grande perte relative d'inertie.
#(to use the partition with the greatest relative loss of inertia.)

numberClasses=best.cutree(arbre)  # by default minimum=3
numberClasses

#numberClasses=3
best.cutree(arbre,min=2) #min number of partitions = 2
best.cutree(arbre, min = 2, graph = TRUE, xlab = "Nombre de classes", 
            ylab = "Inertie relative")

#we cut in 3 classes because the minimum best partition is 2 and the second best one is 3
typo <- cutree(arbre, 3)
freq(typo)
#summary(typo)
#plot(typo)

#graphical representation of scores(acp$li) in the new HCA 

par(mfrow = c(1, 2))
library(RColorBrewer)
s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")
#s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = as.factor(typo), ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 3 classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 3, border = "red3")


#s.class(acp$li, as.factor(typo), 3, 4, col = brewer.pal(3,"Set1"), sub = "Axes 3 et 4")


############ chi square hierarchichal classification for women  ############
#change typo into data frame
dftypo<-data.frame(typo)
dftypo$obs <- seq.int(nrow(dftypo))
scoresw<-data.frame(em.pca$x)
dftypo$pc1<-scoresw$PC1
dftypo$pc2<-scoresw$PC2

#get emotions from pc1
dftypo$disgust<-w1$disgust
dftypo$anger<-w1$anger
dftypo$fear<-w1$fear
dftypo$sadness<-w1$sadness
dftypo$happiness<-w1$happiness
dftypo$neutral<-w1$neutral
dftypo$contempt<-w1$contempt
dftypo$age<-w1$age
dftypo$webSite<-w1$webSite

dfhca<-data.frame(dftypo$obs,dftypo$disgust,dftypo$anger,dftypo$fear,dftypo$sadness,dftypo$happiness,dftypo$neutral,dftypo$contempt,dftypo$pc1,dftypo$pc2,dftypo$typo,dftypo$age,dftypo$webSite)

names(dfhca)<-c('obs','disgust','anger','fear','sadness','h','n','c','pc1','pc2','class','age','webSite')

View(dfhca)
View(dftypo)
View(log.em)
library(sqldf)

c1<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=1")
c2<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=2")
c3<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=3")

call<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca")

c1all<-sqldf("select * from dfhca where class=1")
c2all<-sqldf("select * from dfhca where class=2")
c3all<-sqldf("select * from dfhca where class=3")

summary(c1all$age)
summary(c2all$age)
summary(c3all$age)

hist(c1all$age)
hist(c2all$age)
hist(c3all$age)

#prepare cross table w1
w1$obs<-seq.int(nrow(w1))
#w1$webSite
#422

tbw1c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website4'")

#create matrix
dfw1<-data.frame(tbw1c1,tbw1c2,tbw1c3)
dfw2<-data.frame(tbw2c1,tbw2c2,tbw2c3)
dfw3<-data.frame(tbw3c1,tbw3c2,tbw3c3)
dfw4<-data.frame(tbw4c1,tbw4c2,tbw4c3)

aux1<-rbind(dfw1,dfw2,dfw3,dfw4)
datachi=data.matrix(aux1)
rownames(datachi) <- c('website1', 'website2','website3', 'website4')
colnames(datachi) <- c('class1', 'class2','class3')
#### websites vs classes

#datachi <- matrix(c(27,7,66), ncol=3, byrow=T)
testchi=chisq.test(datachi)
#X-squared = 35.468, df = 6, p-value = 3.497e-06 -> we reject Ho(independet) so that there is dependency between classes and websites(websites influence classes).


#visualize the residuals 
library(corrplot)

#plot residuals
corrplot(testchi$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchi$residuals^2/testchi$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

library(vcd) 
assocstats(datachi)

#### websites vs classes

######## classes per website aesthetics usability
uh1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")

ul1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")

ah1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")

al1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")

uh1 
uh2
uh3

ul1
ul2
ul3

ah1
ah2
ah3

al1
al2
al3

datachiu <- matrix(c(52,11,68,164,248,236), ncol=3, byrow=T)
datachia <- matrix(c(160,236,206,56,23,98), ncol=3, byrow=T)

datachiu <- matrix(c(uh1,uh2,uh3,ul1,ul2,ul3), ncol=3, byrow=T)

testchiu=chisq.test(datachiu)

#plot residuals
corrplot(testchiu$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchiu$residuals^2/testchiu$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachiu)


testchia=chisq.test(datachia)
#plot residuals
corrplot(testchia$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchia$residuals^2/testchia$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachia)

#########




############ chi square hierarchichal classification for women  ############

########### t-test


###########


############ hierarchichal classification for women >=27 ############


############### compute confidence intervals per emotion per class - women >=27 #############
library(rcompanion)
call<-sqldf("select class,w1.webSite,(w1.disgust),'disgust',(w1.anger),'anger',(w1.fear),'fear',(w1.sadness),'fear',(w1.happiness),'happiness',(w1.neutral),'neutral',(w1.contempt),'contempt' from dfhca as hca,w1 where  hca.obs=w1.obs  ")
#stat.desc(c1w1)

#View(call)

a<-groupwiseMean(disgust ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional =  TRUE)

a$emotion<-"disgust"




b<-groupwiseMean(anger ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

b$emotion<-"anger"

c<-groupwiseMean(fear ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

c$emotion<-"fear"


d<-groupwiseMean(sadness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

d$emotion<-"sadness"

e<-groupwiseMean(happiness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

e$emotion<-"happiness"

f<-groupwiseMean(neutral ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

f$emotion<-"neutral"

g<-groupwiseMean(contempt ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

g$emotion<-"contempt"

pc1gm<-groupwiseMean(pc1 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     traditional      = TRUE)
pc1gm$emotion<-"pc1"

pc2gm<-groupwiseMean(pc2 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     traditional      = TRUE)

pc2gm$emotion<-"pc2"

pc1all<-groupwiseMean(pc1 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      traditional      = TRUE)
pc1all$emotion<-"pc1"

pc2all<-groupwiseMean(pc2 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      traditional      = TRUE)
pc2all$emotion<-"pc2"

stat.desc(scores)
ciall<-rbind(a,b,c,d,e,f,g)
View(ciall)

ciall1<-sqldf("select * from ciall where class=1 and webSite='website1'")
ciall2<-sqldf("select * from ciall where class=1 and webSite='website2'")
ciall3<-sqldf("select * from ciall where class=1 and webSite='website3'")
ciall4<-sqldf("select * from ciall where class=1 and webSite='website4'")

c2all1<-sqldf("select * from ciall where class=2 and webSite='website1'")
c2all2<-sqldf("select * from ciall where class=2 and webSite='website2'")
c2all3<-sqldf("select * from ciall where class=2 and webSite='website3'")
c2all4<-sqldf("select * from ciall where class=2 and webSite='website4'")

c3all1<-sqldf("select * from ciall where class=3 and webSite='website1'")
c3all2<-sqldf("select * from ciall where class=3 and webSite='website2'")
c3all3<-sqldf("select * from ciall where class=3 and webSite='website3'")
c3all4<-sqldf("select * from ciall where class=3 and webSite='website4'")

#fear anger and disgst
#ciall1fad1<-sqldf("select * from ciall where class=1 and webSite='website1' and (emotion='fear' or emotion='anger' or emotion='disgust') ")
ciall1fad1<-sqldf("select * from ciall where class=1 and (emotion='fear') ")
ciall1fad2<-sqldf("select * from ciall where class=1 and (emotion='anger') ")
ciall1fad3<-sqldf("select * from ciall where class=1 and (emotion='disgust') ")
ciall1fad4<-sqldf("select * from ciall where class=1 and (emotion='happiness') ")
ciall1fad5<-sqldf("select * from ciall where class=1 and (emotion='sadness') ")
ciall1fad6<-sqldf("select * from ciall where class=1 and (emotion='neutral') ")
ciall1fad7<-sqldf("select * from ciall where class=1 and (emotion='contempt') ")

ciall2fad1<-sqldf("select * from ciall where class=2 and (emotion='fear') ")
ciall2fad2<-sqldf("select * from ciall where class=2 and (emotion='anger') ")
ciall2fad3<-sqldf("select * from ciall where class=2 and (emotion='disgust') ")
ciall2fad4<-sqldf("select * from ciall where class=2 and (emotion='happiness') ")
ciall2fad5<-sqldf("select * from ciall where class=2 and (emotion='sadness') ")
ciall2fad6<-sqldf("select * from ciall where class=2 and (emotion='neutral') ")
ciall2fad7<-sqldf("select * from ciall where class=2 and (emotion='contempt') ")

ciall3fad1<-sqldf("select * from ciall where class=3 and (emotion='fear') ")
ciall3fad2<-sqldf("select * from ciall where class=3 and (emotion='anger') ")
ciall3fad3<-sqldf("select * from ciall where class=3 and (emotion='disgust') ")
ciall3fad4<-sqldf("select * from ciall where class=3 and (emotion='happiness') ")
ciall3fad5<-sqldf("select * from ciall where class=3 and (emotion='sadness') ")
ciall3fad6<-sqldf("select * from ciall where class=3 and (emotion='neutral') ")
ciall3fad7<-sqldf("select * from ciall where class=3 and (emotion='contempt') ")


pc1c<-sqldf("select * from pc1gm where class=1  ")
pc2c<-sqldf("select * from pc1gm where class=2  ")
pc3c<-sqldf("select * from pc1gm where class=3  ")

pc2c1<-sqldf("select * from pc2gm where class=1  ")
pc2c2<-sqldf("select * from pc2gm where class=2  ")
pc2c3<-sqldf("select * from pc2gm where class=3  ")

pc1all<-sqldf("select * from pc1all ")
pc2all<-sqldf("select * from pc2all ")



require(ggplot2)
library(cowplot)


################ emotion per class per websites ##########

p1f<-ggplot(ciall1fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c1")
p1a<-ggplot(ciall1fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c1")
p1d<-ggplot(ciall1fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c1")
p1h<-ggplot(ciall1fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c1")
p1s<-ggplot(ciall1fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c1")
p1n<-ggplot(ciall1fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c1")
p1c<-ggplot(ciall1fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c1")

p2f<-ggplot(ciall2fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c2")
p2a<-ggplot(ciall2fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c2")
p2d<-ggplot(ciall2fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c2")
p2h<-ggplot(ciall2fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c2")
p2s<-ggplot(ciall2fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c2")
p2n<-ggplot(ciall2fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c2")
p2c<-ggplot(ciall2fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c2")

p3f<-ggplot(ciall3fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c3")
p3a<-ggplot(ciall3fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c3")
p3d<-ggplot(ciall3fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c3")
p3h<-ggplot(ciall3fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c3")
p3s<-ggplot(ciall3fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c3")
p3n<-ggplot(ciall3fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c3")
p3c<-ggplot(ciall3fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c3")


plot_grid(p1f,p1a,p1d,p1h,p1s,p1n,p1c, ncol = 4, nrow = 2)
plot_grid(p2f,p2a,p2d,p2h,p2s,p2n,p2c, ncol = 4, nrow = 2)
plot_grid(p3f,p3a,p3d,p3h,p3s,p3n,p3c, ncol = 4, nrow = 2)

################ emotion per class per websites ##########


p1<-ggplot(ciall1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w1")
p2<-ggplot(ciall2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w2") 
p3<-ggplot(ciall3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w3")
p4<-ggplot(ciall4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w4")

p5<-ggplot(c2all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w1")
p6<-ggplot(c2all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w2") 
p7<-ggplot(c2all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w3")
p8<-ggplot(c2all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w4")

p9<-ggplot(c3all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w1")
p10<-ggplot(c3all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w2") 
p11<-ggplot(c3all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w3")
p12<-ggplot(c3all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w4")

plot_grid(p1,p2,p3,p4, ncol = 2, nrow = 2)
plot_grid(p5,p6,p7,p8, ncol = 2, nrow = 2)
plot_grid(p9,p10,p11,p12, ncol = 2, nrow = 2)




ppc1c1<-ggplot(pc1c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c1")
ppc1c2<-ggplot(pc2c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c2")
ppc1c3<-ggplot(pc3c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c3")



ppc2c1<-ggplot(pc2c1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c1")
ppc2c2<-ggplot(pc2c2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c2")
ppc2c3<-ggplot(pc2c3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c3")

plot_grid(ppc1c1,ppc1c2,ppc1c3, ncol = 3, nrow = 1)
plot_grid(ppc2c1,ppc2c2,ppc2c3, ncol = 3, nrow = 1)


ppc1all<-ggplot(pc1all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites ")
ppc2all<-ggplot(pc2all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites ")


library(party)
dfhcaaux<-dfhca
dfhcaaux[dfhcaaux$class == 1]<-"class1"


fit <- ctree( age~class,data=dfhca,controls = ctree_control(maxdepth = 10))
fit1 <- ctree( class~age,data=dfhca,controls = ctree_control(maxdepth = 10))
plot(fit, main="Age-class tree ")
plot(fit1, main="Age-class tree ")




############### compute confidence intervals per emotion per class - women >= 27 #############

############### RCIS computation > 27 ##############

####### men rcis ###############

############### RCIS computation <= 27 MEN ##############
#                     age     genre      webSite              R1_NB5      R1_Mieux   
#2017-04-04 13:10:01: 348   Min.   :19   F:1393   Length:3376        Min.   :1.00   Atlas: 450  
#2017-03-14 13:46:48: 162   1st Qu.:23   H:1983   Class :character   1st Qu.:1.00   Kenzi:2926  
#2017-03-14 11:29:00: 155   Median :27            Mode  :character   Median :1.00               
#2017-03-14 10:44:59: 154   Mean   :31                               Mean   :1.52               
#2017-04-05 17:37:28: 129   3rd Qu.:32 
# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#remove outliers (heuristic far the data concentration)

summary(w1)
w1<-w1[-2492,]
w1<-w1[-1352,]
w1<-w1[-3320,]

w1<-w1[-3319,]
w1<-w1[-2595,]
#w1<-w1[-1541,]




w1<-subset(w1,genre=='H')
stat.desc(w1$age)
summary(w1$age)

count(w1)
w2<-subset(w1,webSite=='2')
w3<-subset(w1,webSite=='3')
w11<-subset(w1,webSite=='1')
w4<-subset(w1,webSite=='4')

#w1<-rbind(w2,w3)
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")

w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'

w1$agecategory[w1$age>=19 & w1$age<=27]<-'Youngers:19-27'
w1$agecategory[w1$age>=27 & w1$age<=32]<-'Young adults:27-32'
w1$agecategory[w1$age>32]<-'Olders:32-67'

## filter < 27
w1a <-sqldf("select * from w1 where age <= 27")
w1b <-sqldf("select * from w1 where age > 27")
count(w1a)
count(w1b)
w1<-w1a
count(w1)

#w1<-w1[-2492,]

stat.desc(w1$age)
agew<-data.frame(w1$userId,w1$age,w1$webSite)
agew1<-sqldf("select distinct * from agew") #21

#View(agew1)
#> stat.desc(w1$age)
#nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean      SE.mean CI.mean.0.95          var 
#1394.00         0.00         0.00        23.00        63.00        40.00     45403.00        27.00        32.57         0.33         0.64       150.07 
#std.dev     coef.var 
#12.25         0.38 

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
nrow(avgds)
ncol(avgds)

log.em <- log(avgds)
count(log.em)
em.periods <- w1$u

#https://www.researchgate.net/post/What_is_the_best_way_to_scale_parameters_before_running_a_Principal_Component_Analysis_PCA


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#scale #a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with S, but in general scaling is advisable. Alternatively, a vector of length equal the number of columns of x can be supplied. The value is passed to scale.
#scale for every variable : normalization (mean(x1) - x1) / standar deviation(x1)
#to see the used scale em.pca$center , #em.pca$scale
# it is called zVar <- (myVar - mean(myVar)) / sd(myVar)


sh=sd(log.em$w1.happiness)
sc=var(log.em$w1.contempt)
sa=var(log.em$w1.anger)
sd=var(log.em$w1.disgust)
sf=var(log.em$w1.fear)
ss=var(log.em$w1.sadness)
sn=var(log.em$w1.neutral)

msh=mean(log.em$w1.happiness)
msc=mean(log.em$w1.contempt)
msa=mean(log.em$w1.anger)
msd=mean(log.em$w1.disgust)
msf=mean(log.em$w1.fear)
mss=mean(log.em$w1.sadness)
msn=mean(log.em$w1.neutral)


#sd

em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

#em.pca$center
#em.pca$scale

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
em.pca$sdev

#CHECKING OUTLIERS - x=scores
x<-data.frame(em.pca$x)

#w1.happiness w1.contempt w1.anger w1.disgust w1.fear w1.sadness w1.neutral
#aux1=-7.6198*0.429 -5.74*0.380  -9.2*0.477 -7.7*0.488 -6.23*0.410 -1.11*0.194 -0.41616*0.027
#aux2<-data.frame(w1$happiness*0.33+
#w1$contempt*0.41+
#w1$anger*0.50+
#w1$disgust*0.57+
#w1$fear*0.23+
#w1$sadness*0.25+
#w1$neutral*-0.18,1)

par(mfrow=c(1,1))
summary(em.pca)

g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
############ contribution variable for women  ############

#inertia: https://pbil.univ-lyon1.fr/ade4/ade4-html/inertia.dudi.html

zebu.acp <-log.em
acp=dudi.pca(log.em)
acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)
#inertieC<-inertia.dudi(acp,row.inertia = TRUE)

#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100

inertieC$col.cum/100

#inertieC$row.abs/100

############ contribution variable for women  ############


############ hierarchichal classification for women <27 ############

library(questionr)
#1, compute distance of acp (#Distance du ??? Il s'agit de la distance utilis?e dans les analyses de correspondance multiples (ACM))
md <- dist.dudi(acp)

#2 Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. 
#Calcul du dendrogramme
#hclust: Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.

arbre <- hclust(md, method = "ward.D2")
plot(arbre, labels = FALSE, main = "Dendrogramme")

#summary(arbre)

#3 how many relevant classes(partitions)
inertie <- sort(arbre$height, decreasing = TRUE)
summary(inertie)

plot(inertie[1:20], type = "s", xlab = "Number de classes",ylab = "Inertie of dendogramme")


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 2(green), 3(red) ou 5(blue) classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 2, border = "green3")
rect.hclust(arbre, 3, border = "red3")
rect.hclust(arbre, 5, border = "blue3")



#let's get the best cut of the tree
library(devtools)
#install_github("larmarange/JLutils")

library(JLutils)
#Par d?faut, best.cutree regarde quelle serait la meilleure partition entre 3 et 20 classes.
#partition ayant la plus grande perte relative d'inertie.
#(to use the partition with the greatest relative loss of inertia.)

numberClasses=best.cutree(arbre)  # by default minimum=3
numberClasses
#numberClasses=3
best.cutree(arbre,min=2) #min number of partitions = 2

best.cutree(arbre, min = 2, graph = TRUE, xlab = "Nombre de classes", 
            ylab = "Inertie relative")

#we cut in 3 classes because the minimum best partition is 2 and the second best one is 3
typo <- cutree(arbre, 3)
freq(typo)
#summary(typo)
#plot(typo)

#graphical representation of scores(acp$li) in the new HCA 


library(RColorBrewer)
s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")
#s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = as.factor(typo), ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 3 classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 3, border = "red3")


#s.class(acp$li, as.factor(typo), 3, 4, col = brewer.pal(3,"Set1"), sub = "Axes 3 et 4")


############ chi square hierarchichal classification for women  ############
#change typo into data frame
dftypo<-data.frame(typo)
dftypo$obs <- seq.int(nrow(dftypo))
scoresw<-data.frame(em.pca$x)
dftypo$pc1<-scoresw$PC1
dftypo$pc2<-scoresw$PC2

#get emotions from pc1
dftypo$disgust<-w1$disgust
dftypo$anger<-w1$anger
dftypo$fear<-w1$fear
dftypo$sadness<-w1$sadness
dftypo$happiness<-w1$happiness
dftypo$neutral<-w1$neutral
dftypo$contempt<-w1$contempt
dftypo$age<-w1$age
dftypo$webSite<-w1$webSite

dfhca<-data.frame(dftypo$obs,dftypo$disgust,dftypo$anger,dftypo$fear,dftypo$sadness,dftypo$happiness,dftypo$neutral,dftypo$contempt,dftypo$pc1,dftypo$pc2,dftypo$typo,dftypo$age,dftypo$webSite)

names(dfhca)<-c('obs','disgust','anger','fear','sadness','h','n','c','pc1','pc2','class','age','webSite')

View(dfhca)
View(dftypo)
View(log.em)
library(sqldf)

c1<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=1")
c2<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=2")
c3<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=3")

call<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca")

c1all<-sqldf("select * from dfhca where class=1")
c2all<-sqldf("select * from dfhca where class=2")
c3all<-sqldf("select * from dfhca where class=3")

summary(c1all$age)
summary(c2all$age)
summary(c3all$age)

hist(c1all$age)
hist(c2all$age)
hist(c3all$age)

#prepare cross table w1
w1$obs<-seq.int(nrow(w1))
#w1$webSite
#422

tbw1c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website4'")

#create matrix
dfw1<-data.frame(tbw1c1,tbw1c2,tbw1c3)
dfw2<-data.frame(tbw2c1,tbw2c2,tbw2c3)
dfw3<-data.frame(tbw3c1,tbw3c2,tbw3c3)
dfw4<-data.frame(tbw4c1,tbw4c2,tbw4c3)

aux1<-rbind(dfw1,dfw2,dfw3,dfw4)
datachi=data.matrix(aux1)
rownames(datachi) <- c('website1', 'website2','website3', 'website4')
colnames(datachi) <- c('class1', 'class2','class3')
#### websites vs classes

#datachi <- matrix(c(27,7,66), ncol=3, byrow=T)
testchi=chisq.test(datachi)
#X-squared = 35.468, df = 6, p-value = 3.497e-06 -> we reject Ho(independet) so that there is dependency between classes and websites(websites influence classes).


#visualize the residuals 
library(corrplot)

#plot residuals
corrplot(testchi$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchi$residuals^2/testchi$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

library(vcd) 
assocstats(datachi)

#### websites vs classes

######## classes per website aesthetics usability
uh1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")

ul1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")

ah1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")

al1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")

uh1 
uh2
uh3

ul1
ul2
ul3

ah1
ah2
ah3

al1
al2
al3

datachiu <- matrix(c(52,11,68,164,248,236), ncol=3, byrow=T)
datachia <- matrix(c(160,236,206,56,23,98), ncol=3, byrow=T)

datachiu <- matrix(c(uh1,uh2,uh3,ul1,ul2,ul3), ncol=3, byrow=T)

testchiu=chisq.test(datachiu)

#plot residuals
corrplot(testchiu$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchiu$residuals^2/testchiu$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachiu)


testchia=chisq.test(datachia)
#plot residuals
corrplot(testchia$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchia$residuals^2/testchia$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachia)

#########




############ chi square hierarchichal classification for women  ############

########### t-test


###########


############ hierarchichal classification for women <27 ############

############### compute confidence intervals per emotion per class - women <27 #############
library(rcompanion)
call<-sqldf("select class,w1.webSite,(w1.disgust),'disgust',(w1.anger),'anger',(w1.fear),'fear',(w1.sadness),'fear',(w1.happiness),'happiness',(w1.neutral),'neutral',(w1.contempt),'contempt' from dfhca as hca,w1 where  hca.obs=w1.obs  ")
#stat.desc(c1w1)
a<-groupwiseMean(disgust ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)
a$emotion<-"disgust"




b<-groupwiseMean(anger ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

b$emotion<-"anger"

c<-groupwiseMean(fear ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

c$emotion<-"fear"


d<-groupwiseMean(sadness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

d$emotion<-"sadness"

e<-groupwiseMean(happiness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

e$emotion<-"happiness"

f<-groupwiseMean(neutral ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

f$emotion<-"neutral"

g<-groupwiseMean(contempt ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

g$emotion<-"contempt"

pc1gm<-groupwiseMean(pc1 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     traditional      = TRUE)
pc1gm$emotion<-"pc1"

pc2gm<-groupwiseMean(pc2 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     traditional      = TRUE)

pc2gm$emotion<-"pc2"

pc1all<-groupwiseMean(pc1 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      traditional      = TRUE)
pc1all$emotion<-"pc1"

pc2all<-groupwiseMean(pc2 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      traditional      = TRUE)
pc2all$emotion<-"pc2"

stat.desc(scores)

ciall<-rbind(a,b,c,d,e,f,g)
View(ciall)
ciall1<-sqldf("select * from ciall where class=1 and webSite='website1'")
ciall2<-sqldf("select * from ciall where class=1 and webSite='website2'")
ciall3<-sqldf("select * from ciall where class=1 and webSite='website3'")
ciall4<-sqldf("select * from ciall where class=1 and webSite='website4'")

c2all1<-sqldf("select * from ciall where class=2 and webSite='website1'")
c2all2<-sqldf("select * from ciall where class=2 and webSite='website2'")
c2all3<-sqldf("select * from ciall where class=2 and webSite='website3'")
c2all4<-sqldf("select * from ciall where class=2 and webSite='website4'")

c3all1<-sqldf("select * from ciall where class=3 and webSite='website1'")
c3all2<-sqldf("select * from ciall where class=3 and webSite='website2'")
c3all3<-sqldf("select * from ciall where class=3 and webSite='website3'")
c3all4<-sqldf("select * from ciall where class=3 and webSite='website4'")

#fear anger and disgst
#ciall1fad1<-sqldf("select * from ciall where class=1 and webSite='website1' and (emotion='fear' or emotion='anger' or emotion='disgust') ")
ciall1fad1<-sqldf("select * from ciall where class=1 and (emotion='fear') ")
ciall1fad2<-sqldf("select * from ciall where class=1 and (emotion='anger') ")
ciall1fad3<-sqldf("select * from ciall where class=1 and (emotion='disgust') ")
ciall1fad4<-sqldf("select * from ciall where class=1 and (emotion='happiness') ")
ciall1fad5<-sqldf("select * from ciall where class=1 and (emotion='sadness') ")
ciall1fad6<-sqldf("select * from ciall where class=1 and (emotion='neutral') ")
ciall1fad7<-sqldf("select * from ciall where class=1 and (emotion='contempt') ")

ciall2fad1<-sqldf("select * from ciall where class=2 and (emotion='fear') ")
ciall2fad2<-sqldf("select * from ciall where class=2 and (emotion='anger') ")
ciall2fad3<-sqldf("select * from ciall where class=2 and (emotion='disgust') ")
ciall2fad4<-sqldf("select * from ciall where class=2 and (emotion='happiness') ")
ciall2fad5<-sqldf("select * from ciall where class=2 and (emotion='sadness') ")
ciall2fad6<-sqldf("select * from ciall where class=2 and (emotion='neutral') ")
ciall2fad7<-sqldf("select * from ciall where class=2 and (emotion='contempt') ")

ciall3fad1<-sqldf("select * from ciall where class=3 and (emotion='fear') ")
ciall3fad2<-sqldf("select * from ciall where class=3 and (emotion='anger') ")
ciall3fad3<-sqldf("select * from ciall where class=3 and (emotion='disgust') ")
ciall3fad4<-sqldf("select * from ciall where class=3 and (emotion='happiness') ")
ciall3fad5<-sqldf("select * from ciall where class=3 and (emotion='sadness') ")
ciall3fad6<-sqldf("select * from ciall where class=3 and (emotion='neutral') ")
ciall3fad7<-sqldf("select * from ciall where class=3 and (emotion='contempt') ")


pc1c<-sqldf("select * from pc1gm where class=1  ")
pc2c<-sqldf("select * from pc1gm where class=2  ")
pc3c<-sqldf("select * from pc1gm where class=3  ")

pc2c1<-sqldf("select * from pc2gm where class=1  ")
pc2c2<-sqldf("select * from pc2gm where class=2  ")
pc2c3<-sqldf("select * from pc2gm where class=3  ")

pc1all<-sqldf("select * from pc1all ")
pc2all<-sqldf("select * from pc2all ")



require(ggplot2)
library(cowplot)


p1<-ggplot(ciall1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w1")
p2<-ggplot(ciall2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w2") 
p3<-ggplot(ciall3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w3")
p4<-ggplot(ciall4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w4")

p5<-ggplot(c2all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w1")
p6<-ggplot(c2all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w2") 
p7<-ggplot(c2all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w3")
p8<-ggplot(c2all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w4")

p9<-ggplot(c3all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w1")
p10<-ggplot(c3all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w2") 
p11<-ggplot(c3all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w3")
p12<-ggplot(c3all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w4")

plot_grid(p1,p2,p3,p4, ncol = 2, nrow = 2)
plot_grid(p5,p6,p7,p8, ncol = 2, nrow = 2)
plot_grid(p9,p10,p11,p12, ncol = 2, nrow = 2)


################ emotion per class per websites ##########

p1f<-ggplot(ciall1fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c1")
p1a<-ggplot(ciall1fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c1")
p1d<-ggplot(ciall1fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c1")
p1h<-ggplot(ciall1fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c1")
p1s<-ggplot(ciall1fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c1")
p1n<-ggplot(ciall1fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c1")
p1c<-ggplot(ciall1fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c1")

p2f<-ggplot(ciall2fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c2")
p2a<-ggplot(ciall2fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c2")
p2d<-ggplot(ciall2fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c2")
p2h<-ggplot(ciall2fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c2")
p2s<-ggplot(ciall2fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c2")
p2n<-ggplot(ciall2fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c2")
p2c<-ggplot(ciall2fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c2")

p3f<-ggplot(ciall3fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c3")
p3a<-ggplot(ciall3fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c3")
p3d<-ggplot(ciall3fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c3")
p3h<-ggplot(ciall3fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c3")
p3s<-ggplot(ciall3fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c3")
p3n<-ggplot(ciall3fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c3")
p3c<-ggplot(ciall3fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c3")


plot_grid(p1f,p1a,p1d,p1h,p1s,p1n,p1c, ncol = 4, nrow = 2)
plot_grid(p2f,p2a,p2d,p2h,p2s,p2n,p2c, ncol = 4, nrow = 2)
plot_grid(p3f,p3a,p3d,p3h,p3s,p3n,p3c, ncol = 4, nrow = 2)

################ emotion per class per websites ##########

ppc1c1<-ggplot(pc1c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c1")
ppc1c2<-ggplot(pc2c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c2")
ppc1c3<-ggplot(pc3c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c3")



ppc2c1<-ggplot(pc2c1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c1")
ppc2c2<-ggplot(pc2c2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c2")
ppc2c3<-ggplot(pc2c3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c3")

plot_grid(ppc1c1,ppc1c2,ppc1c3, ncol = 3, nrow = 1)
plot_grid(ppc2c1,ppc2c2,ppc2c3, ncol = 3, nrow = 1)


ppc1all<-ggplot(pc1all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites ")
ppc2all<-ggplot(pc2all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites ")


library(party)
dfhcaaux<-dfhca
dfhcaaux[dfhcaaux$class == 1]<-"class1"


fit <- ctree( age~class,data=dfhca,controls = ctree_control(maxdepth = 10))
fit1 <- ctree( class~age,data=dfhca,controls = ctree_control(maxdepth = 10))
plot(fit, main="Age-class tree ")
plot(fit1, main="Age-class tree ")




############### compute confidence intervals per emotion per class - men < 27 #############


############### RCIS computation <= 27 ##############

############### RCIS computation > 27 ##############
#                     age     genre      webSite              R1_NB5      R1_Mieux   
#2017-04-04 13:10:01: 348   Min.   :19   F:1393   Length:3376        Min.   :1.00   Atlas: 450  
#2017-03-14 13:46:48: 162   1st Qu.:23   H:1983   Class :character   1st Qu.:1.00   Kenzi:2926  
#2017-03-14 11:29:00: 155   Median :27            Mode  :character   Median :1.00               
#2017-03-14 10:44:59: 154   Mean   :31                               Mean   :1.52               
#2017-04-05 17:37:28: 129   3rd Qu.:32 

# log transform 
print('######### PCA individual ######################################################')
w1<-MyData

#get rid of two null rows (noisy data) ,2 users responded the Survey without interacting in the UI for period3 and period1
#28 28  115 period3      high        low          0         0      0        0     0        0        0     0          0         0      0        0     0        0        0     0     0       4
#37 37  182 period1      high       high ...
w1<-subset(w1,happiness!=0)
#remove outliers (heuristic far the data concentration)

summary(w1)
w1<-w1[-2492,]
w1<-w1[-1352,]
w1<-w1[-3320,]

w1<-w1[-3319,]
w1<-w1[-2595,]
#w1<-w1[-1541,]



w1<-subset(w1,genre=='H')

count(w1)
w2<-subset(w1,webSite=='2')
w3<-subset(w1,webSite=='3')
w11<-subset(w1,webSite=='1')
w4<-subset(w1,webSite=='4')

#w1<-rbind(w2,w3)
#w1<-sqldf("select * from w1 where webSite==3 || webSite==2")

w1$u[w1$webSite==1]<-'high'
w1$u[w1$webSite==2]<-'high'
w1$u[w1$webSite==3]<-'low'
w1$u[w1$webSite==4]<-'low'

w1$a[w1$webSite==1]<-'low'
w1$a[w1$webSite==2]<-'high'
w1$a[w1$webSite==3]<-'low'
w1$a[w1$webSite==4]<-'high'


w1$webSite[w1$webSite==1]<-'website1'
w1$webSite[w1$webSite==2]<-'website2'
w1$webSite[w1$webSite==3]<-'website3'
w1$webSite[w1$webSite==4]<-'website4'

w1$agecategory[w1$age>=19 & w1$age<=27]<-'Youngers:19-27'
w1$agecategory[w1$age>=27 & w1$age<=32]<-'Young adults:27-32'
w1$agecategory[w1$age>32]<-'Olders:32-67'

## filter < 27
w1a <-sqldf("select * from w1 where age <= 27")
w1b <-sqldf("select * from w1 where age > 27")
count(w1a)
count(w1b)
w1<-w1b
count(w1)

#w1<-w1[-2492,]

stat.desc(w1$age)
agew<-data.frame(w1$userId,w1$age,w1$webSite)
agew1<-sqldf("select distinct * from agew") #21

View(agew1)
#> stat.desc(w1$age)
#nbr.val     nbr.null       nbr.na          min          max        range          sum       median         mean      SE.mean CI.mean.0.95          var 
#1394.00         0.00         0.00        23.00        63.00        40.00     45403.00        27.00        32.57         0.33         0.64       150.07 
#std.dev     coef.var 
#12.25         0.38 

avgds<-data.frame( w1$happiness,w1$contempt,w1$anger,w1$disgust,w1$fear,w1$sadness,w1$neutral)
nrow(avgds)
ncol(avgds)

log.em <- log(avgds)
count(log.em)
em.periods <- w1$a

#https://www.researchgate.net/post/What_is_the_best_way_to_scale_parameters_before_running_a_Principal_Component_Analysis_PCA


# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
#scale #a logical value indicating whether the variables should be scaled to have unit variance before the analysis takes place. The default is FALSE for consistency with S, but in general scaling is advisable. Alternatively, a vector of length equal the number of columns of x can be supplied. The value is passed to scale.
#scale for every variable : normalization (mean(x1) - x1) / standar deviation(x1)
#to see the used scale em.pca$center , #em.pca$scale
# it is called zVar <- (myVar - mean(myVar)) / sd(myVar)


sh=sd(log.em$w1.happiness)
sc=var(log.em$w1.contempt)
sa=var(log.em$w1.anger)
sd=var(log.em$w1.disgust)
sf=var(log.em$w1.fear)
ss=var(log.em$w1.sadness)
sn=var(log.em$w1.neutral)

msh=mean(log.em$w1.happiness)
msc=mean(log.em$w1.contempt)
msa=mean(log.em$w1.anger)
msd=mean(log.em$w1.disgust)
msf=mean(log.em$w1.fear)
mss=mean(log.em$w1.sadness)
msn=mean(log.em$w1.neutral)


#sd

em.pca <- prcomp(log.em,
                 center = TRUE,
                 scale. = TRUE)

#em.pca$center
#em.pca$scale

# plot method
plot(em.pca, type = "l")
# The loadings (or eigenvectors) are simply our rotation values.
loadings <- em.pca$rotation
# summary method
em.pca$sdev

#CHECKING OUTLIERS - x=scores
x<-data.frame(em.pca$x)

#w1.happiness w1.contempt w1.anger w1.disgust w1.fear w1.sadness w1.neutral
#aux1=-7.6198*0.429 -5.74*0.380  -9.2*0.477 -7.7*0.488 -6.23*0.410 -1.11*0.194 -0.41616*0.027
#aux2<-data.frame(w1$happiness*0.33+
#w1$contempt*0.41+
#w1$anger*0.50+
#w1$disgust*0.57+
#w1$fear*0.23+
#w1$sadness*0.25+
#w1$neutral*-0.18,1)
#julixplot
summary(em.pca)

#dev.new()



g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = em.periods, ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


############ contribution variable for women  ############

#inertia: https://pbil.univ-lyon1.fr/ade4/ade4-html/inertia.dudi.html

zebu.acp <-log.em
acp=dudi.pca(log.em)
acp$eig

inertieC<-inertia.dudi(acp,col.inertia = TRUE)

#inertieC<-inertia.dudi(acp,row.inertia = TRUE)

#absolute contribution
inertieC$col.abs/100
#quality of the contribution
inertieC$col.rel/100

inertieC$col.cum/100
inertieC
#inertieC$row.abs/100

############ contribution variable for women  ############


############ hierarchichal classification for women >=27 ############

library(questionr)
#1, compute distance of acp (#Distance du ??? Il s'agit de la distance utilis?e dans les analyses de correspondance multiples (ACM))
md <- dist.dudi(acp)

#2 Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it. 
#Calcul du dendrogramme
#hclust: Hierarchical cluster analysis on a set of dissimilarities and methods for analyzing it.

arbre <- hclust(md, method = "ward.D2")
plot(arbre, labels = FALSE, main = "Dendrogramme")

#summary(arbre)

#3 how many relevant classes(partitions)
inertie <- sort(arbre$height, decreasing = TRUE)
summary(inertie)

plot(inertie[1:20], type = "s", xlab = "Number de classes",ylab = "Inertie of dendogramme")


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 2(green), 3(red) ou 5(blue) classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 2, border = "green3")
rect.hclust(arbre, 3, border = "red3")
rect.hclust(arbre, 5, border = "blue3")



#let's get the best cut of the tree
library(devtools)
#install_github("larmarange/JLutils")

library(JLutils)
#Par d?faut, best.cutree regarde quelle serait la meilleure partition entre 3 et 20 classes.
#partition ayant la plus grande perte relative d'inertie.
#(to use the partition with the greatest relative loss of inertia.)

numberClasses=best.cutree(arbre)  # by default minimum=3
numberClasses

#numberClasses=3
best.cutree(arbre,min=2) #min number of partitions = 2
best.cutree(arbre, min = 2, graph = TRUE, xlab = "Nombre de classes", 
            ylab = "Inertie relative")

#we cut in 3 classes because the minimum best partition is 2 and the second best one is 3
typo <- cutree(arbre, 3)
freq(typo)
#summary(typo)
#plot(typo)

#graphical representation of scores(acp$li) in the new HCA 

par(mfrow = c(1, 2))
library(RColorBrewer)
s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")
#s.class(acp$li, as.factor(typo), col = brewer.pal(3,"Set1"), sub = "pc1 et pc2")


g <- ggbiplot(em.pca, obs.scale = 1, var.scale = 1, 
              groups = as.factor(typo), ellipse = FALSE, 
              circle = TRUE)

g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)


#plot 2, 5, and 8 classes but how many relevant classes ? how many are most important ?
plot(arbre, labels = FALSE, main = "Partition en 3 classes", 
     xlab = "", ylab = "", sub = "", axes = FALSE, hang = -1)
rect.hclust(arbre, 3, border = "red3")


#s.class(acp$li, as.factor(typo), 3, 4, col = brewer.pal(3,"Set1"), sub = "Axes 3 et 4")


############ chi square hierarchichal classification for women  ############
#change typo into data frame
dftypo<-data.frame(typo)
dftypo$obs <- seq.int(nrow(dftypo))
scoresw<-data.frame(em.pca$x)
dftypo$pc1<-scoresw$PC1
dftypo$pc2<-scoresw$PC2

#get emotions from pc1
dftypo$disgust<-w1$disgust
dftypo$anger<-w1$anger
dftypo$fear<-w1$fear
dftypo$sadness<-w1$sadness
dftypo$happiness<-w1$happiness
dftypo$neutral<-w1$neutral
dftypo$contempt<-w1$contempt
dftypo$age<-w1$age
dftypo$webSite<-w1$webSite

dfhca<-data.frame(dftypo$obs,dftypo$disgust,dftypo$anger,dftypo$fear,dftypo$sadness,dftypo$happiness,dftypo$neutral,dftypo$contempt,dftypo$pc1,dftypo$pc2,dftypo$typo,dftypo$age,dftypo$webSite)

names(dfhca)<-c('obs','disgust','anger','fear','sadness','h','n','c','pc1','pc2','class','age','webSite')

View(dfhca)
View(dftypo)
View(log.em)
library(sqldf)

c1<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=1")
c2<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=2")
c3<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca where class=3")

call<-sqldf("select avg(disgust),avg(anger),avg(fear),avg(sadness),avg(h),avg(n),avg(c),stdev(disgust),stdev(anger),stdev(fear),stdev(sadness),stdev(h),stdev(n),stdev(c),variance(disgust),variance(anger),variance(fear),variance(sadness),variance(h),variance(n),variance(c) from dfhca")

c1all<-sqldf("select * from dfhca where class=1")
c2all<-sqldf("select * from dfhca where class=2")
c3all<-sqldf("select * from dfhca where class=3")

summary(c1all$age)
summary(c2all$age)
summary(c3all$age)

hist(c1all$age)
hist(c2all$age)
hist(c3all$age)

#prepare cross table w1
w1$obs<-seq.int(nrow(w1))
#w1$webSite
#422

tbw1c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and w1.webSite ='website4'")

tbw1c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website1'")
tbw2c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website2'")
tbw3c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website3'")
tbw4c3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and w1.webSite ='website4'")

#create matrix
dfw1<-data.frame(tbw1c1,tbw1c2,tbw1c3)
dfw2<-data.frame(tbw2c1,tbw2c2,tbw2c3)
dfw3<-data.frame(tbw3c1,tbw3c2,tbw3c3)
dfw4<-data.frame(tbw4c1,tbw4c2,tbw4c3)

aux1<-rbind(dfw1,dfw2,dfw3,dfw4)
datachi=data.matrix(aux1)
rownames(datachi) <- c('website1', 'website2','website3', 'website4')
colnames(datachi) <- c('class1', 'class2','class3')
#### websites vs classes

#datachi <- matrix(c(27,7,66), ncol=3, byrow=T)
testchi=chisq.test(datachi)
#X-squared = 35.468, df = 6, p-value = 3.497e-06 -> we reject Ho(independet) so that there is dependency between classes and websites(websites influence classes).


#visualize the residuals 
library(corrplot)

#plot residuals
corrplot(testchi$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchi$residuals^2/testchi$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

library(vcd) 
assocstats(datachi)

#### websites vs classes

######## classes per website aesthetics usability
uh1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")
uh3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website2')  ")

ul1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")
ul3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website3' or w1.webSite ='website4')  ")

ah1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")
ah3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website2' or w1.webSite ='website4')  ")

al1<-sqldf("select count(*) from dfhca as hca,w1 where class=1 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al2<-sqldf("select count(*) from dfhca as hca,w1 where class=2 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")
al3<-sqldf("select count(*) from dfhca as hca,w1 where class=3 and hca.obs=w1.obs and (w1.webSite ='website1' or w1.webSite ='website3')  ")

uh1 
uh2
uh3

ul1
ul2
ul3

ah1
ah2
ah3

al1
al2
al3

datachiu <- matrix(c(52,11,68,164,248,236), ncol=3, byrow=T)
datachia <- matrix(c(160,236,206,56,23,98), ncol=3, byrow=T)

datachiu <- matrix(c(uh1,uh2,uh3,ul1,ul2,ul3), ncol=3, byrow=T)

testchiu=chisq.test(datachiu)

#plot residuals
corrplot(testchiu$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchiu$residuals^2/testchiu$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachiu)


testchia=chisq.test(datachia)
#plot residuals
corrplot(testchia$residuals, is.cor = FALSE)
# Contibution in percentage (%)
contrib <- 100*testchia$residuals^2/testchia$statistic
round(contrib, 2)
# Visualize the contribution
corrplot(contrib, is.cor = FALSE)
assocstats(datachia)

#########




############ chi square hierarchichal classification for women  ############

########### t-test


###########


############ hierarchichal classification for women >=27 ############


############### compute confidence intervals per emotion per class - women >=27 #############
library(rcompanion)
call<-sqldf("select class,w1.webSite,(w1.disgust),'disgust',(w1.anger),'anger',(w1.fear),'fear',(w1.sadness),'fear',(w1.happiness),'happiness',(w1.neutral),'neutral',(w1.contempt),'contempt' from dfhca as hca,w1 where  hca.obs=w1.obs  ")
#stat.desc(c1w1)

#View(call)

a<-groupwiseMean(disgust ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional =  TRUE)

a$emotion<-"disgust"




b<-groupwiseMean(anger ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

b$emotion<-"anger"

c<-groupwiseMean(fear ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

c$emotion<-"fear"


d<-groupwiseMean(sadness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

d$emotion<-"sadness"

e<-groupwiseMean(happiness ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

e$emotion<-"happiness"

f<-groupwiseMean(neutral ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

f$emotion<-"neutral"

g<-groupwiseMean(contempt ~ class + webSite,
                 data = call,
                 conf = 0.95,
                 digits = 3,
                 traditional      = TRUE)

g$emotion<-"contempt"

pc1gm<-groupwiseMean(pc1 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     traditional      = TRUE)
pc1gm$emotion<-"pc1"

pc2gm<-groupwiseMean(pc2 ~ class + webSite,
                     data = dfhca,
                     conf = 0.95,
                     digits = 3,
                     traditional      = TRUE)

pc2gm$emotion<-"pc2"

pc1all<-groupwiseMean(pc1 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      traditional      = TRUE)
pc1all$emotion<-"pc1"

pc2all<-groupwiseMean(pc2 ~  webSite,
                      data = dfhca,
                      conf = 0.95,
                      digits = 3,
                      traditional      = TRUE)
pc2all$emotion<-"pc2"

stat.desc(scores)
ciall<-rbind(a,b,c,d,e,f,g)
View(ciall)

ciall1<-sqldf("select * from ciall where class=1 and webSite='website1'")
ciall2<-sqldf("select * from ciall where class=1 and webSite='website2'")
ciall3<-sqldf("select * from ciall where class=1 and webSite='website3'")
ciall4<-sqldf("select * from ciall where class=1 and webSite='website4'")

c2all1<-sqldf("select * from ciall where class=2 and webSite='website1'")
c2all2<-sqldf("select * from ciall where class=2 and webSite='website2'")
c2all3<-sqldf("select * from ciall where class=2 and webSite='website3'")
c2all4<-sqldf("select * from ciall where class=2 and webSite='website4'")

c3all1<-sqldf("select * from ciall where class=3 and webSite='website1'")
c3all2<-sqldf("select * from ciall where class=3 and webSite='website2'")
c3all3<-sqldf("select * from ciall where class=3 and webSite='website3'")
c3all4<-sqldf("select * from ciall where class=3 and webSite='website4'")

#fear anger and disgst
#ciall1fad1<-sqldf("select * from ciall where class=1 and webSite='website1' and (emotion='fear' or emotion='anger' or emotion='disgust') ")
ciall1fad1<-sqldf("select * from ciall where class=1 and (emotion='fear') ")
ciall1fad2<-sqldf("select * from ciall where class=1 and (emotion='anger') ")
ciall1fad3<-sqldf("select * from ciall where class=1 and (emotion='disgust') ")
ciall1fad4<-sqldf("select * from ciall where class=1 and (emotion='happiness') ")
ciall1fad5<-sqldf("select * from ciall where class=1 and (emotion='sadness') ")
ciall1fad6<-sqldf("select * from ciall where class=1 and (emotion='neutral') ")
ciall1fad7<-sqldf("select * from ciall where class=1 and (emotion='contempt') ")

ciall2fad1<-sqldf("select * from ciall where class=2 and (emotion='fear') ")
ciall2fad2<-sqldf("select * from ciall where class=2 and (emotion='anger') ")
ciall2fad3<-sqldf("select * from ciall where class=2 and (emotion='disgust') ")
ciall2fad4<-sqldf("select * from ciall where class=2 and (emotion='happiness') ")
ciall2fad5<-sqldf("select * from ciall where class=2 and (emotion='sadness') ")
ciall2fad6<-sqldf("select * from ciall where class=2 and (emotion='neutral') ")
ciall2fad7<-sqldf("select * from ciall where class=2 and (emotion='contempt') ")

ciall3fad1<-sqldf("select * from ciall where class=3 and (emotion='fear') ")
ciall3fad2<-sqldf("select * from ciall where class=3 and (emotion='anger') ")
ciall3fad3<-sqldf("select * from ciall where class=3 and (emotion='disgust') ")
ciall3fad4<-sqldf("select * from ciall where class=3 and (emotion='happiness') ")
ciall3fad5<-sqldf("select * from ciall where class=3 and (emotion='sadness') ")
ciall3fad6<-sqldf("select * from ciall where class=3 and (emotion='neutral') ")
ciall3fad7<-sqldf("select * from ciall where class=3 and (emotion='contempt') ")


pc1c<-sqldf("select * from pc1gm where class=1  ")
pc2c<-sqldf("select * from pc1gm where class=2  ")
pc3c<-sqldf("select * from pc1gm where class=3  ")

pc2c1<-sqldf("select * from pc2gm where class=1  ")
pc2c2<-sqldf("select * from pc2gm where class=2  ")
pc2c3<-sqldf("select * from pc2gm where class=3  ")

pc1all<-sqldf("select * from pc1all ")
pc2all<-sqldf("select * from pc2all ")



require(ggplot2)
library(cowplot)


################ emotion per class per websites ##########

p1f<-ggplot(ciall1fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c1")
p1a<-ggplot(ciall1fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c1")
p1d<-ggplot(ciall1fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c1")
p1h<-ggplot(ciall1fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c1")
p1s<-ggplot(ciall1fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c1")
p1n<-ggplot(ciall1fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c1")
p1c<-ggplot(ciall1fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c1")

p2f<-ggplot(ciall2fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c2")
p2a<-ggplot(ciall2fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c2")
p2d<-ggplot(ciall2fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c2")
p2h<-ggplot(ciall2fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c2")
p2s<-ggplot(ciall2fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c2")
p2n<-ggplot(ciall2fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c2")
p2c<-ggplot(ciall2fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c2")

p3f<-ggplot(ciall3fad1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("fear for all websites in c3")
p3a<-ggplot(ciall3fad2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("anger for all websites in c3")
p3d<-ggplot(ciall3fad3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("disgust for all websites in c3")
p3h<-ggplot(ciall3fad4, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("happiness for all websites in c3")
p3s<-ggplot(ciall3fad5, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("sadness for all websites in c3")
p3n<-ggplot(ciall3fad6, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("neutral for all websites in c3")
p3c<-ggplot(ciall3fad7, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Trad.upper , ymin =Trad.lower )) +ggtitle("contempt for all websites in c3")


plot_grid(p1f,p1a,p1d,p1h,p1s,p1n,p1c, ncol = 4, nrow = 2)
plot_grid(p2f,p2a,p2d,p2h,p2s,p2n,p2c, ncol = 4, nrow = 2)
plot_grid(p3f,p3a,p3d,p3h,p3s,p3n,p3c, ncol = 4, nrow = 2)

################ emotion per class per websites ##########


p1<-ggplot(ciall1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w1")
p2<-ggplot(ciall2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c1w2") 
p3<-ggplot(ciall3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w3")
p4<-ggplot(ciall4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c1w4")

p5<-ggplot(c2all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w1")
p6<-ggplot(c2all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c2w2") 
p7<-ggplot(c2all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w3")
p8<-ggplot(c2all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c2w4")

p9<-ggplot(c3all1, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w1")
p10<-ggplot(c3all2, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("c3w2") 
p11<-ggplot(c3all3, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w3")
p12<-ggplot(c3all4, aes(x = emotion, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower ))  +ggtitle("c3w4")

plot_grid(p1,p2,p3,p4, ncol = 2, nrow = 2)
plot_grid(p5,p6,p7,p8, ncol = 2, nrow = 2)
plot_grid(p9,p10,p11,p12, ncol = 2, nrow = 2)




ppc1c1<-ggplot(pc1c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c1")
ppc1c2<-ggplot(pc2c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c2")
ppc1c3<-ggplot(pc3c, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites in c3")



ppc2c1<-ggplot(pc2c1, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c1")
ppc2c2<-ggplot(pc2c2, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c2")
ppc2c3<-ggplot(pc2c3, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites in c3")

plot_grid(ppc1c1,ppc1c2,ppc1c3, ncol = 3, nrow = 1)
plot_grid(ppc2c1,ppc2c2,ppc2c3, ncol = 3, nrow = 1)


ppc1all<-ggplot(pc1all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc1 for all websites ")
ppc2all<-ggplot(pc2all, aes(x = webSite, y = Mean)) +  geom_point(size = 1) +  geom_errorbar(aes(ymax =Normal.upper , ymin =Normal.lower )) +ggtitle("pc2 for all websites ")

dev.set()
library(party)
dfhcaaux<-dfhca
dfhcaaux[dfhcaaux$class == 1]<-"class1"


fit <- ctree( age~class,data=dfhca,controls = ctree_control(maxdepth = 10))
fit1 <- ctree( class~age,data=dfhca,controls = ctree_control(maxdepth = 10))
plot(fit, main="Age-class tree ")
plot(fit1, main="Age-class tree ")




############### compute confidence intervals per emotion per class - women >= 27 #############

############### RCIS computation > 27 ##############


###### MEN rCIS ################










############### RCIS computation ##############
