rm(list=ls()) 

#---------------------
# Projet Robert Romain Thenot Damien 
# Student Performance Data Set : https://archive.ics.uci.edu/ml/datasets/Student+Performance
#---------------------
# install.packages("corrplot")
# install.packages("hydroGOF")
# install.packages("randomForest")

## LIBRARIES
library(corrplot)
library(hydroGOF)
library(randomForest)

d1=read.table("student-mat.csv",sep=";",header=TRUE,as.is = FALSE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d3)) # 382 students


#ici on a trois tableau le premier avec les eleves en math, le deuxieme avec les eleves en portugais et le dernier avec les eleves
# qui sont a la fois en portugais et en math. On fait donc les matrices de correlation pour savoir ce qui est "correler le plus au resultat
# final. Pour commencer on va uniquement travailler sur d1 et d2

d1[,1] = as.numeric(d1[,1])
d2[,1] = as.numeric(d2[,1])
#school GP = 1 , MS = 2
d1[,2] = as.numeric(d1[,2])
d2[,2] = as.numeric(d2[,2])
#sex F = 1, M =2 
d1[,4] = as.numeric(d1[,4])
d2[,4] = as.numeric(d2[,4])
#address R = 1, U = 2
d1[,5] = as.numeric(d1[,5])
d2[,5] = as.numeric(d2[,5])
#famsize GT3 = 1, LE3 = 1
d1[,6] = as.numeric(d1[,6])
d2[,6] = as.numeric(d2[,6])
#Pstatus A = 1, T=2
d1[,9] = as.numeric(d1[,9])
d2[,9] = as.numeric(d2[,9])
#Mjob at_home = 1, health = 2, other = 3, service = 4, teacher =5
d1[,10] = as.numeric(d1[,10])
d2[,10] = as.numeric(d2[,10])
#Fjob at_home = 1, health = 2, other = 3, service = 4, teacher =5
d1[,11] = as.numeric(d1[,11])
d2[,11] = as.numeric(d2[,11])
#reason course = 1, home = 2, other = 3, reputation = 4
d1[,12] = as.numeric(d1[,12])
d2[,12] = as.numeric(d2[,12])
#guardian father = 1, mother = 2, other = 3
d1[,16] = as.numeric(d1[,16])
d2[,16] = as.numeric(d2[,16])
#schoolsup no = 1, yes = 2
d1[,17] = as.numeric(d1[,17])
d2[,17] = as.numeric(d2[,17])
#famsup no = 1, yes = 2
d1[,18] = as.numeric(d1[,18])
d2[,18] = as.numeric(d2[,18])
#paid no = 1, yes = 2
d1[,19] = as.numeric(d1[,19])
d2[,19] = as.numeric(d2[,19])
#activities no = 1, yes = 2
d1[,20] = as.numeric(d1[,20])
d2[,20] = as.numeric(d2[,20])
#nusery no = 1, yes = 2
d1[,21] = as.numeric(d1[,21])
d2[,21] = as.numeric(d2[,21])
#higher no = 1, yes = 2
d1[,22] = as.numeric(d1[,22])
d2[,22] = as.numeric(d2[,22])
#internet no = 1, yes = 2
d1[,23] = as.numeric(d1[,23])
d2[,23] = as.numeric(d2[,23])
#romantic no = 1, yes = 2

mcord1 = cor(d1)
mcord2 = cor(d2)

corrplot(mcord2, type="upper", col=c("black", "white"),
         bg="lightblue")
#corrplot(mcord1, type="upper", col=c("black", "white"),
         #bg="lightblue")

yd2.G1 = d2[,31] 
yd2.G2 = d2[,32] 
yd2.G3 = d2[,33] 

xd2.G1 = cbind(d2[,0:30],d2[,32:33])
xd2.G2 = cbind(d2[,0:31],d2[,33])
names(xd2.G2)[32]<-"G3"
xd2.G3 = d2[,0:32]
#Matrice decouper pour les notes de chaques période en 90 % train et 10 % test
yd2.G1.train = yd2.G1[1:584]
yd2.G1.test = yd2.G1[585:649]
yd2.G2.train = yd2.G2[1:584]
yd2.G2.test = yd2.G2[585:649]
yd2.G3.train = yd2.G3[1:584]
yd2.G3.test = yd2.G3[585:649]

d2.G1.train = d2[1:584,]
d2.G1.test = d2[585:649,]
xd2.G1.train = xd2.G1[1:584,]
xd2.G1.test = xd2.G1[585:649,]
d2.G2.train = d2[1:584,]
d2.G2.test = d2[585:649,]
xd2.G2.train = xd2.G2[1:584,]
xd2.G2.test = xd2.G2[585:649,]
d2.G3.train = d2[1:584,]
d2.G3.test = d2[585:649,]
xd2.G3.train = xd2.G3[1:584,]
xd2.G3.test = xd2.G3[585:649,]

G1.lm = step(lm(G1~ .,d2.G1.train), direction='backward')


G2.lm = step(lm(G2~ .,d2.G2.train), direction='backward')


G3.lm = step(lm(G3~ .,d2.G3.train), direction='backward')

# summary(G1.lm)
# summary(G2.lm)
# summary(G3.lm)

#Modele AIC
G1.lm = step(lm(G1~ .,d2.G1.train), direction='backward')
G2.lm = step(lm(G2~ .,d2.G2.train), direction='backward')
G3.lm = step(lm(G3~ .,d2.G3.train), direction='backward')
#Modele en utilisant tout les attributs
G1.all.lm = lm(G1~ .,d2.G1.train)
G2.all.lm = lm(G2~ .,d2.G2.train)
G3.all.lm = lm(G3~ .,d2.G3.train)
#Modele basé sur la matrice de correlation
G1.cr.lm = update(G1.all.lm,.~.-school-sex-age-guardian-traveltime-failures-schoolsup-paid-romantic-freetime-goout-Dalc-Walc-health-absences)
G2.cr.lm = update(G2.all.lm,.~.-school-sex-age-guardian-traveltime-failures-schoolsup-paid-romantic-freetime-goout-Dalc-Walc-health-absences)
G3.cr.lm = update(G3.all.lm,.~.-school-sex-age-guardian-traveltime-failures-schoolsup-paid-romantic-freetime-goout-Dalc-Walc-health-absences)
#modèle randomforest
G1.rf.lm = randomForest(G1~., data=d2.G1.train, ntree=500, na.action = na.omit)
G2.rf.lm = randomForest(G2~., data=d2.G2.train, ntree=500, na.action = na.omit)
G3.rf.lm = randomForest(G3~., data=d2.G3.train, ntree=500, na.action = na.omit)

# predictionG1 = predict(G1.lm,xd2.G1.test,interval='prediction',level=0.95)
# predictionG2 = predict(G2.lm,xd2.G2.test,interval='prediction',level=0.95)
# predictionG3 = predict(G3.lm,xd2.G3.test,interval='prediction',level=0.95)

predictionAllAttributes = predict(G3.all.lm, xd2.G3.test, interval='prediction', level=0.95)
rmse(predictionAllAttributes[, "fit"], yd2.G3.test)

predictionCr = predict(G3.cr.lm, xd2.G3.test, interval='prediction', level=0.95)
rmse(predictionCr[, "fit"], yd2.G3.test)

predictionAIC = predict(G3.lm,xd2.G3.test,interval='prediction',level=0.95)
rmse(predictionAIC[, "fit"], yd2.G1.test)

predictionRF = predict(G3.rf.lm, xd2.G3.test, interval='prediction', level=0.95)
rmse(predictionRF, yd2.G3.test)

resultRMSERandomForest = {}
ntrees = c(250, 500, 1000, 1500, 2000)
for(ntree in ntrees){
  rf.mod = randomForest(G3~., data=d2.G3.train, ntree=ntree, na.action = na.omit)
  predictionRFNTree = predict(rf.mod, xd2.G3.test, interval='prediction', level=0.95)
  resultRMSERandomForest[ntree] = rmse(predictionRFNTree, yd2.G3.test)
}
plot(resultRMSERandomForest, ylab="RMSE", xlab="Number of trees")
