library(car)
library(eftar)

attach(usair026)
summary(usair026)
#APARTADO A (ESTÁ HECHARDO ESTO)
usa0<-lm(y ~ x1 + x2 +x3 +x4+ x5 +x6); usa0 #Creamos y vemos el modelo
coef(usa0)
summary(usa0)
#QUITAMOS X6 QUE ES EL PVALOR MAS ALTO
usa01<-usa0<-lm(y ~ x1 + x2 +x3 +x4+ x5); usa01
summary(usa01)
coef(usa01)

#APARTADO B (???? NO ENTENDEMOS LO QUE NOS PIDE)
usa11 <- lm(y ~ x1+x4+x5+x6); usa11
usa12 <- lm(y ~ x2+x3); usa12
test1<-anova(usa11,usa0) # con funcion anova
test2<-anova(usa12,usa0)
test1
test2
#APARTADO C (HECHO)

datos<-cbind(usair026[26,])#Datos de Cincinnati
datos
predict(usa0,newdata = datos,interval = "predict", level = 0.99)#PREDICCION AL 99% CON INTERVALO DE PREDICCION (BASTANTE LOCO LO QUE DA LA WEA)

##################################################################
attach(esparragos)
#APARTADO A (HECHO)
esparragos<-cbind(esparragos,(fibra.sensorial/peso)*100)
names(esparragos)<-c("clase", "peso", "fibra.sensorial", "y")
f<-esparragos$clase
y<-esparragos$y
esparragos
plot(y ~ f)

#APARTADO B (HECHO)
Z<-diag(nlevels(f)) [f,]; Z
esp0<-lm(y ~ 0 + Z); esp0
tapply(y,f,mean)

#APARTADO C (HECHO)
h0 <-cbind(1,-diag(4)); h0
linearHypothesis(esp0, h0)
esp00 <- lm(y ~ 1); que00
mean(y)
anova

#APARTADO D (FALTA LO DE ANOVA)
h1<-c(1,1,-1,-1,-1); h1
linearHypothesis(esp0,h1)

#APARTADO E (ESTA MAL)
h2<-c(1,-1,0,0,0);h2
linearHypothesis(esp0,h2)
#APARTADO F
#APARTADO G
#APARTADO H
