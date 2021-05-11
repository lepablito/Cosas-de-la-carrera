library(car)
library(eftar)
library(MASS)

quesos

y<-quesos$proteina; y
f<-quesos$tipo; f

Z<-diag(nlevels(f)) [f,]; Z

que0<-lm(y ~ 0 + Z); que0
tapply(y,f,mean)
model.matrix(que0)

summary(que0)
anova(que0)

h0 <-cbind(1,-diag(2)); h0
linearHypothesis(que0, h0)
que00 <- lm(y ~ 1); que00
mean(y)
anova(que00,que0)

R<-rbind(c(1,0,0), h0); R
R1<- solve(R); R1
X<- Z%*%R1; X
que01<-lm(y ~ 0 + X[,1]);que01
anova(que01,que0)

h1<-rbind(c(2,-1,-1),c(0,1,-1)); h1
linearHypothesis(que0,h1[1,])
linearHypothesis(que0,h1[2,])
plot(y ~ f) #no hay simetria podemos ver que las varianzas no son iguales
##################contraste de la foto
R<-rbind(c(1,0,0), h0); R
R1<- solve(R); R1
X<- Z%*%R1; X

que01<-lm(y ~ 0 + X[,-1]); que01
anova(que01, que0)

que02<-lm(y ~ 0 + X[,-2] ); que02
anova(que02,que0)

que03<-lm(y ~ 0 + X[,-3]); que03
anova(que03, que0)

summary(lm(y ~ X[,-1]))# contraste b1=b2=0 da lo mismo que el test de igualdad de media.

##############################################
library(MASS)
fractions(R1) #fracciones para las matrices
############################################
vainas #libreria eftar

y<-vainas$anchura
x<-vainas$longitud
f<-vainas$variedad

vaV1<- lm(y~ x, subset = f=="V1"); vaV1
vaV2<- lm(y ~ x, subset = f=="V2"); vaV2
vaV3<- lm(y ~ x, subset = f=="V3"); vaV3
#con ggplot podemos representar estas regresiones (el señor no sabe hacerlo)

Z<-diag(nlevels(f)) [f,]; Z #matriz z para las tres variedades
#para obtener matriz con los valores en lugar de con unos
matrix(x,nrow = length(x),ncol = nlevels(f))
#producto elemento a elemento y unimos a la matriz Z
X<-cbind(Z,Z*matrix(x,nrow = length(x),ncol = nlevels(f)))

lm(y ~ 0 + X)#1 y 4 son de V1, 2 y 5 son de v2, 3 y 6 son de v3

