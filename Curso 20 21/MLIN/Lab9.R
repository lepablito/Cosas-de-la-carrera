#leyendo archivo de los libros de la bibliografia
overdue<-read.table("C:/Users/pablm/Downloads/overdue.txt",header=TRUE);overdue

y<-overdue$LATE; y #Variable respuesta
x<-overdue$BILL; x

mod1<-lm(y~x)
plot(y~x)#Vemos si hay linealidad
plot(mod1)

#z<-rep(0,length(y));z
f<-rep("C",length(y));f
f[1:48]<-"R"; f
f<-factor(f); f

mod2<-lm(y~f*x); mod2
dummy.coef(mod2)
beta2<-coef(mod2); beta2

plot(y~x)
abline(beta2[c(1,3)])
abline(beta2[c(1,3)]+beta2[c(2,4)], lty=2)
plot(mod2)

model.matrix(mod2)

#contrastes de tratatamiento
C<-contr.treatment(levels(f)); C
M<- cbind(1,C); M

Z<-diag(2)[f,];Z
X0<-cbind(Z,Z*x);X0#matriz original usada en el modelo (matriz de partida)

lm(y~X0-1)
beta2
beta2[c(1,3)]+beta2[c(2,4)] #Esto sale porque la reparametrizacion del modelo que hace R
lm(y~x,subset= f=="C")
lm(y~x,subset= f=="R")#Los valores coinciden
#Este es un ejemplo de como hacer un modelo en el que vienen incluidas las regresiones de ambos grupos

C1<-cbind(c(-1,1));C1

