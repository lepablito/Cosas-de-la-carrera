#PRIMER LABORATORIO MLIN FORMULAS:

data()#datos que contiene el R
#Ver los datos de los paquetes
data(package="UVAejemplos")
data(package="eftar")

#package que vamos a usar:
#thr,sylvestris,quesos

#instalar los datos y paquetes que necesitamos INSTALARLO EN EL ORDENADOR
install.packages(c("eftar", "UVAejemplos"), contriburl="http://www.eio.uva.es/~eargu/R")
library(eftar)
thr #datos
names(thr)#para saber los nombres de las variables y el orden
nrow(thr)#numero de filas
thr$temp#acceder a la columna temp del cojunto de datos thr

#funcion search() lista cuales son los paquetes a los que tengo acceso
attach(thr)#añade la hoja de datos como si fuera una libreria
detach(thr) #quita la hoja de datos o libreria, si no le ponemos nada quita el primer elemento delpues del entorno global

sylvestris#otro conjunto de datos
n<-nrow(sylvestris);n

x1<-sylvestris$diametro
x2<-sylvestris$altura
#HACER LA MATRIZ Y
#si quieres convertir en una matriz la y
#matrix(y,ncol=1)
#otra manera as.matrix(y)
y<-cbind(sylvestris$volumen)

#HACER LA MATRIZ X
x<-cbind(1,x1,x2)#1 es el intercept 

#formula de la estimacion: beta =(X'X)^-1 X'sub(mu y|x)
solve(t(x)%*%x)%*%t(x)%*%y
beta<-solve(crossprod(x))%*%crossprod(x,y)#hace lo mismo que lo e arriba
x%*%beta #son los valores de Y que han tomado realmente

#Los residuos:
e<-y - x%*%beta
crossprod(e)#suma de cuadrados 


#FUNCIONES QUE UTILIZA EN R PARA HACER TODO LO ANTERIOR
X.<-cbind(x1,x2) #X[,-1] me quita la primera columna
a1<-lsfit(X.,y)#ajuste 1
lsfit(X.,y,intercept=F)

#funcion que utilizaremos normalmente
a2<-lm(y ~ x1 + x2)#forma de ajustar el modelo
unclass(a2)#para ver todo lo que saca la funcion lm
coef(a2)#vector de coeficientes
resid(a2)#residuos
model.matrix(a2)#matriz de diseño

#como deberiamos ponerlo
lm(volumen ~ diametro+altura,sylvestris)
lm(sylvestris)#coge la primera variable  como Y y el resto como regresores
lm(volumen ~ .,sylvestris)#el punto significa todas las variables

#si queremos ajustar solo modelo parciales
lm(volumen~diametro,sylvestris)

#ajustar un modelo sin el termino independiente:
lm(volumen ~diametro+altura -1,sylvestris)

#modelo solo con termino independiente
lm(volumen ~1,sylvestris)

quesos
unclass(quesos$tipo)
diag(3)#matriz identidad de tamaño 3*3
diag(3)[quesos$tipo,]
plot(proteina ~ tipo, data=quesos)
