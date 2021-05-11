#SEGUNDO LABORATORIO DE MODELOS LINEALES:
library(eftar)
names(thr)

#Dos maneras de representarlo
plot(thr)
plot(temp,h.rel,data=thr)
plot(h.rel~temp)

#modelo ajustado
thr.lm.1<-lm(h.rel~temp,thr)#primer modelo ajustado con lm
class(thr.lm.1)
name(thr.lm.1)

#Coeficientes del modelo
coef(thr.lm.1)

#Valores ajustados
fitted(thr.lm.1)

#Residuos
resid(thr.lm.1)

#Para recuperar la matriz de diseño
model.matrix(thr.lm.1)

#Recueperar exactamente los datos
model.frame(thr.lm.1)
formula(thr.lm.1)

#GRAFICOS
plot(h.rel~temp)
abline(thr.lm.1)

#Intervalo de confianza para los coeficientes 
confint(thr.lm.1)#con level=lo que sea cambiamos la confianza

#Resumen mas amplio del modelo
summary(thr.lm.1)
#Podemos observar la simetria de los errores: Mediana proxima a cero y cuantiles parecidos
#pvalor que nos da serai para este test: H0:betai=0 H1:betai!=0
#Residual standard error = estimador de sigma
#Adjusted R-squared -> R-cuadrado 

#La utilizaremos para hacer test de hipotesis
anova(thr.lm.1)#nos da el mismo valor que  summary si solo ponemos eso

#Ajustar el modelo
lm(h.rel~1,thr)#ajusta solo con el termino independiente
#otra manera de ajustarlo, partimos del modelo global que tenemos
#primero el modelo, luego la formula (.~.-temp todas las variables menos temp)
thr.lm.2<-update(thr.lm.1,.~.-temp)
#ahora utilizamos el anova
anova(thr.lm.1,thr.lm.2)#si te queda un valor negativo es que lo has metido al reves
plot(thr.lm.1)

####################################
#nuevo conjunto de datos,sylvestris
names(sylvestris)
#volumen=variable respuesta, diametro,altura=variables regresoras
pairs(sylvestris)#matriz de dispersion
#el diametro se ajusta de manera lineal pero la altura no parece lineal
#Cuanto mayor es la altura mas dispersos estan los puntos por lo tanto podemos 
#no cumplir la hipotesis de homocedasticidad

#Ajustamos el modelo con logaritmos
sylvestris.lm.1<-lm(log(volumen)~log(diametro)+log(altura))

#Intervalos de confianza
confint(sylvestris.lm.1,level=0.95)
#                   2.5 %    97.5 %
#(Intercept)   -9.5374928 -8.951712
#log(diametro)  1.6393735  1.851518
#log(altura)    0.8976626  1.127968
summary(sylvestris.lm.1)
anova(sylvestris.lm.1)

#Test sobre el termino independiente, es el test donde le quitamos el termino independiente
sylvestris.lm.2<-update(sylvestris.lm.1,.~.-1)
anova(sylvestris.lm.2,sylvestris.lm.1)#Cuando son transformaciones de otra funcion

#Test sobre una variable solo tengo que qutar esa variable
sylvestris.lm.3<-update(sylvestris.lm.1,.~.-log(diametro))
anova(sylvestris.lm.3,sylvestris.lm.1)
#rechazamos la hipotesis nula por lo tanto ese coeficiente es igual a 0

sylvestris.lm.4<-update(sylvestris.lm.1,.~.-log(altura))
anova(sylvestris.lm.3,sylvestris.lm.1)

#Modelo solo con el termino independiente
sylvestris.lm.5<-update(sylvestris.lm.1,.~.-.)
anova(sylvestris.lm.5,sylvestris.lm.1)

#Pvalores
anova(sylvestris.lm.5,sylvestris.lm.4,sylvestris.lm.1)
#Modelos que realmente ajustamos(FORMULA)
formula(sylvestris.lm.5)
formula(sylvestris.lm.4)
formula(sylvestris.lm.1)

###############################
quesos#nuevo conjunto de datos
plot(proteina~tipo,data=quesos)#Diagrama de cajas
quesos.lm.1<-lm(proteina~tipo,data=quesos)
model.matrix(quesos.lm.1)#la matriz no se ajusta al modelo con termino independiente
#quitandole el termino independiente
quesos.lm.2<-lm(proteina~tipo-1,data=quesos)
model.matrix(quesos.lm.2)
tapply(quesos$proteina,quesos$tipo,mean)
#¿Como se podrai hacer el tesr de igualdad de medias?