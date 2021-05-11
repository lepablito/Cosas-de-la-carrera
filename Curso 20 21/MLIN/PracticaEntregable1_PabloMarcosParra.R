#PRACTICA PROPUESTA 1 PABLO MARCOS PARRA
#=====================EJERCICIO 1=====================================
library(eftar)
attach(usair026)

#--APARTADO A--
mod1<-lm(y ~ ., data=usair026); mod1
summary(mod1)
mod2<-update(mod1,. ~ . - x6); mod2
summary(mod2) #En este caso no hay ninguno que sea no significativo

#--APARTADO B--
m0<-lm(y ~ 0)
m1<-lm(y ~ 1)
m2<-lm(y ~ x1 + x4 + x5)
m3<-lm(y ~ x1 + x3 + x5 + x2 + x3)
anova(m0,m1,m2,m3)
#De manera inversa:
m2<-lm(y ~ x2 + x3)
m3<-lm(y ~ x2 + x3 + x1 + x4 + x5)
anova(m0,m1,m2,m3)

#--APARTADO C--
datos<-cbind(usair026[26,]) #bindeamos la fila 26 (cincinnati) a la variable datos
predict(mod2,newdata = datos,interval="confidence",level = 0.99)


#=====================EJERCICIO 2=====================================
library(car)
attach(esparragos)

#--APARTADO A--
y<-fibra.sensorial*100/peso
esparragos<-cbind(esparragos,y)
plot(clase,y,col= c("red", "red4", "green","lawngreen","lightgreen"))
#PARA TODOS LOS APARTADOS A PARTIR DE AQUI
Z<-diag(nlevels(clase)) [clase,]
f<- clase
Z<-diag(nlevels(f))[f,]
modelo1 <- lm(y ~ Z-1,); 
modelo1

#--APARTADO B--
tapply(y,clase,mean)

#--APARTADO C--
H0 <-cbind(1,-diag(4));

R <- rbind(c(1,0,0,0,0),H0); R
R1 <- solve(R); R1
X <- Z %*% R1;
modeloA <- lm(y ~ 0 +X[,1]);
linearHypothesis(modelo1,H0)
anova(modeloA, modelo1)

#--APARTADO D--
H1 <-c(3,3,-2,-2,-2)

R <- rbind(H1, cbind(diag(4),0)); R
R1 <- solve(R); R1
X <- Z %*% R1;
modeloD <- lm(y ~ X[,-1])
linearHypothesis(modelo1,H1)
anova(modeloD, modelo1)

#--APARTADO E--
H2 <-c(1,-1,0,0,0)

R <- rbind(H2,c(3,3,-2,-2,-2),c(0,1,-1,-1,0),c(0,0,1,-1,-1),c(1,1,1,1,1))
R1 <- solve(R); R1
X <- Z %*% R1;
modeloE <- lm(y ~ X[,-1])
linearHypothesis(modelo1,H2)
anova(modeloE, modelo1)

#--APARTADO F--
H30<-c(0,0,1,-1,0) #mu3=mu4
H31<-c(0,0,0,1,-1) #mu4=mu5

R <- rbind(H30,H31,c(0,1,-1,-1,0),c(0,0,1,-1,-1),c(1,1,1,1,1));R
R1 <- solve(R); R1
X <- Z %*% R1;
modeloF0 <- lm(y ~ X[,-1])

linearHypothesis(modelo1,H30)
anova(modeloF0, modelo1)

modeloF1 <- lm(y ~ X[,-2]) #Reutilizamos la X del contraste anterior

linearHypothesis(modelo1,H31)
anova(modeloF1, modelo1)

#--APARTADO G--
H4<-c(0,0,1,-1/2,-1/2)

R <- rbind(H4,c(3,3,-2,-2,-2),c(0,1,-1,-1,0),c(0,0,0,1,-1),c(1,1,1,1,1))
R1 <- solve(R);
X <- Z %*%R1
modeloG<-lm(y ~ 0 + X[,-1])
linearHypothesis(modelo1,H4)
anova(modeloG, modelo1)

#--APARTADO H--
H5<-c(0,0,0,1,-1)
modeloH<-lm(y ~ 0 + X[,-4])
linearHypothesis(modelo1,H5)
anova(modeloH, modelo1)