library(faraway)

nrow(gala)
gala

mo1 <- lm(Species ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)
lm(Species ~ . - Endemics, gaña) # mismo ajuste que el de arriba 

par(mfrow = c(2,3)); plot(mo1, which = 1:6)
# Heterogeneidad de varianzas, valores mas alejados entre si a valores mas grandes


# problema influencia con la isla de isabela, tiene la distancia de cook muy grande

plot(fitted(mo1), residuals(mo1), xlab = "Valores ajustados", ylab="Residuos")
abline(h=0, lty=2, col="red")
# ejemplo de construccion de grafico a mano

plot(fitted(mo1), rstandard(mo1), xlab = "Valores ajustados", ylab="Residuos")
abline(h=0, lty=2, col="red")
# lo mismo con residuos estandar
abline(h=c(-2,2), lty=2, col=2)

identify(fitted(mo1), rstandard(mo1))

gala[c(16, 19, 25), ]

# Grafico de normalidad
qqnorm(rstandard(mo1)); qqline(rstandard(mo1))

plot(cooks.distance(mo1),type="h", ylab="Distancias de Cook")

influence.measures(mo1)
help("influence.measures")

plot(fitted(mo1), residuals(mo1), xlab = "Valores ajustados", ylab="Residuos")
abline(h=0, lty=2, col="red")


# modelo 2 
mo2 <- lm(sqrt(Species) ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)
par(mfrow=c(2,3)); plot(mo2, which=1:6)
# Conteo -> ditribucion de poisson -> utilizar transformacion de la raiz cuadrada
# Estabiliza la varianza

library(MASS)
boxcox(mo1); abline(v=0.5, lty = 3)
boxcox(mo2)


bc <- boxcox(mo1, plot=F); bc
bc$x[which.max(bc$y)]

mo3 <- lm(Species^0.3 ~ Area + Elevation + Scruz + Nearest + Adjacent, gala)
par(mfrow=c(2,3)); plot(mo3, which=1:6)

gala[c(16, 19, 25), ]

nrow(gala)

z <- rep(0, 30); z[16] <- 1; z # apañar isasbela

mo4 <- lm(Species^0.3 ~ Area + Elevation + Scruz + Nearest + Adjacent + z, gala)
mo4
par(mfrow=c(2,3)); plot(mo4, which=1:6)

# Ejercicio 4

# Modelo ajustado
globwarm[1:10,]

mod <- lm(nhtemp ~ . - year, globwarm)

# residuos frente a year y residuos i+1 
par(mfrow=c(1,2))
with(globwarm, plot(residuals(mod) ~ year, na.omit(globwarm), type="b", ylab="Residuos"))
abline(h=0, lty=2)
n <- length(residuals(mod))
plot(tail(residuals(mod), n-1) ~ head(residuals(mod), n -1))
abline(lm(tail(residuals(mod), n-1) ~ head(residuals(mod), n-1)))

# Resumen de la recta de regresion
summary(lm(tail(residuals(mod), n-1) ~ head(residuals(mod), n -1)))

plot(residuals(mod)[-1] ~ residuals(mod)[-n], xlab="Residuos i", ylab="residuos")
abline(h=0,v=0, lty=2)

coagulation
y <- coagulation$coag; f <- coagulation$diet

m1 <- lm(y~ f); m1 

m1$contrasts
names(m1)

contr.treatment(levels(f))
solve(cbind(1, contr.treatment(levels(f))))

contr.sum(levels(f))
solve(cbind(1, contr.sum(levels(f))))

C <- contr.sum(levels(f))
solve(cbind(1, contr.sum(levels(f))))
solve(crossprod(C)) %*% t(C)


m1 <- lm(y~ f, contrasts=list(f=contr.sum)); m1 
# Ventaja:
dummy.coef(m1) # devuelve la media(el primero) y el resto los coeficientes diferenciales


