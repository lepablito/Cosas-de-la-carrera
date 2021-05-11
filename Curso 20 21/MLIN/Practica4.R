library(eftar)
library(car)

# Modelo utilizado
attach(sylvestris)
y <- log(volumen); x1 <- log(diametro); x2 <- log(altura)
syl0 <- lm(y ~ x1 +x2);syl0
summary(syl0)



# contraste H0: b1 - 2*b2 = 0
H3 <- c(0, 1, -2); H3
linearHypothesis(syl0, H3) # library(car
syl3 <- lm(y ~ I(2*x1+x2)); syl3
anova(syl3, syl0) # con funcion anova

R <- rbind(H3, cbind(diag(2),0))
R1 <- solve(R); R1
X <- model.matrix(syl0); X
XR <- X %*% R1; XR
syl31 <- lm(y ~ XR[,2:3])
lm(y ~ XR[, 3])
anova(syl31, syl0) # Con funcion anova

SSEs <- deviance(syl0); SSEs
dfEs <- df.residual(syl0); dfEs
SSEc <- deviance(syl3); SSEc
dfEc <- df.residual(syl3); dfEc


f <- ((SSEc - SSEs)/(dfEc-dfEs))/(SSEs/dfEs); f
dfEc-dfEs # se puede ver en la tabla anova el 1
# Calculamos el pvalor
pf(f, dfEc-dfEs, dfEs)
1-pf(f, dfEc-dfEs, dfEs) # probilidad de ser mayor ( estadistico de la F)
pf(f, dfEc-dfEs, dfEs, lower.tail = F) # mismo resultado 

################################################################

quesos

tapply(quesos$proteina, quesos$tipo, mean)

y <- quesos$proteina;y
f <- quesos$tipo; f

unclass(f)# ver lo que nos guardaba el factor

# levels(f) nos da los nombres de los factores "comercial" "oveja"     "vaca" 
# nlevels(f) nos da el numero de factores 3
diag(nlevels(f))

z <- diag(nlevels(f))[f,];z

que0 <- lm(y~ 0 + z);que0

tapply(y, f, mean)
model.matrix(que0)

plot(y~f)
anova(que0)

H0 <- cbind(1, -diag(2)); H0
linearHypothesis(que0, H0) # library(car

apply(z, 1, sum)# modelo con restricciones seria el modelo solo con termino independiente
que00 <- lm(y~1);que00
mean(y)# al ser solo en interceto nos da el mismo valor
anova(que00,que0)#contraste

#otra manera seria hacer las matrices 

R <- rbind(c(1,0,0),H0)
R1 <- solve(R); R1
X <- z %*% R1; X
que01 <- lm(y ~ 0 +X[,1]); que01
anova(que01, que0) # Con funcion anova

########################################################33

quesos$tipo
plot(proteina~tipo, data=quesos)

quesos.lm.1 <- lm(proteina~tipo-1, data=quesos)
model.matrix(quesos.lm.1) # para que ajuste con la matriz quitamos el intercepto




