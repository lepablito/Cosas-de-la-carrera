library(eftar)
library(MASS)
y <- quesos$proteina
f <- quesos$tipo
mod0 <- lm(y ~ 0+f)
Z <- model.matrix(mod0)
mod1 <- lm(y ~ f)
model.matrix(mod1)
mod1$contrasts
C1 <- contr.treatment(levels(f))
M1 <- cbind(1, contr.treatment(levels(f)))
X <- Z%*%M

cbind(1, Z%*%C1)

solve(M1)

summary(mod1)
#pagina 79 mas o menos

f1 <- factor(f, levels=c("oveja", "vaca", "comercial"))

C2 <- contr.helmert(levels(f1))
M2 <- cbind(1, C2)
fractions(solve(M2))

mod2 <- lm(y ~ f1, contrasts=list(f1="contr.helmert"))
mod2$contrasts
model.matrix(mod2)
summary(mod2)
mod2 <- lm(y ~ f1, contrasts = list(f1=C2))
dummy.coef(mod2)