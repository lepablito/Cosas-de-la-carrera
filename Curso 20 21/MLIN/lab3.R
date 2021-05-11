#library(Rcmdr)
library(car)
library(eftar)

attach(sylvestris)
y<-log(volumen); x1<- log(diametro); x2<- log(altura)
sy10<-lm(y ~ x1 + x2); sy10 #Creamos y vemos el modelo
confidenceEllipse(sy10) #Elipse de confianza para beta1 y beta2
#help("confidenceEllipse")
#Hipotesis H0: b1=2 y b2=1
H1<- cbind(0,diag(2)) #[0 1 0
                      # 0 0 1]
h1<-c(2,1) #hipotesis
linearHypothesis(sy10,H1,h1)
#El valor que nos da es 2.451e-06 por lo que rechazamos H0
#help("linearHypothesis")
#Ahora hacemos H0: b1=b2=0
linearHypothesis(sy10,H1)
summary(sy10) #Tambien vale
sy12 <- lm(y ~ 1)
anova(sy12,sy10)
#Para quitar el termino independiente:
lm(y ~ 0+x1+x2)
lm(y ~ -1+x1+x2)

sy11 <- lm(y ~ 1, offset = 2 * x1 + x2)
anova(sy11,sy10)

#H0: b1-2*b2=0 (es una combinacion lineal)
#Luego hacemos [0 1 -2]*[b0;b1;b2]=0
#Resolviendo y=b0+2*b2*x1+b2*x2+Epsilon
#y=b0+b2*(2x1+x2)+Epsilon
H3 <-c(0,1,-2)
linearHypothesis(sy10,H3)#El lado derecho por defecto es 0
syl3<-lm(y ~ I(2*x1+x2)); #Como solo hay un regresor usamos la I
anova(syl3,sy10)

quesos
H1q<-diag(3) #Oveja, vaca,comercial
levels(quesos$tipo) #Sale ordenado
unclass(quesos$tipo)
diag(3)[quesos$tipo,]
#H0: mu1=mu2=m3
#O bien intentar H0:mu2=m3 o H0: mu1=(mu2+mu3)/2
#y=b1x1+b2x2+b3x3+Epsilon=mu1z1+mu2z2+m3z3+Epsilon

attach(vainas)
vaV1<-lm(anchura ~ longitud, subset=variedad=="V1")
vaV2<-lm(anchura ~ longitud, subset=variedad=="V2")
vaV3<-lm(anchura ~ longitud, subset=variedad=="V3")
