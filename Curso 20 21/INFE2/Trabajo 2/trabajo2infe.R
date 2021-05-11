library(nortest)
set.seed(69)
n<-25
tamsim<-10000
alfa<-c(0.9,0.95,0.99,0.999)
M<-matrix(rexp(25*10000),25,10000)
T<-rep(0,10000)
for(i in 1:10000){
  T[i]<-ks.test(M[,i],pexp,1/mean(M[,i]))$statistic
}
valorestabla<-quantile(T,alfa)

y<-c(0.5088460,4.7784456,5.8640388,5.2947294,2.1367571,3.6835381,1.7578674,1.4463360,5.4000721,8.5328018,4.2067582,5.4932311,1.1580206,0.2325523,0.6011006,8.6225102,0.6864657,2.9142432,7.6363457,0.3445848,1.7555547,10.1100430,0.9012126,3.2912391,0.5992558)
#a)
y<-sort(y)
EMV<-1/mean(y)
dn<-ks.test(y,pexp,1/mean(y))$stat

#simulamos la fila de la tabla correspondiente a n=5
M<-matrix(rexp(25*10000),25,10000)
#Calculas el ks.test para cada col
cont<-0
T<-rep(0,10000)
for(i in 1:10000){
  T[i]<-ks.test(M[,i],pexp,1/mean(M[,i]))$statistic
  if(T[i]>dn){
    cont<-cont+1
  }
}
pvalor<-cont/10000
pvalor#no podemos rechazar que x provenga de una exponencial

n<-50
nm<-10000
crit.a <-valorestabla[3] # No se cuantos grados de libertad tenemos
Mp<-matrix(rgamma(n*nm,2,0.7),n,nm)
T<-rep(0,nm)
cont<-0
for(i in 1:nm){
  T[i]<-ks.test(Mp[,i],pexp)$statistic
  if(T[i]>crit.a)
    cont<-cont+1
}
potencia<-cont/nm
potencia