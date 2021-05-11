library(SyncRNG)
library(RColorBrewer)
library(reshape2)
library(viridis)

generaLaberinto<-function(size, ratio, semilla){
  s<-SyncRNG(seed=semilla)
  tammatriz<-size*2+1
  matriz<-matrix(nrow=tammatriz,ncol=tammatriz)
#Rellena con habitación o pared  
  for (i in 1:tammatriz){
    for (j in 1:tammatriz){
      if (i%%2==0 & j%%2==0){
        matriz[i,j]<-0
      }else{
        matriz[i,j]<-10
      }
    }
  }
  
  numpuertas<-2*(size-1)*(size-1)
  open<-ceiling(ratio*numpuertas)
  posicionesx<-sample(1:20, open, replace=TRUE)
  posicionesy<-sample(1:20, open, replace=TRUE)
  for (i in 1:open){
    x<-posicionesx[i]
    y<-posicionesy[i]
    matriz[x,y]<-0
  }
#Quita paredes
 " for (i in 1:open+1){
    verthorz<-s$rand()%%2
    if (verthorz==0){
      fila<-s$rand()%%size
      columna<-s$rand()%%(size-1)
      fila<-fila*2+2
      columna<-columna*2+3
    }else{
      fila<-s$rand()%%(size-1)
      columna<-s$rand()%%size
      fila<-fila*2+3
      columna<-columna*2+2
    }
    matriz[fila+1,columna+1]<-0
  }
  #Escoge una habitacion de salida
  filsalida<-s$rand()%%size
  colsalida<-s$rand()%%size
  matriz[filsalida*2+1,colsalida*2+1]<--1
  
  #Escoge habitación destino
  fildestino<-s$rand()%%size
  coldestino<-s$rand()%%size
  matriz[fildestino*2+1,coldestino*2+1]<--2
"
  return (matriz)
}
n<-100
matriz<-generaLaberinto(10,0.5,420)
image(matriz)
