library(SyncRNG)
library(viridis)
library(RColorBrewer)
library(installr)
#GENERAMOS EL LABERINTO
generaLaberinto<-function(size, ratio,semilla,semillapesos){
  s<-SyncRNG(seed=semilla)
  sPesos<-SyncRNG(seed=semillapesos)
  mat_size<-(size*2)+1
  matriz<-matrix(ncol=mat_size, nrow=mat_size)
  
  #Rellena la habitación o pared
  for (i in 1:mat_size){
    for (j in 1:mat_size){
      if (i%%2==0 & j%%2==0){
        matriz[i,j]<-0
      }
      else{
        matriz[i,j]<-10
      }
    }
  }
  num_doors<-2*(size-1)*(size-1)
  open<-ratio*num_doors
  
  #Quita paredes
  for (i in 1:open){
    vert_horz<-s$randi()%%2   
    if  (vert_horz == 0){
      row<-s$randi()%%(size)
      col<-s$randi()%%(size-1)
      row<-row * 2 + 2
      col<-col * 2 + 3
    }
    else{
      row = s$randi()%%(size-1)
      col = s$randi()%%(size)
      row = row * 2 + 3
      col = col * 2 + 2
    }
    matriz[row,col]<-sPesos$randi()%%9
  }
  #Escoge y marca habitación de salida
  rowSal = s$randi()%%size
  colSal = s$randi()%%size
  matriz[rowSal*2+2,colSal*2+2]<--1
  
  #Escoge y marca habitación de destino
  rowDes = s$randi()%%size
  colDes = s$randi()%%size
  matriz[rowDes*2+2,colDes*2+2]<--2
  
  lista<-list("matriz"=matriz, "rowsal"=rowSal*2+2, "colsal"=colSal*2+2, "rowdes"=rowDes*2+2, "coldes"=colDes*2+2)
  return (lista)
}
#################
#APLICAMOS ALGORITMO DE DIJKSTRA SOBRE EL LABERINTO
algoritmoDijkstra<-function(matrizoriginal, rowsal, colsal, rowdes, coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  tammat<-dim(matrizoriginal)
  #creamos matriz auxiliar donde vamos a ir metiendo la "cola"
  matrizalg<-matrix(nrow=tammat[1], ncol = tammat[2])
  #Si no son paredes metemos inicializamos el valor en infinito, si es pared ya guardamos un peso alto
  for (i in 1:tammat[1]){
    for (j in 1:tammat[2]){
      if (matrizoriginal[i,j]!=10){
        matrizalg[i,j]<-Inf
      }else{
        matrizoriginal[i,j]<--10
      }
    }
  }
  #Comenzamos metiendo el punto de salida con valor 0
  matrizoriginal[rowdes,coldes]<-s$randi()%%9
  matrizoriginal[rowsal,colsal]<-0
  matrizalg[rowsal,colsal]<-0
  #COMIENZA EL ALGORITMO
  while (TRUE){
    pos<-arrayInd(which.min(matrizalg), dim(matrizalg))
  
    if(!is.na(matrizalg[pos[1]+1,pos[2]])){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1]+1,pos[2]]
      if (disttent<matrizalg[pos[1]+1,pos[2]]){
        matrizalg[pos[1]+1,pos[2]]<-disttent
      }
    }
    
    if(!is.na(matrizalg[pos[1]-1,pos[2]])){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1]-1,pos[2]]
      if (disttent<matrizalg[pos[1]-1,pos[2]]){
        matrizalg[pos[1]-1,pos[2]]<-disttent
      }
    }
    
    if(!is.na(matrizalg[pos[1],pos[2]+1])){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]+1]
      if (disttent<matrizalg[pos[1],pos[2]+1]){
        matrizalg[pos[1],pos[2]+1]<-disttent
      }
    }
    
    if(!is.na(matrizalg[pos[1],pos[2]-1])){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]-1]
      if (disttent<matrizalg[pos[1],pos[2]-1]){
        matrizalg[pos[1],pos[2]-1]<-disttent
      }
    }
    
    #Como esta visitado el nodo le metemos valor tentativo y marcamos como visitado
    matrizoriginal[pos[1],pos[2]]<-matrizalg[pos[1],pos[2]]
    matrizalg[pos[1],pos[2]]<-NA
    
    if(is.na(matrizalg[rowdes,coldes])){
      encontrado<-1
      break
    }
    if(is.infinite(min(matrizalg, na.rm = TRUE))){
      encontrado<-0
      break
    }
  }
  #Los nodos no visitados los guardamos con un valor negativo
  for (i in 1:nrow(matrizalg)){
    for (j in 1:ncol(matrizalg)){
      if (is.infinite(matrizalg[i,j]) | !is.na(matrizalg[i,j])){
        matrizoriginal[i,j]<--9
      }
    }
  }
  #marcamos de nuevo la salida y el destino
  matrizoriginal[rowsal,colsal]<-0
  lista<-list("matriz"=matrizoriginal, "encontrado"=encontrado)
  return (lista)
}
#################
trazacamino<-function(matriz,matrizpesos, rowsal,colsal,rowdes,coldes,encontrado){
  if(encontrado==1){
    matriz[rowsal,colsal]<-0
    pos<-c(rowdes,coldes)
    camino<-c(rowdes,coldes)
    while (TRUE){
      if(pos[1]==rowsal & pos[2]==colsal){
        break
      }
      matriz[pos[1],pos[2]]<--10
      minimos<-c(Inf,Inf,Inf,Inf)
      
      if(matriz[pos[1]+1,pos[2]]>-1){
        minimos[1]<-matriz[pos[1]+1,pos[2]]
      }
      if(matriz[pos[1]-1,pos[2]]>-1){
        minimos[2]<-matriz[pos[1]-1,pos[2]]
      }
      if(matriz[pos[1],pos[2]+1]>-1){
        minimos[3]<-matriz[pos[1],pos[2]+1]
      }
      if(matriz[pos[1],pos[2]-1]>-1){
        minimos[4]<-matriz[pos[1],pos[2]-1]
      }
      
      menor<-min(minimos)
      pesomayor<-c(-3,-3,-3,-3)
      if(minimos[1]==menor){
        pesomayor[1]<-matrizpesos[pos[1]+1,pos[2]]
      }
      if(minimos[2]==menor){
        pesomayor[2]<-matrizpesos[pos[1]-1,pos[2]]
      }
      if(minimos[3]==menor){
        pesomayor[3]<-matrizpesos[pos[1],pos[2]+1]
      }
      if(minimos[4]==menor){
        pesomayor[4]<-matrizpesos[pos[1],pos[2]-1]
      }
      
      elegido<-which.max(pesomayor)
      if (elegido==1){
        camino<-rbind(camino,c(pos[1]+1,pos[2]))
        pos<-c(pos[1]+1,pos[2])
      }
      else if (elegido==2){
        camino<-rbind(camino,c(pos[1]-1,pos[2]))
        pos<-c(pos[1]-1,pos[2])
      }
      else if (elegido==3){
        camino<-rbind(camino,c(pos[1],pos[2]+1))
        pos<-c(pos[1],pos[2]+1)
      }
      else{
        camino<-rbind(camino,c(pos[1],pos[2]-1))
        pos<-c(pos[1],pos[2]-1)
      }
    }
  }
  else{
    camino<-c(rowdes,coldes)
  }
  return (camino)
}
############################
conjuntofrontera<-function(matrizoriginal,rowsal,colsal,rowdes,coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  tammat<-dim(matrizoriginal)
  #creamos matriz auxiliar donde vamos a ir metiendo la "cola"
  matrizalg<-matrix(nrow=tammat[1], ncol = tammat[2])
  explorado<-matrix(nrow=tammat[1], ncol = tammat[2])
  matrizalg[rowsal,colsal]<-0
  while(TRUE){
    if(length(frontera)==0){
      encontrado<-0
      break
    }
    if(pos[1]==rowdes & pos[2]==coldes){
      encontrado<-1
      break
    }
    pos<-c(frontera[c(1,2)])
    frontera<-frontera[-c(1,2)]
    explorado[pos[1],pos[2]]<-0
    if(is.na(explorado[pos[1]+1,pos[2]]) & !is.na(matrizalg[pos[1]+1,pos[2]])){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1]+1,pos[2]]
      if (disttent<matrizalg[pos[1]+1,pos[2]]){
        matrizalg[pos[1]+1,pos[2]]<-disttent
      }
      frontera<-rbind(frontera,c(pos[1]+1,pos[2]))
    }
    
    if(!is.na(matrizalg[pos[1]-1,pos[2]])){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1]-1,pos[2]]
      if (disttent<matrizalg[pos[1]-1,pos[2]]){
        matrizalg[pos[1]-1,pos[2]]<-disttent
      }
    }
    
    if(!is.na(matrizalg[pos[1],pos[2]+1])){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]+1]
      if (disttent<matrizalg[pos[1],pos[2]+1]){
        matrizalg[pos[1],pos[2]+1]<-disttent
      }
    }
    
    if(!is.na(matrizalg[pos[1],pos[2]-1])){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]-1]
      if (disttent<matrizalg[pos[1],pos[2]-1]){
        matrizalg[pos[1],pos[2]-1]<-disttent
      }
    }
  }
  
}
#################
lista<-generaLaberinto(30,1,5432,6785)
colores<- hcl.colors(15, "Greens 2") [5:15] #Cogemos 10 colores para los 10 pesos 0 a 9
image(1:nrow(lista$matriz), 1:ncol(lista$matriz),lista$matriz,col=c("red","turquoise1",colores, "black"))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)
#############################
dijkstra<-algoritmoDijkstra(lista$matriz,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,999)
matrizdijkstra<-dijkstra$matriz
pal <- colorRampPalette(c("black", hcl.colors(100000, "plasma")), bias=1)
image(1:nrow(matrizdijkstra), 1:ncol(matrizdijkstra),matrizdijkstra, col=pal(length(unique(matrizdijkstra))))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)
############################
camino<-trazacamino(matrizdijkstra,lista$matriz, lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,dijkstra$encontrado)
if(dijkstra$encontrado==1){
  for (i in 1:nrow(camino)-1){
    points(camino[i,1],camino[i,2],col="cyan", pch=16,cex=0.5)
  }
}