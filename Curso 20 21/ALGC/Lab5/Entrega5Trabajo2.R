#TAREA 5 ALGORITMOS PABLO MARCOS PARRA
library(SyncRNG)
library(viridis)
library(RColorBrewer)
options(warn=-1)
#############################
#generaLaberinto sirve para crear el laberinto poniendo las habitaciones, paredes y puertas

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
        matriz[i,j]<--10
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
    matriz[row,col]<-sPesos$randi()%%9+1
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
###############################

dijsktraBidireccional<-function(matrizoriginal,rowsal,colsal,rowdes,coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  
  tammat<-dim(matrizoriginal)
  frontera1<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista frontera salida
  explorado1<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista explorado salida
  disttent1<-matrizoriginal

  frontera2<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista frontera destino
  explorado2<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista explorado destino
  disttent2<-matrizoriginal
  
  #Comenzamos metiendo en la matriz de pesos el punto de salida con valor 0 y dando un valor 0 a 9 (distinto de -2) al punto destino
  
  frontera1[rowsal,colsal]<-0 #Metemos la salida en la lista frontera salida
  frontera2[rowdes,coldes]<-0 #Metemos la salida en la lista frontera destino
  i<-1
  while(TRUE){
    if (i%%2==0){
      #Si la lista frontera está vacia, devolvemos no encontrado
      if(is.infinite(min(frontera1, na.rm = TRUE))){
        encontrado<-0
        break
      }
      #Cogemos el minimo de la lista frontera
      pos<-arrayInd(which.min(frontera1), dim(frontera1))
      #Guardamos la distancia tentativa del punto actual
      disttent1[pos[1],pos[2]]<-frontera1[pos[1],pos[2]]
      #Marcamos el punto como explorado
      explorado1[pos[1],pos[2]]<-0
      
      #Para los vecinos del punto que estamos explorando, si no es una pared y no esta explorado calculamos la distancia tentativa.
      #Si es la primera vez que la calculamos directamente metemos la distancia calculada, si no comprobamos si es menor que la distancia tentativa que
      #ya estaba
      if(is.na(explorado1[pos[1]+1,pos[2]]) & matrizoriginal[pos[1]+1,pos[2]]!=-10){
        disttent<-frontera1[pos[1],pos[2]]+matrizoriginal[pos[1]+1,pos[2]]
        if(!is.na(frontera1[pos[1]+1,pos[2]])){
          if (disttent<frontera1[pos[1]+1,pos[2]]){
            frontera1[pos[1]+1,pos[2]]<-disttent
          }
        }else{
          frontera1[pos[1]+1,pos[2]]<-disttent
        }
      }
      
      if(is.na(explorado1[pos[1]-1,pos[2]]) & matrizoriginal[pos[1]-1,pos[2]]!=-10){
        disttent<-frontera1[pos[1],pos[2]]+matrizoriginal[pos[1]-1,pos[2]]
        if(!is.na(frontera1[pos[1]-1,pos[2]])){
          if (disttent<frontera1[pos[1]-1,pos[2]]){
            frontera1[pos[1]-1,pos[2]]<-disttent
          }
        }else{
          frontera1[pos[1]-1,pos[2]]<-disttent
        }
      }
      
      if(is.na(explorado1[pos[1],pos[2]+1]) & matrizoriginal[pos[1],pos[2]+1]!=-10){
        disttent<-frontera1[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]+1]
        if(!is.na(frontera1[pos[1],pos[2]+1])){
          if (disttent<frontera1[pos[1],pos[2]+1]){
            frontera1[pos[1],pos[2]+1]<-disttent
          }
        }else{
          frontera1[pos[1],pos[2]+1]<-disttent
        }
      }
      
      if(is.na(explorado1[pos[1],pos[2]-1]) & matrizoriginal[pos[1],pos[2]-1]!=-10){
        disttent<-frontera1[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]-1]
        if(!is.na(frontera1[pos[1],pos[2]-1])){
          if (disttent<frontera1[pos[1],pos[2]-1]){
            frontera1[pos[1],pos[2]-1]<-disttent
          }
        }else{
          frontera1[pos[1],pos[2]-1]<-disttent
        }
      }
      frontera1[pos[1],pos[2]]<-NA #Sacamos el punto de la lista frontera
      if (!is.na(explorado1[pos[1],pos[2]]) & !is.na(explorado2[pos[1],pos[2]])){
        encontrado<-1
        puntoencuentro<-pos
        break
      }
      i<-i+1
    }
    
    
    else{
      #Si la lista frontera está vacia, devolvemos no encontrado
      if(is.infinite(min(frontera2, na.rm = TRUE))){
        encontrado<-0
        break
      }

      #Cogemos el minimo de la lista frontera
      pos<-arrayInd(which.min(frontera2), dim(frontera2))
      #Guardamos la distancia tentativa del punto actual
      disttent2[pos[1],pos[2]]<-frontera2[pos[1],pos[2]]
      #Marcamos el punto como explorado
      explorado2[pos[1],pos[2]]<-0
      
      #Para los vecinos del punto que estamos explorando, si no es una pared y no esta explorado calculamos la distancia tentativa.
      #Si es la primera vez que la calculamos directamente metemos la distancia calculada, si no comprobamos si es menor que la distancia tentativa que
      #ya estaba
      if(is.na(explorado2[pos[1]+1,pos[2]]) & matrizoriginal[pos[1]+1,pos[2]]!=-10){
        disttent<-frontera2[pos[1],pos[2]]+matrizoriginal[pos[1]+1,pos[2]]
        if(!is.na(frontera2[pos[1]+1,pos[2]])){
          if (disttent<frontera2[pos[1]+1,pos[2]]){
            frontera2[pos[1]+1,pos[2]]<-disttent
          }
        }else{
          frontera2[pos[1]+1,pos[2]]<-disttent
        }
      }
      
      if(is.na(explorado2[pos[1]-1,pos[2]]) & matrizoriginal[pos[1]-1,pos[2]]!=-10){
        disttent<-frontera2[pos[1],pos[2]]+matrizoriginal[pos[1]-1,pos[2]]
        if(!is.na(frontera2[pos[1]-1,pos[2]])){
          if (disttent<frontera2[pos[1]-1,pos[2]]){
            frontera2[pos[1]-1,pos[2]]<-disttent
          }
        }else{
          frontera2[pos[1]-1,pos[2]]<-disttent
        }
      }
      
      if(is.na(explorado2[pos[1],pos[2]+1]) & matrizoriginal[pos[1],pos[2]+1]!=-10){
        disttent<-frontera2[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]+1]
        if(!is.na(frontera2[pos[1],pos[2]+1])){
          if (disttent<frontera2[pos[1],pos[2]+1]){
            frontera2[pos[1],pos[2]+1]<-disttent
          }
        }else{
          frontera2[pos[1],pos[2]+1]<-disttent
        }
      }
      
      if(is.na(explorado2[pos[1],pos[2]-1]) & matrizoriginal[pos[1],pos[2]-1]!=-10){
        disttent<-frontera2[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]-1]
        if(!is.na(frontera2[pos[1],pos[2]-1])){
          if (disttent<frontera2[pos[1],pos[2]-1]){
            frontera2[pos[1],pos[2]-1]<-disttent
          }
        }else{
          frontera2[pos[1],pos[2]-1]<-disttent
        }
      }
      frontera2[pos[1],pos[2]]<-NA #Sacamos el punto de la lista frontera
      if (!is.na(explorado1[pos[1],pos[2]]) & !is.na(explorado2[pos[1],pos[2]])){
        encontrado<-1
        puntoencuentro<-pos
        break
      }
      i<-i+1
    }
  }
  #Si no esta explorado y no es pared, metemos un numero negativo para que luego se pinte bien
  for (i in 1:nrow(frontera1)){
    for (j in 1:ncol(frontera1)){
      if (is.na(explorado1[i,j])){
        disttent1[i,j]<--9
      }
    }
  }
  for (i in 1:nrow(frontera2)){
    for (j in 1:ncol(frontera2)){
      if (is.na(explorado2[i,j])){
        disttent2[i,j]<--9
      }
    }
  }
  for (i in 1:nrow(matrizoriginal)){
    for (j in 1:ncol(matrizoriginal)){
      if(disttent1[i,j]==-9 & disttent2[i,j]!=-9){
        matrizoriginal[i,j]<-disttent2[i,j]
      }
      else if (disttent1[i,j]!=-9 & disttent2[i,j]==-9){
        matrizoriginal[i,j]<-disttent1[i,j]
      }
      else if (disttent1[i,j]!=-9 & disttent2[i,j]!=-9){
        matrizoriginal[i,j]<-disttent1[i,j]+disttent2[i,j]
      }
      else if (matrizoriginal[i,j]!=-10){
        matrizoriginal[i,j]<--9
      }
    }
  }
  #Obtenemos la matriz de calor final
  
  lista<-list("matriz"=matrizoriginal, "burbuja1"=disttent1,"burbuja2"=disttent2, "encontrado"=encontrado, "rowcruce" =puntoencuentro[1], "colcruce"=puntoencuentro[2])
  return (lista)
}
###############################################################
caminoBi<-function(disttent1, disttent2, rowsal,colsal,rowdes,coldes,rowcruce,colcruce,encontrado){
  camino<-c()
  if(encontrado==1){
    pos<-c(rowcruce,colcruce)
    while (TRUE){
      points(pos[1],pos[2],col="cyan", pch=16,cex=0.5)
      #Si hemos encontrado el punto de llegada nos salimos del bucle
      if(pos[1]==rowsal & pos[2]==colsal){
        break
      }
      points(pos[1],pos[2],col="cyan", pch=16,cex=0.5)
      disttent1[pos[1],pos[2]]<--10
      minimos<-c(Inf,Inf,Inf,Inf) #Inicializamos una lista con los posibles candidatos
      #Para los vecinos del punto actual, si son positivos se toma su distancia
      if(disttent1[pos[1]+1,pos[2]]>-1){
        minimos[1]<-disttent1[pos[1]+1,pos[2]]
      }
      if(disttent1[pos[1]-1,pos[2]]>-1){
        minimos[2]<-disttent1[pos[1]-1,pos[2]]
      }
      if(disttent1[pos[1],pos[2]+1]>-1){
        minimos[3]<-disttent1[pos[1],pos[2]+1]
      }
      if(disttent1[pos[1],pos[2]-1]>-1){
        minimos[4]<-disttent1[pos[1],pos[2]-1]
      }
      
      elegido<-which.min(minimos) #Elegimos el de menor distancia
      
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
    pos<-c(rowcruce,colcruce)
    while (TRUE){
      points(pos[1],pos[2],col="cyan", pch=16,cex=0.5)
      #Si hemos encontrado el punto de llegada nos salimos del bucle
      if(pos[1]==rowdes & pos[2]==coldes){
        break
      }
      points(pos[1],pos[2],col="cyan", pch=16,cex=0.5)
      disttent2[pos[1],pos[2]]<--10
      minimos<-c(Inf,Inf,Inf,Inf) #Inicializamos una lista con los posibles candidatos
      #Para los vecinos del punto actual, si son positivos se toma su distancia
      if(disttent2[pos[1]+1,pos[2]]>-1){
        minimos[1]<-disttent2[pos[1]+1,pos[2]]
      }
      if(disttent2[pos[1]-1,pos[2]]>-1){
        minimos[2]<-disttent2[pos[1]-1,pos[2]]
      }
      if(disttent2[pos[1],pos[2]+1]>-1){
        minimos[3]<-disttent2[pos[1],pos[2]+1]
      }
      if(disttent2[pos[1],pos[2]-1]>-1){
        minimos[4]<-disttent2[pos[1],pos[2]-1]
      }
      
      elegido<-which.min(minimos) #Elegimos el de menor distancia
      
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
  #Si no hay camino posible
  else{
    camino<-c(rowdes,coldes)
  }
  return (camino)
}
#############################################################
lista<-generaLaberinto(100,1,420,4321)

colores<- hcl.colors(15, "Greens 2") [0:10] #Cogemos 10 colores para los 10 pesos 0 a 9
image(1:nrow(lista$matriz), 1:ncol(lista$matriz),lista$matriz,col=c( "black","red","turquoise1",colores))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)
#############################
#############################
t<-proc.time()
dijkstra<-dijsktraBidireccional(lista$matriz,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,999)
tiempobidireccional<-proc.time()-t
matrizdijkstra<-dijkstra$matriz

pal <- colorRampPalette(c("black", hcl.colors(100000, "plasma")), bias=1)
image(1:nrow(matrizdijkstra), 1:ncol(matrizdijkstra),matrizdijkstra, col=pal(length(unique(matrizdijkstra))))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)
######################################################
camino<-caminoBi(dijkstra$burbuja1, dijkstra$burbuja2, lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,dijkstra$rowcruce,dijkstra$colcruce,dijkstra$encontrado)
