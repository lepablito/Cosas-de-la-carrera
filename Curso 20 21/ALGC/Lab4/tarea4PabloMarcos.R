#TAREA 4 ALGORITMOS PABLO MARCOS PARRA
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
#algoritmoDijkstra aplica el algoritmo de dijkstra para llegar del punto salida al punto llegada

algoritmoDijkstra<-function(matrizoriginal, rowsal, colsal, rowdes, coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  tammat<-dim(matrizoriginal)
  #creamos matriz auxiliar donde vamos a ir metiendo la "cola"
  matrizalg<-matrix(nrow=tammat[1], ncol = tammat[2])
  #Si no son paredes metemos inicializamos el valor en infinito, si es pared mantenemos el NA
  for (i in 1:tammat[1]){
    for (j in 1:tammat[2]){
      if (matrizoriginal[i,j]!=-10){
        matrizalg[i,j]<-Inf
      }
    }
  }
  #Comenzamos metiendo el punto de salida con valor 0 y doy valor al peso del nodo destino
  matrizoriginal[rowdes,coldes]<-s$randi()%%9
  matrizoriginal[rowsal,colsal]<-0
  matrizalg[rowsal,colsal]<-0
  #COMIENZA EL ALGORITMO
  while (TRUE){
    pos<-arrayInd(which.min(matrizalg), dim(matrizalg))#Coge el valor minimo de las distancias tentativas
    #Ahora para todos los vecinos: si no es NA (corresponderia a una pared) calculamos la distancia tentativa del vecino sumando la distancia tentativa
    #del punto que estamos explorando + peso del vecino, si la distancia tentativa es menor que la que esta guardada (inicialmente es infinito) guardamos
    #la distancia tentativa obtenida
    
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
    
    #Si en la matriz de distancias tentativas el destino es NA termina el algoritmo y devuelve camino encontrado
    if(is.na(matrizalg[rowdes,coldes])){
      encontrado<-1
      break
    }
    #Si el minimo de las distancias tentativas es infinito, no se puede seguir buscando porque no hay más candidatos. Devolvemos no encontrado.
    if(is.infinite(min(matrizalg, na.rm = TRUE))){
      encontrado<-0
      break
    }
  }
  #Los nodos no explorados los guardamos con un valor negativo
  for (i in 1:nrow(matrizalg)){
    for (j in 1:ncol(matrizalg)){
      if (is.infinite(matrizalg[i,j]) | !is.na(matrizalg[i,j])){
        matrizoriginal[i,j]<--9
      }
    }
  }
  #marcamos de nuevo la salida
  matrizoriginal[rowsal,colsal]<-0
  lista<-list("matriz"=matrizoriginal, "encontrado"=encontrado) #Devolvemos la matriz obtenida de distancias y si hemos encontrado el destino o no
  return (lista)
}

############################
#conjuntofrontera aplica una version del algoritmo de dijkstra usando el conjunto frontera para llegar del punto salida al punto llegada

conjuntofrontera<-function(matrizoriginal,rowsal,colsal,rowdes,coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  
  tammat<-dim(matrizoriginal)
  matrizalg<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista frontera
  explorado<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista explorado
  
  #Comenzamos metiendo en la matriz de pesos el punto de salida con valor 0 y dando un valor 0 a 9 (distinto de -2) al punto destino
  matrizoriginal[rowdes,coldes]<-s$randi()%%9
  matrizoriginal[rowsal,colsal]<-0

  matrizalg[rowsal,colsal]<-0 #Metemos la salida en la lista frontera
  pos<-c(rowsal,colsal)
  while(TRUE){
    #Si la lista frontera está vacia, devolvemos no encontrado
    if(is.infinite(min(matrizalg, na.rm = TRUE))){
      encontrado<-0
      break
    }
    #Si el nodo actual es el de destino salimos del bucle y devolvemos encontrado
    if(pos[1]==rowdes & pos[2]==coldes){
      encontrado<-1
      break
    }
    #Cogemos el minimo de la lista frontera
    pos<-arrayInd(which.min(matrizalg), dim(matrizalg))
    #Guardamos la distancia tentativa del punto actual
    matrizoriginal[pos[1],pos[2]]<-matrizalg[pos[1],pos[2]]
    #Marcamos el punto como explorado
    explorado[pos[1],pos[2]]<-0
    
    #Para los vecinos del punto que estamos explorando, si no es una pared y no esta explorado calculamos la distancia tentativa.
    #Si es la primera vez que la calculamos directamente metemos la distancia calculada, si no comprobamos si es menor que la distancia tentativa que
    #ya estaba
    if(is.na(explorado[pos[1]+1,pos[2]]) & matrizoriginal[pos[1]+1,pos[2]]!=-10){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1]+1,pos[2]]
      if(!is.na(matrizalg[pos[1]+1,pos[2]])){
        if (disttent<matrizalg[pos[1]+1,pos[2]]){
          matrizalg[pos[1]+1,pos[2]]<-disttent
        }
      }else{
        matrizalg[pos[1]+1,pos[2]]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1]-1,pos[2]]) & matrizoriginal[pos[1]-1,pos[2]]!=-10){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1]-1,pos[2]]
      if(!is.na(matrizalg[pos[1]-1,pos[2]])){
        if (disttent<matrizalg[pos[1]-1,pos[2]]){
          matrizalg[pos[1]-1,pos[2]]<-disttent
        }
      }else{
        matrizalg[pos[1]-1,pos[2]]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1],pos[2]+1]) & matrizoriginal[pos[1],pos[2]+1]!=-10){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]+1]
      if(!is.na(matrizalg[pos[1],pos[2]+1])){
        if (disttent<matrizalg[pos[1],pos[2]+1]){
          matrizalg[pos[1],pos[2]+1]<-disttent
        }
      }else{
        matrizalg[pos[1],pos[2]+1]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1],pos[2]-1]) & matrizoriginal[pos[1],pos[2]-1]!=-10){
      disttent<-matrizalg[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]-1]
      if(!is.na(matrizalg[pos[1],pos[2]-1])){
        if (disttent<matrizalg[pos[1],pos[2]-1]){
          matrizalg[pos[1],pos[2]-1]<-disttent
        }
      }else{
        matrizalg[pos[1],pos[2]-1]<-disttent
      }
    }
    matrizalg[pos[1],pos[2]]<-NA #Sacamos el punto de la lista frontera
  }
  #Si no esta explorado y no es pared, metemos un numero negativo para que luego se pinte bien
  for (i in 1:nrow(matrizalg)){
    for (j in 1:ncol(matrizalg)){
      if (is.na(explorado[i,j]) & matrizoriginal[i,j]!=-10){
        matrizoriginal[i,j]<--9
      }
    }
  }
  matrizoriginal[rowsal,colsal]<-0
  lista<-list("matriz"=matrizoriginal, "encontrado"=encontrado)
  return (lista)
}
#################
#trazacamino sale del punto destino y los nodos con menor distancia hasta llegar al punto salida y devuelve un posible candidato a camino por dikjstra

trazacamino<-function(matriz,matrizpesos, rowsal,colsal,rowdes,coldes,encontrado){
  #Si existe un camino posible
  if(encontrado==1){
    matriz[rowsal,colsal]<-0
    pos<-c(rowdes,coldes)
    camino<-c(rowdes,coldes)#Inicializamos la lista de nodos del camino saliendo del destino
    while (TRUE){
      #Si hemos encontrado el punto de llegada nos salimos del bucle
      if(pos[1]==rowsal & pos[2]==colsal){
        break
      }
      points(pos[1],pos[2],col="cyan", pch=16,cex=0.5)
      matriz[pos[1],pos[2]]<--10
      minimos<-c(Inf,Inf,Inf,Inf) #Inicializamos una lista con los posibles candidatos
      #Para los vecinos del punto actual, si son positivos se toma su distancia
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
      
      menor<-min(minimos) #Elegimos el de menor distancia
      #ESTO SIRVE PARA EN CASO DE EMPATE ESCOGER UN CANDIDATO EN BASE A SU PESO
      pesomayor<-c(-3,-3,-3,-3)
      empates<-0
      if(minimos[1]==menor){
        pesomayor[1]<-matrizpesos[pos[1]+1,pos[2]]
        empates<-empates+1
      }
      if(minimos[2]==menor){
        pesomayor[2]<-matrizpesos[pos[1]-1,pos[2]]
        empates<-empates+1
      }
      if(minimos[3]==menor){
        pesomayor[3]<-matrizpesos[pos[1],pos[2]+1]
        empates<-empates+1
      }
      if(minimos[4]==menor){
        pesomayor[4]<-matrizpesos[pos[1],pos[2]-1]
        empates<-empates+1
      }
      elegido<-which.max(pesomayor) #Elegimos el que mayor peso
      #Guardamos en la lista del camino
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
#################

############################
lista<-generaLaberinto(100,1,1234,4321)

colores<- hcl.colors(15, "Greens 2") [0:10] #Cogemos 10 colores para los 10 pesos 0 a 9
image(1:nrow(lista$matriz), 1:ncol(lista$matriz),lista$matriz,col=c( "black","red","turquoise1",colores))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)
#############################
#############################
t<-proc.time()
dijkstra<-algoritmoDijkstra(lista$matriz,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,999)
tiemponormal<-proc.time()-t
matrizdijkstra<-dijkstra$matriz

pal <- colorRampPalette(c("black", hcl.colors(100000, "plasma")), bias=1)
image(1:nrow(matrizdijkstra), 1:ncol(matrizdijkstra),matrizdijkstra, col=pal(length(unique(matrizdijkstra))))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)
#############################
#############################
t<-proc.time()
dijkstrafrontera<-conjuntofrontera(lista$matriz,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,999)
tiempofrontera<-proc.time()-t
matrizdijkstrafrontera<-dijkstrafrontera$matriz

pal <- colorRampPalette(c("black", hcl.colors(100000, "plasma")), bias=1)
image(1:nrow(matrizdijkstrafrontera), 1:ncol(matrizdijkstrafrontera),matrizdijkstrafrontera, col=pal(length(unique(matrizdijkstrafrontera))))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)
############################
############################
camino<-trazacamino(matrizdijkstra,lista$matriz, lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,dijkstra$encontrado)

