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
##########################################################################
##########################################################################
#conjuntofrontera aplica una version del algoritmo de dijkstra usando el conjunto frontera para llegar del punto salida al punto llegada

conjuntofrontera<-function(matrizoriginal,rowsal,colsal,rowdes,coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  
  tammat<-dim(matrizoriginal)
  matrizalg<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista frontera
  explorado<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista explorado
  
  #Comenzamos metiendo en la matriz de pesos el punto de salida con valor 0 y dando un valor 0 a 9 (distinto de -2) al punto destino
  matrizoriginal[rowdes,coldes]<-s$randi()%%9+1
  matrizoriginal[rowsal,colsal]<-0
  
  matrizalg[rowsal,colsal]<-0 #Metemos la salida en la lista frontera
  pos<-c(rowsal,colsal)
  puntosexplorados<-0 #Vamos a contar el numero de puntos que se exploran
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
    puntosexplorados<-puntosexplorados+1
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
  lista<-list("matriz"=matrizoriginal, "encontrado"=encontrado, "npuntos"=puntosexplorados)
  return (lista)
}
################################################################################
################################################################################
#trazacamino sale del punto destino y los nodos con menor distancia hasta llegar al punto salida y devuelve un posible candidato a camino por dikjstra

trazacamino<-function(matriz, rowsal,colsal,rowdes,coldes,encontrado){
  #Si existe un camino posible
  if(encontrado==1){
    matriz[rowsal,colsal]<-0
    pos<-c(rowdes,coldes)
    while (TRUE){
      #Si hemos encontrado el punto de llegada nos salimos del bucle
      if(pos[1]==rowsal & pos[2]==colsal){
        break
      }
      points(pos[1],pos[2],col="cyan", pch=16,cex=0.5) #Pintamos el punto que forma parte del camino
      matriz[pos[1],pos[2]]<--10 #Lo marcamos como "visitado"
      minimos<-c(Inf,Inf,Inf,Inf) #Inicializamos una lista con los posibles candidatos
      #Para los vecinos del punto actual, si son positivos se toma su distancia tentativa
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
      #Esta parte nos permitira hallar todos los caminos posibles
      elegido<-min(minimos) #Obtenemos la distancia tentativa minima
      #Vamos a contar el numero de empates por si acaso de que halla dos caminos o mas posibles
      numempates<-0
      empates<-c()
      if (minimos[1]==elegido){
        numempates<-numempates+1
        empates<-c(empates,pos[1]+1,pos[2])
      }
      if (minimos[2]==elegido){
        numempates<-numempates+1
        empates<-c(empates,pos[1]-1,pos[2])
      }
      if (minimos[3]==elegido){
        numempates<-numempates+1
        empates<-c(empates,pos[1],pos[2]+1)
      }
      if (minimos[4]==elegido){
        numempates<-numempates+1
        empates<-c(empates,pos[1],pos[2]-1)
      }
      #Si hemos obtenido solo un minimo entonces ese es el unico candidato posible para el camino
      if(numempates==1){
        pos<-c(empates[1],empates[2])
      }
      #Si hay mas de una opcion llamamos recursivamente a la funcion para poder pintar todos los caminos
      #Para el ultimo elemento de la lista lo guardamos en la variable pos para que en la proxima vuelta del bucle sea el que se explore
      else{
        while(length(empates)!=2){
          pos<-c(empates[1],empates[2])
          empates<-empates[-c(1,2)]
          trazacamino(matriz, rowsal,colsal,pos[1],pos[2],1)
        }
        pos<-c(empates[1],empates[2])
      }
    }
  }
}
################################################################################
################################################################################
#dijkstraBidireccional aplica una version del algoritmo de dijkstra usando el conjunto frontera usando como salidas ambos puntos (destino y salida)
dijkstraBidireccional<-function(matrizoriginal,rowsal,colsal,rowdes,coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  
  tammat<-dim(matrizoriginal)
  frontera1<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista frontera salida
  explorado1<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista explorado salida
  disttent1<-matrizoriginal#Guardaremos las distancias tentativas para esta "burbuja"
  
  frontera2<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista frontera destino
  explorado2<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista explorado destino
  disttent2<-matrizoriginal#Guardaremos las distancias tentativas para esta "burbuja"
  
  #Comenzamos metiendo en la matriz de pesos el punto de salida con valor 0 y dando un valor 0 a 9 (distinto de -2) al punto destino
  
  frontera1[rowsal,colsal]<-0 #Metemos la salida en la lista frontera salida
  frontera2[rowdes,coldes]<-0 #Metemos la salida en la lista frontera destino
  puntosexplorados<-0 #Vamos a contar en numero de puntos que exploramos
  i<-1
  #En el bucle haremos dijkstra frontera para ambas listas, entrando de manera alternativa en ambas listas
  while(TRUE){
    if (i%%2==0){
      #Si la lista frontera está vacia, devolvemos no encontrado
      if(is.infinite(min(frontera1, na.rm = TRUE))){
        puntoencuentro<-c(NA,NA)
        encontrado<-0
        break
      }
      puntosexplorados<-puntosexplorados+1
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
      #Si hemos encontrado un punto visitado en ambas listas salimos del bucle
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
        puntoencuentro<-c(NA,NA)
        encontrado<-0
        break
      }
      puntosexplorados<-puntosexplorados+1
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
      #Si hemos encontrado un punto visitado en ambas listas salimos del bucle
      if (!is.na(explorado1[pos[1],pos[2]]) & !is.na(explorado2[pos[1],pos[2]])){
        encontrado<-1
        puntoencuentro<-pos
        break
      }
      i<-i+1
    }
  }
  #ESTOS DOS BUCLES SERVIRAN PARA LUEGO PINTAR EL CAMINO
  for (i in 1:nrow(disttent1)){
    for (j in 1:ncol(disttent1)){
      if (is.na(explorado1[i,j])){
        disttent1[i,j]<--9
      }
    }
  }
  for (i in 1:nrow(disttent2)){
    for (j in 1:ncol(disttent2)){
      if (is.na(explorado2[i,j])){
        disttent2[i,j]<--9
      }
    }
  }
  #Obtenemos la matriz de calor final
  for (i in 1:nrow(matrizoriginal)){
    for (j in 1:ncol(matrizoriginal)){
      if(!is.na(explorado1[i,j]) & is.na(explorado2[i,j])){
        matrizoriginal[i,j]<-disttent1[i,j]
      }
      else if(is.na(explorado1[i,j]) & !is.na(explorado2[i,j])){
        matrizoriginal[i,j]<-disttent2[i,j]
      }
      else if(is.na(explorado1[i,j]) & is.na(explorado2[i,j]) & matrizoriginal[i,j]!=-10){
        matrizoriginal[i,j]<--9
      }
      else if(!is.na(explorado1[i,j]) & !is.na(explorado2[i,j])){
        matrizoriginal[i,j]<-disttent1[i,j]+disttent2[i,j]
      }
    }
  }
  lista<-list("matriz"=matrizoriginal, "burbuja1"=disttent1,"burbuja2"=disttent2, "encontrado"=encontrado, 
              "rowcruce" =puntoencuentro[1], "colcruce"=puntoencuentro[2], "npuntos"=puntosexplorados)
  return (lista)
}
################################################################################
################################################################################
#caminoBi pinta el camino en el caso dijkstra bidireccional
caminoBi<-function(disttent1, disttent2, rowsal,colsal,rowdes,coldes,rowcruce,colcruce,encontrado){
  #Llamamos trazacamino para pintar el camino primero desde el punto de encuentro hasta la salida y luego desde el punto de encuentro hasta el destino
  trazacamino(disttent1, rowsal,colsal,rowcruce,colcruce,encontrado)
  trazacamino(disttent2, rowdes,coldes,rowcruce,colcruce,encontrado)
}
################################################################################
################################################################################
#algoritmoaestrellav1 aplica la version optimizada con la heuristica A* del algoritmo de dijkstra usando el conjunto frontera para llegar del punto salida al punto llegada
#En esta version calculamos "on the fly" (cada vez que lo necesitamos) la distancia manhattan
algoritmoaestrellav1<-function(matrizoriginal,rowsal,colsal,rowdes,coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  
  tammat<-dim(matrizoriginal)
  matrizalg<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista frontera
  explorado<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista explorado
  matrizdist<-matrix(nrow=tammat[1], ncol = tammat[2])#Lista con las distancias tentativas sin distancia manhattan para luego poder hacer el mapa de calor
  
  #Comenzamos metiendo en la matriz de pesos el punto de salida con valor 0 y dando un valor 0 a 9 (distinto de -2) al punto destino
  matrizoriginal[rowdes,coldes]<-s$randi()%%9+1
  matrizoriginal[rowsal,colsal]<-0
  puntosexplorados<-0 #Vamos a contar el numero de puntos que exploramos
  matrizalg[rowsal,colsal]<-0 #Metemos la salida en la lista frontera
  matrizdist[rowsal,colsal]<-0#Guardamos la distancia tentativa del punto inicial
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
    puntosexplorados<-puntosexplorados+1
    #Cogemos el minimo de la lista frontera
    pos<-arrayInd(which.min(matrizalg), dim(matrizalg))
    #Guardamos la distancia tentativa del punto actual
    matrizoriginal[pos[1],pos[2]]<-matrizdist[pos[1],pos[2]]
    #Marcamos el punto como explorado
    explorado[pos[1],pos[2]]<-0
    
    #Para los vecinos del punto que estamos explorando, si no es una pared y no esta explorado calculamos la distancia tentativa.
    #Si es la primera vez que la calculamos directamente metemos la distancia calculada, si no comprobamos si es menor que la distancia tentativa que
    #ya estaba. En este caso en la lista frontera guardamos distanciatentativa+distanciamanhattan del vecino correspondiente
    if(is.na(explorado[pos[1]+1,pos[2]]) & matrizoriginal[pos[1]+1,pos[2]]!=-10){
      disttent<-matrizdist[pos[1],pos[2]]+matrizoriginal[pos[1]+1,pos[2]]
      if(!is.na(matrizalg[pos[1]+1,pos[2]])){
        if (disttent<matrizdist[pos[1]+1,pos[2]]){
          matrizalg[pos[1]+1,pos[2]]<-disttent+(abs(rowdes-(pos[1]+1))+abs(coldes-pos[2]))
          matrizdist[pos[1]+1,pos[2]]<-disttent
        }
      }else{
        matrizalg[pos[1]+1,pos[2]]<-disttent+(abs(rowdes-(pos[1]+1))+abs(coldes-pos[2]))
        matrizdist[pos[1]+1,pos[2]]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1]-1,pos[2]]) & matrizoriginal[pos[1]-1,pos[2]]!=-10){
      disttent<-matrizdist[pos[1],pos[2]]+matrizoriginal[pos[1]-1,pos[2]]
      if(!is.na(matrizalg[pos[1]-1,pos[2]])){
        if (disttent<matrizdist[pos[1]-1,pos[2]]){
          matrizalg[pos[1]-1,pos[2]]<-disttent+(abs(rowdes-(pos[1]-1))+abs(coldes-pos[2]))
          matrizdist[pos[1]-1,pos[2]]<-disttent
        }
      }else{
        matrizalg[pos[1]-1,pos[2]]<-disttent+(abs(rowdes-(pos[1]-1))+abs(coldes-pos[2]))
        matrizdist[pos[1]-1,pos[2]]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1],pos[2]+1]) & matrizoriginal[pos[1],pos[2]+1]!=-10){
      disttent<-matrizdist[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]+1]
      if(!is.na(matrizalg[pos[1],pos[2]+1])){
        if (disttent<matrizdist[pos[1],pos[2]+1]){
          matrizalg[pos[1],pos[2]+1]<-disttent+(abs(rowdes-pos[1])+abs(coldes-(pos[2]+1)))
          matrizdist[pos[1],pos[2]+1]<-disttent
        }
      }else{
        matrizalg[pos[1],pos[2]+1]<-disttent+(abs(rowdes-pos[1])+abs(coldes-(pos[2]+1)))
        matrizdist[pos[1],pos[2]+1]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1],pos[2]-1]) & matrizoriginal[pos[1],pos[2]-1]!=-10){
      disttent<-matrizdist[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]-1]
      if(!is.na(matrizalg[pos[1],pos[2]-1])){
        if (disttent<matrizdist[pos[1],pos[2]-1]){
          matrizalg[pos[1],pos[2]-1]<-disttent+(abs(rowdes-pos[1])+abs(coldes-(pos[2]-1)))
          matrizdist[pos[1],pos[2]-1]<-disttent
        }
      }else{
        matrizalg[pos[1],pos[2]-1]<-disttent+(abs(rowdes-pos[1])+abs(coldes-(pos[2]-1)))
        matrizdist[pos[1],pos[2]-1]<-disttent
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
  lista<-list("matriz"=matrizoriginal, "encontrado"=encontrado,  "npuntos"=puntosexplorados)
  return (lista)
}
################################################################################
################################################################################
#algoritmoaestrellav1 aplica la version optimizada con la heuristica A* del algoritmo de dijkstra usando el conjunto frontera para llegar del punto salida al punto llegada
#En esta version calculamos al principio todas las distancias manhattan y las guardamos en una matriz
algoritmoaestrellav2<-function(matrizoriginal,rowsal,colsal,rowdes,coldes,semillapesos){
  s<-SyncRNG(seed=semillapesos)
  
  tammat<-dim(matrizoriginal)
  matrizalg<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista frontera
  explorado<-matrix(nrow=tammat[1], ncol = tammat[2]) #Lista explorado
  matrizdist<-matrix(nrow=tammat[1], ncol = tammat[2])#Distancias tentativas
  distanciaman<-matrix(nrow=tammat[1], ncol = tammat[2])#Distancias Manhattan
  #Calculamos todas las distancias manhattan
  for (i in 1:nrow(matrizalg)){
    for (j in 1:ncol(matrizalg)){
      distanciaman[i,j]<-abs(rowdes-i)+abs(coldes-j)
    }
  }
  
  #Comenzamos metiendo en la matriz de pesos el punto de salida con valor 0 y dando un valor 0 a 9 (distinto de -2) al punto destino
  matrizoriginal[rowdes,coldes]<-s$randi()%%9+1
  matrizoriginal[rowsal,colsal]<-0
  puntosexplorados<-0
  matrizalg[rowsal,colsal]<-0 #Metemos la salida en la lista frontera
  matrizdist[rowsal,colsal]<-0
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
    puntosexplorados<-puntosexplorados+1
    #Cogemos el minimo de la lista frontera
    pos<-arrayInd(which.min(matrizalg), dim(matrizalg))
    #Guardamos la distancia tentativa del punto actual
    matrizoriginal[pos[1],pos[2]]<-matrizdist[pos[1],pos[2]]
    #Marcamos el punto como explorado
    explorado[pos[1],pos[2]]<-0
    
    #Para los vecinos del punto que estamos explorando, si no es una pared y no esta explorado calculamos la distancia tentativa.
    #Si es la primera vez que la calculamos directamente metemos la distancia calculada, si no comprobamos si es menor que la distancia tentativa que
    #ya estaba. En este caso en la lista frontera guardamos distanciatentativa+distanciamanhattan del vecino correspondiente
    if(is.na(explorado[pos[1]+1,pos[2]]) & matrizoriginal[pos[1]+1,pos[2]]!=-10){
      disttent<-matrizdist[pos[1],pos[2]]+matrizoriginal[pos[1]+1,pos[2]]
      if(!is.na(matrizalg[pos[1]+1,pos[2]])){
        if (disttent<matrizdist[pos[1]+1,pos[2]]){
          matrizalg[pos[1]+1,pos[2]]<-disttent+distanciaman[pos[1]+1,pos[2]]
          matrizdist[pos[1]+1,pos[2]]<-disttent
        }
      }else{
        matrizalg[pos[1]+1,pos[2]]<-disttent+distanciaman[pos[1]+1,pos[2]]
        matrizdist[pos[1]+1,pos[2]]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1]-1,pos[2]]) & matrizoriginal[pos[1]-1,pos[2]]!=-10){
      disttent<-matrizdist[pos[1],pos[2]]+matrizoriginal[pos[1]-1,pos[2]]
      if(!is.na(matrizalg[pos[1]-1,pos[2]])){
        if (disttent<matrizdist[pos[1]-1,pos[2]]){
          matrizalg[pos[1]-1,pos[2]]<-disttent+distanciaman[pos[1]-1,pos[2]]
          matrizdist[pos[1]-1,pos[2]]<-disttent
        }
      }else{
        matrizalg[pos[1]-1,pos[2]]<-disttent+distanciaman[pos[1]-1,pos[2]]
        matrizdist[pos[1]-1,pos[2]]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1],pos[2]+1]) & matrizoriginal[pos[1],pos[2]+1]!=-10){
      disttent<-matrizdist[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]+1]
      if(!is.na(matrizalg[pos[1],pos[2]+1])){
        if (disttent<matrizdist[pos[1],pos[2]+1]){
          matrizalg[pos[1],pos[2]+1]<-disttent+distanciaman[pos[1],pos[2]+1]
          matrizdist[pos[1],pos[2]+1]<-disttent
        }
      }else{
        matrizalg[pos[1],pos[2]+1]<-disttent+distanciaman[pos[1],pos[2]+1]
        matrizdist[pos[1],pos[2]+1]<-disttent
      }
    }
    
    if(is.na(explorado[pos[1],pos[2]-1]) & matrizoriginal[pos[1],pos[2]-1]!=-10){
      disttent<-matrizdist[pos[1],pos[2]]+matrizoriginal[pos[1],pos[2]-1]
      if(!is.na(matrizalg[pos[1],pos[2]-1])){
        if (disttent<matrizdist[pos[1],pos[2]-1]){
          matrizalg[pos[1],pos[2]-1]<-disttent+distanciaman[pos[1],pos[2]-1]
          matrizdist[pos[1],pos[2]-1]<-disttent
        }
      }else{
        matrizalg[pos[1],pos[2]-1]<-disttent+distanciaman[pos[1],pos[2]-1]
        matrizdist[pos[1],pos[2]-1]<-disttent
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
  lista<-list("matriz"=matrizoriginal, "encontrado"=encontrado,  "npuntos"=puntosexplorados)
  return (lista)
}
################################################################################
################################################################################
#AHORA CORREMOS EL CÓDIGO
#creamos el laberinto
#lista<-generaLaberinto(200,1,69,54321)#Ejemplo mas cercanos
#lista<-generaLaberinto(200,1,420,4321)#Ejemplo mas lejanos
#lista<-generaLaberinto(5,1,1234,4321)#Ejemplo sin camino posible
#lista<-generaLaberinto(5,1,420,4321)#Ejemplo mismo punto destino-salida
#lista<-generaLaberinto(5,1.2,1234,4321)#Ejemplo pequeño
lista<-generaLaberinto(100,1,69420,54321)#Ejemplo mediano dos caminos

colores<- hcl.colors(15, "Greens 2") [0:10] #Cogemos 10 colores para los 10 pesos 0 a 9
image(1:nrow(lista$matriz), 1:ncol(lista$matriz),lista$matriz,col=c( "black","red","turquoise1",colores))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)

################################################################################
#CONJUNTO FRONTERA
t<-proc.time()
dijkstrafrontera<-conjuntofrontera(lista$matriz,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,999)
tiempofrontera<-proc.time()-t
matrizdijkstrafrontera<-dijkstrafrontera$matriz
dijkstrafrontera$npuntos

pal <- colorRampPalette(c("black", hcl.colors(100000, "plasma")), bias=1)
image(1:nrow(matrizdijkstrafrontera), 1:ncol(matrizdijkstrafrontera),matrizdijkstrafrontera, col=pal(length(unique(matrizdijkstrafrontera))))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)

#PINTAMOS EL CAMINO
trazacamino(matrizdijkstrafrontera,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,dijkstrafrontera$encontrado)

################################################################################
#DIJKSTRA BIDIRECCIONAL
t<-proc.time()
bidireccional<-dijkstraBidireccional(lista$matriz,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,999)
tiempobidireccional<-proc.time()-t
matrizbidireccional<-bidireccional$matriz
bidireccional$npuntos

pal <- colorRampPalette(c("black", hcl.colors(100000, "plasma")), bias=1)
image(1:nrow(matrizbidireccional), 1:ncol(matrizbidireccional),matrizbidireccional, col=pal(length(unique(matrizbidireccional))))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)

#PINTAMOS EL CAMINO
caminoBi(bidireccional$burbuja1, bidireccional$burbuja2, lista$rowsal,lista$colsal,
                 lista$rowdes,lista$coldes,bidireccional$rowcruce,bidireccional$colcruce,bidireccional$encontrado)

################################################################################
#A* VERSION ON THE FLY
t<-proc.time()
dijkstraestrellav1<-algoritmoaestrellav1(lista$matriz,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,999)
v1tiempoaestrella<-proc.time()-t
matrizestrellav1<-dijkstraestrellav1$matriz
dijkstraestrellav1$npuntos

pal <- colorRampPalette(c("black", hcl.colors(100000, "plasma")), bias=1)
image(1:nrow(matrizestrellav1), 1:ncol(matrizestrellav1),matrizestrellav1, col=pal(length(unique(matrizestrellav1))))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)

#PINTAMOS EL CAMINO
trazacamino(matrizestrellav1,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,dijkstraestrellav1$encontrado)

################################################################################
################################################################################
#A* VERSION PRECALCULADO
t<-proc.time()
dijkstraestrellav2<-algoritmoaestrellav2(lista$matriz,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,999)
v2tiempoaestrella<-proc.time()-t
matrizestrellav2<-dijkstraestrellav2$matriz
dijkstraestrellav2$npuntos

pal <- colorRampPalette(c("black", hcl.colors(100000, "plasma")), bias=1)
image(1:nrow(matrizestrellav2), 1:ncol(matrizestrellav2),matrizestrellav2, col=pal(length(unique(matrizestrellav2))))
points(lista$rowsal,lista$colsal, col="blue", pch=16, cex=1)
points(lista$rowdes,lista$coldes, col="green", pch=16,cex=1)

#PINTAMOS EL CAMINO
trazacamino(matrizestrellav2,lista$rowsal,lista$colsal,lista$rowdes,lista$coldes,dijkstraestrellav2$encontrado)

################################################################################