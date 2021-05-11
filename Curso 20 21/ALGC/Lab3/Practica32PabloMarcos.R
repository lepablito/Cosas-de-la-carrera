#PABLO MARCOS PARRA
#PRACTICA 3: HILLCLIMBING Y MONTE-CARLO
library(RColorBrewer)
library(reshape2)
library(viridis)

palette(plasma(30)) #paleta de colores
set.seed(69420)#Establecemos una semilla para que los experimentos sean reproducibles
n<-1000
x<-seq(0,pi,length.out = n)
y<-x
##################################################################################
#Funcion para crear los graficos y obtener el maximo absoluto y sus coordenadas
pintar<-function(matriz,m){
  datos<-melt(matriz)
  representacion<-matriz
  #Añadimos los bordes para evitar errores
  matriz<-cbind(rep(-100,n),matriz,rep(-100,n)) 
  matriz<-rbind(rep(-100,n+2),matriz,rep(-100,n+2))
  #Sacamos una lista "aleatoria" de puntos
  xs<-sample(1:n,m)
  ys<-sample(1:n,m)
  maximo<--100
  #Representamos el gráfico
  image(x,y,representacion,col=plasma(30))
  #Soltamos los paracaidistas m veces
  for (i in 1:m){
    ini<-c(xs[i],ys[i]) #Elegimos el punto de inicio
    representacion[ini[1],ini[2]]<-"green"
    points(x[ini[1]],y[ini[2]],col="green",pch=15,cex=1000/n) 
    #Mientras el maximo (relativo o absoluto) no se haya encontrado lo busca
    while(1){
      valores<-c(matriz[ini[1],ini[2]],matriz[ini[1]+1,ini[2]],
                 matriz[ini[1]-1,ini[2]],matriz[ini[1],ini[2]+1],matriz[ini[1],ini[2]-1])
      maximorel<-max(valores)#Escogemos el maximo de los 5 puntos
      valmax<-which.max(valores)#Devolvemos el indice donde está el máximo
      if (valmax==1){
        #Si hemos encontrado un maximo local
        representacion[ini[1]-1,ini[2]-1]<-"red"
        points(x[ini[1]-1],y[ini[2]-1],col="red",pch=15,cex=1000/n) 
        break;
      }
      else{
        if(valmax==2){
          ini<-c(ini[1]+1,ini[2])
        }else if(valmax==3){
          ini<-c(ini[1]-1,ini[2])
        }else if(valmax==4){
          ini<-c(ini[1],ini[2]+1)
        }else{
          ini<-c(ini[1],ini[2]-1)
        }
        #Dibujamos el camino
        representacion[ini[1]-1,ini[2]-1]<-"black"
        points(x[ini[1]-1],y[ini[2]-1],col="black",pch=15,cex=500/n) 
      }
    }
    #Comprobamos si es un maximo absoluto o solo local
    if(maximorel>maximo){
      maximo<-maximorel
      coordenadas<-c(ini[1]-1, ini[2]-1)
    }
  }
  maximo<-c(maximo,coordenadas)
  return(maximo)#Devolvemos el valor del maximo y el punto en el que esta en la matriz
}
################################################################################
#CREAMOS LAS MATRICES CON LAS FUNCIONES PEDIDAS
funcion1<-function(x,y,n){
  res<-c()
  for(i in x){
    fil<-c()
    for (j in y){
      fil<-append(fil,sin(i)+cos(j)+sin(i)*cos(j)+sin(i*2))
    }
    res<-rbind(res,fil)
  }
  rownames(res)<-x #titulos de las filas son los valores de x
  colnames(res)<-y #titulos de las columnas son los valores de y
  return(res)
}
funcion2<-function(x,y,n){
  res<-c()
  for(i in x){
    fil<-c()
    for (j in y){
      fil<-append(fil,2*sin(i)*cos(j/2)+i+log(abs(j-pi/2)))
    }
    res<-rbind(res,fil)
  }
  rownames(res)<-x #titulos de las filas son los valores de x
  colnames(res)<-y #titulos de las columnas son los valores de y
  return(res)
}
funcion3<-function(x,y,n){
  res<-c()
  for(i in x){
    fil<-c()
    for (j in y){
      fil<-append(fil,sin(i)*cos(j)+sqrt(i*j))
    }
    res<-rbind(res,fil)
  }
  rownames(res)<-x #titulos de las filas son los valores de x
  colnames(res)<-y #titulos de las columnas son los valores de y
  return(res)
}
funcion4<-function(x,y,n){
  res<-c()
  for(i in x){
    fil<-c()
    for (j in y){
      fil<-append(fil,sin(i*7)+cos((j+pi/4)*4)+(i+j))
    }
    res<-rbind(res,fil)
  }
  rownames(res)<-x #titulos de las filas son los valores de x
  colnames(res)<-y #titulos de las columnas son los valores de y
  return(res)
}
######################################################################################
#Para cada funcion creamos la matriz y dibujamos su grafico, metemos el resultado en una matriz cuya primera fila es el valor del maximo, la segunda el
#valor de las x y la tercera el valor de las y; cada columna corresponde a una funcion
matriz1<-funcion1(x,y,n)
maximos<-rbind(pintar(matriz1,5))
matriz2<-funcion2(x,y,n)
maximos<-rbind(maximos,pintar(matriz2,10))
matriz3<-funcion3(x,y,n)
maximos<-rbind(maximos,pintar(matriz3,5))
matriz4<-funcion4(x,y,n)
maximos<-rbind(maximos,pintar(matriz4,30))

colnames(maximos)<-c("Valor","X","Y")
rownames(maximos)<-c("f1","f2","f3","f4")
maximos
###################################################################
#He creado una funcion para hallar experimentalmente la probabilidad
probabilidad<-function(matriz,m,maximoabs){
  cont<-0
  matriz<-cbind(rep(-100,n),matriz,rep(-100,n)) 
  matriz<-rbind(rep(-100,n+2),matriz,rep(-100,n+2))
  xs<-sample(1:n,m, replace = TRUE)
  ys<-sample(1:n,m, replace = TRUE)
  maximo<--100
  for (i in 1:m){
    ini<-c(xs[i],ys[i]) #Elegimos el punto de inicio
    
    while(1){
      valores<-c(matriz[ini[1],ini[2]],matriz[ini[1]+1,ini[2]],matriz[ini[1]-1,ini[2]],matriz[ini[1],ini[2]+1],matriz[ini[1],ini[2]-1])
      maximorel<-max(valores)
      valmax<-which.max(valores)
      if (valmax==1){
        break;
      }
      else{
        if(valmax==2){
          ini<-c(ini[1]+1,ini[2])
        }else if(valmax==3){
          ini<-c(ini[1]-1,ini[2])
        }else if(valmax==4){
          ini<-c(ini[1],ini[2]+1)
        }else{
          ini<-c(ini[1],ini[2]-1)
        }
      }
    }
    if (maximorel==maximoabs){
      cont<-cont+1
    }
    if(maximorel>maximo){
      maximo<-maximorel
      coordenadas<-c(ini[1]-1, ini[2]-1)
    }
  }
  prob<-cont/m
  maximo<-c(maximo,coordenadas)
  return(prob)
}
#################################################################################################################################

#Para f1 y f3 sabemos por el grafico que solo hay un maximo local asi que la probabilidad de encontrar el maximo absoluto es 1
#Para los casos de f2 y f4 vamos a hallar la probabilidad experimentalmente
probb<-0
for (i in 1:50){
  for (j in 1:50){
    probb<-probb+probabilidad(matriz2,i,maximos[2,1])
  }
}
probb<-probb/2500



probd<-0
for (i in 1:50){
  for (j in 1:50){
    probd<-probd+probabilidad(matriz4,j,maximos[4,1])
  }
}
probd<-probd/2500
############################################################################################################################
#Calcular el maximo absoluto recorriendo toda la matriz o usando el algoritmo de los paracaidistas con 98% probabilidades
#Recorremos toda la matriz
t<-proc.time()
maxrecorrido<--100
for (i in 1:n){
  for (j in 1:n){
    if (matriz4[i,j]>maxrecorrido){
      maxrecorrido<-matriz4[i,j]
    }
  }
}
tiemporecorrido<-proc.time()-t
maxrecorrido
tiemporecorrido

#Funcion para sacar el máximo (sin otros calculos)
comprobacion<-function(matriz,m){
  matriz<-cbind(rep(-100,n),matriz,rep(-100,n)) 
  matriz<-rbind(rep(-100,n+2),matriz,rep(-100,n+2))
  xs<-sample(1:n,m)
  ys<-sample(1:n,m)
  maximo<--100
  #Soltamos los paracaidistas m veces
  for (i in 1:m){
    ini<-c(xs[i],ys[i]) #Elegimos el punto de inicio
    while(1){
      valores<-c(matriz[ini[1],ini[2]],matriz[ini[1]+1,ini[2]],
                 matriz[ini[1]-1,ini[2]],matriz[ini[1],ini[2]+1],matriz[ini[1],ini[2]-1])
      maximorel<-max(valores)
      valmax<-which.max(valores)
      if (valmax==1){
        break;
      }
      else{
        if(valmax==2){
          ini<-c(ini[1]+1,ini[2])
        }else if(valmax==3){
          ini<-c(ini[1]-1,ini[2])
        }else if(valmax==4){
          ini<-c(ini[1],ini[2]+1)
        }else{
          ini<-c(ini[1],ini[2]-1)
        }
      }
    }
    #Comprobamos si es un maximo absoluto o solo local
    if(maximorel>maximo){
      maximo<-maximorel
    }
  }
  return(maximo)
}
t<-proc.time()
comprobacion(matriz4,35)
tiempohill<-proc.time()-t
tiempohill
