#PABLO MARCOS PARRA
#LAB 3.3
library(RColorBrewer)
library(reshape2)
library(viridis)
#TAREA 1
#g(x,y) = cos((x*x+y*y)*12)/(2*((x*x+y*y)*6.28+1))  Rango [-1:2][-1:2]

palette(plasma(30)) #paleta de colores
##################################################################################
n<-1000
x<-seq(-1,2,length.out = n)
y<-x

#FUNCION PARA CREAR LA MATRIZ
funciont1<-function(x,y,n){
  res<-c()
  for(i in x){
    fil<-c()
    for (j in y){
      fil<-append(fil,cos((i*i+j*j)*12)/(2*((i*i+j*j)*6.28+1)))
    }
    res<-rbind(res,fil)
  }
  rownames(res)<-x #titulos de las filas son los valores de x
  colnames(res)<-y #titulos de las columnas son los valores de y
  return(res)
}
#CREAMOS LA MATRIZ
matrizt1<-funciont1(x,y,n)
#PINTAMOS LA MATRIZ
image(x,y,matrizt1,col=plasma(30))
set.seed(69420)#Establecemos una semilla para que los experimentos sean reproducibles

#Funcion para crear los graficos y obtener el maximo absoluto y sus coordenadas
calculamax<-function(matriz,m){
  #Añadimos los bordes para evitar errores
  matriz<-cbind(rep(-100,n),matriz,rep(-100,n)) 
  matriz<-rbind(rep(-100,n+2),matriz,rep(-100,n+2))
  
  #Sacamos una lista "aleatoria" de puntos
  xs<-sample(1:n,m)
  ys<-sample(1:n,m)
  maximo<--100
  
  #Soltamos los paracaidistas m veces
  for (i in 1:m){
    ini<-c(xs[i],ys[i]) #Elegimos el punto de inicio
    #Mientras el maximo (relativo o absoluto) no se haya encontrado lo busca
    while(1){
      valores<-c(matriz[ini[1],ini[2]],matriz[ini[1]+1,ini[2]],
                 matriz[ini[1]-1,ini[2]],matriz[ini[1],ini[2]+1],matriz[ini[1],ini[2]-1])
      maximorel<-max(valores)
      valmax<-which.max(valores)
      if (valmax==1){
        #Si hemos encontrado un maximo local
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
      coordenadas<-c(ini[1]-1, ini[2]-1)
    }
  }
  maximo<-c(maximo,coordenadas)
  return(maximo)#Devolvemos el valor del maximo y el punto en el que esta en la matriz
}

maximos<-rbind(calculamax(matrizt1,50))

#Visualmente para mi es muy complicado asi que lo hare experimentalmente
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
      valores<-c(matriz[ini[1],ini[2]],matriz[ini[1]+1,ini[2]],matriz[ini[1]-1,ini[2]],
                 matriz[ini[1],ini[2]+1],matriz[ini[1],ini[2]-1])
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

probt1<-0
for (i in 1:50){
  for (j in 1:50){
    probt1<-probt1+probabilidad(matrizt1,i,maximos[1,1])
  }
}
probt1<-probt1/2500 #Obtengo 0.08641733252 de probabiidad
#Para sacar el p=0.999 hacemos como lo explicado en el lab 3.2 (Consideramos éxito encontrar el máximo global)
1-probt1
#p(no sacar el global)=0.9135827
#p(Algun exito)=0.9135827*0.9135827*0.9135827...= 0.9135827^n=0.001
#Despejamos y obtenemos n=77
######################################
#Tarea 2
#h(x,y) = x/(x*x+1) - y/(y*y+1) + 2*(sqrt(x*x+y*y)-1)/((sqrt(x*x+y*y)-1)*(sqrt(x*x+y*y)-1)+1)   Rango [-6:6][-6:6]
#n=2000 puntos
n<-2000
x<-seq(-6,6,length.out = n)
y<-x
#FUNCION PARA CREAR LA MATRIZ TAREA 2
funciont2<-function(x,y,n){
  res<-c()
  for(i in x){
    fil<-c()
    for (j in y){
      fil<-append(fil,i/(i*i+1) - j/(j*j+1) + 2*(sqrt(i*i+j*j)-1)/((sqrt(i*i+j*j)-1)*(sqrt(i*i+j*j)-1)+1))
    }
    res<-rbind(res,fil)
  }
  rownames(res)<-x #titulos de las filas son los valores de x
  colnames(res)<-y #titulos de las columnas son los valores de y
  return(res)
}
#CREAMOS LA MATRIZ
t<-proc.time()
matrizt2<-funciont2(x,y,n)
tiempoinicializacion<-proc.time()-t
set.seed(69420)#Establecemos una semilla para que los experimentos sean reproducibles

#FUNCION MONTECARLO MODIFICADA (APARTADO 1)
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
#APARTADO 2
t<-proc.time()
maximot2<-comprobacion(matrizt2,1000)
tiempo<-proc.time()-t
tiempo

#APARTADO 3 CALCULO ON FLY
onfly<-function(matriz,m){
  xx<-seq(-6,6,length.out = n)
  yy<-xx
  #añadimos bordes
  matriz<-cbind(rep(-100,n),matriz,rep(-100,n)) 
  matriz<-rbind(rep(-100,n+2),matriz,rep(-100,n+2))
  xs<-sample(1:n,m)
  ys<-sample(1:n,m)
  maximo<--100
  #Soltamos los paracaidistas m veces
  for (i in 1:m){
    ini<-c(xs[i],ys[i])
    while(1){
      i1<-xx[ini[1]]
      j1<-yy[ini[2]]
      #Calculamos el punto
      punto<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
      matriz[ini[1],ini[2]]<-punto
      
      if(ini[1]==n){#Si estamos en un borde metemos -100 para evitar errores
        puntoderecha<--100
      }else{
        if(is.na(matriz[ini[1]+1,ini[2]])){#Si no ha sido calculado antes se calcula
          i1<-xx[ini[1]+1]
          puntoderecha<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
          matriz[ini[1]+1,ini[2]]<-puntoderecha
        }else{#Si ha sido calculado antes se saca de la memoria
          puntoderecha<-matriz[ini[1]+1,ini[2]]
        }
      }
      if(ini[1]==1){
        puntoizquierda<--100
      }else{
        if(is.na(matriz[ini[1]-1,ini[2]])){
          i1<-xx[ini[1]-1]
          puntoizquierda<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
          matriz[ini[1]-1,ini[2]]<-puntoizquierda
        }else{
          puntoizquierda<-matriz[ini[1]-1,ini[2]]
        }
      }
      i1<-xx[ini[1]]
      
      if(ini[2]==n){
        puntoarriba<--100
      }else{
        if(is.na(matriz[ini[1],ini[2]+1])){
          j1<-yy[ini[2]+1]
          puntoarriba<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
          matriz[ini[1], ini[2]+1]<-puntoarriba
        }else{
          puntoarriba<-matriz[ini[1],ini[2]+1]
        }
      }
      
      if(ini[2]==1){
        puntoabajo<--100
      }else{
        if(is.na(matriz[ini[1],ini[2]-1])){
          j1<-yy[ini[2]-1]
          puntoabajo<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
          matriz[ini[1], ini[2]-1]<-puntoabajo
        }else{
          puntoabajo<-matriz[ini[1],ini[2]-1]
        }
      }
      
      j1<-xx[ini[2]]
      #Hallamos el máximo de los 5 puntos
      valores<-c(punto,puntoderecha,puntoizquierda,puntoarriba,puntoabajo)
      maximorel<-max(valores)
      valmax<-which.max(valores)
      
      if (valmax==1){
        break
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
  print(maximo)
  #Eliminamos los bordes sobrantes
  matriz<-matriz[,-1]
  matriz<-matriz[,-(n+1)]
  matriz<-matriz[-1,]
  matriz<-matriz[-(n+1),]
  return(matriz)
}

#CREAMOS MATRIZ VACIA
nuevamatrizt2<-matrix(nrow=n,ncol=n)
#CALCULAMOS EL MAXIMO Y LA MATRIZ
t<-proc.time()
nuevamatrizt2<-onfly(nuevamatrizt2,1000)
tiempoonfly<-proc.time()-t
tiempoonfly

#APARTADO 4 (SIN VISITAR CAMINOS YA RECORRIDOS)
yarecorrido<-function(matriz,m){
  xx<-seq(-6,6,length.out = n)
  yy<-xx
  #añadimos bordes
  matriz<-cbind(rep(-100,n),matriz,rep(-100,n)) 
  matriz<-rbind(rep(-100,n+2),matriz,rep(-100,n+2))
  xs<-sample(1:n,m)
  ys<-sample(1:n,m)
  maximo<--100
  #Soltamos los paracaidistas m veces
  for (i in 1:m){
    ini<-c(xs[i],ys[i])
    while(1){
      if(!is.na(matriz[ini[1],ini[2]])){
        break
      }
      i1<-xx[ini[1]]
      j1<-yy[ini[2]]
      
      punto<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
      matriz[ini[1],ini[2]]<-punto
      
      if(ini[1]==n){
        puntoderecha<--100
      }else{
        if(is.na(matriz[ini[1]+1,ini[2]])){
          i1<-xx[ini[1]+1]
          puntoderecha<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
        }
      }
      if(ini[1]==1){
        puntoizquierda<--100
      }else{
        if(is.na(matriz[ini[1]-1,ini[2]])){
          i1<-xx[ini[1]-1]
          puntoizquierda<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
        }
      }
      i1<-xx[ini[1]]
      
      if(ini[2]==n){
        puntoarriba<--100
      }else{
        if(is.na(matriz[ini[1],ini[2]+1])){
          j1<-yy[ini[2]+1]
          puntoarriba<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
        }
      }
      
      if(ini[2]==1){
        puntoabajo<--100
      }else{
        if(is.na(matriz[ini[1],ini[2]-1])){
          j1<-yy[ini[2]-1]
          puntoabajo<-i1/(i1*i1+1) - j1/(j1*j1+1) + 2*(sqrt(i1*i1+j1*j1)-1)/((sqrt(i1*i1+j1*j1)-1)*(sqrt(i1*i1+j1*j1)-1)+1)
        }
      }
      
      j1<-xx[ini[2]]
      
      valores<-c(punto,puntoderecha,puntoizquierda,puntoarriba,puntoabajo)
      maximorel<-max(valores)
      valmax<-which.max(valores)
      
      if (valmax==1){
        break
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
  print(maximo)
  #Eliminamos los bordes sobrantes
  matriz<-matriz[,-1]
  matriz<-matriz[,-(n+1)]
  matriz<-matriz[-1,]
  matriz<-matriz[-(n+1),]
  return(matriz)
}
#CREAMOS MATRIZ VACIA
nuevamatrizt2v2<-matrix(nrow=n,ncol=n)
#CALCULAMOS EL TIEMPO QUE TARDA
t<-proc.time()
nuevamatrizt2<-onfly(nuevamatrizt2v2,1000)
tiempoonflyv2<-proc.time()-t
tiempoonflyv2
