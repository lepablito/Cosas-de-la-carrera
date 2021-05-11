library(RColorBrewer)
library(reshape2)

paleta<-palette(brewer.pal(n=9,name="RdYlGn")[-(1:2)]) #paleta de colores

n<-100
x<-seq(0,pi,length.out = n)
y<-x

funcion<-function(x,y,n){
  res<-c()
  for(i in x){
    fil<-c()
    for (j in y){
      fil<-append(fil,sin(i)+sin(j)+cos(i)*sin(j))
    }
    res<-rbind(res,fil)
  }
  rownames(res)<-x #titulos de las filas son los valores de x
  colnames(res)<-y #titulos de las columnas son los valores de y
  return(res)
}
matriz<-funcion(x,y,n)

datos<-melt(matriz) #Esto nos servira para poder hacer la representacion, nos
#repite los valores de x e y para tener todos los puntos juntos con su valor de
#la funcion anterior en una matriz de 3 columnas y n*n filas
head(datos) #para que veais a lo que me refiero ejecutad esto

representacion<-matriz + 1 # Guardamos la matriz de valores en otra matriz que
#usaremos para hacer la representacion de los colores y le pone + 1, porque 
#los que valen 0 equivalen al color blanco, pero si le sumamos 1 los 0s ahora 
#valdran 1 que equivalen ya a otro color mas distinguible

ini<-c(sample(1:n,1),sample(1:n,1)) #Elegimos el punto de inicio

#La funcion plot le damos los valores de los puntos x e y conseguidos con melt
#de tal manera que nos pondra un punto para todos los n*n puntos de la matriz
#con la matriz representacion como colores de los puntos para ver el degradado
plot(datos$Var1,datos$Var2,col=representacion,pch=15,cex=50/n)
#pch es la forma del punto y cex el tamaño del punto (he puesto ese porque se
#me ajustaba bien al tamaño de mi pantalla pero se puede probar mas tamaños)

representacion[ini[1],ini[2]]<-"green" #pongo el punto de inicio de color verd
points(x[ini[1]],y[ini[2]],col="green",pch=15,cex=50/n) 

#usando points puede dibujar encima del grafico inicial, en este caso pinto 
#el punto de inicio del camino

#A partir de aqui se puede hacer el HillClimbing actualizando el grafico con
#points en cada iteración
