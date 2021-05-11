##!/usr/bin/python
# -*- coding: utf-8 -*-
import gtk
import gtk.gdk
import gobject
import time
from random import randint 
from __builtin__ import False
from gtk import FALSE
class casilla:
    #CLASE CASILLA CON ATRIBUTOS QUE SE SOBREENTIENDEN
    def __init__(self, fila, columna, mina, estado, marcado,minascolindantes):
        self.fila = fila
        self.columna = columna
        self.mina = mina
        self.estado = estado
        self.marcado = marcado
        self.minascolindantes=minascolindantes
        self.estadoymina=False
class juego():

    def __init__(self):
        #CREADOR DEL JUEGO
        self.glade=gtk.Builder()
        self.glade.add_from_file('menu.glade')
        self.menu()#Abre el menu

        self.iconos = []
        self.juegoacabado=False
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_cerrada.png"))  # Cerrada 0
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_marcada.png"))  # Marcada bien 1
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_marcada_error.png"))  # Marcada mal 2
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_0.png"))  # 0 minas 3
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_1.png"))  # 1 mina 4
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_2.png"))  # 2 minas 5
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_3.png"))  # 3 minas 6
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_4.png"))  # 4 minas 7
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_5.png"))  # 5 minas 8
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_6.png"))  # 6 minas 9
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_mina.png"))  # hay mina 10
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_boom.png"))  # mina explota 11
        self.iconos.append(gtk.gdk.pixbuf_new_from_file("ycelda_question.png"))  # no se sabe 12
        self.numjugada=1 #PARA SI ES EL PRIMER CLICK QUE HACE PARA ABRIR, PARA REASIGNAR UNA MINA
        self.tpo0=-1
        gobject.idle_add(self.tiempo)#Esta función ayuda a que el cronometro funcione
    def menu(self):
        #MENU
        self.ventana=self.glade.get_object('window1') #VENTANA DEL MENU
        
        self.filcol=self.glade.get_object("filcol") #ETIQUETAS FILCOL Y CONTMINAS
        self.cantminas=self.glade.get_object("cantminas")
        #SE ASIGNAN LOS BOTONES CON LOS EVENTOS Y SUS RESPECTIVOS CREADORES DE TABLETO
        self.botonfacil=self.glade.get_object('button1') 
        self.botonfacil.connect("clicked",self.crearfacil)
        self.botonfacil.connect("enter",self.mostrarinfo, ("9x9","10 minas"))
        
        self.botonmedio=self.glade.get_object('button2')
        self.botonmedio.connect("enter",self.mostrarinfo, ("16x16","40 minas"))
        self.botonmedio.connect("clicked",self.crearmedio)
        
        self.botondificil=self.glade.get_object('button3')
        self.botondificil.connect("clicked",self.creardificil)
        self.botondificil.connect("enter",self.mostrarinfo, ("16x30","99 minas"))
        
        self.botonfichero=self.glade.get_object('button4')
        self.botonfichero.connect("clicked", self.abrir_fichero)
        self.botonfichero.connect("enter",self.mostrarinfo, ("Lee fichero", " "))
        
        self.glade.connect_signals({
            "salir":self.on_window1_delete_event,
            })
        self.ventana.show()
    def mostrarinfo(self, widget, data=None):
        #CAMBIA LA INFORMACION, CADA VEZ QUE EL RATON PASA POR ALGUNA DE LAS 3 OPCIONES PRINCIPALES
        self.filcol.set_text(data[0])
        self.cantminas.set_text(data[1])
        return None
    """Se observara que self.fil y self.col tienen un numero mayor de filas y columnas, lo cual es usado para evitar que se desborde"""
    def crearfacil(self, widget):
        self.opt=1
        self.minas=10
        self.fil=11
        self.col=11
        self.filaa=9
        self.colu=9
        self.crear_tablero(self.fil,self.col,self.minas,self.opt)
        return None
    def crearmedio(self, widget):
        self.opt=2
        self.minas=40
        self.fil=18
        self.col=18
        self.filaa=16
        self.colu=16
        self.crear_tablero(self.fil,self.col,self.minas,self.opt)
        return None
    def creardificil(self, widget):
        self.opt=3
        self.minas=99
        self.fil=18
        self.col=32
        self.filaa=16
        self.colu=30
        self.crear_tablero(self.fil,self.col,self.minas,self.opt)
        return None
    def abrir_fichero(self, widget):
        """La funcion es parecida a la que se usa en el FAQ"""
        self.opt=4
        dlg = gtk.FileChooserDialog(
            "Abrir fichero", None,
            gtk.FILE_CHOOSER_ACTION_OPEN,
            (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
             gtk.STOCK_OPEN, gtk.RESPONSE_OK))

        if dlg.run() == gtk.RESPONSE_OK:
            res = dlg.get_filename()
        else:
            res = None
        dlg.destroy()
        self.fichero=res
        if res!=None:
            self.crear_tablero(0,0,0,self.opt)
    def crear_tablero(self, fil, col, minas, opt):
        #ES EL CREADOR DE TODOS LOS ICONOS DEL TABLERO INICIALES, ES DECIR, CON LAS CASILLAS CERRADAS Y EL BOTON DE REINICIO
        self.matriz=[]
        if self.opt!=4:
            self.matriz=[[0 for j in range(col)]for i in range (fil)]
            for i in range (fil):
                for j in range (col):
                    self.matriz[i][j]= casilla (i,j,False,False,False,0) #ASIGNADOR DE CASILLAS
        muro=self.glade.get_object('fixed1') #EL "CORCHO" EN EL QUE "CLAVAREMOS" LOS ICONOS
        self.ventana.hide() #OCULTA EL MENU
        self.juegaso=self.glade.get_object('juegaso')
        self.juegaso.show() #MUESTRA LA VENTANA DE JUEGO
        self.timer=self.glade.get_object('timer') #INICIALIZA EL CRONOMETRO
        self.timer.set_text("00:00")
        lista =[]
        if self.opt!=4:
            #Este es el metodo normal, para las opciones que no ejecutan un fichero
            for i in range(0,minas):
                #ASIGNADOR DE MINAS
                hacer=True
                while hacer:
                    coordenada1=randint(1,fil-2) 
                    coordenada2=randint(1,col-2)
                    if not ((coordenada1,coordenada2) in lista):
                        lista.append((coordenada1,coordenada2))
                        hacer=False
                        #SI LOS ALEATORIOS NO SE ENCUENTRAN IGUALES EN LA TUPLA, ENTONCES SE AÃƒÆ’Ã¢â‚¬ËœADEN A ESTA
            for i in range(0,minas):
                mfila=lista[i][0]
                mcolumna=lista[i][1]
                self.matriz[mfila][mcolumna].mina=True
                #SE ASIGNAN LAS MINAS DEFINITIVAMENTE
        else:
            """ESTE ELSE SE ENCARGA DE LEER EL FICHERO, FILAS, COLUMNAS Y CREAR LA MATRIZ ASIGNADA, TAMBIEN ASIGNA LAS MINAS"""
            with open(self.fichero) as f:
                contenido= f.readlines()
            contenido = [x.strip() for x in contenido]#cada casilla de la lista es igual a una linea del fichero
            #A partir de aqui (los procedimientos son iguales) es evaluar si cada numero de la primera linea tiene dos cifras
            #si es asi entonces concatena ambas cifras y es el n. de filas y columnas
            fil=int(contenido[0][0])
            if contenido[0][1]!=" ": #asigna la unidad y el "fil" anterior pasa a ser la decena
                fil=int(contenido[0][0]+contenido[0][1]) #lo concatena y lo pasa a int
            col=int(contenido[0][-1]) #lo mismo ocurre con las columnas, pero en este caso leyendo las dos ultimas
            if contenido[0][-2]!=" ":
                col=int(contenido[0][-2]+contenido[0][-1])
            self.fil=fil+2
            self.col=col+2
            self.matriz=[[0 for j in range(self.col)]for i in range (self.fil)] #creamos la matriz
            self.minas=0
            self.colu=col
            self.filaa=fil
            for i in range(self.fil):
                for j in range (self.col):
                    self.matriz[i][j]= casilla(i,j,False,False,False,0)
                    """LA DIFERENCIA ES QUE LA DISPOSICION DE LAS MINAS YA ESTA HECHA, POR LO QUE SOLO HAY QUE COLOCARLAS EN SUS RESPECTIVAS CELDAS"""
            for i in range (1,fil+1):
                for j in range (1,col+1):
                    if contenido[i][j-1]=="*":
                        self.matriz[i][j].mina=True
                        self.minas+=1
                    elif contenido[i][j-1]==".":
                        self.matriz[i][j+1].mina=False
            

        self.marcadas=self.minas #MARCAS
        self.mmarcadas=0 #MINAS MARCADAS CORRECTAMENTE
        
        if self.opt!=4:
            self.matrizgrafica=[[0 for j in range(col+2)]for i in range (fil)] #MATRIZ GRAFICA
        else:
            self.matrizgrafica=[[0 for j in range(col+2)]for i in range (fil+2)] #MATRIZ GRAFICA      
        
        self.nico=self.glade.get_object('nico')
        reinicio=gtk.gdk.pixbuf_new_from_file('nico.png') #ICONO DE REINICIO
        self.nico.set_image(gtk.Image())
        self.nico.get_image().set_from_pixbuf(reinicio)
        #LA SIGUIENTE ESTRUCTURA "SWITCH" FUNCIONA DE LA MISMA MANERA SEGUN LA OPCION ELEGIDA: REINICIA LOS ICONOS Y EL ESTADO DE JUEGOACABADO
        if self.opt==1:
            self.numjugada=1
            self.juegoacabado=False
            self.nico.connect('clicked', self.crearfacil)
        elif self.opt==2:
            self.numjugada=1
            self.juegoacabado=False
            self.nico.connect('clicked', self.crearmedio)
        elif self.opt==3:
            self.numjugada=1
            self.juegoacabado=False
            self.nico.connect('clicked', self.creardificil)
        elif self.opt==4:
            self.juegoacabado=False
            self.nico.connect('clicked', self.reiniciofichero)
            
        self.nico.show()
        
        self.mensaje=self.glade.get_object('mensaje') #avisos para el jugador
        self.minasrestantes=self.glade.get_object('minasrestantes') 
        self.minasrestantes.set_text("Minas: "+str(self.minas)) #cantidad de minas restantes
        
        self. cuadro= self.glade.get_object('casillote') #fila y columna de la casilla en la que esta el raton
        
        if self.opt!=4:
            for i in range (1, fil-1):
                for j in range(1, col-1):
                    #clava los botones en "el corcho (muro)
                    self.matrizgrafica[i][j]=self.icono=gtk.Button(None)
                    self.icono.set_relief(gtk.RELIEF_NONE)
                    self.icono.set_image(gtk.Image())
                    self.icono.get_image().set_from_pixbuf(self.iconos[0])
                    self.icono.connect('enter',self.encima,(i,j))
                    self.icono.set_events(gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK)
                    self.icono.connect("button-release-event", self.clickeado, (i,j,fil,col)) #la funcion clickeado realiza la apertura, la informacion o la marcacion de la casilla
    
                    self.icono.show()
                    """lo siguiente se usa para asignar unas coordenadas a los botones dentro del muro"""
                    if i%2!=0:
                        muro.put(self.icono, 10+20*(j-1),0+20*(i-1))
                    else:
                        muro.put(self.icono, 0+20*(j-1),0+20*(i-1))
        else:
            """lo mismo pero para las condiciones especiales del fichero"""
            for i in range (1, fil+1):
                for j in range(1, col+1):
                    self.matrizgrafica[i][j]=self.icono=gtk.Button(None)
                    self.icono.set_relief(gtk.RELIEF_NONE)
                    self.icono.set_image(gtk.Image())
                    self.icono.get_image().set_from_pixbuf(self.iconos[0])
                    self.icono.connect('enter',self.encima,(i,j))
                    self.icono.set_events(gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK)
                    self.icono.connect("button-release-event", self.clickeado, (i,j,fil,col))
    
                    self.icono.show()
                    if i%2!=0:
                        muro.put(self.icono, 10+20*(j-1),0+20*(i-1))
                    else:
                        muro.put(self.icono, 0+20*(j-1),0+20*(i-1))
        return None
    def reiniciofichero(self,widget):
        self.crear_tablero(0,0,0,4)
    def abrir(self, i, j):#Es la funcion encargada de abrir la celda seleccionada
        self.iconocasillas(None, i, j)#Llama a esta funcion para colocar el simbolo que corresponda
        if not self.matriz[i][j].mina:
            if self.matriz[i][j].minascolindantes==0:
                """si la casilla no tiene mina y el numero de minas colindantes es 0, abre las casillas adyacentes. 
                A continuacion están declarados todos los casos posibles. Primero comprueba que no esta abierta y despues la abre"""
                if i==1:
                    if j==1:
                        if not self.matriz[i][j+1].estado:
                            self.matriz[i][j+1].estado=True
                            self.abrir(i,j+1)
                        if not self.matriz[i+1][j].estado:
                            self.matriz[i+1][j].estado=True
                            self.abrir(i+1,j)
                        if not self.matriz[i+1][j+1].estado:
                            self.matriz[i+1][j+1].estado=True
                            self.abrir(i+1,j+1)
                    elif j==self.colu:
                        if not self.matriz[i][j-1].estado:
                            self.matriz[i][j-1].estado=True
                            self.abrir(i,j-1)
                        if not self.matriz[i+1][j].estado:
                            self.matriz[i+1][j].estado=True
                            self.abrir(i+1,j)
                    else:
                        if not self.matriz[i][j+1].estado:
                            self.matriz[i][j+1].estado=True
                            self.abrir(i,j+1)
                        if not self.matriz[i+1][j+1].estado:
                            self.matriz[i+1][j+1].estado=True
                            self.abrir(i+1,j+1)
                        if not self.matriz[i][j-1].estado:
                            self.matriz[i][j-1].estado=True
                            self.abrir(i,j-1)
                        if not self.matriz[i+1][j].estado:
                            self.matriz[i+1][j].estado=True
                            self.abrir(i+1,j)
                elif i==self.filaa:
                    if i%2==0:
                        if j==1:
                            if not self.matriz[i][j+1].estado:
                                self.matriz[i][j+1].estado=True
                                self.abrir(i,j+1)
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                        elif j==self.colu:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i-1][j-1].estado:
                                self.matriz[i-1][j-1].estado=True
                                self.abrir(i-1,j-1)
                            if not self.matriz[i][j-1].estado:
                                self.matriz[i][j-1].estado=True
                                self.abrir(i,j-1)
                        else:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i-1][j-1].estado:
                                self.matriz[i-1][j-1].estado=True
                                self.abrir(i-1,j-1)
                            if not self.matriz[i][j+1].estado:
                                self.matriz[i][j+1].estado=True
                                self.abrir(i,j+1)
                            if not self.matriz[i][j-1].estado:
                                self.matriz[i][j-1].estado=True
                                self.abrir(i,j-1)
                    else:
                        if j==1:
                            if not self.matriz[i-1][j+1].estado:
                                self.matriz[i-1][j+1].estado=True
                                self.abrir(i-1,j+1)
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i][j+1].estado:
                                self.matriz[i][j+1].estado=True
                                self.abrir(i,j+1)
                        elif j==self.colu:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i][j-1].estado:
                                self.matriz[i][j-1].estado=True
                                self.abrir(i,j-1)
                        else:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i-1][j+1].estado:
                                self.matriz[i-1][j+1].estado=True
                                self.abrir(i-1,j+1)
                            if not self.matriz[i][j-1].estado:
                                self.matriz[i][j-1].estado=True
                                self.abrir(i,j-1)
                            if not self.matriz[i][j+1].estado:
                                self.matriz[i][j+1].estado=True
                                self.abrir(i,j+1)
                else:
                    if j==1:
                        if i%2==0:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i+1][j].estado:
                                self.matriz[i+1][j].estado=True
                                self.abrir(i+1,j)
                            if not self.matriz[i][j+1].estado:
                                self.matriz[i][j+1].estado=True
                                self.abrir(i,j+1)
                        else:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i-1][j+1].estado:
                                self.matriz[i-1][j+1].estado=True
                                self.abrir(i-1,j+1)
                            if not self.matriz[i][j+1].estado:
                                self.matriz[i][j+1].estado=True
                                self.abrir(i,j+1)
                            if not self.matriz[i+1][j].estado:
                                self.matriz[i+1][j].estado=True
                                self.abrir(i+1,j)
                            if not self.matriz[i+1][j+1].estado:
                                self.matriz[i+1][j+1].estado=True
                                self.abrir(i+1,j+1)
                    elif j==self.colu:
                        if i%2==0:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i-1][j-1].estado:
                                self.matriz[i-1][j-1].estado=True
                                self.abrir(i-1,j-1)
                            if not self.matriz[i][j-1].estado:
                                self.matriz[i][j-1].estado=True
                                self.abrir(i,j-1)
                            if not self.matriz[i+1][j].estado:
                                self.matriz[i+1][j].estado=True
                                self.abrir(i+1,j)
                            if not self.matriz[i+1][j-1].estado:
                                self.matriz[i+1][j-1].estado=True
                                self.abrir(i+1,j-1)
                        else:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i][j-1].estado:
                                self.matriz[i][j-1].estado=True
                                self.abrir(i,j-1)
                            if not self.matriz[i+1][j].estado:
                                self.matriz[i+1][j].estado=True
                                self.abrir(i+1,j)
                    else:
                        if i%2==0:
                            if not self.matriz[i-1][j-1].estado:
                                self.matriz[i-1][j-1].estado=True
                                self.abrir(i-1,j-1)
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i][j-1].estado:
                                self.matriz[i][j-1].estado=True
                                self.abrir(i,j-1)
                            if not self.matriz[i+1][j].estado:
                                self.matriz[i+1][j].estado=True
                                self.abrir(i+1,j)
                            if not self.matriz[i+1][j-1].estado:
                                self.matriz[i+1][j-1].estado=True
                                self.abrir(i+1,j-1)
                            if not self.matriz[i][j+1].estado:
                                self.matriz[i][j+1].estado=True
                                self.abrir(i,j+1)
                        else:
                            if not self.matriz[i-1][j].estado:
                                self.matriz[i-1][j].estado=True
                                self.abrir(i-1,j)
                            if not self.matriz[i-1][j+1].estado:
                                self.matriz[i-1][j+1].estado=True
                                self.abrir(i-1,j+1)
                            if not self.matriz[i][j+1].estado:
                                self.matriz[i][j+1].estado=True
                                self.abrir(i,j+1)
                            if not self.matriz[i][j-1].estado:
                                self.matriz[i][j-1].estado=True
                                self.abrir(i,j-1)
                            if not self.matriz[i+1][j].estado:
                                self.matriz[i+1][j].estado=True
                                self.abrir(i+1,j)
                            if not self.matriz[i+1][j+1].estado:
                                self.matriz[i+1][j+1].estado=True
                                self.abrir(i+1,j+1)
    def reasignarmina(self, fil, col, nfil, ncol):#Mueve la mina de una posicion a otra, esto se aplica a la primera jugada.
        for i in range (1, fil-1):
            for j in range (1, col-1):
                if self.matriz[i][j].mina==False:
                    self.matriz[i][j].mina=True #se asigna la mina y se elimina de la otra
                    self.matriz[nfil][ncol].mina=False #nfil y ncol son la fila y columna de la casilla que ha sido pulsada
                    break 
            if self.matriz[nfil][ncol].mina==False:
                break
    def clickeado(self, widget, event, data=None):#Declaramos los eventos que deben ocurrir si click izquierdo o click derecho
        if self.juegoacabado:#si el juego acaba vuelve a empezar
            self.mensaje.set_text("El juego ha terminado, reinicia")
            self.juegaso.destroy()
            self.__init__()
        elif event.button==1:#CLICK IZQUIERDO
            if self.numjugada==1:
                self.tpo0=time.time()#Comienza a correr el cronometro
            if self.numjugada==1 & self.matriz[data[0]][data[1]].mina==True:
                """si no se ha marcado o abierto y la primera a abrir tiene mina, entonces se reubica la mina"""
                self.reasignarmina(data[2], data[3], data[0], data[1])
                self.numjugada+=1
            if self.matriz[data[0]][data[1]].marcado:
                self.mensaje.set_text("No se puede abrir una casilla marcada")
            else:
                self.matriz[data[0]][data[1]].estado=True
                self.mensaje.set_text("Casilla abierta")
                self.abrir(data[0],data[1])
            if self.matriz[data[0]][data[1]].mina:
                """Si hay mina, el juego acaba"""
                self.tpo0=-1
                self.mensaje.set_text("GAME OVER")
                self.juegoacabado=True
                self.matriz[data[0]][data[1]].estadoymina=True #marcador de que el jugador ha perdido y el metodo iconocasillas tiene que destapar todas las minas sin iconos mas que la propia mina
                self.nico=self.glade.get_object('nico')
                reinicio=gtk.gdk.pixbuf_new_from_file('nicolassad.jpg') #cambio de icono
                self.nico.set_image(gtk.Image())
                self.nico.get_image().set_from_pixbuf(reinicio)
                for i in range (1, self.fil-1):
                    for j in range(1, self.col-1):
                        self.matriz[i][j].estado=True
                        self.iconocasillas(None,i,j)
            self.numjugada+=1
        elif event.button==2:

            if self.matriz[data[0]][data[1]].estado:
                print "Casilla abierta"
            else:
                print "Casilla cerrada"
            if self.matriz[data[0]][data[1]].marcado:
                print "Casilla marcada"
            else:
                print "Casilla no marcada"
            print str(self.matriz[data[0]][data[1]].minascolindantes)+' minas colindantes\n'
        elif event.button==3:#CLICK DERECHO
            if self.matriz[data[0]][data[1]].marcado:
                self.minas+=1
                if self.matriz[data[0]][data[1]].mina:
                    self.mmarcadas-=1
                self.minasrest=self.minasrestantes.set_text("Minas: "+str(self.minas))
                self.matriz[data[0]][data[1]].marcado=False 
                self.mensaje.set_text("La casilla ha sido desmarcada")
                for i in range(1,self.fil-1):
                    for j in range(1,self.col-1):
                        self.iconocasillas(None, i, j)

            elif self.matriz[data[0]][data[1]].estado:
                self.mensaje.set_text("Casilla ya abierta")
            else:
                self.mensaje.set_text("Casilla marcada")
                self.minas-=1
                if self.matriz[data[0]][data[1]].mina:
                    self.mmarcadas+=1
                    if self.mmarcadas==self.marcadas:#Si todas las minas marcadas estan marcadas correctamente ganas la partida
                        self.tpo0=-1
                        self.mensaje.set_text("VICTORIA, click para reiniciar")
                        self.juegoacabado=True
                        for i in range (1, data[2]-1):
                            for j in range(1, data[3]-1):
                                if not self.matriz[i][j].estado:
                                    self.matriz[i][j].estado=True
                                    self.iconocasillas(None,i,j)
                        self.nico=self.glade.get_object('nico')
                        reinicio=gtk.gdk.pixbuf_new_from_file('nicolas.jpg')
                        self.nico.set_image(gtk.Image())
                        self.nico.get_image().set_from_pixbuf(reinicio)
                self.minasrestantes.set_text("Minas: "+str(self.minas))
                self.matriz[data[0]][data[1]].marcado=True
                for i in range(1,self.fil-1):
                    for j in range(1,self.col-1):
                        self.iconocasillas(None, i, j)
            self.numjugada+=1
    def tiempo(self):#CRONOMETRO
        if self.tpo0!=-1:#En init inicializamos tpo0 como -1, esto cambia cuando empiezas la partida y empieza a correr el reloj
            dt = int(time.time() - self.tpo0)
            self.timer.set_text("{0:02}:{1:02}".format(dt/60,dt%60))
        return True
    def numero_minas(self,i,j):#señala el numero de minas colindantes
        n=0
        if self.juegoacabado:
            return 0
        if i%2!=0:
            if self.matriz[i-1][j].mina:
                n+=1
            if self.matriz[i-1][j+1].mina:
                n+=1
            if self.matriz[i][j+1].mina:
                n+=1
            if self.matriz[i+1][j+1].mina:
                n+=1
            if self.matriz[i+1][j].mina:
                n+=1
            if self.matriz[i][j-1].mina:
                n+=1
        else:
            if self.matriz[i-1][j].mina:
                n+=1
            if self.matriz[i-1][j-1].mina:
                n+=1
            if self.matriz[i][j+1].mina:
                n+=1
            if self.matriz[i+1][j-1].mina:
                n+=1
            if self.matriz[i+1][j].mina:
                n+=1
            if self.matriz[i][j-1].mina:
                n+=1   
        if i%2!=0:
            if self.matriz[i-1][j].marcado:
                n-=1
            if self.matriz[i-1][j+1].marcado:
                n-=1
            if self.matriz[i][j+1].marcado:
                n-=1
            if self.matriz[i+1][j+1].marcado:
                n-=1
            if self.matriz[i+1][j].marcado:
                n-=1
            if self.matriz[i][j-1].marcado:
                n-=1
        else:
            if self.matriz[i-1][j].marcado:
                n-=1
            if self.matriz[i-1][j-1].marcado:
                n-=1
            if self.matriz[i][j+1].marcado:
                n-=1
            if self.matriz[i+1][j-1].marcado:
                n-=1
            if self.matriz[i+1][j].marcado:
                n-=1
            if self.matriz[i][j-1].marcado:
                n-=1      
        return n
    def iconocasillas(self,widget,i,j):#pone los simbolos de cada casilla
        self.matriz[i][j].minascolindantes=self.numero_minas(i,j)
        if self.matriz[i][j].estadoymina:
            self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[11])#mina explotando             
        elif self.matriz[i][j].marcado:
            self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[1])#marca            
        elif self.matriz[i][j].estado:
            if self.matriz[i][j].mina:
                self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[10])#mina
            else:
                if self.matriz[i][j].minascolindantes==0:
                    self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[3])#casilla vacia
                elif self.matriz[i][j].minascolindantes==1:
                    self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[4])
                elif self.matriz[i][j].minascolindantes==2:
                    self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[5])
                elif self.matriz[i][j].minascolindantes==3:
                    self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[6])
                elif self.matriz[i][j].minascolindantes==4:
                    self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[7])
                elif self.matriz[i][j].minascolindantes==5:
                    self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[8])
                elif self.matriz[i][j].minascolindantes==6:
                    self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[9])
                else:
                    self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[12])
        elif self.matriz[i][j].estado==False & self.matriz[i][j].marcado==False:
            self.matrizgrafica[i][j].get_image().set_from_pixbuf(self.iconos[0])           
        return None

    def encima(self, widget, data = None):#al pasar por encima de la casilla, te indica sus coordenadas
        self.cuadro.set_text(str(data))
        return None
    def on_window1_delete_event(self,widget):
        gtk.main_quit()


if __name__=='__main__':
    app=juego()
    gtk.main()