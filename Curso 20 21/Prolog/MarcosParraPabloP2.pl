multiplicar_lista([],[]).%Caso base con lista vacia
multiplicar_lista([X],[X]).%Caso en el que solo queda un elemento por multiplicar, la respuesta es el propio elemento
multiplicar_lista([X1,X2|Cola],L):-multiplicar_lista(Cola, Z), S is X1*X2, append([S], Z, L).%Caso general, cogemos los dos primeros elementos de la cabeza y los multiplicamos, finalmente metemos la respuesta de la multiplicacion con append en la lista respuesta
