sucesion(0,2):-!. %Caso inicial S(0)=2
sucesion(N,X):- 0 is mod(N,2), N1 is N-1, sucesion(N1,X1), X is  X1*4. %Caso par, es decir, modulo/resto de N entre 2 es igual a 0
sucesion(N,X):- 1 is mod(N,2), N1 is N-1, sucesion(N1,X1), X is X1*4-2. %Caso impar, es decir, modulo/resto de N entre 2 es igual a 1
