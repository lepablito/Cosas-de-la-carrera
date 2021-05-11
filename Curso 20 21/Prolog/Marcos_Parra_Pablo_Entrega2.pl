/*Declaramos los operadores*/
:-op(40, xfy, &).
:-op(50, xfy, --->).
:-dynamic asked/1, answered/2. 

/*Tabla predicados predefinidos*/
built_in(_ is _). built_in(_ > _). built_in(_ < _).
built_in(_ = _). built_in(_ =:= _). built_in(_ =< _).
built_in(_ >= _). built_in(functor(_, _, _)).
built_in(read(_)). built_in(write(_)).

/* Resolver y eliminar respuestas al final de la consulta */
preguntas(G):- psolve(G),!, limpiar. /*Si éxito */
preguntas(_):-limpiar,fail. /* Si fallo */
/* Eliminar respuestas almacenadas en una consulta */
limpiar:- retract(asked(_)), fail.
limpiar:- retract(answered(_,_)),fail.
limpiar:-!.

psolve(true):-!.
psolve((A & B)) :-!, psolve(A), psolve(B).
psolve(A):- built_in(A), !, A.
psolve(A) :- (B ---> A), psolve(B).

/*Miramos si ya esta hecha y respondida la pregunta*/
psolve(A):-asked(A),answered(A,_).

/*Realiza la pregunta y la guarda en asked y answered*/
psolve(A) :- pregunta(A), preguntar(A,Res), Res=si, assertz(asked(A)), assertz(answered(A,Res)).
preguntar(A,Res):-mostrar_pregunta(A), read(Res).
mostrar_pregunta(A) :- nl,write('¿ '), write(A), write(' ?:').


/*Base de conocimiento*/

pregunta(ok(_)).

/* regla para las lamparas/luces */
light(L) & ok(L) & live(L) ---> lit(L).

/* conexion de cables a la tension */
connected_to(W,W1) &  live(W1) ---> live(W).

/* hechos básicos: que sabemos del problema */
true ---> live(outside).
true ---> light(l1).
true ---> light(l2).
true ---> down(s1).
true ---> up(s2).
true ---> up(s3).
true ---> connected_to(l1,w0).

up(s2) & ok(s2) ---> connected_to(w0,w1).
down(s2) & ok(s2) ---> connected_to(w0,w2).
up(s1) & ok(s1) ---> connected_to(w1,w3).
down(s1) & ok(s1) ---> connected_to(w2,w3).
true ---> connected_to(l2,w4).
up(s3) & ok(s3) ---> connected_to(w4,w3).
true ---> connected_to(p1,w3).
ok(cb1) ---> connected_to(w3,w5).
true ---> connected_to(p2,w6).
ok(cb2) ---> connected_to(w6,w5).

true ---> connected_to(w5,outside).
