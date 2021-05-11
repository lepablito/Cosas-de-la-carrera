:-op(40, xfy, &).
:-op(50, xfy, --->).

qpsolve(true, 0) :- !.
qpsolve((A & B), Z) :- !, qpsolve(A, X), qpsolve(B, Y), mayor(X, Y, Z).
qpsolve(A, M) :- (B ---> A), qpsolve(B, N), !, M is N+1.

mayor(A, B, R) :- A >= B, !, R is A; !, R is B.


% Declaraciones verdaderas
true ---> live(outside).			% El cable externo tiene tensión
true ---> light(l1).				% l1 es una bombilla
true ---> light(l2).				% l2 es una bombilla
true ---> down(s1).				% El interruptor s1 está abierto
true ---> up(s2).				% El interruptor s2 está cerrado
true ---> on(s3).				% El interruptor s3 está encendido
true ---> connected_to(w5,outside).		% El cable w5 está conectado al cable externo
true ---> connected_to(p1,w3).			% El enchufe p1 está conectado al cable w3
true ---> connected_to(p2,w6).			% El enchufe p2 está conectado al cable w6
true ---> connected_to(l1,w0).			% La bombilla l1 está conectada al cable w0
true ---> connected_to(l2,w4).			% La bombilla l2 está conectada al cable w4
true ---> ok(_).

					% Circuit breakers
ok(cb1) ---> connected_to(w3,w5).		% Si cb1 funciona correctamente entonces w5 está conectado a w3
ok(cb2) ---> connected_to(w6,w5).		% Si cb2 funciona correctamente entonces w5 está conectado a w6

					% Switchs
up(s1) & ok(s1) ---> connected_to(w1,w3).	% Si el interruptor s1 está abierto y funciona correctamente entonces el cable w1 está conectado al cable w3
down(s1) & ok(s1) ---> connected_to(w2,w3).	% Si el interruptor s1 está cerrado y funciona correctamente entonces el cable w2 está conectado al cable w3
up(s2) & ok(s2) ---> connected_to(w0,w1).	% Si el interruptor s2 está abierto y funciona correctamente entonces el cable w0 está conectado al cable w1
down(s2) & ok(s2) ---> connected_to(w0,w2).	% Si el interruptor s2 está cerrado y funciona correctamente entonces el cable w0 está conectado al cable w2
on(s3) & ok(s3) ---> connected_to(w4,w3).	% Si el interruptor s3 está encendido y funciona correctamente entonces el cable w4 está conectado al cable w3

					% Condiciones generales
light(L) & ok(L) & live(L) ---> lit(L).		% Si una bombilla funciona correctamente y le llega tensión, entonces se enciende
connected_to(W,W1) & live(W1) ---> live(W).	% Si un cable está conectado a otro al que le llega tensión, entonces tiene tensión