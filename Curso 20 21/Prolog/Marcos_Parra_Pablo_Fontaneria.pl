:-op(40, xfy, &).
:-op(50, xfy, --->).
%Metainterprete base
solve(true):-!.
solve((A & B)) :-!, solve(A), solve(B).
solve(A) :- (B ---> A), solve(B).

%Base conocimiento fontaneria.

true ---> presion(p1).%p1 tiene presion
true ---> on(_).%todos los grifos estan abiertos
true ---> taponado(sink). %sink esta taponado
true ---> notaponado(bath).%bath no esta taponado

%hay presion si hay presion en p1 y los grifos estan abiertos
presion(p1) & on(t1) ---> presion(p2).
presion(p1) & on(t1) ---> presion(p3).

%hay flujo si hay presion y el grifo esta abierto
presion(p2) & on(t2) ---> flujo(shower).
presion(p3) & on(t3) ---> flujo(sink).

%hay agua si hay flujo
flujo(sink) ---> agua(sink).
flujo(shower) ---> agua(bath).

%flujo en las tuberias de desague
notaponado(bath) & agua(bath) ---> flujo(d2).
notaponado(sink) & agua(sink) ---> flujo(d3).

%hay flujo en d1 si hay flujo en d2 y d3 (d1 es bajante de ambas)
flujo(d2) ---> flujo(d1).
flujo(d3) ---> flujo(d1).

% el suelo esta mojado si algun elemento rebosa (taponado y con flujo)
taponado(sink) & flujo(sink) ---> agua(floor).
taponado(bath) & flujo(shower) ---> agua(floor).
