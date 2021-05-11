function m = f1(A,i,j)
%Operación elemental F1 por filas
%   Intercambia fila i por fila j
I=A(i,:);
J=A(j,:);
A(i,:)=J;
A(j,:)=I;
m=A;
end

