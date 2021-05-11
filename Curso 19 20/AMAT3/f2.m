function m = f2(A,i,alpha)
%Operación por filas 2
%   multiplica la fila i de la matriz A por alpha
A(i,:)=alpha*A(i,:);
m=A;

end

