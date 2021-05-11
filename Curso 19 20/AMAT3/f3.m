function m = f3(A,j,i,alpha)
%Operación por filas f3
%   f(j)--> f(j)+alpha*f(i)
A(j,:)=A(j,:)+alpha*A(i,:);
m=A;
end

