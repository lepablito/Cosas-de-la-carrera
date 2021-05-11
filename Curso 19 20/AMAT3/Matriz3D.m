%% Práctica 5_2: Representación de matrices en 3D


function[]=Matriz3D(A);
    [m,n]=size(A)
    % Se intercalan filas y columnas de ceros entre cada fila y columna de A
    % Se obtiene así una matriz de dimensión (2m+1) x (2n+1)
    % La visualización con la función mesh mejora
    AA=zeros(2*m+1,n);AA(2:2:2*m,:)=A;
    A=zeros(2*m+1,2*n+1);A(:,2:2:2*n)=AA;
    % se representan los valores de la matriz en 3D tomando como alturas
    % los valores que tiene la matriz
    figure
    x=0:2*n; y=0:2*m;
    mesh(x,y,abs(A(2*m+1:-1:1, 2*n+1:-1:1)))
    % axis([0 2*n 0 2*m])
     axis off
    %pause(1)
end