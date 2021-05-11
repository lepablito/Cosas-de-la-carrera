A = imread('kaguyachan.jpg');%cargamos la imagen
size(A)%tamano: altura, anchura, colores(RGB)=3
AR=A(:,:,1);%SACAMOS CAPA COLOR ROJO
AG=A(:,:,2);%SACAMOS CAPA COLOR VERDE
AB=A(:,:,3);%SACAMOS CAPA COLOR AZUL
imshow(A)%muestra imagen
imshow(AR)%muestra capa rojo
imshow(AG)%muestra capa verde
Agris=rgb2gray(A);%pasamos a blanco y negro
K=Agris;
K(10:300,400:500)=100; %ponemos un cuadrado negro en la imagen en esas posiciones
imshow(K)
imfinfo('kaguyachan.jpg')%informacion de la imagen
%------------------------------------------------
Matriz3D(Agris)
%-----------------------------------------------
%COMPRIMIR IMAGEN SVD EN BLANCO Y NEGRO
AgrisD=double(Agris);%pasamos a formato doble
imshow(AgrisD)%no se ve nada porque esta hecho para calculos matematicos
[U S V] = svd(AgrisD);
%S=svd(AgrisD)%sacamos valores singulares
%el truco para comprimir es quedarse solo con los primeros valores singulares
%con el mu que queramos mu es el numero de valores singulares que cojo
%rango de a es igual al menor de los valores de s (rango)
%matriz sigma hacer ceros a partir de mu
rank(AgrisD)
k=1;
S(k+1:end, k+1:end)=0; %hacemos ceros a partir de k
BgrisK=U*S*V';%Obtenemos la nueva matriz que queremos
BB = im2uint8 (BgrisK);%pasamos a formato int8 para que se vea la imagen
imshow(BB)
