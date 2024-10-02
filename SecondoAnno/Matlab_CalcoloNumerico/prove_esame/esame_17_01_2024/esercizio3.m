close all ; clear all; clc;
disp ('Esercizio 3') ;
%
format rat
A = [ -1 , 4 , -3/2; 2 , 1/2 , 5; -3 , 3/4 , -2]; b = [ -1 , 0 , 2]';
[ Lpar , Rpar , ppar ] = lu(A , 'vector') ; % N.B.: senza ’vector ’ , ppar e’ una matrice !!
disp ('Soluzione mediante fatt . LR con pivoting parziale :') ;
xLU = utrisol ( Rpar , ltrisol ( Lpar , b ( ppar ) ) )
detA = prod ( diag ( Rpar ) ) % N.B.: SOLO perche ’ in questo caso det (P) = 1

format short
residuo = b - A * xLU ;
normaInfTermNoto = norm (b , inf ) ;
fprintf '\ nResiduo normalizzato in norma infinito :\n') ;
disp ( residuo / normaInfTermNoto ) ;

format rat
% seconda parte : fattorizzazione QR
disp ('Soluzione mediante fatt . QR:') ; % ATTENZIONE : qr () non lavora come lu () !!
[Q , R ] = qr( A ) ; % Se si invoca [Q,R,P] = qr(A), P permuta COLONNE , quindi
xQR = utrisol ( R , Q'* b ) % occorre permut . la sol ., non b: xQR = P * utrisol (R, Q '*b)

format short
% controllo dei risultati ottenuti nel quesito teorico
xTeoria = [ -37 , -12 , 16]' / 35;
fprintf ('\ nDifferenze relative :') ;
fprintf ('\n| xLU - xTeoria | ./ | xTeoria |\n| xQR - xTeoria | ./ | xTeoria | :\n') ;
diffRel = abs( [ xLU - xTeoria , xQR - xTeoria ] ) ...
            ./ repmat ( abs ( xTeoria ) , 1 , 2)