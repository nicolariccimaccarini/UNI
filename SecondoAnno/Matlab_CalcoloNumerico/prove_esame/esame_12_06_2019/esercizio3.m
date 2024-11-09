close all; clear all; clc;
disp('Esercizio 3');

% prima parte
A = [4, -2, 1, 0; -6, 4, 5/2, 0; 1, 5/2, -6, 1; 0, 0, 1, 4];
b = [0, 2, -6, 0]';
[L, R, p] = gauss2(A);
disp('Soluzione mediante fatt. LR con pivoting parziale:');
x1 = solupper(R, sollower(L, b(p)));

fattLU = (b - A*x1) / norm(b, 'inf');
fprintf('\nResiduo normalizzato con fattorizzazione LU: %g\n\n', fattLU);

% seconda parte
disp('Rotazione di Givens:');
[Q1 , y1] = planerot( A (1:2 , 1) ) ;
G12 = eye(4) ; G12(1:2 , 1:2) = Q1
A1 = G12*[ A , b ]
[ Q2 , y2 ] = planerot( A1 ([1 ,3] , 1) ) ;
G13 = eye(4) ; G13([1 ,3] , [1 ,3]) = Q2
A2 = G13 * A1
[ Q3 , y3 ] = planerot( A2 (2:3 , 2) ) ;
G23 = eye(4) ; G23(2:3 , 2:3) = Q3
A3 = G23 * A2
[ Q4 , y4 ] = planerot( A3 (3:4 , 3) ) ;
G34 = eye (4) ; G34(3:4 , 3:4) = Q4
A4 = G34 * A3
disp ('Fattorizzazione QR con Givens :');
R1 = triu( A4(: ,1:4) ) % A4 (: , end ) = Q ’*b
Q = ( G34 * G23 * G13 * G12 )'
disp ('Soluzione mediante fattorizzazione QR:') ;
x2 = solupper( R1 , A4 (: , end ) )
fprintf ('\nResiduo normalizzato con fattorizzazione QR:') ;
fprintf ('\n%g', ( b - A * x2 ) / norm (b , inf ) ) ;
fprintf ('\n\n') ;