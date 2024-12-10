close all; clear all; clc;
disp('Esercizio 3');

format rational
A = [1, -2, 4; -3, 2, 0; 0, -5, 3];
b = [-3, 1, 0]';
xTeoria = -(1/59) * [ 25 , 8 , 40 ]';

[L, R, p, q, detA] = gauss_pivtot(A)
c = ltrisol(L, b(p))
xs = utrisol(R, c)
Q = eye(3); Q = Q(:, q); xs = Q*xs;

format default
disp('Residuo normalizzato in norma infinito:');
r = (b - A*xs) / norm(b, 'inf')

% Soluzione del sistema Bx = d
B = A * A'; d = [1, -2, 6]';
disp('FAttorizzazione di Cholesky di B:');
[L1, flagB] = chol(B, 'lower');
if ( ~flagB )
    fprintf('\nB e'' definita positiva . Soluzione mediante alg . di Cholesky :\n') ;
    x2 = utrisol(L1' , ltrisol(L1 , d))
else
    fprintf('\nB non e'' definita positiva .') ;
    fprintf('\nSoluzione mediante fattorizzazione QR:') ;
    [Q1, R1] = qr(B);
    x2 = utrisol(R1, Q1'*d)
end