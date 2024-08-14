close all;
clear all;
clc;

disp('Esercizio 3');

A = [1 -1 2 0; -1 2 0 4; 0 5 1 -2; 0 0 2 1];
b = [0 2 -1 0];
xTeoria = [-14 8 11 -22]'/15;

% prima parte
[L, R, p, detA] = gauss2(A);
x1 = solupper(R, sollower(L, b(p)));
% oppure: y1 = sollower(L, b(P)); x1 = solupper(R, y1)
disp('Massima differenza in modulo dalla soluzione teorica:');
disp(max(abs(x1 - xTeoria)));
disp('Residuo normalizzato (in norma infinito):');
disp((b - A*x1) / norm(b, inf));

% seconda parte
B = A' * A;
disp('Fattorizzazione di Cholesky:');
[Lchol, p] = chol(B, 'lower'); % Lchol e' triangolare inferiore
if (~p) 
    fprintf('\nB e'' definita positiva\n');
else 
    error('\nB non e'' definita positiva\n');
end 

c  = [7/3, 1, 2, -5/6];
x2 = solupper( Lchol', sollower(Lchol, c))