close all
clear all
clc

A = [1, 0, 2; 1, 3, 1; 5, 2, -1];
b = [3, 5, 6]';

[L, R, P, deter] = gauss2(A);   
% gauss2 si occupa di fare il pivoting parziale

% risolvo Ax = b ma io so che PA = LR
% PAx = Pb --> LRx = Pb --> Rx = LPb
% definisco y = LPb e Rx = y

y = sollower(L, b(P));
x = solupper(R, y);

% residuo in norma infinito normalizzato
rnin = (b - A*x)/norm(b, inf);

fprintf('residuo: %g\n', rnin);
