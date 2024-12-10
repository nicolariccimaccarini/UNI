close all
clear all
clc

A = [2, 1, 0; 1, 2, 1; 0, 1, 2];

% inversa con Gauss-Jordan
[Agj] = gaussJordan(A);

% inversa con LR
[L, R] = lu(A);

% Ax=I A=LR LRx=I => Rx=L^(-1) => x = L^(-1)*R(-1)
Linv = invupper(L');
Rinv = invupper(R);
ALR = Rinv * Linv'

% inversa con pivoting
[L, R, P] = lu(A);
Linv = invupper(L');
Rinv = invupper(R);
ALRP = Rinv * Linv * P

% inversa con Cholesky
L = cholesky(A);
Linv = invupper(L');
Achol = Linv * Linv'