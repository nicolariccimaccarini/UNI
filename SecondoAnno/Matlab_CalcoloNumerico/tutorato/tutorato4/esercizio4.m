close all
clear all
clc

A = [1, 3, -1; 2, 4, 1; 3, 6, -2];
b = [2, 7, 7]';

[L, R, P, Q] = gauss_pivtot(A)

fprintf("Soluzione con pivoting totale\n");
% Ax = b --> PAQ = LR --> PAQx = PbQ
% Lrx = PbQ --> Rx = LPbQ
% LPbQ = y  Rx = y

y = sollower(L, P*b);
x = solupper(R, y);
x = Q * x

fprintf("Residuo normalizzato: %g\n", (b - A*x)/norm(b, inf));

[LL, RR, PP, QQ] = lu(A)
yy = sollowe(LL, PP*B);
xx = solupper(RR, yy);
fprintf('Funzione con matlab:\n')
xx = QQ * x
    
fprintf("Residuo normalizzato con matlab: %g\n", (b - A*xx)/norm(b, inf));