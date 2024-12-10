clolse all
clear all
clc

A = [1, 0, 1; 0, 1, 1; 2, -1, 1];
b = [1, 0, 1]'; % lo mettiamo trasposto perche' e' un vettore colonna

[L, R, deter] = gauss1(A);

% Ax = b --> ma grazie alla fattorizzazione A = LR --> LRx = b --> RX = Lb
% Lb = y   Rx = y

y = sollower(L, b);
x = solupper(R, y);