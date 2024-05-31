close all
clear all
clc

x = [0.1, 0.4, 1.1, 1.5];                   % nodi
y = [1.6094, 0.2231, -0.7885, -1.0986];     % f nei nodi
punto = 1.2;

[p, coeff] = polyLagrange(x, y, punto);

fpunto = -log(2*punto);
fprintf('L'' errore vale: %g\n', abs(fpunto - p));

punti = [0.2, 0.9, 2.7];
[p, coeff] = polyLagrange(x, y, punti);
fx1 = -log(2*punti);
fprintf('Errori relativi commessi: %g\n', abs(p - fx1)/abs(fx1));    