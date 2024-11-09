close all; clear all; clc
disp('Esercizio 1');

p = [3, -9, 0, 11, -pi, 0, -2]';
x0 = input('Inserisci il punto (numero reale) nel quale valutare il polinomio: x0 = ');

[r, q] = ruffiniHorner(p, x0);
[r1, q1] = ruffiniHorner(q, x0);
[r2, q2] = ruffiniHorner(q1, x0);

fprintf('\nValore del polinomio in x0: p(x0) = %g', r);
fprintf('\nValore della derivata prima in x0: p''(x0) = %g', r1);
fprintf('\nValore della derivata seconda in x0: p''''(x0) = %g\n', 2*r2);