close all; clear all; clc;
disp('Esercizio 1');

p6 = zeros(7, 1);
p6(1) = -(exp(-pi));
p6(2) = -1.4;
p6(3) = 1;
p6(5) = -log(207.13);
p6(7) = 3 * pi;

x0 = input('Inserire il punto nel quale valurare il polinomio (double): x0 = ');

[r, q]    = ruffiniHorner(p6, x0);
[derp, q1] = ruffiniHorner(q, x0);
[ders, q2] = ruffiniHorner(q1, x0);

fprintf('\nValore del polinomo in x0: p(%g) = %g', x0, r);
fprintf('\nValore della derivata prima in x0: p''(%g) = %g', x0, derp);
fprintf('\nValore della derivata seconda in x0: p''''(%g) = %g\n\n', x0, 2*ders);

ph = fplot(@(x)(polyval(p6, x)), [0.8, 1.7]);
