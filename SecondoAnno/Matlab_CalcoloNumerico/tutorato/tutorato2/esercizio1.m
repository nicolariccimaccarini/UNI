close all
clear all
clc

% 3x^6 - 9x^5 + 11x^3 - pix^2 - 2

p = [3 -9 0 11 -pi 0 -2]';
x0 = input('inserire il punto: x0 = ');

[r, q] = ruffini_horner(p, x0);     % valore funzione in x0
[r2, q2] = ruffini_horner(q, x0);   % calcolo della derivata prima
[r3, q3] = ruffini_horner(q2, x0);  % parte della derivata seconda

fprintf('\n Valore del polinomio in x0 = %g', r);
fprintf('\n Valore della derivata prima in xo = %g', r2);
fprintf('\n Valore della derivata seconda in x0 = %g', 2*r3);