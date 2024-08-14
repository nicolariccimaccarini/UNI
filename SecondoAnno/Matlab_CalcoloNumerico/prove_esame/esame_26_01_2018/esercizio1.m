close all
clear all
clc

disp('Esercizio 1');

% semplificazione del polinomio
% 3(x-2)^2(x+1)x^3-(x+pi)x^2-2
% 3(x^2+4-4x)(x+1)x^3 -x^3 -pix^2 -2
% (3x^2+12-12x)(x^4+x^3)-x^3-pix^2-2
% 3x^6+3x^5+12x^4+12x^3-12x^5-12x^4-x^3-pix^2-2
% 3x^6 -9x^5 +0x^4 +11x^3 -pix^2 +0x -2
p = [3 -9 0 11 -pi 0 -2];

x0 =  input('Inserire ilil punto (numero reale) nel quale valutare il polinomuio x0 = ');

% valore del polinomio in x0
[r1, q1] = ruffiniHorner(p, x0);
% valore della derivata prima in x0
[r2, q2] = ruffiniHorner(q1, x0);
% valore della derivata seconda in x0
[r3, q3] = ruffiniHorner(q2, x0);

% output dei valori
fprintf('\nValore del polinomio in x0: p(x0) = %g\n', r1);
fprintf('\nValore della derivata prima in x0: p''(x0) = %g\n', r2);
fprintf('\nValore della derivata seconda in x0: p''''(x0) = %g\n', 2*r3);