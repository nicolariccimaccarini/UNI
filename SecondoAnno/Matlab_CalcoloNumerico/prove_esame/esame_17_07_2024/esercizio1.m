close all; clear all; clc;
disp('Esercizio 1');

p7 = zeros(8, 1);

p7(1) = -abs(max(0.25*exp(-1.2), 0.77*pi^1.06) - log2(5/pi));          % c7
p7(2) = 0.025 * log(0.21^(-0.07) + 4);                                 % c6
p7(4) = acos(tan(0.64) - log10(sin(2*pi^2) - 3.43E-2));                % c4
p7(6) = -ceil(0.005E+03 * pi/23);                                      % c2
p7(7) = 15.4 - sqrt(7.32 * asin(exp(3) / tan(-1.611)));                % c1
p7(8) = min([0.6*pi^(-3), -sqrt(3.1), -8.51E-7]);                      % c0

x0 = input('Inserire il valore nel quale valutare il polinomio (double): x0 = ');

[r, q]     = ruffiniHorner(p7, x0);
[derp, q1] = ruffiniHorner(q, x0);
[ders, q2] = ruffiniHorner(q1, x0);

fprintf('\nValore del polinomio in x0: p(%g) = %g', x0, r);
fprintf('\nValore della derivata prima in x0: p''(%g) = %g', x0, derp);
fprintf('\nValore della derivata seconda in x0: p''''(%g) = %g\n\n', x0, 2*ders);

ph = fplot(@(x)(polyval(p7, x)), [-1.5, 1.2]);