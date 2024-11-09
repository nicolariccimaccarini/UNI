close all; clear all; clc;
disp('Esercizio 1');

p7 = zeros(8, 1);
p7(1) = abs(sin(pi^(3.06)) - min(3/pi, 2/exp(1)));              % c7
p7(3) = log10(tan(0.4998 * pi));                                % c5
p7(4) = (23/5) - acos(-sqrt(pi/7));                             % c4
p7(6) = exp(-5.3 * sin(0.25 + 3 * pi)) - factorial(6)/5^(4);    % c2
p7(7) = max([5.09E-02; 3/17; log(2^6); tan(2.83)]);             % c1
p7(8) = log(log10(log2(13.07^185)));                            % c0

x0 = input('Inserire il valore nel quale valutare il proprio polinomio (double) x0 = ');

[r, q]     = ruffiniHorner(p7, x0);
[derp, q1] = ruffiniHorner(q, x0);
[ders, q2] = ruffiniHorner(q1, x0);
fprintf('\nValore del polinomio in x0: p(x0) = %g', r);
fprintf('\nValore della derivata prima in x0: p''(%g) = %g', x0, derp);
fprintf('\nValore della derivata seconda in x0: p''''(%g) = %g\n\n', x0, 2*ders);

ph = fplot(@(x)(polyval(p7, x)), [-1.2 0.1]);
