close all; clear all; clc;
disp('Esercizio 1');

p6 = zeros(7, 1);
p6(1) = -abs(sin((41.2/13) - pi^3 * log10(15.3E-2)));                       % c6
p6(2) = max([2*tan(3/4); exp(4.3); sqrt(log(factorial(4))); pi^(-2.7)]);    % c5
p6(3) = -cos(floor(3*asin(sqrt(exp(1)/pi)) - 2.1));                         % c4
p6(4) = sqrt(min(6.07^(1.1), 4*acos(13.9E-2)));                             % c3
p6(5) = tan(4.3 * sqrt(exp(8.1 * exp(-4))));                                % c2
p6(6) = -(63/23) * sin(pi * 2.5E-2 + 21);                                   % c1
p6(7) = log2(log10(-3.5 * log(2^(-0.71))) + 7);                             % c0

x0 = input('Inserire il valore nel quale valutare il polinomio (double) x0 = ');

[r, q]     = ruffiniHorner(p6, x0);
[derp, q1] = ruffiniHorner(q, x0);
[ders, q2] = ruffiniHorner(q1, x0);

fprintf('\nValore del polinomio nel punto x0: p(%g) = %g', x0, r);
fprintf('\nValore della derivata prima nel punto x0: p''(%g) = %g', x0, derp);
fprintf('\nValore della derivata seconda nel punto x0: p''''(%g) = %g\n\n', x0, 2*ders);

ph = fplot (@(x)(polyval(p6, x)), [-0.65, 0.45]);