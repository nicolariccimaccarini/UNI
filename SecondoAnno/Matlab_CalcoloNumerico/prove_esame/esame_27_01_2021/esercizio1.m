close all;
clear all;
clc;

disp('Esercizio 1');

p6 = zeros(7, 1);

p6(1) = log10(pi^(7/4) * max(exp(2)-5, tan(0.5)));  % c6
p6(2) = cos((abs(-0.7 * exp(-0.2)))^(1/3));         % c5
p6(4) = -sin(0.2 + exp(-1.6));                      % c3
p6(6) = acos(3.7e-1) + 0.25;                        % c1
p6(7) = log(sqrt(pi^3) + 2/3);                      % c0

x0 = input('Inserire il punto nel quale valutare il proprio polinomio (double): x0 = ');
[r, q] = ruffiniHorner(p6, x0);
[derp, q1] = ruffiniHorner(q, x0);
[ders, q2] = ruffiniHorner(q1, x0);
fprintf('\n Valore del polinomio in x0: p(%g) = %g', x0, r);
fprintf('\n Derivata prima del polinomio in x0: p''(%g) = %g', x0, derp);
fprintf('\n Derivata seconda del polinomio in x0: p''''(%g) = %g', x0, 2*ders);
fh = fplot(@(x)(polyval(p6,  x)), [-1.5, 1.2]);