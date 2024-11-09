close all; clear all; clc;
disp('Esercizio 1');

p6 = zeros(7, 1);

p6 (1) = -cos( 0.3 + exp (1.7) ) ;                                  % c6
p6 (2) = sin( -( 57.3 * exp (2.7) )^(1/5) ) ;                       % c5
p6 (4) = 13/7 - asin ( 0.24*pi ) ;                                  % c3
p6 (6) = log2 ( sqrt (pi) * max (exp (7) -3 , tan (12.8) ) ) ;      % c1
p6 (7) = min( [log(pi^3) , 3/2 , exp(3.5) ]' ) ;                    % c0

x0 = input('Inserire il punto nel quale valutare il polinomio (double): x0 = ');

[r, q] = ruffiniHorner(p6, x0);
[derp, q1] = ruffiniHorner(q, x0);
[ders, q2] = ruffiniHorner(q1, x0);

fprintf('\n Valore del polinomio in x0 = p(%g) = %g', x0, r);
fprintf('\n Derivata prima del polinomio in x0 = p''(%g) = %g', x0, derp);
fprintf('\n Derivata seconda del polinomio in x0 = p''''(%g) = %g \n\n', x0, 2*ders);

fh = fplot(@(x)(polyval(p6, x)), [-0.75, 2.0]);