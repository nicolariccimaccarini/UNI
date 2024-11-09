close all; clear all; clc;
disp('Esercizio 5');

rng(5); % inizializzazione del seme per il generatore di numeri pseudo-casuali
a = -1.9; b = -0.6; N = 11; n = 3;
x = rand(N, 1) * (b-a) + a;
f = @(x)((log(x + 2) - 3) .* cos(-pi * x));
y = f(x);

% costruzione esplicita del sistema delle equazioni normali e sua soluzione
A = [ x.^[3, 2], x, ones(N, 1)];
B = A' * A; c = A' * y;
q3 = B \ c;

% controllo della soluzione
q3check = polyfit(x, y, n)';
disp('Massima differenza in modulo fra i coefficienti dei due polinomi:');
disp(norm(q3 - q3check, inf));

xx = linspace(a, b, 201)';
yy = f(xx);
zz = polyval(q3, xx);

% grafico delle curve e dei dati
ph = plot(xx, yy, '-b', xx, zz, '-r', x, y, 'ok');
xlim([a, b]);
xh = xlabel('x'); yh = ylabel(sprintf('valori di f(x) e q_%d^*(x)', n));
th = title(sprintf('Funzione, campionamento e cubica approssimate su %d nodi', N));
lh = legend({'funzione', ...
             sprintf('polinomio ai minimi quadrati di grado %d', n), ...
             'dati campionati'}, ...
             'Location','southeast');