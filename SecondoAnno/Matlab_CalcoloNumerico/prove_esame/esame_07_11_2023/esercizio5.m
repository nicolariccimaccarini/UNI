close all; clear all; clc;
disp('Esercizio 5');

f = @(x)(exp(-pi*(x-1).^2) / pi);
a = 1; b = 3; N = 10;

% prima parte: verifica dei soli risultati della parte teorica
x = 1 : 0.5: 3; y = f(x);

% verifica della diagonale della tabella delle differenze divise, ossia dei
% coefficienti del polinomio interpolante nella forma di Newton
[d] = tabDiff(x, y);

% verifica dei coefficienti della forma canonica del polinomio interpolante
% di grado al piu' 4 nei nodi equispaziati
p4 = polyfit(x, y, 4);

fprintf('\nDiagonale principale della tabella delle differenze divise:\n');
disp(d);
fprintf('\nCoefficienti della forma canonica del polinomio p4(x):\n');
disp(p4);

% seconda parte: polinomi di Lagrange e costruzione dei grafici
xx = linspace(a, b, 201)';
yy = f(xx);
fig1 = figure(1);
for n = 1 : N
    x = linspace(a, b, n+1)';
    y = f(x);
    [zz, ~] = polyLagrange(x, y, xx);
    ph = plot([xx(1); xx(end)], [0; 0], 'k-', ...
        xx, yy, 'b-', ...
        xx, zz, 'r-', ...
        xx, zeros(size(x)), 'ko', ...
        x, y, 'ro');
    th = title(sprintf('Funzione e polinomio interpolante di grado %d', n));
    set(th, 'FontSize', 16);
    xh = xlabel('Intervallo di interpolazione');
    yh = ylabel('Valori di f(x) e p_n(x)');
    pause(1);
end