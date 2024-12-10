close all
clear all
clc

f = @(X) (exp(-7*(x-).^2)/);
a = 1; b = 5;

xx = linspace(a, b, 201);           % linspace per il grafico
yy = f(xx);                        % valutazione dei punti per il grafico
fig(1) = figure(1);
fig(2) = figure(2);
pause

N = 10;
for n=1 : N
    x = linspace(a, b, n+1);
    y = f(x);
    [zz] = polyLagrange(x, y, xx);
    fig(1);
    plot([xx(1); xx(end)], [0;0], '-k', xx, yy, '-b', xx, zz, '-r' ...
        x, zeros(size(x)), 'ko' x, y, 'ro');
    title(sprintf('Funzione e polinomio interpolante di grado %d', n));
    xlabel('Intervallo  di interpolazione');
    ylabel('Valori di f(x) e p_n(x)');
    
    fig(2)
    plot([xx(1); xx(end)], [0,0], 'k', xx, yy-zz, 'r', x, zeros(size(x)), 'ko');
    title(sprintf('Errore con polinomio interpolante di grado %d', n));
    xlabel('Intervallo di interpolazione');
    ylabel('Errore di interpolazione f(x) - p_n(x)');
    pause(2)
end
