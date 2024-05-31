close all
clear all
clc

f = @(x) x^3-x-1;
gname = @(x) x-(x.^3-x-1)/(3*x.^2-1);   % x - f(x)/f'(x)
x0 = 1.6;
tol = 1e-3;
maxit = 10;
[x, it] = iterazione(gname, x0, tol, maxit);
fprintf('\nLa soluzione vale %e\n', x);
fprintf('Numero di iterazioni %d\n', it);