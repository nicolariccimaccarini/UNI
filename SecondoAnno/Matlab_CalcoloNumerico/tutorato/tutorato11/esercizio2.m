close all
clear all
clc

x  = linspace(0, 1.7, 1000);
f  = @(x) x.^2 - 10*x.^2 + 29*x -20;
fp = 3*x.^2 - 2*x + 29;

plot(x, f(x))

c         = (0 + 1.7)/2;
toln      = 1e-9;
tol       = 1e-3;
maxit     = 100;
[xn, itn] = newton(f, fp, c, toln, tol, maxit);
x0        = 0;
x1        = c;
[xs, its] = secanti(f, x0, x1, tol, tol, maxit);

fprintf('Soluzione con Newton = %e e numero di iterazioni = %d\n', xn, itn);
fprintf('Soluzione con Secanti = %e e numero di iterazioni = %d\n', xs, its);
