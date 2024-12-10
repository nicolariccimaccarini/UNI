close all
clear all
clc

x0 = 0;
x = linspace(-1, 1);
f  = @(x) x.^2 + 2*x.*exp(x) + exp(2*x);
f1 = @(x) 2.*(exp(x)+1).*exp(2*x) + 2;
f2 = @(x) 2.*exp(x).*4.*exp(2*x) + 2;
plot(x, f(x));
title('Funzione');
tol = 1e-1;
maxit = 10000000;
[xh, ith] = halley(f, f1, f2, x0, tol, tol, maxit)
m = 2;
[xn, tn] = newton_multipli(f, f1, x0, tol, tol, maxit, m)