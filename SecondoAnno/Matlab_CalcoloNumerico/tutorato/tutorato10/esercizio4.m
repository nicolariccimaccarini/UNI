close all
clear all
clc

x = linspace(0, 3, 100);
f = @(x) 2*cos(x) - exp(x) + 5/2;
plot(x, f(x))
title('Grafico della funzione');

x0 = -0.3;
tol = 1e-6;
maxit = 100;
fp = @(x) -exp(x) - 2*sin(x);
[xn, it] = newton(f, fp, x0, tol, tol, maxit);
fprintf('Vaqlore della radice = %e\n', xn);
fprintf('Iterazioni usate = %d\n', it);

alpha = 1.1814639003600901;
err = abs(xn - alpha);
fprintf('Errore assoluto = %e\n', err);