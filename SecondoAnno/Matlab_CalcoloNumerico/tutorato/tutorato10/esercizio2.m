close all
clear all
clc

a = 2;
b = 3;
tol = 1e-4;
x = linspace(a, b);
f = @(x) x.^3-25;
[xsol, fc, it] = bisezione(f, a, b, tol);
fprintf('numero di iterazioni = %d e soluzione = %e\n', it, xsol);