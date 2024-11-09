close all; clear all; clc;
disp('Esercizio 4');

M = [4 -2/3 0; -1.5 3 0.5; 0 -0.4 -4];
z = [0.5; -7/3; -5/6]; 
x0 = 3 * ones(size(z));

disp('Metodo di Jacobi:');
d     = 1 ./ diag(M);
J     = diag(d) * (tril(M, -1) + triu(M, 1))
cJ    = z .* d
rhoJ  = abs(eigs(J, 1))
RinfJ = -log(rhoJ)

disp('Metodo di Gauss-Seidel:');
GS     = tril(M) \ [-triu(M, 1) z];
cGS    = GS(:, end), GS(:, end) = []
rhoGS  = abs(eigs(GS, 1))
RinfGS = -log(rhoGS)

xJk = x0; xGSk = x0;
for k = 1:5
    xJk  = J * xJk + cJ;
    xGSk = GS * xGSk + cGS;
end

fprintf('\nAppross. a 5 iterazioni con il metodo di Jacobi:');
xJk
fprintf('\nAppross. a 5 iterazioni con il metodo di Gauss-Seidel');
xGSk

fprintf('\nSoluzione del sistema e differenze relative con xJk e xGSk:');
xs = M \ z

normxs   = norm(xs, 'inf');
errRelJ  = norm(xJk - xs, 'inf') / normxs;
errRelGS = norm(xGSk - xs, 'inf') / normxs;