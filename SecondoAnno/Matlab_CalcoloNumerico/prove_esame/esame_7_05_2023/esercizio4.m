close all; clear all; clc;
disp('Esercizio 4');

A = [7, 0, 2; 0, 7, 2; 1, 3, 7];
b = [2, -1, 1]';
invD = diag(1 ./ diag(A));

format rat
J  = -invD * (tril(A, -1) + triu(A, 1))
GS = -tril(A) \ triu(A, 1)

format short
eigsJ  = eig(J)
eigsGS = eig(GS) 
rhoJ   = max(abs(eigsJ));
RinfJ  = -log(rhoJ);
rhoGS  = max(abs(eigsGS));
RinfGS = -log(rhoGS);
fprintf('\nrho(J) = %e, rho(GS) = %e', rhoJ, rhoGS);
fprintf('\nRinf(J) = %f, Rinf(GS) = %f\n', RinfJ, RinfGS);

% Soluzione con il metodo di Gauss-Seidel
tol = 1.0e-6; maxit = 50; x0 = [2, -2, 2]';
[xGS, iterGS] = gaussSeidel(A, b, x0, maxit, tol);
fprintf('\nNumero di iterazioni di Gauss-Seidel: iterGS = %d', iterGS);
fprintf('\nSoluzione calcolata con il metodo di Gauss-Seidel:');
xGS

% Soluzione con il metodo di Jacobi
iterJ = 0; xJ = x0; cJ = b ./ diag(A); stop = 0;
while(~stop)
    iterJ = iterJ + 1;
    xold  = xJ;
    xJ    = J * xold + cJ;
    stop  = (norm(xold - xJ, inf) < tol*norm(xJ, inf)) || (iterJ == maxit);
end

fprintf('\nNumero di iterazioni di Jacobi: iterJ = %d', iterJ);
fprintf('\nSoluzione calcolata con il metodo di Jacobi:');
xJ