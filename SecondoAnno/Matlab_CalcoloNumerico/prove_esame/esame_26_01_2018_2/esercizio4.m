close all
clear all
clc

A = [9 -3 -1; -2 9 0; -2 0 9];
b = [5 -2 3]';
invD = diag(1./diag(A));
J = -invD * (tril(A, -1) + triu(A, 1));
GS = -tril(A) \ triu(A, 1);     % -inv(tril(A)) * triu(A)
eigsJ = eig(J)
eigsGS = eig(GS)
rhoJ = max(abs(eigsJ));
rhoGS = max(abs(eigsGS));
RinfJ = -log(rhoJ);
RingGS = -log(rhoGS);
fprintf('\n rho(J) = %e, rho(GS) = %e', rhoJ, rhoGS);
fprintf('\n RinfJ = %e, RinfGS = %e', RinfJ, RingGS);

%metodo di Jacobi
tol = 1e-5;
maxit = 100;
x0 = zeros(3,1);
[xJ, iterJ] = jacobi(A, b, x0, maxit, tol);
fprintf('\n Numero di iterazioni Jacobi = %d\n', iterJ);
fprintf('\n Soluzione con Jacobi: \n');
disp(xJ)

%soluzione con Gauss-Seidel
iterGS = 0; xGS = x0; cGS = tril(A)\b; stop = 0;
while (~ stop)
    iterGS = iterGS + 1;
    xold = xGS;
    xGS = GS * xold + cGS;
    stop = (norm(xold-xGS, inf) < tol*norm(xGS, inf) || (iterGS == maxit));
end
fprintf('\n Numero di iterazioni Gauss-Seidel = %d\n', iterGS);
fprintf('\n Soluzione con Gauss-Seidel: \n');
disp(xGS)