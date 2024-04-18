close all
clear all
clc

A = [2, -1, 0; -1, 3, 1; 0, 1, 5];
b = [1, 3, 6]';
x = [0, 0, 0]';
maxit = 100;
tol = 1e-2;

% metodo di Jacobi
[xj, kj] = jacobi(A, b, x, maxit, tol);

fprintf('Soluzione con Jacobi %e\n', xj);
fprintf('Numero di iterazioni Jacobi %d\n', kj);

% raggio spettrale Jacobi
% J = D^(-1) * (L+U)
Dinv = diag(1./diag(A));
L = -tril(A, -1);
U = -triu(A, 1);
J = Dinv * (L+U);
eigJ = eig(J);      % eif -> funzione di matlab che mi tira fuori gli autovalori
                    % eigs -> quando ho matrici sparse
rhoJ = max(abs(eig(J)));

% velocita' di convergenza
RinfJ = -log(rhoJ);

fprintf('Raggio spettrale Jacobi = %e e velocita''di convergenza = %e\n', rhoJ, RinfJ);

% metodo Gauss-Seidel
[xg, kg] = gauss_seidel(A, b, x, maxit, tol);

fprintf('Soluzione con Gauss-Seidel %e\n', xg);
fprintf('Numero di iterazioni con Gauss-Seidel %d\n', kg);

% G = (D-L)^(-1) * U
% D = diag(diag(A));
G = tril(A) \ triu(A, 1);
eigGS = eig(G);
rhoGS = max(abs(eigGS));
Rinfg = -log(rhoGS);
fprintf('Raggio spettrale Gauss-Seidel = %e, velicita di convergenza = %e\n', rhoGS, Rinfg);

% metodo SOR
omega = 1.2;
[xs, ks] = sor(A, b, x, omega, maxit, tol);
fprintf('Soluzione con SOR %e\n', xs);
fprintf('Numero di iterazioni con SOR %d\n', ks);

% valore ottimale di omega
omegas = 2 / (1 + sqrt(1 - rhoJ^2));
% velocita di convergenza
Rinfs = -log(omegas-1);
fprintf('Valore ottimale di omega = %e, velocita di convergenza = %e\n', omegas, Rinfs);