close all
clear all
clc

n  = 4; %n = 10;
An = spdiags([-ones(n, 1), 4*ones(n, 1), -ones(n, 1)], [-1, 0, 1], n, n);
bn = [3; 2*ones(n-2, 1); 3];    % termine noto

% matrice di Jacobi
% su carta J=D^-1(L+U) con L=-L U=-U su matlab posso fare J=-D^-1(L+U)
Jn    = -diag(1./diag(An)) + (tril(An, -1) + triu(An, -1));
%x(k+1)=Jx(k)+D^(-1)*b
cJn   = bn./diag(An) ;            
rhoJn = abs(eigs(Jn))
RJn   = -log(rhoJn)

% nostra versione di Jacobi
xk = zeros(n, 1);
for k=1 : 5
    xk = Jn * xk + cJn;
end

tol = 0.1e-5;
maxit = 5;
x0 = zeros(n, 1);
[x5Jn, itertJn] = jacobi(An, bn, x0, maxit, tol);
differel = norm(xk-x5Jn,'inf')/norm(x5Jn,'inf')
