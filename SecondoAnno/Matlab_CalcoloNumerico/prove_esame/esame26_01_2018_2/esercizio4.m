close all; clear all; clc;
disp('Esercizio 4');

A = [9, -3, -1; -2, 9, 0; -2, 0, 9];
b = [5, -2, 3]';

invD   = diag (1 ./ diag(A));
J      = - invD * ( tril (A , -1) + triu (A ,1) ) ;
GS     = -tril( A ) \ triu(A, 1) ;
eigsJ  = eig( J )
eigsGS = eig( GS )
rhoJ   = max ( abs( eigsJ ) ) ; RinfJ = -log( rhoJ ) ;
rhoGS   = max( abs( eigsGS ) ) ; RinfGS = -log( rhoGS ) ;
fprintf ('\nrho (J) = %e, rho (GS) = %e', rhoJ , rhoGS );
fprintf ('\nRinf (J) = %f, Rinf (GS) = %f\n', RinfJ , RinfGS ) ;
% Soluzione con il metodo di Jacobi
tol = 1.0e-5; maxit = 100; x0 = zeros(3 ,1) ;
[ xJ , iterJ ] = jacobi (A , b , x0 , maxit , tol ) ;
fprintf ('\nNumero di iterazioni di Jacobi : iterJ = %d', iterJ ) ;
fprintf ('\nSoluzione calcolata con il metodo di Jacobi :') ;
xJ
% Soluzione con il metodo di Gauss - Seidel
iterGS = 0; xGS = x0 ; cGS = tril ( A ) \ b ; stop = 0;
while( ~ stop )
    iterGS = iterGS + 1;
    xold   = xGS ;
    xGS    = GS * xold + cGS ;
    stop   = (norm (xold - xGS , inf) < tol * norm (xGS , inf)) || (iterGS == maxit);
end
fprintf ('\nNumero di iterazioni di Gauss - Seidel : iterGS = %d', iterGS);
fprintf ('\nSoluzione calcolata con il metodo di Gauss - Seidel :');
xGS