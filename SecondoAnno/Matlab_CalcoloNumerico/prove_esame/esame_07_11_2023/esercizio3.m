close all; clear all; clc;
disp('Esercizio 3');

format rat
A = [-1 3 -2; 2 1 4; 5 -2 8]; b = [1,-3, 2]';
[Lpar, Rpar, ppar] = lu(A, 'vector'); % N.B.: senza ’vector’, ppar e’ una matrice!!
disp('Soluzione mediante fatt. LR con pivoting parziale:');
xs = utrisol(Rpar, ltrisol(Lpar, b(ppar)))
% c = ltrisol(Lpar, b(ppar))
% xs = utrisol(Rpar, c)
detA = prod(diag(Rpar))

format short
residuo = b- A*xs;
normaInfTermNoto = norm(b, inf);
fprintf('\nResiduo normalizzato in norma infinito:\n');
disp(residuo / normaInfTermNoto);

% controllo dei risultati ottenuti nel quesito teorico
xTeoria = [52,-1,-31]' / 7;
diffMaxAbsSol = norm(xs- xTeoria, inf);
fprintf('\nDifferenza assoluta massima in modulo nella soluzione:\n');
disp(diffMaxAbsSol);

F = A(2:3, [1; 3]);
[Q, y] = planerot(F(:,1));
RF = Q*F
detF = prod(diag(RF))

% definita positivita’ mediante il metodo di Cholesky
B = 0.25*Rpar*Rpar'; c = [-0.75, 4.03,-6.17]';
[Lchol, nonpd] = chol(B, 'lower');
if ( ~nonpd )
    fprintf('\nMatrice B definita positiva.\n');
    fprintf('\nSi risolve Bx = c con il metodo di Cholesky:');
    xB = utrisol(Lchol', ltrisol( Lchol, c))
else
    fprintf('\nMatrice B non definita positiva: nonpd = %d.', nonpd);
    fprintf('\nSi risolve Bx = c con il metodo di Gauss con pivoting parziale:');
    xB = B \ c
end