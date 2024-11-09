close all; clear all; clc;
disp('Esercizio 3');

format rat 
A = [-1, 4, -3/2; 2, 1/2, 5; -3, 3/4, -2];
b = [-1, 0, 2]';
[Lpar, Rpar, ppar] = lu(A, 'vector');
disp('Soluzione mediante fattorizzazione LR con pivoting parziale: ');
xLU = utrisol(Rpar, ltrisol(Lpar, b(ppar)))
detA = prod(diag(Rpar))

format short
residuo = b - A*xLU;
normaInfTermNoto = norm(b, inf);
fprintf('\n Residuo normalizzato in norma infinito: \n');
disp(residuo / normaInfTermNoto);

format rat
% seconda parte: fattorizzazione QR
disp('Soluzione mediante fatt. QR:');
[Q, R] = qr(A);
xQR = utrisol(R, Q'*b)

format short
xTeoria = [-37, -12, 16]' / 35;
fprintf('\nDifferenze relative:');
fprintf ('\n| xLU - xTeoria | ./ | xTeoria |\n| xQR - xTeoria | ./ | xTeoria | :\n');
diffRel = abs([xLU - xTeoria, xQR - xTeoria]) ./ repmat(abs(xTeoria), 1, 2)