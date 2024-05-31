close all 
clear all
clc

A = [1, 1, 1,; 0, 1, -1; 1, 1, 2];
b = [3, 0, 4]';
[Q, R] = qrfact(A);
fprintf('La soluzione di A vale \n');
% Ax = b --> Rx = Q'b
XQR = sollupper(R, Q'*b)
fprintf('\n Residuo normalizzato: %g\n', (b-A*XQR)/norm(b, 'inf'));

% Calcolol la matrice inversa
% AX = I --> QRX = I --> quindi RX c= Q' --> quindi X = R^-1 * Q'
[Rinv] = invupper(R);
Ainv = Rinv*Q';

% Controllo con qr matlab
[Qmat, Rmat] = qr(A)
fprintf('La soluzione vale: \n');
xm = sollupper(Rmat, Qmat'*b)
fprintf('\n Residuo normalizzato: %g\n', (b-A*xm)/norm(b, 'inf'));