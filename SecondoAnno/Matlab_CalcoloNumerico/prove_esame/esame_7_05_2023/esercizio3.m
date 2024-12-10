close all; clear all; clc;
disp('Esercizio 3');

format rat
A = [-1, -(2/3), 3; 4, 0, -5; 0, 3/4, -3]; 
b = [-2, -1, 0.5]';
[L, U, pidx] = lu(A, 'vector')
disp('Soluzione mediante fatt. LU con pivoting parziale:');
xLU = utrisol(U, ltrisol(L, b(pidx)))

% controllo dei risultati ottenuti nel quesito teorico
format short
xTeoria = (1/11) * [73/3, 94, 65/3];
diffMaxAbsSol = norm(xLU - xTeoria, 'inf');
fprintf('\nDifferenza assoluta massima in modulo della soluzione: /n');
disp(diffMaxAbsSol);

% Valutazione dell'affidabilita' della soluzione calcolata
residuo = b - A * xLU;
normaInfTermNoto = norm(b, 'inf');
normaInfResiduo = norm(residuo, 'inf');
fprintf('\nResiduo normalizzato in forma infinito:\n');
disp(residuo / normaInfTermNoto);

stimaErrore = utrisol(U, ltrisol(L, residuo(pidx)));
stimaNormaInvA = norm(stimaErrore, 'inf');
stimaCondA = norm(A, 'inf') * stimaNormaInvA
maggErrRel = stimaCondA * normaInfResiduo / normaInfTermNoto

% Calcolo della soluzione con fattorizzazione ortogonale
[Q, R] = qr(A)

format rat
xQR = utrisol(R, Q'*b)
format short

% differenze relative della soluzione teorica con funzione 'qr' da quella
% con funzione 'lu' e da quella teorica
DiffRelSolPar = abs(xQR - xLU) ./ abs(xLU)
DiffRelSolQR = abs(xQR - xTeoria) ./ abs(xTeoria)