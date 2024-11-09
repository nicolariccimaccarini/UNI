close all; clear all; clc
disp('Esercizio 3');

% prima parte
A = [1, 3, -1; 1, 2, 1/2; 5, 10, -1];
b = [-2, 1/2, -1]';

[L, R, p, detA] = gauss2(A);
x1 = solupper(R, solupper(L, b))

fprintf('\nNorma del residuo normalizzato = %g\n\n', ...
        norm(b - A*x1, inf) / norm(b, inf));

[L1, R1, P1] = lu(A)
disp('[L1, R1, P1] = lu(A)');
fprintf('\nmax(|L - L1|) = %g', max(abs(L(:) - L1(:))));
fprintf('\nmax(|R - R1|) = %g', max(abs(R(:) - R1(:))));
P = eye(3); P = P(p, :);
fprintf('\nmax(|L - L1|) = %g', max(abs(P(:) - P1(:))));

% seconda parte
n = 30;
normInfSol = zeros(n, 1);
normInfSol(1) = norm(x1, inf);
c = b(p); p2 = p(2);
for k = 2 : n
    c(p2) = 0.5 * c(p2);
    normInfSol(k) = norm(R \ (L \ c), inf);
end
plot(normInfSol, 'bo');