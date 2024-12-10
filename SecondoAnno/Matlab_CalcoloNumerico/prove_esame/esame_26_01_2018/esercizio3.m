close all
clear all
clc

A = [1 3 -1; 1 2 0.5; 5 10 -1];
b = [-2 0.5 -1]';

[L, R, p] = gauss2(A);
x1 = solupper(R, sollower(L, b(p))) % se P e' matrice allora P*b
fprintf('\n Norma del residuo normalizzato = %g \n', norm(b-a*x1, inf)/norm(b, inf));
% se norma del residuo normalizzato allora norma sotto e sopra
% se residuo normalizzato allora norma solo sotto\
[L1, R1, P1] = lu(A);
fprintf('\n max|L-L1| = %g\n', max(abs(L(:)-L1(:))));
fprintf('\n max|L-L1| = %g\n', max(abs(R(:)-R1(:))));
P = eye(3);
P = P(p ,:);
fprintf('\n max|L-L1| = %g\n', max(abs(P(:)-P1(:))));

% seconda parte
n = 30;
norminfsol = zeros(n, 1);
norminfsol(1) = norm(x1, inf);
for k=2 : n
    b = [-2 2^(-k) -1]';
    norminfsol(k) = norm(R\(L\b(p)), inf);
end 
plot(norminfsol, 'bo')