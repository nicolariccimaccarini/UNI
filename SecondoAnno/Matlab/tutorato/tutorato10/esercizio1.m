close all
clear all
clc

x = [-5 -4 0 4 5]';
y = [-6 -5 7 0 3]';
n = 3;  % grado del polinomio
m = lenght(x);
A = zeros(m, n+1);
A(:, n+1) = ones(m, 1);

for i=n : -1 : 1
    A(:, i) = A(:, i+1).*x;
end

[alpha, res, r] = regressLineare(A, y);
zz = linspace(min(x), max(x));
pol = polyval(alpha, zz);

% grafico
plot(x, y, 'o', zz, pol, 'r');
legend('originale', 'polinomio');

fprintf('Somma dei minimi quadrati = %e\n', res);

% seconda parte
n = 1;
A2 = zeros(m, n+1);
A2(:, 2) = ones(m, 1);
A2(:, 1) = exp(x);
[alpha2, res, r] = regressLineare(A2, y);
% gli alpha sono da grado maggiore a minore
pol2 = alpha2(2) + alpha2(1) + exp(zz);
figure(2);
plot(x, y, 'o', zz, pol2, 'r');
legend('originale', 'polinomio exp(x)');
fprintf('A1 = %e\n A2 = %e\n', alpha(2), alpha(1))