function [L, R, deter] = gauss1(A)
n = max(size(A));

for k=1 : n-1
    tol = eps * abs(A(k,k));
    if (abs(A(k,k)) < tol)
        error('fattorizzazione non effettuabile');
    else
        A((k+1):n, k) = A((k+1):n, k)/A(k,k);
        A((k+1):n, (k+1):n) = A((k+1):n, (k+1):n) - A((k+1):n, k) * A(k, (k+1):n);
    end
end

deter = prod(diag(A));
R = triu(A);
L = eye(n) + tril(A, -1);