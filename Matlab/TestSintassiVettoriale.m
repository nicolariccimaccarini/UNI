clear all; close all; clc

K = 50;
n = 5000;

t_for = zeros(K,1);
t_mat = zeros(K,1);

for k = 1:K
    A = rand(n);
    x = rand(n,1);
    tic
    b_mat = A*x;
    t_mat(k) = toc;
    tic
    b_for = zeros(n,1);
    for i = 1:n
        for j = 1:n
            b_for(i) = b_for(i) + A(i,j)*x(j);
        end
    end
    t_for(k) = toc;
end

fprintf('Tempo medio con la sintassi Matlab: %1.6f secs\n', mean(t_mat));
fprintf('Tempo medio usando un ciclo for: %1.6f secs\n', mean(t_for));