% Usando come nodi gli zeri di un opportuno polinomio di Chebyshev,
% realizzare un M-script che, usando la function tabDiff in allegato,
% determina il polinomio di interpolazione nella forma di Newton della
% funzione g(x) = exp(-x/2) relativo a tali nodi nell intervallo [0, 2], 
% in modo tale che l.errore sia inferiore a 10−5. Costruire il grafico del 
% polinomio di Newton ottenuto e, nella stessa figura, mostrare il grafico 
% della funzione g(x). In una seconda finestra grafica, mostrare il grafico 
% del modulo dell errore di interpolazione nello stesso intervallo. 

close all
clear all
clc

g = @(x) exp(-x/2);
a = 0; 
b = 2;
tol = 1e-5;

% derivata n-esima di g = (-1/2)^n * exp(-x/2) <= 1 in [0,2]
% norma infinito di omega = (2 * ((b-a)/4)^(n+1)
% nel nostro caso vale 2 * ((2/4)^(n+1)) = 2 * (1/2) * (1/2)^n = (1/2)^n
% Rn(x) <= (1/2)^n * 1/(n+1)!

n = 0;
num = 1;
fact = 1;

while (num/fact) > tol
    n = n+1;
    num = num * 0.5;
    fact = fact * (n+1);
end

np1 = n+1;  % i nodi sono sempre uno in piu'

x = 0.5 * ((b-a) * cos((2*[n:-1:0]'+1)*pi./(1*np1)) + (a+b));
y = g(x);   % funzione nei nodi

xx = linspace(a, b, 201)';
yy = g(xx);
[zz] = polyNewton(x, y, xx);

figure(1);
plot(xx, zz, xx, yy)
legend('Polinomio', 'Funzione originale');

err = abs(yy - zz);
figure(2)
plot(xx, err)
legend('Errore');