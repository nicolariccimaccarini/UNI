% Scrivere un M-script file che calcoli il polinomio pn(x) di grado n interpolante la
% funzione f(x)=log(x+1)+2*sin(3x+1)+0.25*pi*exp(-4/3) in [a, b] sugli n+1 nodi di
% Chebychev, utilizzando a tale scopo la TabDiff. Lo script generi poi in uno
% stesso grafico le due curve f(x) e pn(x), evidenziando nodi e punti di
% interpolazione e dotando il grafico di titolo, etichette degli assi e legenda. Lo
% script valuti infine l’errore relativo percentuale en(x) = |pn(x) − f(x)|/|f(x)|
% negli N punti di un campionamento uniforme dell’intervallo [a, b], disegnando
% il corrispondente grafico in una seconda finestra grafica. Utilizzare un
% campionamento di [a, b] con N ≥ 201. Evidenziare opportunamente in tale
% grafico i nodi di interpolazione. Provare lo script con n + 1 = 6, N = 201, [a, b] =
% [0.5, 3] e seme uguale a 4 per il generatore di numeri pseudo-casuali

close all
clear all
clc

rng(4); % fisso il seme del generatore dei numeri casuali
a = 0.5; b = 3; npunti = n+1;
N = 201;
C = 0.25 * pi * exp(-4/3);
fun = @(x) (log(x+1) + 2 * sin(3*x+1) + C);
t = cos((2 * [n:-1:0]' + 1)*  pi(2*n+2));   % nodi Cheb
xcheb = 0.5 * ((b-a) * t + a + b);          % nodi spostati e pronti all'uso
fcheb = fun(xcheb);
d = tabDiff(xcheb, fcheb);
s = 1; p5 = d(1);

for k=1 : n
    s = conv(s, [1, -xcheb(k)]);
    p5 = [0, p5] + d(k+1) * s;
end 

check = norm(p5 - polyfit(xcheb, fcheb, n), inf);
xx = linspace(a, b, N);
ff = fun(xx);
yy = polyval(p5, xx);
errel = abs(yy - ff)./abs(ff);
figure(1)
plot(xx, ff, '-b', xx, yy, '-r', xx([1,N]), [0,0], '-k',...
     xcheb, zeros(npunti,1), '.b', xcheb, fcheb, '*r');
legend('funzione f(x)', 'polinomio p_5', 'nodi di Cheb',...
       'Valore dei nodi', 'funzione in cheb');
figure(2)
plot(xx, errel, '-r', xx([1,N]), [0,0], '-k', ...
     xcheb, zeros(npunti,1), '.b');