function [x , fx , it ] = myNewton ( fname , fpname , x0 , tolx , tolf , maxit )
% MYNEWTON - Metodo di Newton per la soluzione di equazioni non lineari
% Determina iterativamente un ’ approssimazione xs della soluzione
% dell ’ equazione non lineare f(x) = 0 mediante il metodo di Newton .
% SYNOPSIS
% [x, fx , it] = newton (fname , fpname , x0 , tolx , tolf , maxit )
% INPUT
% fname ( string o func . handle ) - Function della funzione f
% fpname ( string o func . handle ) - Function della derivata prima f’ di f
% x0 ( double ) - Stima iniziale
% tolx ( double ) - Distanza minima fra iterati successivi
% tolf ( double ) - Soglia verso zero dei valori di f(x)
% maxit ( integer ) - Numero massimo di interazioni
% OUTPUT
% x ( double ) - Approssimazione della soluzione dell ’ equazione
% fx ( double ) - Valore della funzione in x
% it ( integer ) - Numero di iterazioni compiute fino all ’ arresto
%
tolfp = min( tolf , 10*eps ) ;
% Metodo di Newton
x = x0 ; fx = feval ( fname , x ) ; it = 0;
stop = ( abs( fx ) < tolf ) ;
while ( ~stop )
it = it + 1;
fpx = feval( fpname , x ) ;
if ( abs( fpx ) < tolfp ) , error ('|f''(xk)| troppo piccolo .'); end
d = fx / fpx ; x = x - d ; fx = feval( fname , x ) ;
stop = ( (abs( fx ) < tolf && abs( d ) < tolx * abs(x) ) ...
|| ( fx == 0) || ( it == maxit ) ) ;

end
if ( it >= maxit )
fprintf ('\ nRaggiunto il massimo numero di iterazioni .\n') ;
end
end % fine della function myNewton .m