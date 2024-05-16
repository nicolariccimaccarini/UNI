function [x, it] = newton_multipli(fname, fpname, x0, tolx, tolf, maxit,m)
tolfp = min( tolf, 10*eps );
% Metodo di Newton multipli
x = x0; fx = feval(fname, x); it = 0;
stop = ( abs(fx) < tolf );
while ( ~stop )
    it = it + 1;
    fpx = feval(fpname, x);
    if (abs(fpx) < tolfp), error('|f''(xk)| troppo piccolo.'); end
    d = fx / fpx; x = x - m*d; fx = feval(fname, x);
    stop = ( (abs(fx) < tolf && abs(d) < tolx*abs(x)) || (fx == 0) || (it == maxit) );
end
if ( it >= maxit )
    fprintf('\nRaggiunto il massimo numero di iterazioni.\n');
end
end