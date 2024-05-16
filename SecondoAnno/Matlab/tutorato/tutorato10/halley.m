function [x, it] = halley(fname, fpname, fsname, x0, tolx, tolf, maxit)
tolfp = min( tolf, 10*eps );
tolfs = min(tolf,10*eps);
% Metodo di Halley
x = x0; fx = feval(fname, x); it = 0;
stop = ( abs(fx) < tolf );
while ( ~stop )
    it = it + 1;
    fpx = feval(fpname, x);
    if (abs(fpx) < tolfp)
        error('|f''(xk)| troppo piccolo.'); 
    end
    num = fx / fpx; 
    fsx=feval(fsname,x);
    if (abs(fsx) < tolfs)
        error('|f''''(xk)| troppo piccolo.'); 
    end
    d=num/(1-num*fsx/fpx);
    x = x - d; 
    fx = feval(fname, x);
    stop = ( (abs(fx) < tolf && abs(d) < tolx*abs(x)) || (fx == 0) || (it == maxit) );
end
if ( it >= maxit )
    fprintf('\nRaggiunto il massimo numero di iterazioni.\n');
end
end