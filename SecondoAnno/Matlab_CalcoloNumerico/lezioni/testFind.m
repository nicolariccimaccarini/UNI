rng('default')

n = 5;
A = fix( rand(n)*18 - 9 );

[idxi, idxj] = find( (A > -3) & (A < 2) );     % cerco gli elementi di -3<A<2