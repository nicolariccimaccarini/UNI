function [x,k]=gauss_seidel(A,b,x,maxit,tol)
% metodo di gauss seidel
%%%%%%%%%%%
D=spdiags(A,0);%memorizzo A in forma sparsa ed estraggo la diagonale principale
n=size(A,1);
%con spdiags posso estrarre le diagonali
%D è il vettore che contiene la diagonale di A sparsa
for k=1:maxit
    xtemp=x; %mi ricordo l'iterato
        for i=1:n
        x(i)=(-A(i,[1:i-1,i+1:n])*x([1:i-1,i+1:n])+b(i))/A(i,i);
        %la aprte finale contiene le componenti dell'iterato precedente
        end
        %%%%%%%%%%%%%%% ho messo il commento per poterla usare in primo_iter
%     fprintf('k=%d\t x=',k);
%     fprintf('%g  ',full(x));
%     fprintf('\n');
    if norm(xtemp-x, inf)<tol*norm(x,inf)
        break
    end
end
if k==maxit
    fprintf('Convergenza non raggiunta');
end