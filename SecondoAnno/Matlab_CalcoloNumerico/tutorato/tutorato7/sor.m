function [x,k]=sor(A,b,x,omega,maxit,tol)
% metodo di gauss seidel estrapolato cioè il sor
%%%%%%%%%%%
D=spdiags(A,0);
n=size(A,1);
for k=1:maxit
    xtemp=x; %mi ricordo l'iterato
        for i=1:n
        x(i)=(-A(i,[1:i-1,i+1:n])*x([1:i-1,i+1:n])+b(i))/A(i,i);
        %la aprte finale contiene le componenti dell'iterato precedente
        x(i)=(1-omega)*xtemp(i)+omega*x(i); %questa è l' unica istruzione che cambia rispetto gauss_seidel
        end
        %%%%%%%%%%%%%%% ho messo il commento per poterla usare in primo_iter
%     fprintf('k=%d\t x=',k);
%     fprintf('%g   ',full(x));
%     fprintf('\n');
    if norm(xtemp-x, inf)<tol*norm(x,inf)
        break
    end
end
if k==maxit
    fprintf('Convergenza non raggiunta');
end