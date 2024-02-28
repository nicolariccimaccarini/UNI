function [ radiciReali ] = radiciGrado2( p2 )
%radiciGrado2 - Calcolo delle radici del polinomio reale di secondo grado
%   Calcolo le radici reali del polinomio
%   p2(x) = a2 * x^2 + a1 * x + a0
%
% SINTASSI
%       [ radici ] = radiciGrado2( p2 )
% INPUT
% p2 (double array) - p2 = [a2; a1; a0]
% OUTPUT
% ...

% controllo dell'input
if ( numel(p2) ~= 3 )
    error('Attenzione: vettore dei coefficienti non corretto');
elseif ( ~isnumeric( p2 ) )
    error('Formato errato del vettore dei coefficienti');
elseif ( ~any( p2 ) )
    % p2 identicamente nullo
else 
    p2 = p2(:);
end

Delta = p2(2)^2 - 4*p2(1)*p2(3);

if (Delta < 0)
    radiciReali = [];
elseif ( Delta == 0 )
    radiciReali = -p2(1) / (2*p2(1));   % x1 = x2 = -b/(2a) 
else  % Delta > 0
    radiciReali(1, 1) = (-p2(2) - sqrt(Delta)) / (2*p2(1))
    radiciReali(1, 1) = (-p2(2) + sqrt(Delta)) / (2*p2(1))
end

end