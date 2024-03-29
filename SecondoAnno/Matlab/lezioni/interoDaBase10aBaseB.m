function [convertito] = interoDaBase10aBaseB(x, Base)
% interoDa10aBaseB - Conversione di un intero da base 10 a base beta > 1 qualunque
%   Conversione mediante il metodo delle divisioni successive del numero x
%   da base 10 a base beta.....
%
% SINOPSYS
%   [convertito] = interoDaBase10aBaseB(x, Base)
% INPUT
% x    (double)  - Numero INTERO in base 10 da convertire
% beta (integer) - Nuova base a cui convertiree il numero: 
%                  intero > 1
% OUTPUT 
% convertito (string) - Stringa contenente la rappresentazione del numero x
%                       in base beta
    
    % cifre = '0123456789ABCDEFGHILMNOPQ'
    % if (beta > 25)
    %     error ( sprintf('Base di beta = %d > 25 = base massima', beta));
    % elseif (beta ~= fix(beta))
    %     error('La base beta deve essere un intero')

   % controlli sull'input 
   if (x <= 0 | Base > 16 )
       fprintf("\n\n i parametri x o Base sono incorretti o non supportati\n\n");
   end

   convertito = '';
   d = '0123456789ABCDEF';

   r = 0;

   while (x ~= 0)
       r = x - fix(x/Base)*Base;
       x = fix(x/Base);
       convertito = strcat(d(r+1), convertito);
   end

disp(convertito);
end