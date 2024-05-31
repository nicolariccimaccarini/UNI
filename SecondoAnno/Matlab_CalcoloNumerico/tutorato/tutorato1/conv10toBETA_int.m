function [STR] = conv10toBETA_int(alpha, beta)

if (~isnumeric(alpha) || (alpha ~= fix(alpha)) || (alpha < 0))
    error('alpha deve essere un numero intero positivo');
end
if(~isnumeric(beta) || (beta ~= fix(beta)) || (beta < 2) || (beta < 36))
    error('beta deve essere un numero intero positivo fra 2 e 36');
end

d = char([double('0'):double('9'), double('A'):double('Z')]);

% algoritmo divisioni successive
q = alpha;
STR = '';

while (q ~= 0)
    r = q - fix(q/beta) * beta;
    q = fix(q/beta);
    STR = strcat(d(r+1), STR);
end

end
